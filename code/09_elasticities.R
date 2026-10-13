# ==============================================================================
# 09_elasticities.R
#
# Demand elasticity and curvature before, during, and after the SOE.
# Responds to Tim (Sept 16, Sept 29) and Xiao (Sept 29): estimate how price
# sensitivity and the curvature of demand changed during the emergency, and
# connect the curvature change to the pass-through attenuation in Mechanism 2
# through Weyl and Fabinger (2013).
#
#   9.1  Period elasticities (pre / SOE / post): OLS, IV with own wholesale
#        cost, IV with within-chain out-of-state prices, OLS with product x
#        week FEs.
#   9.2  Curvature: quadratic in log price, interacted with period. Reports,
#        for each period, the elasticity at the period's mean price, the
#        Weyl-Fabinger inverse elasticity of marginal surplus 1/eps_ms, and
#        the implied monopoly pass-through with constant marginal cost.
#   9.3  State-specific period elasticities.
#
# ------------------------------------------------------------------------------
# SPECIFICATION (analogue of Butters, Sacks, and Seo 2025, eq. 3)
#
#   ln Q_ist = a_SOE*SOE_st + a_post*postSOE_st
#              + eta*x_ist + eta_SOE*(x_ist*SOE_st) + eta_post*(x_ist*postSOE_st)
#              + theta_{store x product x year} + phi_{product x week-of-year}
#
#   x_ist = ln p_ist minus its store-product-year mean (log real net price),
#   so a_SOE is the SOE shift in demand at the average price. Elasticity is
#   eta pre-SOE, eta + eta_SOE during, eta + eta_post after.
#
# Curvature (9.2) adds kappa*x^2 and its period interactions. The elasticity
# at log price x is e(x) = eta_r + 2*kappa_r*x. With D(p) = exp(a + eta*ln p
# + kappa*(ln p)^2):
#   (log D)''          = (2*kappa - e) / p^2      -> log-concave iff 2*kappa < e
#   1/eps_ms           = (e - 2*kappa) / e^2      (Weyl-Fabinger 2013; > 0 iff
#                                                  log-concave; = 1/e under
#                                                  constant elasticity)
#   rho (monopoly, constant MC) = 1 / (1 + 1/eps_ms)
#   rho (conduct theta held constant) = 1 / (1 + theta/eps_ms)
# A rise in 1/eps_ms from pre-SOE to SOE lowers pass-through for any theta > 0,
# which is the curvature mechanism Tim proposed for Mechanism 2.
#
# ------------------------------------------------------------------------------
# LIMITATIONS (carry into the manuscript)
#   - Butters et al. identify elasticity changes from twelve calendar months
#     repeated across years. Here there are three regimes (pre, SOE, post) and
#     the SOE occurs once, so the SOE elasticity is identified from price
#     variation within the SOE window (mostly promotions), not from repeated
#     cycles.
#   - Own-cost IV assumes wholesale cost changes are not driven by local demand
#     shocks. The within-chain out-of-state price IV (DellaVigna and Gentzkow
#     2019; Butters et al. 2025) assumes demand shocks are not common across
#     states; national pandemic shocks in a given week would violate this.
#     Column 4 (product x week FEs) absorbs common weekly shocks instead.
#   - Conduct theta is held constant across periods (working assumption).
#   - Standard errors clustered at the store level (ELAS_CLUSTER). G = 5 state
#     clusters is too few for IV inference.
#
# Depends on: panel_est, save_tex(), SAVE_CSV (from 02_build_panel.R / run_all.R)
#
# Outputs (tables_latex/):
#   27_tab_elasticity_period.tex      (9.1, regression)
#   27b_tab_elasticity_by_period.tex  (9.1, implied elasticities by period)
#   28_tab_elasticity_curvature.tex   (9.2, regression)
#   28b_tab_curvature_implied.tex     (9.2, e, 1/eps_ms, implied pass-through)
#   29_tab_elasticity_state.tex       (9.3)
#
# Outputs (figures/):
#   22_fig_elasticity_period.png
# ==============================================================================

message("Estimating demand elasticities and curvature (09_elasticities.R) ...")

if (!exists("ELAS_CLUSTER"))        ELAS_CLUSTER        <- ~ store_id
if (!exists("ELAS_USE_HAUSMAN_IV")) ELAS_USE_HAUSMAN_IV <- TRUE

PERIODS <- c("Pre-SOE", "SOE", "Post-SOE")


# ==============================================================================
# DATA
# ==============================================================================

el_data <- panel_est %>%
  filter(is.finite(lnp), is.finite(lnw), is.finite(upc_week_volume),
         upc_week_volume > 0) %>%
  mutate(
    lnq    = log(upc_week_volume),
    period = factor(case_when(SoE == 1L     ~ "SOE",
                              postSoE == 1L ~ "Post-SOE",
                              TRUE          ~ "Pre-SOE"),
                    levels = PERIODS),
    store_product_year = interaction(store_id, product, week_year, drop = TRUE),
    product_woy        = interaction(product, week_of_year, drop = TRUE)
  ) %>%
  group_by(store_product_year) %>%
  mutate(
    x   = lnp - mean(lnp),
    z_w = lnw - mean(lnw)
  ) %>%
  ungroup() %>%
  mutate(
    x_soe  = x * SoE,      x_post  = x * postSoE,
    x2     = x^2,
    x2_soe = x2 * SoE,     x2_post = x2 * postSoE,
    z_soe  = z_w * SoE,    z_post  = z_w * postSoE,
    z2     = z_w^2,
    z2_soe = z2 * SoE,     z2_post = z2 * postSoE
  )

# Within-chain, out-of-state mean of x for the same product and week
# (leave-own-state-out). Undefined for single-state chains (Retailer 5).
if (ELAS_USE_HAUSMAN_IV) {
  hausman <- el_data %>%
    group_by(retailer_id, product, week_seq, sst) %>%
    summarise(s_x = sum(x), n_x = n(), .groups = "drop") %>%
    group_by(retailer_id, product, week_seq) %>%
    mutate(tot_x = sum(s_x), tot_n = sum(n_x)) %>%
    ungroup() %>%
    mutate(h = if_else(tot_n > n_x, (tot_x - s_x) / (tot_n - n_x), NA_real_)) %>%
    select(retailer_id, product, week_seq, sst, h)

  el_data <- el_data %>%
    left_join(hausman, by = c("retailer_id", "product", "week_seq", "sst")) %>%
    mutate(h_soe = h * SoE, h_post = h * postSoE)
}

message(sprintf("Elasticity panel: %s obs; %s store-product-years.",
                format(nrow(el_data), big.mark = ","),
                format(n_distinct(el_data$store_product_year), big.mark = ",")))


# ==============================================================================
# HELPERS
# ==============================================================================

# Coefficient by name; IV models prefix endogenous regressors with "fit_".
cf <- function(b, nm) {
  if (nm %in% names(b)) return(unname(b[[nm]]))
  fit_nm <- paste0("fit_", nm)
  if (fit_nm %in% names(b)) return(unname(b[[fit_nm]]))
  0
}

# Delta-method estimate and SE for a scalar function of the coefficients
delta_est <- function(model, f) {
  b <- coef(model)
  V <- vcov(model)
  est  <- f(b)
  grad <- vapply(seq_along(b), function(k) {
    h  <- max(abs(b[k]) * 1e-6, 1e-8)
    bp <- b; bp[k] <- b[k] + h
    bm <- b; bm[k] <- b[k] - h
    (f(bp) - f(bm)) / (2 * h)
  }, numeric(1))
  c(estimate = est, se = sqrt(drop(t(grad) %*% V %*% grad)))
}

fmt_est_se <- function(est, se, digits = 3) {
  p <- 2 * pnorm(-abs(est / se))
  stars <- if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
  c(paste0(formatC(est, digits = digits, format = "f"), stars),
    paste0("(", formatC(se, digits = digits, format = "f"), ")"))
}

ELAS_DICT <- c(
  "x"       = "$x_{ist}$ (log price)",
  "x_soe"   = "$x_{ist} \\times SOE_{st}$",
  "x_post"  = "$x_{ist} \\times postSOE_{st}$",
  "x2"      = "$x^2_{ist}$",
  "x2_soe"  = "$x^2_{ist} \\times SOE_{st}$",
  "x2_post" = "$x^2_{ist} \\times postSOE_{st}$",
  "SoE"     = "$SOE_{st}$",
  "postSoE" = "$postSOE_{st}$"
)


# ==============================================================================
# 9.1 PERIOD ELASTICITIES
# ==============================================================================

m_el_ols <- feols(
  lnq ~ SoE + postSoE + x + x_soe + x_post | store_product_year + product_woy,
  data = el_data, cluster = ELAS_CLUSTER
)

m_el_iv_cost <- feols(
  lnq ~ SoE + postSoE | store_product_year + product_woy |
    x + x_soe + x_post ~ z_w + z_soe + z_post,
  data = el_data, cluster = ELAS_CLUSTER
)

m_el_iv_haus <- if (ELAS_USE_HAUSMAN_IV) {
  feols(
    lnq ~ SoE + postSoE | store_product_year + product_woy |
      x + x_soe + x_post ~ h + h_soe + h_post,
    data = el_data %>% filter(is.finite(h)), cluster = ELAS_CLUSTER
  )
} else NULL

# Product x week FEs absorb common weekly demand shocks (e.g., national panic
# buying). SOE levels are then identified only from staggered expiry.
m_el_ols_pw <- feols(
  lnq ~ SoE + postSoE + x + x_soe + x_post | store_product_year + product^week_fe,
  data = el_data, cluster = ELAS_CLUSTER
)

el_models <- list("(1) OLS"                  = m_el_ols,
                  "(2) IV: own cost"         = m_el_iv_cost,
                  "(3) IV: chain out-of-state price" = m_el_iv_haus,
                  "(4) OLS, product x week FE" = m_el_ols_pw)
el_models <- el_models[!vapply(el_models, is.null, logical(1))]

etable(el_models)

etable(
  el_models,
  tex     = TRUE,
  file    = "tables_latex/27_tab_elasticity_period.tex",
  title   = "Demand elasticity before, during, and after the SOE",
  label   = "tab:elasticity_period",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + ivf,
  dict    = ELAS_DICT,
  notes   = c(
    "Dependent variable: log weekly quantity sold, $\\ln Q_{ist}$.",
    "$x_{ist}$ is the log real net price minus its store-product-year mean.",
    "Pre-SOE elasticity: coefficient on $x_{ist}$; SOE and post-SOE elasticities add the interactions.",
    "Columns (1)-(3): store-product-year and product-by-week-of-year FEs. Column (4): store-product-year and product-by-week FEs.",
    "Column (2) instruments price with the store's own log wholesale cost (demeaned the same way).",
    "Column (3) instruments with the mean demeaned log price of the same chain's stores in other states, same product and week.",
    "Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/27_tab_elasticity_period.tex")

# Implied elasticity by period, each specification
period_elas <- purrr::imap_dfr(el_models, function(m, spec) {
  fns <- list(
    "Pre-SOE"  = function(b) cf(b, "x"),
    "SOE"      = function(b) cf(b, "x") + cf(b, "x_soe"),
    "Post-SOE" = function(b) cf(b, "x") + cf(b, "x_post"),
    "SOE minus pre" = function(b) cf(b, "x_soe")
  )
  purrr::imap_dfr(fns, function(f, per) {
    d <- delta_est(m, f)
    tibble::tibble(spec = spec, period = per, estimate = d[["estimate"]], se = d[["se"]])
  })
})

print(period_elas %>% mutate(across(c(estimate, se), ~ round(.x, 3))), n = Inf)
if (SAVE_CSV) write.csv(period_elas, "tables_csv/27b_elasticity_by_period.csv", row.names = FALSE)

period_elas_tbl <- period_elas %>%
  group_by(spec, period) %>%
  summarise(val = list(fmt_est_se(estimate, se)), .groups = "drop") %>%
  tidyr::unnest_wider(val, names_sep = "_") %>%
  tidyr::pivot_longer(c(val_1, val_2), names_to = "row", values_to = "cell") %>%
  tidyr::pivot_wider(names_from = spec, values_from = cell) %>%
  mutate(period = factor(period, levels = c(PERIODS, "SOE minus pre"))) %>%
  arrange(period, row) %>%
  mutate(Period = if_else(row == "val_1", as.character(period), "")) %>%
  select(Period, all_of(names(el_models)))

save_tex(
  kbl(period_elas_tbl, format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Implied price elasticity of demand by period",
      label   = "elasticity_by_period",
      align   = paste0("l", strrep("r", length(el_models)))) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(general = c(
      "Elasticities computed from Table~\\\\ref{tab:elasticity_period}. Standard errors (delta method) in parentheses.",
      "Signif. codes: ***: 0.01, **: 0.05, *: 0.1"),
      general_title = "", escape = FALSE),
  "27b_tab_elasticity_by_period.tex"
)

g_elas <- period_elas %>%
  filter(period %in% PERIODS) %>%
  mutate(period = factor(period, levels = PERIODS),
         spec   = factor(spec, levels = names(el_models))) %>%
  ggplot(aes(x = period, y = estimate, ymin = estimate - 1.96 * se,
             ymax = estimate + 1.96 * se, color = spec)) +
  geom_hline(yintercept = -1, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Price elasticity of demand",
       title = "Price elasticity of demand before, during, and after the SOE",
       subtitle = "95% CIs, store-clustered. Dashed line = unit elasticity.",
       color = NULL) +
  theme_bw() +
  theme(legend.position = "top", plot.subtitle = element_text(size = 8))

g_elas

ggsave("figures/22_fig_elasticity_period.png", g_elas, width = 9, height = 5, dpi = 300)
message("Saved: figures/22_fig_elasticity_period.png")


# ==============================================================================
# 9.2 CURVATURE
# ==============================================================================

m_curv_ols <- feols(
  lnq ~ SoE + postSoE + x + x_soe + x_post + x2 + x2_soe + x2_post |
    store_product_year + product_woy,
  data = el_data, cluster = ELAS_CLUSTER
)

m_curv_iv <- feols(
  lnq ~ SoE + postSoE | store_product_year + product_woy |
    x + x_soe + x_post + x2 + x2_soe + x2_post ~
    z_w + z_soe + z_post + z2 + z2_soe + z2_post,
  data = el_data, cluster = ELAS_CLUSTER
)

curv_models <- list("(1) OLS" = m_curv_ols, "(2) IV: own cost" = m_curv_iv)

etable(curv_models)

etable(
  curv_models,
  tex     = TRUE,
  file    = "tables_latex/28_tab_elasticity_curvature.tex",
  title   = "Curvature of demand before, during, and after the SOE",
  label   = "tab:elasticity_curvature",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + ivf,
  dict    = ELAS_DICT,
  notes   = c(
    "Dependent variable: $\\ln Q_{ist}$. Quadratic in $x_{ist}$, the demeaned log real net price.",
    "Column (2) instruments $x$, $x^2$, and their period interactions with the demeaned log wholesale",
    "cost, its square, and their period interactions.",
    "FEs: store-product-year and product-by-week-of-year. Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/28_tab_elasticity_curvature.tex")

# Mean demeaned log price in each period (where the elasticity is evaluated)
xbar <- el_data %>%
  group_by(period) %>%
  summarise(xbar = mean(x), .groups = "drop") %>%
  { setNames(.$xbar, as.character(.$period)) }

curv_pieces <- function(b, per) {
  eta   <- cf(b, "x")  + switch(per, "Pre-SOE" = 0, "SOE" = cf(b, "x_soe"),  "Post-SOE" = cf(b, "x_post"))
  kappa <- cf(b, "x2") + switch(per, "Pre-SOE" = 0, "SOE" = cf(b, "x2_soe"), "Post-SOE" = cf(b, "x2_post"))
  e <- eta + 2 * kappa * xbar[[per]]
  list(e = e, kappa = kappa, inv_ems = (e - 2 * kappa) / e^2)
}

curv_implied <- purrr::imap_dfr(curv_models, function(m, spec) {
  out <- purrr::map_dfr(PERIODS, function(per) {
    stats <- list(
      "Elasticity at period mean price, e" = function(b) curv_pieces(b, per)$e,
      "Curvature, kappa"                    = function(b) curv_pieces(b, per)$kappa,
      "Inverse elasticity of marginal surplus, 1/eps_ms" = function(b) curv_pieces(b, per)$inv_ems,
      "Implied pass-through, monopoly (theta = 1)" =
        function(b) 1 / (1 + curv_pieces(b, per)$inv_ems)
    )
    purrr::imap_dfr(stats, function(f, stat) {
      d <- delta_est(m, f)
      tibble::tibble(spec = spec, period = per, stat = stat,
                     estimate = d[["estimate"]], se = d[["se"]])
    })
  })
  # Change in 1/eps_ms from pre-SOE to SOE: > 0 means demand became more
  # log-concave, which lowers pass-through for any conduct theta > 0.
  d_chg <- delta_est(m, function(b) curv_pieces(b, "SOE")$inv_ems -
                                    curv_pieces(b, "Pre-SOE")$inv_ems)
  bind_rows(out, tibble::tibble(spec = spec, period = "SOE minus pre",
                                stat = "Inverse elasticity of marginal surplus, 1/eps_ms",
                                estimate = d_chg[["estimate"]], se = d_chg[["se"]]))
})

message("Curvature and implied pass-through by period:")
print(curv_implied %>% mutate(across(c(estimate, se), ~ round(.x, 3))), n = Inf)
if (SAVE_CSV) write.csv(curv_implied, "tables_csv/28b_curvature_implied.csv", row.names = FALSE)

curv_tbl <- curv_implied %>%
  group_by(spec, stat, period) %>%
  summarise(val = list(fmt_est_se(estimate, se)), .groups = "drop") %>%
  tidyr::unnest_wider(val, names_sep = "_") %>%
  tidyr::pivot_longer(c(val_1, val_2), names_to = "row", values_to = "cell") %>%
  mutate(col = paste(spec, period, sep = " | ")) %>%
  select(stat, row, col, cell) %>%
  tidyr::pivot_wider(names_from = col, values_from = cell) %>%
  mutate(stat = factor(stat, levels = c("Elasticity at period mean price, e",
                                        "Curvature, kappa",
                                        "Inverse elasticity of marginal surplus, 1/eps_ms",
                                        "Implied pass-through, monopoly (theta = 1)"))) %>%
  arrange(stat, row) %>%
  mutate(Statistic = if_else(row == "val_1", as.character(stat), "")) %>%
  mutate(Statistic = dplyr::recode(Statistic,
    "Elasticity at period mean price, e" = "Elasticity at period mean price, $e$",
    "Curvature, kappa" = "Curvature, $\\kappa$",
    "Inverse elasticity of marginal surplus, 1/eps_ms" = "$1/\\epsilon_{ms}$",
    "Implied pass-through, monopoly (theta = 1)" = "Implied pass-through, $\\theta = 1$"))

curv_cols <- as.vector(outer(names(curv_models), c(PERIODS, "SOE minus pre"),
                             paste, sep = " | "))
curv_cols <- curv_cols[order(match(sub(" \\|.*", "", curv_cols), names(curv_models)))]
curv_cols <- curv_cols[curv_cols %in% names(curv_tbl)]
curv_tbl  <- curv_tbl %>% select(Statistic, all_of(curv_cols)) %>%
  mutate(across(-Statistic, ~ tidyr::replace_na(.x, "")))

header_counts <- table(factor(sub(" \\|.*", "", curv_cols), levels = names(curv_models)))
names(curv_tbl) <- c("Statistic", sub(".*\\| ", "", curv_cols))

save_tex(
  kbl(curv_tbl, format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Demand curvature and implied pass-through by period",
      label   = "curvature_implied",
      align   = paste0("l", strrep("r", length(curv_cols)))) %>%
    add_header_above(c(" " = 1, as.list(header_counts) %>% unlist())) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(general = c(
      "From Table~\\\\ref{tab:elasticity_curvature}. $e = \\\\eta_r + 2\\\\kappa_r \\\\bar{x}_r$ is the elasticity at the period's mean demeaned log price.",
      "$1/\\\\epsilon_{ms} = (e - 2\\\\kappa)/e^2$ (Weyl and Fabinger 2013); positive values mean log-concave demand.",
      "Implied pass-through $= 1/(1 + 1/\\\\epsilon_{ms})$ for a monopolist with constant marginal cost; with conduct",
      "$\\\\theta$ held constant it is $1/(1 + \\\\theta/\\\\epsilon_{ms})$. ``SOE minus pre'' is the change in $1/\\\\epsilon_{ms}$.",
      "Delta-method standard errors in parentheses. Signif. codes: ***: 0.01, **: 0.05, *: 0.1"),
      general_title = "", escape = FALSE),
  "28b_tab_curvature_implied.tex"
)


# ==============================================================================
# 9.3 STATE-SPECIFIC PERIOD ELASTICITIES
# ==============================================================================

el_states <- sort(unique(as.character(el_data$sst)))
for (el_st in el_states) {
  el_ind <- as.integer(el_data$sst == el_st)
  el_data[[paste0("x_",  el_st)]] <- el_data$x      * el_ind
  el_data[[paste0("xs_", el_st)]] <- el_data$x_soe  * el_ind
  el_data[[paste0("xp_", el_st)]] <- el_data$x_post * el_ind
  el_data[[paste0("z_",  el_st)]] <- el_data$z_w    * el_ind
  el_data[[paste0("zs_", el_st)]] <- el_data$z_soe  * el_ind
  el_data[[paste0("zp_", el_st)]] <- el_data$z_post * el_ind
}

endog_state <- paste(c(paste0("x_", el_states), paste0("xs_", el_states), paste0("xp_", el_states)),
                     collapse = " + ")
inst_state  <- paste(c(paste0("z_", el_states), paste0("zs_", el_states), paste0("zp_", el_states)),
                     collapse = " + ")
lev_state   <- "i(sst, SoE) + i(sst, postSoE)"

m_state_ols <- feols(
  as.formula(paste("lnq ~", lev_state, "+", endog_state,
                   "| store_product_year + product_woy")),
  data = el_data, cluster = ELAS_CLUSTER
)

m_state_iv <- feols(
  as.formula(paste("lnq ~", lev_state, "| store_product_year + product_woy |",
                   endog_state, "~", inst_state)),
  data = el_data, cluster = ELAS_CLUSTER
)

state_elas <- purrr::imap_dfr(list("OLS" = m_state_ols, "IV: own cost" = m_state_iv),
                              function(m, spec) {
  purrr::map_dfr(el_states, function(s) {
    fns <- list(
      "Pre-SOE"  = function(b) cf(b, paste0("x_", s)),
      "SOE"      = function(b) cf(b, paste0("x_", s)) + cf(b, paste0("xs_", s)),
      "Post-SOE" = function(b) cf(b, paste0("x_", s)) + cf(b, paste0("xp_", s))
    )
    purrr::imap_dfr(fns, function(f, per) {
      d <- delta_est(m, f)
      tibble::tibble(spec = spec, state = s, period = per,
                     estimate = d[["estimate"]], se = d[["se"]])
    })
  })
})

print(state_elas %>% mutate(across(c(estimate, se), ~ round(.x, 3))), n = Inf)
if (SAVE_CSV) write.csv(state_elas, "tables_csv/29_elasticity_state.csv", row.names = FALSE)

state_tbl <- state_elas %>%
  group_by(spec, state, period) %>%
  summarise(val = list(fmt_est_se(estimate, se)), .groups = "drop") %>%
  tidyr::unnest_wider(val, names_sep = "_") %>%
  tidyr::pivot_longer(c(val_1, val_2), names_to = "row", values_to = "cell") %>%
  mutate(col = paste(spec, period, sep = " | ")) %>%
  select(state, row, col, cell) %>%
  tidyr::pivot_wider(names_from = col, values_from = cell) %>%
  arrange(state, row) %>%
  mutate(State = if_else(row == "val_1", state, ""))

state_cols <- as.vector(t(outer(c("OLS", "IV: own cost"), PERIODS, paste, sep = " | ")))
state_tbl  <- state_tbl %>% select(State, all_of(state_cols))
names(state_tbl) <- c("State", rep(PERIODS, 2))

save_tex(
  kbl(state_tbl, format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "State-specific price elasticity of demand by period",
      label   = "elasticity_state",
      align   = "lrrrrrr") %>%
    add_header_above(c(" " = 1, "OLS" = 3, "IV: own cost" = 3)) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(general = c(
      "Same specification as Table~\\\\ref{tab:elasticity_period}, columns (1)-(2), with the price terms and SOE levels interacted with state.",
      "Delta-method standard errors (store-clustered) in parentheses. Signif. codes: ***: 0.01, **: 0.05, *: 0.1"),
      general_title = "", escape = FALSE),
  "29_tab_elasticity_state.tex"
)

message("Elasticity and curvature analysis complete.")

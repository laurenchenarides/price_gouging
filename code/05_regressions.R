# ==============================================================================
# 05_regressions.R
#
# Empirical Model; Results: Retail Prices; Results: Markups
#   5.1  Price level regressions (5.1a: state heterogeneity)
#   5.1b Wild cluster bootstrap robustness (price)
#   5.2  Margin level regressions (5.2a: state heterogeneity)
#   5.2b Wild cluster bootstrap robustness (margin)
#   5.3  Cost regressions (accounting check)
#   5.4  Robustness requested in review (Sept 2026 comments):
#        5.4a Excluding bananas
#        5.4b Post-SOE window capped at 26 and 52 weeks after expiry
#        5.4c State heterogeneity with capped post-SOE windows (Mississippi)
#        5.4d Event study around SOE activation
#        5.4e Leave-one-state-out jackknife
#        Within-R2 is added to Tables 08 and 10 (fitstat wr2).
#
# Specification:
#   P_ist = alpha + beta1*SOE_st + beta2*postSOE_st + gamma_j + delta_i
#
# Fixed effects: product and store. No week FEs (collinear with SOE indicator).
# Standard errors: clustered at the state level (enforcement varies at
#   state-week level; allows arbitrary within-state serial correlation).
# Robustness: wild cluster bootstrap with Webb (2023) weights (preferred
#   for small G; here G = 5 states).
#
# Depends on: panel_est, save_tex() (from 02_build_panel.R)
#
# Outputs (tables_latex/):
#   08_tab_price_reg.tex
#   09_tab_price_reg_state_heterog.tex
#   08b_tab_price_wcb.tex
#   10_tab_margin_reg.tex
#   11_tab_margin_reg_state_heterog.tex
#   10b_tab_margin_wcb.tex
#   10c_tab_cost_reg.tex
#   10d_tab_cost_reg_state_heterog.tex
#   08c_tab_price_robust.tex                (5.4a, 5.4b)
#   10e_tab_margin_robust.tex               (5.4a, 5.4b)
#   09b_tab_price_state_heterog_capped.tex  (5.4c)
#   11b_tab_margin_state_heterog_capped.tex (5.4c)
#   08d_tab_loso_jackknife.tex              (5.4e)
#
# Outputs (figures/):
#   08_fig_price_coef_baseline.png
#   09_fig_price_coef_state_heterog.png
#   10_fig_margin_coef_prepost.png
#   11_fig_margin_coef_state_heterog.png
#   08b_fig_price_event_study.png           (5.4d)
# ==============================================================================

# remotes::install_github("s3alfisc/summclust")
# remotes::install_github("s3alfisc/fwildclusterboot")

library(fwildclusterboot)

message("Estimating price and margin level regressions ...")

reg_data <- panel_est %>%
  filter(is.finite(p_ist), is.finite(margin_nom))


# ==============================================================================
# HELPER: wild cluster bootstrap for a single parameter
# ==============================================================================
# Uses Webb (2023) weights, recommended for G < 10.
# Returns a one-row tibble with the OLS estimate and bootstrap CI / p-value.
# clustid must be a column name present in the model's data.
# ==============================================================================

run_wcb <- function(model, param, B = 9999, seed = 42) {
  set.seed(seed)
  dqrng::dqset.seed(seed)
  bt <- boottest(
    model,
    param    = param,
    B        = B,
    clustid  = "sst",
    type     = "webb",
    conf_int = TRUE
  )
  tibble::tibble(
    term     = param,
    estimate = coef(model)[param],
    ci_low   = bt$conf_int[1],
    ci_high  = bt$conf_int[2],
    p_boot   = bt$p_val
  )
}

format_wcb_table <- function(wcb_df, term_labels) {
  wcb_df %>%
    mutate(
      term     = dplyr::recode(term, !!!term_labels),
      estimate = formatC(estimate, digits = 3, format = "f"),
      ci       = paste0("[", formatC(ci_low,  digits = 3, format = "f"), ", ",
                        formatC(ci_high, digits = 3, format = "f"), "]"),
      p_boot   = formatC(p_boot, digits = 3, format = "f")
    ) %>%
    select(Term = term, Estimate = estimate,
           `95\\% Bootstrap CI` = ci, `Bootstrap $p$` = p_boot)
}


# ==============================================================================
# 5.1 PRICE LEVEL REGRESSIONS
# ==============================================================================

m_price_soe <- feols(
  p_ist ~ SoE | product + store_id,
  data = reg_data, cluster = ~ sst
)

m_price_prepost <- feols(
  p_ist ~ SoE + postSoE | product + store_id,
  data = reg_data, cluster = ~ sst
)

etable(list("(1)" = m_price_soe, "(2)" = m_price_prepost))

etable(
  list("(1)" = m_price_soe, "(2)" = m_price_prepost),
  tex     = TRUE,
  file    = "tables_latex/08_tab_price_reg.tex",
  title   = "Price regressions: SOE and post-SOE on nominal prices",
  label   = "tab:price_reg",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2 + wr2,
  dict    = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$"),
  notes   = c(
    "Dependent variable: nominal retail price $p_{ist}$ (dollars per unit or pound).",
    "Fixed effects: product ($\\gamma_j$) and store ($\\delta_i$).",
    "Standard errors clustered at the state level.",
    "See Table~\\ref{tab:price_wcb} for wild cluster bootstrap inference."
  )
)
message("Saved: tables_latex/08_tab_price_reg.tex")


# ------------------------------------------------------------------------------
# 5.1a State heterogeneity in price effects
# ------------------------------------------------------------------------------

m_price_state_het <- feols(
  p_ist ~ 0 + i(sst, SoE) + i(sst, postSoE) | product + store_id,
  data = reg_data, cluster = ~ sst
)

# Custom interleaved coef/SE table for state heterogeneity
build_state_interleaved <- function(model, during_col = "During SOE", post_col = "Post-SOE") {
  broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl(":SoE$|:postSoE$", term)) %>%
    mutate(
      state  = sub("sst::([A-Z]+):.*", "\\1", term),
      period = if_else(grepl(":SoE$", term), during_col, post_col),
      stars  = case_when(
        p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",  TRUE            ~ ""
      ),
      coef_str = paste0(formatC(estimate,   digits = 3, format = "f"), stars),
      se_str   = paste0("(", formatC(std.error, digits = 4, format = "f"), ")")
    ) %>%
    select(state, period, coef_str, se_str) %>%
    pivot_wider(names_from = period,
                values_from = c(coef_str, se_str),
                names_glue  = "{period}_{.value}") %>%
    arrange(state) %>%
    rename(State = state) %>%
    mutate(row_type = "coef") %>%
    bind_rows(mutate(., row_type = "se")) %>%
    arrange(State, row_type) %>%
    mutate(
      display_state = if_else(row_type == "coef", State, ""),
      !!during_col := if_else(row_type == "coef",
                              .data[[paste0(during_col, "_coef_str")]],
                              .data[[paste0(during_col, "_se_str")]]),
      !!post_col   := if_else(row_type == "coef",
                              .data[[paste0(post_col, "_coef_str")]],
                              .data[[paste0(post_col, "_se_str")]])
    ) %>%
    select(display_state, all_of(c(during_col, post_col))) %>%
    rename(State = display_state)
}

price_state_interleaved <- build_state_interleaved(m_price_state_het)

save_tex(
  kbl(price_state_interleaved,
      format = "latex", booktabs = TRUE,
      caption = "State heterogeneity in SOE and post-SOE price effects (nominal retail price)",
      label   = "tab:price_reg_state_heterog",
      align   = "lrr", escape = FALSE) %>%
    add_header_above(c(" " = 1, "Nominal retail price $p_{ist}$" = 2), escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = c(
        "Dependent variable: nominal retail price $p_{ist}$.",
        "Fixed effects: product and store.",
        "Standard errors clustered at the state level ($G = 5$) in parentheses.",
        "Signif. codes: ***: 0.01, **: 0.05, *: 0.1"
      ),
      general_title = "", escape = FALSE
    ),
  "09_tab_price_reg_state_heterog.tex"
)

# Coefficient plots
price_coef_df <- bind_rows(
  broom::tidy(m_price_prepost, conf.int = TRUE) %>%
    filter(term %in% c("SoE", "postSoE")) %>%
    mutate(model = "Baseline",
           term  = recode(term, SoE = "During SOE", postSoE = "Post-SOE")),
  broom::tidy(m_price_state_het, conf.int = TRUE) %>%
    filter(grepl(":SoE$|:postSoE$", term)) %>%
    mutate(
      state  = sub("sst::([A-Z]+):.*", "\\1", term),
      period = if_else(grepl(":SoE$", term), "During SOE", "Post-SOE"),
      model  = "State heterogeneity",
      term   = paste0(state, " (", period, ")")
    )
)

g_price_coef <- ggplot(
  price_coef_df %>% filter(model == "Baseline"),
  aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  geom_text(aes(label = round(estimate, 3)), vjust = -1.0, size = 3.5) +
  labs(x = NULL, y = "Coefficient (nominal price, $)",
       title = "SOE and post-SOE effects on nominal retail price") +
  theme_bw()

g_price_coef

ggsave("figures/08_fig_price_coef_baseline.png", g_price_coef,
       width = 7, height = 5, dpi = 300)
message("Saved: figures/08_fig_price_coef_baseline.png")

g_price_state <- ggplot(
  price_coef_df %>% filter(model == "State heterogeneity"),
  aes(x = state, y = estimate, ymin = conf.low, ymax = conf.high,
      color = period, shape = period)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_text(aes(label = round(estimate, 3)),
            position = position_dodge(width = 0.5), vjust = -1.0, size = 3.2,
            show.legend = FALSE) +
  scale_shape_manual(values = c("During SOE" = 16, "Post-SOE" = 17)) +
  labs(x = "State", y = "Coefficient (nominal price, $)",
       title = "State-specific SOE and post-SOE effects on nominal retail price",
       color = NULL, shape = NULL) +
  theme_bw()

g_price_state

ggsave("figures/09_fig_price_coef_state_heterog.png", g_price_state,
       width = 9, height = 5, dpi = 300)
message("Saved: figures/09_fig_price_coef_state_heterog.png")

broom::tidy(m_price_state_het, conf.int = TRUE) %>%
  filter(grepl(":SoE$|:postSoE$", term)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# ==============================================================================
# 5.1b ROBUSTNESS: WILD CLUSTER BOOTSTRAP — PRICE
# ==============================================================================
# Complements state-clustered SEs. Webb (2023) weights preferred for G < 10.
# Applied to the baseline pooled model (m_price_prepost). The state-
# heterogeneity model uses the same state-level clustering; per-state
# bootstrap CIs are available on request but are not reported here.
# ==============================================================================

message("Running wild cluster bootstrap for price regressions (B = 9999) ...")
t0_price <- proc.time()

price_wcb <- bind_rows(
  run_wcb(m_price_prepost, "SoE"),
  run_wcb(m_price_prepost, "postSoE")
)

message(sprintf("Price bootstrap done in %.1f seconds.", (proc.time() - t0_price)["elapsed"]))

price_wcb_tbl <- format_wcb_table(
  price_wcb,
  term_labels = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$")
)

if (SAVE_CSV) write.csv(price_wcb, "tables_csv/08b_tab_price_wcb.csv", row.names = FALSE)

save_tex(
  kbl(price_wcb_tbl,
      format  = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Wild cluster bootstrap inference for SOE and post-SOE price effects. OLS estimates from the baseline model (Table~\\ref{tab:price_reg}); confidence intervals and $p$-values from wild cluster bootstrap with Webb (2023) weights ($B = 9{,}999$ replications, $G = 5$ state clusters).",
      label   = "tab:price_wcb",
      align   = "lrrr") %>%
    kable_styling(latex_options = c("hold_position")),
  "08b_tab_price_wcb.tex"
)
message("Saved: tables_latex/08b_tab_price_wcb.tex")


# ==============================================================================
# 5.2 MARGIN LEVEL REGRESSIONS
# ==============================================================================
# Outcome: margin_nom = p_ist - w_ist (nominal dollar margin).
# Same FE and clustering as price regressions.
# ==============================================================================

message("Estimating margin level regressions ...")

m_margin_soe <- feols(
  margin_nom ~ SoE | product + store_id,
  data = reg_data, cluster = ~ sst
)

m_margin_prepost <- feols(
  margin_nom ~ SoE + postSoE | product + store_id,
  data = reg_data, cluster = ~ sst
)

etable(list("(1)" = m_margin_soe, "(2)" = m_margin_prepost))

etable(
  list("(1)" = m_margin_soe, "(2)" = m_margin_prepost),
  tex     = TRUE,
  file    = "tables_latex/10_tab_margin_reg.tex",
  title   = "Margin regressions: SOE and post-SOE effects on nominal dollar margin",
  label   = "tab:margin_reg",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2 + wr2,
  dict    = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$"),
  headers = list("Nominal margin $M_{ist}$" = 2),
  notes   = c(
    "Dependent variable: nominal dollar margin $M_{ist} = p_{ist} - w_{ist}$.",
    "Column (1): SOE only. Column (2): SOE and post-SOE.",
    "Fixed effects: product and store.",
    "Standard errors clustered at the state level.",
    "See Table~\\ref{tab:margin_wcb} for wild cluster bootstrap inference."
  )
)
message("Saved: tables_latex/10_tab_margin_reg.tex")


# ------------------------------------------------------------------------------
# 5.2a State heterogeneity in margin effects
# ------------------------------------------------------------------------------

m_margin_state_het <- feols(
  margin_nom ~ 0 + i(sst, SoE) + i(sst, postSoE) | product + store_id,
  data = reg_data, cluster = ~ sst
)

margin_state_interleaved <- build_state_interleaved(m_margin_state_het)

save_tex(
  kbl(margin_state_interleaved,
      format = "latex", booktabs = TRUE,
      caption = "State heterogeneity in SOE and post-SOE margin effects (nominal dollar margin)",
      label   = "tab:margin_reg_state_heterog",
      align   = "lrr", escape = FALSE) %>%
    add_header_above(c(" " = 1, "Nominal dollar margin $M_{ist}$" = 2), escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = c(
        "Dependent variable: nominal dollar margin $M_{ist} = p_{ist} - w_{ist}$.",
        "Fixed effects: product and store.",
        "Standard errors clustered at the state level ($G = 5$) in parentheses.",
        "Signif. codes: ***: 0.01, **: 0.05, *: 0.1"
      ),
      general_title = "", escape = FALSE
    ),
  "11_tab_margin_reg_state_heterog.tex"
)

margin_coef_df <- broom::tidy(m_margin_prepost, conf.int = TRUE) %>%
  filter(term %in% c("SoE", "postSoE")) %>%
  mutate(term = recode(term, SoE = "During SOE", postSoE = "Post-SOE"))

g_margin_coef <- ggplot(margin_coef_df,
                        aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  geom_text(aes(label = round(estimate, 3)), vjust = -1.0, size = 3.5) +
  labs(x = NULL, y = "Coefficient (nominal margin, $)",
       title = "SOE and post-SOE effects on nominal dollar margin") +
  theme_bw()

g_margin_coef

ggsave("figures/10_fig_margin_coef_prepost.png", g_margin_coef,
       width = 7, height = 5, dpi = 300)
message("Saved: figures/10_fig_margin_coef_prepost.png")

margin_state_coef_df <- broom::tidy(m_margin_state_het, conf.int = TRUE) %>%
  filter(grepl(":SoE$|:postSoE$", term)) %>%
  mutate(
    state  = sub("sst::([^:]+):.*", "\\1", term),
    period = factor(if_else(grepl(":SoE$", term), "During SOE", "Post-SOE"),
                    levels = c("During SOE", "Post-SOE"))
  )

g_margin_state_coef <- ggplot(margin_state_coef_df,
                              aes(x = state, y = estimate, ymin = conf.low, ymax = conf.high,
                                  color = period, shape = period)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_text(aes(label = round(estimate, 3)),
            position = position_dodge(width = 0.5),
            vjust = -1.0, size = 3.2, show.legend = FALSE) +
  labs(x = "State", y = "Coefficient (nominal margin, $)",
       title = "State-specific SOE and post-SOE effects on nominal dollar margin",
       color = NULL, shape = NULL) +
  theme_bw()

g_margin_state_coef

ggsave("figures/11_fig_margin_coef_state_heterog.png", g_margin_state_coef,
       width = 9, height = 5, dpi = 300)
message("Saved: figures/11_fig_margin_coef_state_heterog.png")

broom::tidy(m_margin_state_het, conf.int = TRUE) %>%
  filter(grepl(":SoE$|:postSoE$", term)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# ==============================================================================
# 5.2b ROBUSTNESS: WILD CLUSTER BOOTSTRAP — MARGIN
# ==============================================================================

message("Running wild cluster bootstrap for margin regressions (B = 9999) ...")
t0_margin <- proc.time()

margin_wcb <- bind_rows(
  run_wcb(m_margin_prepost, "SoE"),
  run_wcb(m_margin_prepost, "postSoE")
)

message(sprintf("Margin bootstrap done in %.1f seconds.", (proc.time() - t0_margin)["elapsed"]))

margin_wcb_tbl <- format_wcb_table(
  margin_wcb,
  term_labels = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$")
)

if (SAVE_CSV) write.csv(margin_wcb, "tables_csv/10b_tab_margin_wcb.csv", row.names = FALSE)

save_tex(
  kbl(margin_wcb_tbl,
      format  = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Wild cluster bootstrap inference for SOE and post-SOE margin effects. OLS estimates from the baseline model (Table~\\ref{tab:margin_reg}); confidence intervals and $p$-values from wild cluster bootstrap with Webb (2023) weights ($B = 9{,}999$ replications, $G = 5$ state clusters).",
      label   = "tab:margin_wcb",
      align   = "lrrr") %>%
    kable_styling(latex_options = c("hold_position")),
  "10b_tab_margin_wcb.tex"
)
message("Saved: tables_latex/10b_tab_margin_wcb.tex")

# ==============================================================================
# 5.3 COST REGRESSIONS (accounting check)
# ==============================================================================

m_cost_soe <- feols(
  w_ist ~ SoE | product + store_id,
  data = reg_data, cluster = ~ sst
)

m_cost_prepost <- feols(
  w_ist ~ SoE + postSoE | product + store_id,
  data = reg_data, cluster = ~ sst
)

etable(list("(1)" = m_cost_soe, "(2)" = m_cost_prepost))

etable(
  list("(1)" = m_cost_soe, "(2)" = m_cost_prepost),
  tex     = TRUE,
  file    = "tables_latex/10c_tab_cost_reg.tex",
  title   = "Cost regressions: SOE and post-SOE on nominal wholesale cost",
  label   = "tab:cost_reg",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$"),
  notes   = c(
    "Dependent variable: nominal wholesale cost $w_{ist}$ (dollars per unit or pound).",
    "Fixed effects: product ($\\gamma_j$) and store ($\\delta_i$).",
    "Standard errors clustered at the state level."
  )
)
message("Saved: tables_latex/10c_tab_cost_reg.tex")

m_cost_state_het <- feols(
  w_ist ~ 0 + i(sst, SoE) + i(sst, postSoE) | product + store_id,
  data = reg_data, cluster = ~ sst
)

etable(
  list("(1)" = m_cost_state_het),
  tex     = TRUE,
  file    = "tables_latex/10d_tab_cost_reg_state_heterog.tex",
  title   = "State heterogeneity in SOE and post-SOE cost effects (nominal wholesale cost)",
  label   = "tab:cost_reg_state_heterog",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes   = c(
    "Dependent variable: nominal wholesale cost $w_{ist}$.",
    "Each coefficient is a state-specific SOE or post-SOE effect.",
    "Fixed effects: product and store.",
    "Standard errors clustered at the state level ($G = 5$)."
  )
)
message("Saved: tables_latex/10d_tab_cost_reg_state_heterog.tex")

cost_state_interleaved <- build_state_interleaved(m_cost_state_het)

save_tex(
  kbl(cost_state_interleaved,
      format = "latex", booktabs = TRUE,
      caption = "State heterogeneity in SOE and post-SOE cost effects (nominal wholesale cost)",
      label   = "tab:cost_reg_state_heterog",
      align   = "lrr", escape = FALSE) %>%
    add_header_above(c(" " = 1, "Nominal wholesale cost $w_{ist}$" = 2), escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = c(
        "Dependent variable: nominal wholesale cost $w_{ist}$.",
        "Fixed effects: product and store.",
        "Standard errors clustered at the state level ($G = 5$) in parentheses.",
        "Signif. codes: ***: 0.01, **: 0.05, *: 0.1"
      ),
      general_title = "", escape = FALSE
    ),
  "10d_tab_cost_reg_state_heterog.tex"
)
message("Saved: tables_latex/10d_tab_cost_reg_state_heterog.tex")


# ==============================================================================
# 5.4 ROBUSTNESS REQUESTED IN REVIEW (Sept 2026 comments)
# ==============================================================================
# Same specification, FEs, and state clustering as Sections 5.1 and 5.2.
# ==============================================================================

message("Estimating review robustness checks (Section 5.4) ...")

POST_CAPS <- c(26L, 52L)

# Keep pre-SOE, SOE, and the first `cap` weeks after expiry. Later post-SOE
# weeks are dropped rather than recoded, so they do not fall into the omitted
# (pre-SOE) category. k_end = week_seq - T_end, so post-SOE weeks have k_end >= 1.
cap_post <- function(df, cap) {
  df %>% filter(postSoE == 0L | (!is.na(k_end) & k_end <= cap))
}

ROBUST_DICT <- c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$")

# ------------------------------------------------------------------------------
# 5.4a + 5.4b. Excluding bananas; post-SOE window capped at 26 and 52 weeks
# ------------------------------------------------------------------------------
# Bananas: flagged as an anchor-price item priced near marginal cost with little
#   promotion (Claude Major 5).
# Caps: the uncapped post-SOE window runs into the 2022-23 inflation period, and
#   states that expired later (Mississippi, Nov 2021) have more of their post
#   window in it (Claude Major 3).
# ------------------------------------------------------------------------------

reg_nobanana <- reg_data %>%
  filter(product != "bananas") %>%
  mutate(product = droplevels(product))

fit_robust_set <- function(outcome) {
  fml <- as.formula(paste(outcome, "~ SoE + postSoE | product + store_id"))
  list(
    "(1) Baseline"         = feols(fml, data = reg_data,              cluster = ~ sst),
    "(2) Excl. bananas"    = feols(fml, data = reg_nobanana,          cluster = ~ sst),
    "(3) Post $\\leq$ 26 wk" = feols(fml, data = cap_post(reg_data, 26L), cluster = ~ sst),
    "(4) Post $\\leq$ 52 wk" = feols(fml, data = cap_post(reg_data, 52L), cluster = ~ sst)
  )
}

price_robust  <- fit_robust_set("p_ist")
margin_robust <- fit_robust_set("margin_nom")

etable(price_robust)

etable(
  price_robust,
  tex     = TRUE,
  file    = "tables_latex/08c_tab_price_robust.tex",
  title   = "Price regressions: robustness to excluding bananas and to capping the post-SOE window",
  label   = "tab:price_robust",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2 + wr2,
  dict    = ROBUST_DICT,
  notes   = c(
    "Dependent variable: nominal retail price $p_{ist}$.",
    "Column (1) reproduces Table~\\ref{tab:price_reg}, column (2).",
    "Column (2) drops bananas.",
    "Columns (3) and (4) keep only the first 26 or 52 weeks after each state's SOE expired;",
    "later post-SOE weeks are dropped from the sample, not recoded.",
    "Fixed effects: product and store. Standard errors clustered at the state level."
  )
)
message("Saved: tables_latex/08c_tab_price_robust.tex")

etable(margin_robust)

etable(
  margin_robust,
  tex     = TRUE,
  file    = "tables_latex/10e_tab_margin_robust.tex",
  title   = "Margin regressions: robustness to excluding bananas and to capping the post-SOE window",
  label   = "tab:margin_robust",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2 + wr2,
  dict    = ROBUST_DICT,
  notes   = c(
    "Dependent variable: nominal dollar margin $M_{ist} = p_{ist} - w_{ist}$.",
    "Column (1) reproduces Table~\\ref{tab:margin_reg}, column (2).",
    "Column (2) drops bananas.",
    "Columns (3) and (4) keep only the first 26 or 52 weeks after each state's SOE expired;",
    "later post-SOE weeks are dropped from the sample, not recoded.",
    "Fixed effects: product and store. Standard errors clustered at the state level."
  )
)
message("Saved: tables_latex/10e_tab_margin_robust.tex")


# ------------------------------------------------------------------------------
# 5.4c State heterogeneity with capped post-SOE windows
# ------------------------------------------------------------------------------
# Tests whether the Mississippi post-SOE outlier shrinks once its post window
# no longer reaches into 2022.
# ------------------------------------------------------------------------------

fit_state_capped <- function(outcome) {
  fml <- as.formula(paste(outcome, "~ 0 + i(sst, SoE) + i(sst, postSoE) | product + store_id"))
  list(
    full  = feols(fml, data = reg_data,               cluster = ~ sst),
    cap26 = feols(fml, data = cap_post(reg_data, 26L), cluster = ~ sst),
    cap52 = feols(fml, data = cap_post(reg_data, 52L), cluster = ~ sst)
  )
}

build_state_capped_table <- function(models) {
  parts <- list(
    build_state_interleaved(models$full,  "During (all)",   "Post (all)"),
    build_state_interleaved(models$cap26, "During (cap 26)", "Post (cap 26)"),
    build_state_interleaved(models$cap52, "During (cap 52)", "Post (cap 52)")
  )
  stopifnot(length(unique(purrr::map_int(parts, nrow))) == 1L,
            identical(parts[[1]]$State, parts[[2]]$State),
            identical(parts[[1]]$State, parts[[3]]$State))
  out <- dplyr::bind_cols(parts[[1]], parts[[2]][, -1], parts[[3]][, -1])
  names(out) <- c("State", rep(c("During SOE", "Post-SOE"), 3))
  out
}

state_capped_note <- c(
  "Each column pair is estimated on a different sample. ``All'' uses every post-SOE week;",
  "``cap 26'' and ``cap 52'' keep only the first 26 or 52 weeks after each state's SOE expired.",
  "Fixed effects: product and store.",
  "Standard errors clustered at the state level ($G = 5$) in parentheses.",
  "Signif. codes: ***: 0.01, **: 0.05, *: 0.1"
)

price_state_capped  <- fit_state_capped("p_ist")
margin_state_capped <- fit_state_capped("margin_nom")

save_tex(
  kbl(build_state_capped_table(price_state_capped),
      format = "latex", booktabs = TRUE,
      caption = "State heterogeneity in price effects with capped post-SOE windows (nominal retail price)",
      label   = "price_state_heterog_capped",
      align   = "lrrrrrr", escape = FALSE) %>%
    add_header_above(c(" " = 1, "All post weeks" = 2, "Post capped at 26 weeks" = 2,
                       "Post capped at 52 weeks" = 2)) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(general = c("Dependent variable: nominal retail price $p_{ist}$.", state_capped_note),
             general_title = "", escape = FALSE),
  "09b_tab_price_state_heterog_capped.tex"
)

save_tex(
  kbl(build_state_capped_table(margin_state_capped),
      format = "latex", booktabs = TRUE,
      caption = "State heterogeneity in margin effects with capped post-SOE windows (nominal dollar margin)",
      label   = "margin_state_heterog_capped",
      align   = "lrrrrrr", escape = FALSE) %>%
    add_header_above(c(" " = 1, "All post weeks" = 2, "Post capped at 26 weeks" = 2,
                       "Post capped at 52 weeks" = 2)) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(general = c("Dependent variable: nominal dollar margin $M_{ist} = p_{ist} - w_{ist}$.",
                         state_capped_note),
             general_title = "", escape = FALSE),
  "11b_tab_margin_state_heterog_capped.tex"
)

# Console check: post-SOE coefficients by state across windows (dollars, not cents)
message("Post-SOE coefficients by state across windows (dollars):")
purrr::imap_dfr(
  list(price = price_state_capped, margin = margin_state_capped),
  function(mods, outcome) {
    purrr::imap_dfr(mods, function(m, window) {
      broom::tidy(m) %>%
        filter(grepl(":postSoE$", term)) %>%
        transmute(outcome, window,
                  state = sub("sst::([^:]+):.*", "\\1", term),
                  estimate = round(estimate, 3))
    })
  }
) %>%
  tidyr::pivot_wider(names_from = window, values_from = estimate) %>%
  print(n = Inf)


# ------------------------------------------------------------------------------
# 5.4d Event study around SOE activation
# ------------------------------------------------------------------------------
# Replaces the pooled SOE dummy with event-time indicators relative to each
# state's activation week (k_start), reference week -1. Leads below -ES_LEADS
# and lags above ES_LAGS are pooled into endpoint bins and not plotted.
#
# All five states activated in the same week, so event time is nearly
# collinear with calendar time and week fixed effects cannot be included.
# This is a check on the timing of the price path (no movement before
# activation), not a difference-in-differences design.
#
# ES_LAGS = 52 keeps every plotted lag inside the SOE for all states
# (Louisiana, the shortest, expired after 53 weeks).
# ------------------------------------------------------------------------------

ES_LEADS <- 26L
ES_LAGS  <- 52L

es_data <- reg_data %>%
  filter(!is.na(k_start)) %>%
  mutate(k_bin = pmin(pmax(k_start, -ES_LEADS - 1L), ES_LAGS + 1L))

m_price_es <- feols(
  p_ist ~ i(k_bin, ref = -1) | product + store_id,
  data = es_data, cluster = ~ sst
)

# Joint test that the plotted leads (-ES_LEADS to -2) are zero. With G = 5
# state clusters the clustered VCOV has rank at most 4, so a joint test of 25
# leads is not valid under state clustering. The joint test therefore uses
# store-clustered SEs; the plotted CIs keep state clustering.
lead_terms <- paste0("k_bin::", seq(-ES_LEADS, -2L))
lead_terms <- lead_terms[lead_terms %in% names(coef(m_price_es))]
es_pretrend <- fixest::wald(summary(m_price_es, cluster = ~ store_id),
                            keep = paste0("^(", paste(lead_terms, collapse = "|"), ")$"),
                            print = FALSE)
message(sprintf("Event study pre-trend test (leads -%d to -2, store-clustered): F = %.2f, p = %.3f",
                ES_LEADS, es_pretrend$stat, es_pretrend$p))

es_coef_df <- broom::tidy(m_price_es, conf.int = TRUE) %>%
  filter(grepl("^k_bin::", term)) %>%
  mutate(k = as.integer(sub("^k_bin::", "", term))) %>%
  filter(k >= -ES_LEADS, k <= ES_LAGS) %>%
  bind_rows(tibble::tibble(k = -1L, estimate = 0, conf.low = 0, conf.high = 0)) %>%
  arrange(k)

if (SAVE_CSV) write.csv(es_coef_df, "tables_csv/08b_price_event_study.csv", row.names = FALSE)

g_price_es <- ggplot(es_coef_df, aes(x = k, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_point(size = 1.2) +
  labs(
    x = "Weeks relative to SOE activation (reference: week -1)",
    y = "Coefficient (nominal price, $)",
    title = "Event study: nominal retail price around SOE activation",
    subtitle = sprintf(
      "Product and store FEs; state-clustered 95%% CIs. Joint pre-trend test (weeks -%d to -2, store-clustered): F = %.2f, p = %.3f.",
      ES_LEADS, es_pretrend$stat, es_pretrend$p)
  ) +
  theme_bw() +
  theme(plot.subtitle = element_text(size = 8))

g_price_es

ggsave("figures/08b_fig_price_event_study.png", g_price_es,
       width = 9, height = 5, dpi = 300)
message("Saved: figures/08b_fig_price_event_study.png")


# ------------------------------------------------------------------------------
# 5.4e Leave-one-state-out jackknife
# ------------------------------------------------------------------------------
# Re-estimates the baseline price and margin models dropping one state at a
# time. With G = 5, a single state (Mississippi) carries much of the state
# heterogeneity, so this shows how much any one state drives the pooled
# estimate. Jackknife SE = sqrt((G - 1) / G * sum((b_g - mean(b_g))^2)).
# ------------------------------------------------------------------------------

run_loso <- function(outcome) {
  fml    <- as.formula(paste(outcome, "~ SoE + postSoE | product + store_id"))
  states <- sort(unique(as.character(reg_data$sst)))
  purrr::map_dfr(states, function(s) {
    m <- feols(fml, data = reg_data %>% filter(sst != s), cluster = ~ sst)
    tibble::tibble(outcome = outcome, dropped = s,
                   term = names(coef(m)), estimate = unname(coef(m)))
  })
}

loso_df <- bind_rows(run_loso("p_ist"), run_loso("margin_nom"))

full_df <- bind_rows(
  tibble::tibble(outcome = "p_ist", term = names(coef(m_price_prepost)),
                 estimate = unname(coef(m_price_prepost))),
  tibble::tibble(outcome = "margin_nom", term = names(coef(m_margin_prepost)),
                 estimate = unname(coef(m_margin_prepost)))
) %>%
  mutate(dropped = "None (full sample)")

jk_se_df <- loso_df %>%
  group_by(outcome, term) %>%
  summarise(estimate = sqrt((n() - 1) / n() * sum((estimate - mean(estimate))^2)),
            .groups = "drop") %>%
  mutate(dropped = "Jackknife SE")

loso_tbl <- bind_rows(full_df, loso_df, jk_se_df) %>%
  mutate(col = paste(outcome, term, sep = "__"),
         estimate = formatC(estimate, digits = 3, format = "f")) %>%
  select(dropped, col, estimate) %>%
  tidyr::pivot_wider(names_from = col, values_from = estimate) %>%
  mutate(dropped = factor(dropped, levels = c("None (full sample)",
                                              sort(unique(loso_df$dropped)),
                                              "Jackknife SE"))) %>%
  arrange(dropped) %>%
  select(`State dropped` = dropped,
         `Price: SOE`      = p_ist__SoE,
         `Price: post-SOE` = p_ist__postSoE,
         `Margin: SOE`      = margin_nom__SoE,
         `Margin: post-SOE` = margin_nom__postSoE)

if (SAVE_CSV) write.csv(loso_tbl, "tables_csv/08d_tab_loso_jackknife.csv", row.names = FALSE)

save_tex(
  kbl(loso_tbl,
      format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Leave-one-state-out estimates of the SOE and post-SOE price and margin effects",
      label   = "loso_jackknife",
      align   = "lrrrr") %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = c(
        "Each row re-estimates the baseline model (Tables~\\\\ref{tab:price_reg} and~\\\\ref{tab:margin_reg}, column 2) dropping the named state.",
        "Dependent variables: nominal retail price and nominal dollar margin (dollars).",
        "Jackknife SE $= \\\\sqrt{\\\\frac{G-1}{G}\\\\sum_g (\\\\hat{\\\\beta}_{(g)} - \\\\bar{\\\\beta})^2}$, $G = 5$."
      ),
      general_title = "", escape = FALSE
    ),
  "08d_tab_loso_jackknife.tex"
)

message("Price and margin regressions complete.")

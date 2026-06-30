# ==============================================================================
# 05_regressions.R
#
# Empirical Model; Results: Retail Prices; Results: Markups
#   9.  Price level regressions (baseline and state heterogeneity)
#   9b. Wild cluster bootstrap robustness (price)
#   10. Margin level regressions (baseline and state heterogeneity)
#   10b. Wild cluster bootstrap robustness (margin)
#   10c. + 10d. Cost regressions (Accounting check)
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
#
# Outputs (figures/):
#   08_fig_price_coef_baseline.png
#   09_fig_price_coef_state_heterog.png
#   10_fig_margin_coef_prepost.png
#   11_fig_margin_coef_state_heterog.png
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

run_wcb <- function(model, param, B = 999, seed = 42) {
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
# 9. PRICE LEVEL REGRESSIONS
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
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
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
# 9a. State heterogeneity in price effects
# ------------------------------------------------------------------------------

m_price_state_het <- feols(
  p_ist ~ 0 + i(sst, SoE) + i(sst, postSoE) | product + store_id,
  data = reg_data, cluster = ~ sst
)

etable(
  list("(1)" = m_price_state_het),
  tex     = TRUE,
  file    = "tables_latex/09_tab_price_reg_state_heterog.tex",
  title   = "State heterogeneity in SOE and post-SOE price effects (nominal retail price)",
  label   = "tab:price_reg_state_heterog",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes   = c(
    "Dependent variable: nominal retail price $p_{ist}$.",
    "Each coefficient is a state-specific SOE or post-SOE effect.",
    "Fixed effects: product and store.",
    "Standard errors clustered at the state level."
  )
)
message("Saved: tables_latex/09_tab_price_reg_state_heterog.tex")

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
# 9b. ROBUSTNESS: WILD CLUSTER BOOTSTRAP — PRICE
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
# 10. MARGIN LEVEL REGRESSIONS
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
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
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
# 10a. State heterogeneity in margin effects
# ------------------------------------------------------------------------------

m_margin_state_het <- feols(
  margin_nom ~ 0 + i(sst, SoE) + i(sst, postSoE) | product + store_id,
  data = reg_data, cluster = ~ sst
)

etable(
  list("(1)" = m_margin_state_het),
  tex     = TRUE,
  file    = "tables_latex/11_tab_margin_reg_state_heterog.tex",
  title   = "State heterogeneity in SOE and post-SOE margin effects (nominal dollar margin)",
  label   = "tab:margin_reg_state_heterog",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes   = c(
    "Dependent variable: nominal dollar margin $M_{ist}$.",
    "Each coefficient is a state-specific SOE or post-SOE margin effect.",
    "Fixed effects: product and store.",
    "Standard errors clustered at the state level."
  )
)
message("Saved: tables_latex/11_tab_margin_reg_state_heterog.tex")

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
# 10b. ROBUSTNESS: WILD CLUSTER BOOTSTRAP — MARGIN
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
# Accounting Check: COST REGRESSIONS
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

message("Price and margin regressions complete.")

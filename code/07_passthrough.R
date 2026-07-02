# ==============================================================================
# 07_passthrough.R
#
# Mechanisms: Mechanism 2 (Variation in Pass-Through)
#
# Purpose: Pass-through regressions and optional duration extension.
#   11.  Baseline pass-through (with and without week FEs)
#   11a. Optional: duration extension (Dur_wk interaction)
#
# Specification (main):
#   Delta_P_ist = alpha + beta1*Delta_w + beta2*(Delta_w*SOE) +
#                 beta3*(Delta_w*postSOE) + gamma_j + delta_i [+ tau_t]
#
# beta1: baseline pass-through (pre-SOE)
# beta2: change in pass-through during SOE
# beta3: change in pass-through after SOE ends
#
# Preferred specification includes week FEs (column 2). The identifying variation
# is Delta_w * SOE, which varies across stores within a week and is not absorbed
# by week fixed effects. The no-FE version is also reported for sensitivity.
#
# Duration extension specification:
#   Delta_P_ist = alpha + beta1*Delta_w + beta2*(Delta_w*SOE) +
#                 beta3*(Delta_w*SOE*Dur_wk) + beta4*(Delta_w*postSOE)
#                 + gamma_j + delta_i + tau_t
#
# Week FEs are included in the duration model. The identifying variation for
# Delta_w*SOE*Dur_wk is cross-state: within a given calendar week, states that
# entered the SOE at different dates will be at different values of Dur_wk.
# This variation is not absorbed by week FEs.
#
# Implied SOE pass-through at duration d = beta1 + beta2 + beta3*d.
# Return to baseline at d* = -beta2 / beta3.
#
# Depends on: panel_est, save_tex(), SAVE_CSV, RUN_DUR_EXTENSION
#
# Outputs (tables_latex/):
#   12_tab_passthrough_reg.tex
#   13_tab_passthrough_duration.tex      (if RUN_DUR_EXTENSION)
#   14_tab_passthrough_implied_soe.tex   (if RUN_DUR_EXTENSION)
#
# Outputs (figures/):
#   12_fig_passthrough_coef.png
#   13_fig_passthrough_duration.png      (if RUN_DUR_EXTENSION)
# ==============================================================================

message("Estimating Mechanism 2 pass-through regressions ...")

pt_data <- panel_est %>%
  filter(is.finite(p_ist), is.finite(margin_nom),
         is.finite(dP), is.finite(dW))


# ==============================================================================
# 11. BASELINE PASS-THROUGH REGRESSIONS
# ==============================================================================

m_pt_no_fe <- feols(
  dP ~ dW + dW:SoE + dW:postSoE | product + store_id,
  data = pt_data, cluster = ~ store_id
)

m_pt_with_fe <- feols(
  dP ~ dW + dW:SoE + dW:postSoE | product + store_id + week_fe,
  data = pt_data, cluster = ~ store_id
)

etable(list("(1) No week FE" = m_pt_no_fe, "(2) With week FE" = m_pt_with_fe))

etable(
  list("(1) No week FE" = m_pt_no_fe, "(2) With week FE" = m_pt_with_fe),
  tex     = TRUE,
  file    = "tables_latex/12_tab_passthrough_reg.tex",
  title   = "Pass-through regressions with and without week fixed effects",
  label   = "tab:passthrough_reg",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = c(
    "dW"         = "$\\Delta w_{ist}$",
    "dW:SoE"     = "$\\Delta w_{ist} \\times SOE_{st}$",
    "dW:postSoE" = "$\\Delta w_{ist} \\times postSOE_{st}$"
  ),
  headers = list("Pass-through: $\\Delta p_{ist}$" = 2),
  notes   = c(
    "Dependent variable: nominal $\\Delta p_{ist}$ (dollars per unit or pound).",
    "Column (2) adds week fixed effects ($\\tau_t$).",
    "Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/12_tab_passthrough_reg.tex")

pt_coef_df <- bind_rows(
  broom::tidy(m_pt_no_fe,   conf.int = TRUE) %>% mutate(spec = "No week FE"),
  broom::tidy(m_pt_with_fe, conf.int = TRUE) %>% mutate(spec = "With week FE")
) %>%
  filter(term %in% c("dW", "dW:SoE", "dW:postSoE")) %>%
  mutate(
    term = recode(term,
                  "dW"         = "Baseline (Δ w)",
                  "dW:SoE"     = "Δ w x SOE",
                  "dW:postSoE" = "Δ w x postSOE"),
    term = factor(term, levels = c("Baseline (Δ w)", "Δ w x SOE", "Δ w x postSOE")),
    spec = factor(spec, levels = c("No week FE", "With week FE"))
  )

g_pt_coef <- ggplot(pt_coef_df,
                    aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = spec)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_text(aes(label = round(estimate, 3)),
            position = position_dodge(width = 0.5),
            vjust = -1.0, size = 3.2, show.legend = FALSE) +
  labs(x = NULL, y = "Coefficient estimate",
       title = "Pass-through coefficients: with and without week fixed effects",
       color = NULL) +
  theme_bw() +
  theme(legend.position = "top")

g_pt_coef

ggsave("figures/12_fig_passthrough_coef.png", g_pt_coef,
       width = 9, height = 5, dpi = 300)
message("Saved: figures/12_fig_passthrough_coef.png")


# ==============================================================================
# 11a. EXTENSION: PASS-THROUGH BY DURATION
# ==============================================================================
# Adds a three-way interaction dW*SOE*Dur_wk to ask how pass-through evolves
# as enforcement continues. Week FEs are included; the duration effect is
# identified through cross-state variation: within a calendar week, states that
# entered the SOE at different dates are at different values of Dur_wk.
#
# Implied SOE pass-through at duration d = beta1 + beta2 + beta3*d.
# Return to baseline at d* = -beta2 / beta3.
# ==============================================================================

if (RUN_DUR_EXTENSION) {
  
  message("Estimating duration extension (Mechanism 2) ...")
  
  pt_dur_data <- pt_data %>%
    filter(is.finite(Dur_st)) %>%
    mutate(Dur_wk = as.numeric(Dur_st))
  
  m_pt_dur <- feols(
    dP ~ dW + dW:SoE + dW:SoE:Dur_wk + dW:postSoE | product + store_id + week_fe,
    data = pt_dur_data, cluster = ~ store_id
  )
  
  etable(
    list("(1)" = m_pt_dur),
    tex     = TRUE,
    file    = "tables_latex/13_tab_passthrough_duration.tex",
    title   = "Pass-through by duration: how pass-through evolves over the emergency window",
    label   = "tab:passthrough_duration",
    digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
    dict    = c(
      "dW"            = "$\\Delta w_{ist}$",
      "dW:SoE"        = "$\\Delta w_{ist} \\times SOE_{st}$",
      "dW:SoE:Dur_wk" = "$\\Delta w_{ist} \\times SOE_{st} \\times Dur^{wk}_{st}$",
      "dW:postSoE"    = "$\\Delta w_{ist} \\times postSOE_{st}$"
    ),
    notes = c(
      "Dependent variable: nominal $\\Delta p_{ist}$.",
      "Includes product, store, and week fixed effects.",
      "$Dur^{wk}_{st}$ = weeks since SOE activation in state $s$, set to 0 outside SOE.",
      "Duration effect identified through cross-state variation: within a calendar week,",
      "states that entered the SOE on different dates are at different values of $Dur^{wk}_{st}$.",
      "The implied SOE pass-through at duration $d$ is $\\hat{\\beta}_1 + \\hat{\\beta}_2 + \\hat{\\beta}_3 d$.",
      "Retailers return to baseline pass-through at $d^* = -\\hat{\\beta}_2 / \\hat{\\beta}_3$ weeks.",
      "Standard errors clustered at the store level."
    )
  )
  message("Saved: tables_latex/13_tab_passthrough_duration.tex")
  
  lincombo_ci <- function(model, L_vec, alpha = 0.05) {
    b  <- coef(model)
    V  <- vcov(model)
    cn <- names(b)
    L  <- setNames(rep(0, length(cn)), cn)
    for (nm in names(L_vec)) if (nm %in% cn) L[nm] <- L_vec[[nm]]
    est <- as.numeric(t(L) %*% b)
    se  <- sqrt(as.numeric(t(L) %*% V %*% L))
    z   <- qnorm(1 - alpha / 2)
    data.frame(estimate = est, se = se,
               conf.low = est - z * se, conf.high = est + z * se)
  }
  
  d_vals <- seq(0, 70, by = 4)
  
  pt_implied <- purrr::map_dfr(d_vals, function(d) {
    L <- list("dW" = 1, "dW:SoE" = 1, "dW:SoE:Dur_wk" = d)
    cbind(data.frame(Dur_wk = d), round(lincombo_ci(m_pt_dur, L), 3))
  })
  
  if (SAVE_CSV) write.csv(pt_implied, "tables_csv/08_tab_passthrough_implied_soe.csv", row.names = FALSE)
  
  save_tex(
    kbl(pt_implied,
        format = "latex", booktabs = TRUE,
        caption = "Implied SOE pass-through at selected enforcement durations. Computed as $\\hat{\\beta}_1 + \\hat{\\beta}_2 + \\hat{\\beta}_3 d$ from the duration model with week fixed effects. $Dur^{wk}$ = weeks since SOE activation.",
        label   = "tab:passthrough_implied_soe",
        align   = "rrrrr") %>%
      kable_styling(latex_options = c("hold_position")),
    "14_tab_passthrough_implied_soe.tex"
  )
  
  g_pt_dur <- ggplot(pt_implied, aes(x = Dur_wk, y = estimate)) +
    geom_hline(yintercept = coef(m_pt_dur)[["dW"]], linetype = "dashed", color = "grey40") +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.8) +
    geom_text(aes(label = round(estimate, 2)), vjust = -0.8, size = 3.5) +
    annotate("text", x = max(d_vals) * 0.85,
             y = coef(m_pt_dur)[["dW"]] + 0.01,
             label = "Baseline (pre-SOE)", size = 3, color = "grey40") +
    labs(
      title    = "Implied SOE pass-through by enforcement duration",
      subtitle = "Dashed line = pre-SOE baseline. Points are linear combinations from the duration model (week FEs included).",
      x        = "Weeks since SOE activation",
      y        = "Implied pass-through (Δ p / Δ w)"
    ) +
    theme_bw() +
    theme(plot.subtitle = element_text(size = 8))
  
  ggsave("figures/13_fig_passthrough_duration.png", g_pt_dur,
         width = 8, height = 5, dpi = 300)
  message("Saved: figures/13_fig_passthrough_duration.png")
  
}

g_pt_dur


# ==============================================================================
# 11b. PASS-THROUGH IN LOGS (ROBUSTNESS -- Sangani 2026)
# ==============================================================================
# Sangani (QJE 2026) shows that pass-through measured in percentages (logs)
# appears incomplete even when pass-through in levels is complete, because
# a $1 increase in a $0.35 cost looks like a larger percentage change than
# the same $1 increase in a $1.20 retail price. The log-pass-through
# coefficient is therefore expected to be approximately equal to the
# cost-to-price ratio (w_ist / p_ist) even under complete levels pass-through.
# This specification estimates log pass-through for comparison and to confirm
# that the attenuation during the SOE is not an artifact of nonlinear
# inflationary pressure on wholesale costs.
# ==============================================================================

pt_data_log <- panel_est %>%
  filter(is.finite(p_ist), is.finite(w_ist),
         p_ist > 0, w_ist > 0) %>%
  arrange(store_id, product, week_seq) %>%
  group_by(store_id, product) %>%
  mutate(
    lag_p_ist = lag(p_ist),
    lag_w_ist = lag(w_ist)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(lag_p_ist), !is.na(lag_w_ist),
    lag_p_ist > 0, lag_w_ist > 0
  ) %>%
  mutate(
    dlogP = log(p_ist) - log(lag_p_ist),
    dlogW = log(w_ist) - log(lag_w_ist)
  ) %>%
  filter(is.finite(dlogP), is.finite(dlogW))

# Sanity check: implied log pass-through under complete levels pass-through
# should be approximately mean(w_ist / p_ist)
pt_data_log %>%
  summarise(
    mean_cost_price_ratio = mean(w_ist / p_ist, na.rm = TRUE),
    median_cost_price_ratio = median(w_ist / p_ist, na.rm = TRUE)
  )
# If log pass-through baseline coefficient ≈ cost-to-price ratio,
# the levels and log results are consistent with Sangani (2026).

m_pt_log_no_fe <- feols(
  dlogP ~ dlogW + dlogW:SoE + dlogW:postSoE | product + store_id,
  data    = pt_data_log,
  cluster = ~ store_id
)

m_pt_log_with_fe <- feols(
  dlogP ~ dlogW + dlogW:SoE + dlogW:postSoE | product + store_id + week_fe,
  data    = pt_data_log,
  cluster = ~ store_id
)

etable(list(
  "(1) No week FE"   = m_pt_log_no_fe,
  "(2) With week FE" = m_pt_log_with_fe
))

etable(
  list(
    "(1) No week FE"   = m_pt_log_no_fe,
    "(2) With week FE" = m_pt_log_with_fe
  ),
  tex    = TRUE,
  file   = "tables_latex/12b_tab_passthrough_log.tex",
  title  = "Pass-through regressions in logs (robustness)",
  label  = "tab:passthrough_log",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict   = c(
    "dlogW"         = "$\\Delta \\log w_{jist}$",
    "dlogW:SoE"     = "$\\Delta \\log w_{jist} \\times SOE_{st}$",
    "dlogW:postSoE" = "$\\Delta \\log w_{jist} \\times postSOE_{st}$"
  ),
  headers = list("Log pass-through: $\\Delta \\log p_{jist}$" = 2),
  notes   = c(
    "Dependent variable: $\\Delta \\log p_{jist}$ (log change in net retail price).",
    "Regressor: $\\Delta \\log w_{jist}$ (log change in wholesale cost).",
    "Fixed effects: product and store; column (2) adds week fixed effects.",
    "Standard errors clustered at the store level.",
    "Robustness check following Sangani (2026): under complete pass-through",
    "in levels, the baseline log pass-through coefficient is expected to",
    "approximate the mean wholesale-cost-to-retail-price ratio."
  )
)
message("Saved: tables_latex/12b_tab_passthrough_log.tex")

message("Pass-through regressions complete.")

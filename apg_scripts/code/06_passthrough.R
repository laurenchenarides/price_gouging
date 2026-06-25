# ==============================================================================
# 06_passthrough.R
#
# Purpose: Section IV.C pass-through regressions and optional duration extension.
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
# Preferred specification is without week FEs: all five states have overlapping
# SOE windows, so within-week cross-state variation in SOE status is limited.
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

message("Estimating Section IV.C pass-through regressions ...")

pt_data <- panel_est %>%
  filter(is.finite(p_ist), is.finite(margin_nom),
         is.finite(dP), is.finite(dW))


# ==============================================================================
# 11. SECTION IV.C: PASS-THROUGH REGRESSIONS
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
                  "dW"         = "Baseline (Delta w)",
                  "dW:SoE"     = "Delta w x SOE",
                  "dW:postSoE" = "Delta w x postSOE"),
    term = factor(term, levels = c("Baseline (Delta w)", "Delta w x SOE", "Delta w x postSOE")),
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

ggsave("figures/12_fig_passthrough_coef.png", g_pt_coef,
       width = 9, height = 5, dpi = 300)
message("Saved: figures/12_fig_passthrough_coef.png")


# ==============================================================================
# 11a. EXTENSION: PASS-THROUGH BY DURATION
# ==============================================================================
# Adds a three-way interaction dW*SOE*Dur_wk to ask how many weeks it takes
# for retailers to return to pre-SOE pass-through after enforcement begins.
#
# Implied SOE pass-through at duration d = beta1 + beta2 + beta3*d.
# Return to baseline at d* = -beta2 / beta3.
# ==============================================================================

if (RUN_DUR_EXTENSION) {

  message("Estimating optional duration extension (Section 11a) ...")

  pt_dur_data <- pt_data %>%
    filter(is.finite(Dur_st)) %>%
    mutate(Dur_wk = as.numeric(Dur_st))

  m_pt_dur <- feols(
    dP ~ dW + dW:SoE + dW:SoE:Dur_wk + dW:postSoE | product + store_id,
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
      "Dependent variable: nominal $\\Delta p_{ist}$. No week FEs.",
      "$Dur^{wk}_{st}$ = weeks since SOE activation in state $g$, set to 0 outside SOE.",
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
        caption = "Implied SOE pass-through at selected enforcement durations. Computed as $\\hat{\\beta}_1 + \\hat{\\beta}_2 + \\hat{\\beta}_3 d$ from the duration model. $Dur^{wk}$ = weeks since SOE activation.",
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
      subtitle = "Dashed line = pre-SOE baseline. Points are linear combinations from the duration model.",
      x        = "Weeks since SOE activation",
      y        = "Implied pass-through (Delta p / Delta w)"
    ) +
    theme_bw() +
    theme(plot.subtitle = element_text(size = 8))

  ggsave("figures/13_fig_passthrough_duration.png", g_pt_dur,
         width = 8, height = 5, dpi = 300)
  message("Saved: figures/13_fig_passthrough_duration.png")

}

message("Pass-through regressions complete.")

# ==============================================================================
# 04_residual_plots.R
#
# Purpose: Section III.C-E residualized trend plots.
#   For each outcome, regress on product and store fixed effects; recover
#   residuals; plot the weekly mean residual over time.
#
# Pooled plots (III.C) are the main paper outputs.
# By-state (III.D) and by-product (III.E) are produced when
# SAVE_OPTIONAL_PLOTS is TRUE.
#
# Depends on: panel_est (from 02_build_panel.R), SAVE_OPTIONAL_PLOTS
#
# Outputs (figures/):
#   04_fig_resid_volume_pooled.png
#   05_fig_resid_price_pooled.png
#   06_fig_resid_cost_pooled.png
#   07_fig_resid_margin_pooled.png
#   fig_resid_*_by_state.png     (when SAVE_OPTIONAL_PLOTS = TRUE)
#   fig_resid_*_by_product.png   (when SAVE_OPTIONAL_PLOTS = TRUE)
# ==============================================================================

message("Building Section III.C-E residualized trend plots ...")

panel_resid <- panel_est %>%
  filter(upc_week_volume > 0,
         is.finite(p_ist), is.finite(w_ist), is.finite(margin_nom)) %>%
  mutate(
    store_id = as.factor(store_id),
    product  = as.factor(product),
    sst      = as.factor(sst)
  )

soe_shade <- panel_resid %>%
  filter(SoE == 1) %>%
  summarise(
    soe_start = min(week_start, na.rm = TRUE),
    soe_end   = max(week_start, na.rm = TRUE)
  )

resid_weekly <- function(df, outcome_col) {
  fml <- as.formula(paste0(outcome_col, " ~ 1 | product + store_id"))
  m   <- feols(fml, data = df, warn = FALSE, notes = FALSE)
  df %>%
    mutate(.resid = resid(m)) %>%
    group_by(week_start) %>%
    summarise(mean_resid = mean(.resid, na.rm = TRUE), .groups = "drop") %>%
    arrange(week_start)
}

resid_weekly_by <- function(df, outcome_col, by_col) {
  groups <- sort(unique(as.character(df[[by_col]])))
  purrr::map_dfr(groups, function(g) {
    df_g <- df %>% filter(as.character(.data[[by_col]]) == g)
    if (nrow(df_g) < 50) return(NULL)
    fml <- as.formula(paste0(outcome_col, " ~ 1 | product + store_id"))
    m   <- feols(fml, data = df_g, warn = FALSE, notes = FALSE)
    df_g %>%
      mutate(.resid = resid(m)) %>%
      group_by(week_start) %>%
      summarise(mean_resid = mean(.resid, na.rm = TRUE), .groups = "drop") %>%
      mutate(group = g)
  })
}

plot_resid_pooled <- function(resid_df, outcome_label, filename) {
  g <- ggplot(resid_df, aes(x = week_start, y = mean_resid)) +
    annotate("rect",
             xmin = soe_shade$soe_start, xmax = soe_shade$soe_end,
             ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "grey50") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_line(linewidth = 0.7) +
    labs(
      title    = paste0("Residualized trend: ", outcome_label),
      subtitle = "Residuals from product and store fixed effects. Shaded = pooled SOE window.",
      x = "Week", y = paste0("Mean residual (", outcome_label, ")")
    ) +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 8))
  ggsave(file.path("figures", filename), g, width = 10, height = 5, dpi = 300)
  message("Saved: figures/", filename)
  invisible(g)
}

plot_resid_grouped <- function(resid_df, outcome_label, group_label, filename) {
  g <- ggplot(resid_df, aes(x = week_start, y = mean_resid)) +
    annotate("rect",
             xmin = soe_shade$soe_start, xmax = soe_shade$soe_end,
             ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "grey50") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~ group, scales = "free_y") +
    labs(
      title    = paste0("Residualized trend by ", group_label, ": ", outcome_label),
      subtitle = "Residuals from product and store fixed effects within each group. Shaded = pooled SOE window.",
      x = "Week", y = paste0("Mean residual (", outcome_label, ")")
    ) +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 8), strip.text = element_text(size = 9))
  ggsave(file.path("figures", filename), g, width = 12, height = 8, dpi = 300)
  message("Saved: figures/", filename)
  invisible(g)
}

resid_outcomes <- list(
  list(col = "upc_week_volume", label = "Volume",        file = "04_fig_resid_volume_pooled.png"),
  list(col = "p_ist",           label = "Price (nom.)",  file = "05_fig_resid_price_pooled.png"),
  list(col = "w_ist",           label = "Cost (nom.)",   file = "06_fig_resid_cost_pooled.png"),
  list(col = "margin_nom",      label = "Margin (nom.)", file = "07_fig_resid_margin_pooled.png")
)

for (o in resid_outcomes) {
  rd <- resid_weekly(panel_resid, o$col)
  plot_resid_pooled(rd, o$label, o$file)
}

if (SAVE_OPTIONAL_PLOTS) {
  for (o in resid_outcomes) {
    stem <- gsub("[^a-z]", "", tolower(o$label))
    rs <- resid_weekly_by(panel_resid, o$col, "sst")
    plot_resid_grouped(rs, o$label, "state",
                       paste0("fig_resid_", stem, "_by_state.png"))
    rp <- resid_weekly_by(panel_resid, o$col, "product")
    plot_resid_grouped(rp, o$label, "product",
                       paste0("fig_resid_", stem, "_by_product.png"))
  }
}

message("Residual trend plots complete.")

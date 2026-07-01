# ==============================================================================
# 07_uniform_pricing.R
#
# Mechanisms: Mechanism 1 (Constant Retail Prices / Uniform Pricing)
#
# Purpose: Uniform pricing analysis.
#   Computes pairwise absolute log price differences within retailer chains
#   across stores, by product and week. Tests whether within-chain price
#   uniformity changed during and after the SOE.
#
# Regression outcome: mean absolute log price difference at the
#   (retailer, product, week) level.
#
# Table structure:
#   Main results    — retailer FE only | product FE only | retailer + product FE
#   Robustness      — retailer + month | product + month | retailer + product + month
#   Heterogeneity   — main: no FE | product FE
#                   — robustness: month FE | product + month FE
#
# Depends on: panel_est, save_tex(), SAVE_CSV
#
# Outputs (tables_latex/):
#   15_tab_uniformity_summary_retail.tex
#   16_tab_uniformity_summary_wholesale.tex
#   17_tab_uniformity_retail_main.tex
#   17b_tab_uniformity_retail_robust.tex
#   18_tab_uniformity_wholesale_main.tex
#   18b_tab_uniformity_wholesale_robust.tex
#   19_tab_uniformity_heterog_retail.tex
#   20_tab_uniformity_heterog_wholesale.tex
#
# Outputs (figures/):
#   14_fig_logdiff_retail_pooled.png
#   15_fig_logdiff_wholesale_pooled.png
#   16_fig_logdiff_retail_by_period.png
#   17_fig_logdiff_wholesale_by_period.png
#   18_fig_uniformity_heterog_coef.png
# ==============================================================================

message("Estimating Mechanism 1 uniform pricing ...")

unif_panel <- panel_est %>%
  filter(p_ist > 0, w_ist > 0) %>%
  mutate(
    period = case_when(
      preSoE  == 1L ~ "Pre-SOE",
      SoE     == 1L ~ "During SOE",
      postSoE == 1L ~ "Post-SOE",
      TRUE          ~ NA_character_
    ),
    period = factor(period, levels = c("Pre-SOE", "During SOE", "Post-SOE"))
  ) %>%
  filter(!is.na(period))


# ==============================================================================
# PAIRWISE LOG DIFFERENCES
# ==============================================================================

make_pairwise_logdiff <- function(df, price_col) {
  df_in <- df %>%
    select(
      retailer = retailer_id, store_id, product,
      week_seq, week_start, period, month_fe,
      price = !!rlang::sym(price_col)
    ) %>%
    group_by(retailer, week_seq) %>%
    mutate(storeno = row_number()) %>%
    ungroup()
  
  inner_join(
    df_in,
    df_in %>% rename(price2 = price, storeno2 = storeno, store_id2 = store_id),
    by = c("retailer", "product", "week_seq", "week_start", "period", "month_fe"),
    relationship = "many-to-many"
  ) %>%
    filter(storeno < storeno2) %>%
    mutate(diff = abs(log(price) - log(price2))) %>%
    select(retailer, product, week_seq, week_start, period, month_fe, diff)
}

pairs_retail    <- make_pairwise_logdiff(unif_panel, "p_ist")
pairs_wholesale <- make_pairwise_logdiff(unif_panel, "w_ist")


# ==============================================================================
# DISTRIBUTION PLOTS
# ==============================================================================

COMMON_BINWIDTH <- 0.01
X_LIM           <- c(0, 0.5)

plot_logdiff_hist <- function(df, title_str, filename) {
  m_val <- median(df$diff, na.rm = TRUE)
  g <- ggplot(df, aes(x = diff)) +
    geom_histogram(aes(y = after_stat(count / sum(count))),
                   binwidth = COMMON_BINWIDTH, fill = "steelblue", color = "white") +
    coord_cartesian(xlim = X_LIM) +
    scale_y_continuous(labels = scales::label_percent()) +
    geom_vline(xintercept = m_val, color = "red", linetype = "dashed") +
    annotate("text", x = m_val + 0.05, y = Inf,
             label = paste0("Median: ", round(m_val, 3)),
             color = "red", vjust = 2, hjust = 1, size = 3) +
    labs(title = title_str,
         x = "Absolute log price difference", y = "Percent of store pairs") +
    theme_minimal()
  ggsave(file.path("figures", filename), g, width = 10, height = 5, dpi = 300)
  message("Saved: figures/", filename)
  invisible(g)
}

plot_logdiff_by_period <- function(df, title_str, filename) {
  g <- ggplot(df, aes(x = diff)) +
    geom_histogram(aes(y = after_stat(count / sum(count))),
                   binwidth = COMMON_BINWIDTH, fill = "steelblue", color = "white") +
    coord_cartesian(xlim = X_LIM) +
    scale_y_continuous(labels = scales::label_percent()) +
    facet_wrap(~ period) +
    labs(title = title_str,
         x = "Absolute log price difference", y = "Percent of store pairs") +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"))
  ggsave(file.path("figures", filename), g, width = 12, height = 5, dpi = 300)
  message("Saved: figures/", filename)
  invisible(g)
}

plot_logdiff_hist(pairs_retail,    "Within-chain retail price uniformity (all periods)",
                  "14_fig_logdiff_retail_pooled.png")
plot_logdiff_hist(pairs_wholesale, "Within-chain wholesale cost uniformity (all periods)",
                  "15_fig_logdiff_wholesale_pooled.png")
plot_logdiff_by_period(pairs_retail,    "Within-chain retail price uniformity by SOE period",
                       "16_fig_logdiff_retail_by_period.png")
plot_logdiff_by_period(pairs_wholesale, "Within-chain wholesale cost uniformity by SOE period",
                       "17_fig_logdiff_wholesale_by_period.png")


# ==============================================================================
# SUMMARY TABLES: mean absolute log diff by retailer and period
# ==============================================================================

make_disp_summary <- function(pairs_df, caption_str, label_str,
                              filename_csv, filename_tex) {
  tbl <- pairs_df %>%
    mutate(
      retailer = paste0("Retailer ", retailer),
      period   = factor(period, levels = c("Pre-SOE", "During SOE", "Post-SOE"))
    ) %>%
    group_by(retailer, period) %>%
    summarise(
      Count       = n(),
      Mean        = mean(diff,   na.rm = TRUE),
      Median      = median(diff, na.rm = TRUE),
      `Std. dev.` = sd(diff,    na.rm = TRUE),
      Variance    = var(diff,   na.rm = TRUE),
      Max         = max(diff,   na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    arrange(retailer, period) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))
  
  if (SAVE_CSV) write.csv(tbl, file.path("tables_csv", filename_csv), row.names = FALSE)
  
  save_tex(
    kbl(tbl,
        format = "latex", booktabs = TRUE,
        caption = caption_str, label = label_str,
        align   = "llrrrrrr",
        format.args = list(big.mark = ",")) %>%
      collapse_rows(columns = 1, latex_hline = "major", valign = "top") %>%
      kable_styling(latex_options = c("hold_position", "scale_down")),
    filename_tex
  )
  message("Saved: tables_latex/", filename_tex)
}

make_disp_summary(
  pairs_retail,
  caption_str  = "Within-chain retail price uniformity by retailer and SOE period. Values are absolute log retail price differences across store pairs within the same chain, product, and week.",
  label_str    = "tab:uniformity_summary_retail",
  filename_csv = "09_tab_uniformity_summary_retail.csv",
  filename_tex = "15_tab_uniformity_summary_retail.tex"
)

make_disp_summary(
  pairs_wholesale,
  caption_str  = "Within-chain wholesale cost uniformity by retailer and SOE period. Values are absolute log wholesale cost differences across store pairs within the same chain, product, and week.",
  label_str    = "tab:uniformity_summary_wholesale",
  filename_csv = "10_tab_uniformity_summary_wholesale.csv",
  filename_tex = "16_tab_uniformity_summary_wholesale.tex"
)


# ==============================================================================
# REGRESSION PANEL: collapse to retailer-product-week
# ==============================================================================

make_uniformity_panel <- function(pairs_df) {
  pairs_df %>%
    group_by(retailer, product, week_seq, period, month_fe) %>%
    summarise(
      Diff_bar         = mean(diff, na.rm = TRUE),
      n_pairs          = n(),
      .groups          = "drop"
    ) %>%
    mutate(
      during           = if_else(period == "During SOE", 1L, 0L),
      post             = if_else(period == "Post-SOE",   1L, 0L),
      retailer_product = interaction(retailer, product, drop = TRUE)
    )
}

disp_retail    <- make_uniformity_panel(pairs_retail)
disp_wholesale <- make_uniformity_panel(pairs_wholesale)


# ==============================================================================
# POOLED UNIFORMITY REGRESSIONS
# ==============================================================================
# Main results: retailer FE | product FE | retailer + product FE
# Robustness:  retailer + month FE | product + month FE |
#              retailer + product + month FE
# ==============================================================================

run_uniformity_regs <- function(df) {
  list(
    # Main
    main_ret     = feols(Diff_bar ~ during + post | retailer,
                         data = df, cluster = ~ retailer_product),
    main_prod    = feols(Diff_bar ~ during + post | product,
                         data = df, cluster = ~ retailer_product),
    main_ret_prod = feols(Diff_bar ~ during + post | retailer + product,
                          data = df, cluster = ~ retailer_product),
    # Robustness
    rob_ret      = feols(Diff_bar ~ during + post | retailer + month_fe,
                         data = df, cluster = ~ retailer_product),
    rob_prod     = feols(Diff_bar ~ during + post | product + month_fe,
                         data = df, cluster = ~ retailer_product),
    rob_ret_prod = feols(Diff_bar ~ during + post | retailer + product + month_fe,
                         data = df, cluster = ~ retailer_product)
  )
}

regs_retail    <- run_uniformity_regs(disp_retail)
regs_wholesale <- run_uniformity_regs(disp_wholesale)

UNIF_DICT  <- c("during" = "During SOE", "post" = "Post-SOE")
UNIF_NOTES_MAIN <- c(
  "Omitted category: pre-SOE period.",
  "Standard errors clustered at the retailer-product level."
)
UNIF_NOTES_ROB <- c(
  "Robustness checks add month fixed effects to each specification.",
  "Omitted category: pre-SOE period.",
  "Standard errors clustered at the retailer-product level."
)

# Retail — main
etable(
  list("(1) Retailer FE"          = regs_retail$main_ret,
       "(2) Product FE"            = regs_retail$main_prod,
       "(3) Retailer + Product FE" = regs_retail$main_ret_prod),
  tex = TRUE, file = "tables_latex/17_tab_uniformity_retail_main.tex",
  title   = "Within-chain retail price uniformity: main results",
  label   = "tab:uniformity_retail_main",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = UNIF_DICT, notes = UNIF_NOTES_MAIN
)
message("Saved: tables_latex/17_tab_uniformity_retail_main.tex")

# Retail — robustness
etable(
  list("(1) Retailer + Month FE"           = regs_retail$rob_ret,
       "(2) Product + Month FE"             = regs_retail$rob_prod,
       "(3) Retailer + Product + Month FE"  = regs_retail$rob_ret_prod),
  tex = TRUE, file = "tables_latex/17b_tab_uniformity_retail_robust.tex",
  title   = "Within-chain retail price uniformity: robustness (month FEs added)",
  label   = "tab:uniformity_retail_robust",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = UNIF_DICT, notes = UNIF_NOTES_ROB
)
message("Saved: tables_latex/17b_tab_uniformity_retail_robust.tex")

# Wholesale — main
etable(
  list("(1) Retailer FE"          = regs_wholesale$main_ret,
       "(2) Product FE"            = regs_wholesale$main_prod,
       "(3) Retailer + Product FE" = regs_wholesale$main_ret_prod),
  tex = TRUE, file = "tables_latex/18_tab_uniformity_wholesale_main.tex",
  title   = "Within-chain wholesale cost uniformity: main results",
  label   = "tab:uniformity_wholesale_main",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = UNIF_DICT, notes = UNIF_NOTES_MAIN
)
message("Saved: tables_latex/18_tab_uniformity_wholesale_main.tex")

# Wholesale — robustness
etable(
  list("(1) Retailer + Month FE"           = regs_wholesale$rob_ret,
       "(2) Product + Month FE"             = regs_wholesale$rob_prod,
       "(3) Retailer + Product + Month FE"  = regs_wholesale$rob_ret_prod),
  tex = TRUE, file = "tables_latex/18b_tab_uniformity_wholesale_robust.tex",
  title   = "Within-chain wholesale cost uniformity: robustness (month FEs added)",
  label   = "tab:uniformity_wholesale_robust",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = UNIF_DICT, notes = UNIF_NOTES_ROB
)
message("Saved: tables_latex/18b_tab_uniformity_wholesale_robust.tex")


# ==============================================================================
# RETAILER HETEROGENEITY REGRESSIONS
# ==============================================================================
# no additional FE | product FE
# month FE         | product + month FE
# ==============================================================================

run_heterog_regs <- function(df) {
  list(
    main_prod    = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) | product,
                         data = df, cluster = ~ retailer_product),
    rob_month    = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) | month_fe,
                         data = df, cluster = ~ retailer_product),
    rob_prod_month = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) | product + month_fe,
                           data = df, cluster = ~ retailer_product)
  )
}

heterog_retail    <- run_heterog_regs(disp_retail)
heterog_wholesale <- run_heterog_regs(disp_wholesale)

HETEROG_NOTES <- c(
  "Each coefficient is a retailer-specific SOE or post-SOE deviation from the pre-SOE period.",
  "Standard errors clustered at the retailer-product level."
)

# Retail heterogeneity — combined (main + robustness)
etable(
  list("(1) Product FE"         = heterog_retail$main_prod,
       "(2) Month FE"           = heterog_retail$rob_month,
       "(3) Product + Month FE" = heterog_retail$rob_prod_month),
  tex = TRUE, file = "tables_latex/19_tab_uniformity_heterog_retail.tex",
  title  = "Retailer heterogeneity in within-chain retail price uniformity",
  label  = "tab:uniformity_heterog_retail",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes  = HETEROG_NOTES
)
message("Saved: tables_latex/19_tab_uniformity_heterog_retail.tex")

# Wholesale heterogeneity — combined (main + robustness)
etable(
  list("(1) Product FE"         = heterog_wholesale$main_prod,
       "(2) Month FE"           = heterog_wholesale$rob_month,
       "(3) Product + Month FE" = heterog_wholesale$rob_prod_month),
  tex = TRUE, file = "tables_latex/20_tab_uniformity_heterog_wholesale.tex",
  title  = "Retailer heterogeneity in within-chain wholesale cost uniformity",
  label  = "tab:uniformity_heterog_wholesale",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes  = HETEROG_NOTES
)
message("Saved: tables_latex/20_tab_uniformity_heterog_wholesale.tex")


# ==============================================================================
# COEFFICIENT PLOTS FOR RETAILER HETEROGENEITY
# ==============================================================================

extract_heterog_coef <- function(heterog_list, outcome_label, specs, spec_label) {
  purrr::map_dfr(specs, function(s) {
    broom::tidy(heterog_list[[s$key]], conf.int = TRUE) %>%
      mutate(spec = s$label)
  }) %>%
    filter(grepl(":during$|:post$", term)) %>%
    mutate(
      retailer = sub(".*retailer::([^:]+):.*", "\\1", term),
      period   = if_else(grepl(":during$", term), "During SOE", "Post-SOE"),
      outcome  = outcome_label,
      spec_grp = spec_label
    )
}

main_specs <- list(
  list(key = "main_prod",  label = "Product FE")
)
rob_specs <- list(
  list(key = "rob_month",      label = "Month FE"),
  list(key = "rob_prod_month", label = "Product + Month FE")
)

build_heterog_plot <- function(coef_df, title_str, filename) {
  g <- ggplot(coef_df,
              aes(x = retailer, y = estimate, ymin = conf.low, ymax = conf.high,
                  color = period, shape = period)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(position = position_dodge(width = 0.6)) +
    geom_text(aes(label = round(estimate, 3)),
              position = position_dodge(width = 0.6),
              vjust = -1.0, size = 2.8, show.legend = FALSE) +
    scale_shape_manual(values = c("During SOE" = 16, "Post-SOE" = 17)) +
    facet_grid(outcome ~ spec) +
    labs(
      x        = "Retailer",
      y        = "Coefficient (mean absolute log price diff)",
      title    = title_str,
      color    = NULL, shape = NULL
    ) +
    theme_bw() +
    theme(legend.position = "top", strip.text = element_text(size = 9))
  
  ggsave(file.path("figures", filename), g, width = 11, height = 8, dpi = 300)
  message("Saved: figures/", filename)
  invisible(g)
}

# Combined coefficient plot (main + robustness, faceted by outcome x spec)
all_specs <- list(
  list(key = "main_prod",      label = "Product FE"),
  list(key = "rob_month",      label = "Month FE"),
  list(key = "rob_prod_month", label = "Product + Month FE")
)

heterog_all_df <- bind_rows(
  extract_heterog_coef(heterog_retail,    "Retail",    all_specs, "all"),
  extract_heterog_coef(heterog_wholesale, "Wholesale", all_specs, "all")
) %>%
  mutate(
    spec    = factor(spec,    levels = c("Product FE", "Month FE", "Product + Month FE")),
    period  = factor(period,  levels = c("During SOE", "Post-SOE")),
    outcome = factor(outcome, levels = c("Retail", "Wholesale"))
  )

build_heterog_plot(
  heterog_all_df,
  title_str = "Retailer heterogeneity in within-chain price uniformity",
  filename  = "18_fig_uniformity_heterog_coef.png"
)

message("Uniform pricing analysis complete.")

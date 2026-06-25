# ==============================================================================
# 07_uniform_pricing.R
#
# Purpose: Section IV.D uniform pricing analysis.
#   Computes pairwise absolute log price differences within retailer chains
#   across stores, by product and week. Tests whether within-chain price
#   uniformity changed during and after the SOE.
#
# Regression outcome: mean absolute log price difference at the
#   (retailer, product, week) level.
#
# Depends on: panel_est, save_tex(), SAVE_CSV
#
# Outputs (tables_latex/):
#   15_tab_uniformity_summary_retail.tex
#   16_tab_uniformity_summary_wholesale.tex
#   17_tab_uniformity_retail.tex
#   18_tab_uniformity_wholesale.tex
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

message("Estimating Section IV.D uniform pricing ...")

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

# Pairwise absolute log price difference within retailer-product-week cells
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

# -- Distribution plots --
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

# -- Summary tables: mean absolute log diff by retailer and period --
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
}

make_disp_summary(
  pairs_retail,
  caption_str  = "Within-chain retail price uniformity by retailer and SOE period. Values are absolute log retail price differences across store pairs within the same chain, product, and week. Retailer 4 excluded.",
  label_str    = "tab:uniformity_summary_retail",
  filename_csv = "09_tab_uniformity_summary_retail.csv",
  filename_tex = "15_tab_uniformity_summary_retail.tex"
)

make_disp_summary(
  pairs_wholesale,
  caption_str  = "Within-chain wholesale cost uniformity by retailer and SOE period. Values are absolute log wholesale cost differences across store pairs within the same chain, product, and week. Retailer 4 excluded.",
  label_str    = "tab:uniformity_summary_wholesale",
  filename_csv = "10_tab_uniformity_summary_wholesale.csv",
  filename_tex = "16_tab_uniformity_summary_wholesale.tex"
)

# -- Regression panel: collapse to retailer-product-week --
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

# -- Pooled uniformity regressions (6 specifications) --
run_uniformity_regs <- function(df) {
  list(
    A  = feols(Diff_bar ~ during + post,
               data = df, cluster = ~ retailer_product),
    A2 = feols(Diff_bar ~ during + post | month_fe,
               data = df, cluster = ~ retailer_product),
    B  = feols(Diff_bar ~ during + post | retailer + product,
               data = df, cluster = ~ retailer_product),
    B2 = feols(Diff_bar ~ during + post | retailer + product + month_fe,
               data = df, cluster = ~ retailer_product),
    C  = feols(Diff_bar ~ during + post | retailer + product,
               data = df, cluster = ~ retailer_product),
    C2 = feols(Diff_bar ~ during + post | retailer + product + month_fe,
               data = df, cluster = ~ retailer_product)
  )
}

regs_retail    <- run_uniformity_regs(disp_retail)
regs_wholesale <- run_uniformity_regs(disp_wholesale)

etable(
  list("(1)" = regs_retail$A, "(2)" = regs_retail$A2,
       "(3)" = regs_retail$B, "(4)" = regs_retail$B2,
       "(5)" = regs_retail$C, "(6)" = regs_retail$C2),
  tex = TRUE, file = "tables_latex/17_tab_uniformity_retail.tex",
  title   = "Within-chain retail price uniformity during and after SOE",
  label   = "tab:uniformity_retail",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = c("during" = "During SOE", "post" = "Post-SOE"),
  notes   = c(
    "Dependent variable: mean absolute log retail price difference across store pairs within retailer-product-week cells.",
    "Omitted category: pre-SOE period.",
    "Standard errors clustered at the retailer-product level."
  )
)
message("Saved: tables_latex/17_tab_uniformity_retail.tex")

etable(
  list("(1)" = regs_wholesale$A, "(2)" = regs_wholesale$A2,
       "(3)" = regs_wholesale$B, "(4)" = regs_wholesale$B2,
       "(5)" = regs_wholesale$C, "(6)" = regs_wholesale$C2),
  tex = TRUE, file = "tables_latex/18_tab_uniformity_wholesale.tex",
  title   = "Within-chain wholesale cost uniformity during and after SOE",
  label   = "tab:uniformity_wholesale",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = c("during" = "During SOE", "post" = "Post-SOE"),
  notes   = c(
    "Dependent variable: mean absolute log wholesale cost difference across store pairs within retailer-product-week cells.",
    "Omitted category: pre-SOE period.",
    "Standard errors clustered at the retailer-product level."
  )
)
message("Saved: tables_latex/18_tab_uniformity_wholesale.tex")

# -- Retailer heterogeneity uniformity regressions --
run_heterog_regs <- function(df) {
  list(
    A  = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post),
               data = df, cluster = ~ retailer_product),
    A2 = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) | month_fe,
               data = df, cluster = ~ retailer_product),
    B  = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) | product,
               data = df, cluster = ~ retailer_product),
    B2 = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) | product + month_fe,
               data = df, cluster = ~ retailer_product)
  )
}

heterog_retail    <- run_heterog_regs(disp_retail)
heterog_wholesale <- run_heterog_regs(disp_wholesale)

heterog_notes <- c(
  "Each coefficient is a retailer-specific SOE or post-SOE deviation from the pre-SOE period.",
  "Standard errors clustered at the retailer-product level."
)

etable(
  list("(1)" = heterog_retail$A, "(2)" = heterog_retail$A2,
       "(3)" = heterog_retail$B, "(4)" = heterog_retail$B2),
  tex = TRUE, file = "tables_latex/19_tab_uniformity_heterog_retail.tex",
  title  = "Retailer heterogeneity in within-chain retail price uniformity",
  label  = "tab:uniformity_heterog_retail",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes  = heterog_notes
)
message("Saved: tables_latex/19_tab_uniformity_heterog_retail.tex")

etable(
  list("(1)" = heterog_wholesale$A, "(2)" = heterog_wholesale$A2,
       "(3)" = heterog_wholesale$B, "(4)" = heterog_wholesale$B2),
  tex = TRUE, file = "tables_latex/20_tab_uniformity_heterog_wholesale.tex",
  title  = "Retailer heterogeneity in within-chain wholesale cost uniformity",
  label  = "tab:uniformity_heterog_wholesale",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes  = heterog_notes
)
message("Saved: tables_latex/20_tab_uniformity_heterog_wholesale.tex")

# -- Coefficient plot for retailer heterogeneity --
extract_heterog_coef <- function(heterog_list, outcome_label) {
  model_pairs <- list(
    list(no_fe = "A",  with_fe = "A2", label = "No additional FEs"),
    list(no_fe = "B",  with_fe = "B2", label = "Product FE")
  )
  purrr::map_dfr(model_pairs, function(pair) {
    bind_rows(
      broom::tidy(heterog_list[[pair$no_fe]],  conf.int = TRUE) %>%
        mutate(month_fe = "No month FE"),
      broom::tidy(heterog_list[[pair$with_fe]], conf.int = TRUE) %>%
        mutate(month_fe = "Month FE")
    ) %>%
      filter(grepl(":during$|:post$", term)) %>%
      mutate(
        retailer = sub(".*retailer::([^:]+):.*", "\\1", term),
        period   = if_else(grepl(":during$", term), "During SOE", "Post-SOE"),
        fe_spec  = pair$label,
        outcome  = outcome_label
      )
  })
}

heterog_coef_df <- bind_rows(
  extract_heterog_coef(heterog_retail,    "Retail"),
  extract_heterog_coef(heterog_wholesale, "Wholesale")
) %>%
  mutate(
    fe_spec  = factor(fe_spec,  levels = c("No additional FEs", "Product FE")),
    month_fe = factor(month_fe, levels = c("No month FE", "Month FE")),
    period   = factor(period,   levels = c("During SOE", "Post-SOE")),
    outcome  = factor(outcome,  levels = c("Retail", "Wholesale"))
  )

g_heterog_coef <- ggplot(heterog_coef_df,
                         aes(x = retailer, y = estimate, ymin = conf.low, ymax = conf.high,
                             color = period, shape = month_fe)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  geom_text(aes(label = round(estimate, 3)),
            position = position_dodge(width = 0.6),
            vjust = -1.0, size = 2.8, show.legend = FALSE) +
  facet_grid(outcome ~ fe_spec) +
  labs(
    x        = "Retailer",
    y        = "Coefficient (mean absolute log price diff)",
    title    = "Retailer heterogeneity in within-chain price uniformity during and after SOE",
    subtitle = "Rows: retail vs wholesale. Columns: FE specification. Shape: with vs without month FE.",
    color    = NULL,
    shape    = NULL
  ) +
  theme_bw() +
  theme(legend.position = "top", plot.subtitle = element_text(size = 8),
        strip.text = element_text(size = 9))

ggsave("figures/18_fig_uniformity_heterog_coef.png", g_heterog_coef,
       width = 13, height = 8, dpi = 300)
message("Saved: figures/18_fig_uniformity_heterog_coef.png")

message("Uniform pricing analysis complete.")

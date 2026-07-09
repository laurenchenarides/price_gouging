# ==============================================================================
# 03_descriptive_tables.R
#
# Purpose: Produce all descriptive tables and figures.
#   5.  DecaData summary, product coverage (Tables II.A-C, Figs II.A-B)
#   6.  Period means (nominal main, real supplementary)
#   7.  Flagged-weeks table and stacked bar figure
#
# Depends on: panel_est (from 02_build_panel.R), save_tex(), SAVE_CSV
#
# Outputs (tables_latex/):
#   01_tab_decadata_summary.tex
#   02_tab_decadata_summary_wide.tex
#   03_tab_product_coverage.tex
#   04_tab_period_means_nominal.tex
#   05_tab_period_means_real.tex
#   06_tab_flagged_weeks_all.tex
#   07_tab_flagged_weeks_T25.tex
#   00_tab_summary_stats.tex
#
# Outputs (figures/):
#   01_fig_volume_and_prices_dual_axis.png
#   02_fig_cost_weekly.png
#   03_fig_flag_cluster_stacked.png
# ==============================================================================


# ==============================================================================
# 5. DECADATA SUMMARY AND PRODUCT COVERAGE
# ==============================================================================

message("Building summary and product coverage tables ...")

# -- SOE window bounds (used for shading in figures) --
soe_window <- panel_est %>%
  filter(SoE == 1) %>%
  summarise(
    soe_start = min(apg_start_date, na.rm = TRUE),
    soe_end   = max(apg_end_date,   na.rm = TRUE)
  )
soe_start <- soe_window$soe_start
soe_end   <- soe_window$soe_end

# -- 4-week window indices for diagnostics --
soe_start_week <- panel_est %>%
  filter(SoE == 1) %>%
  summarise(v = min(as.integer(week_seq), na.rm = TRUE)) %>% pull(v)
soe_end_week <- panel_est %>%
  filter(SoE == 1) %>%
  summarise(v = max(as.integer(week_seq), na.rm = TRUE)) %>% pull(v)

pre_weeks        <- seq.int(soe_start_week - 4L, soe_start_week - 1L)
soe_weeks_first4 <- seq.int(soe_start_week,      soe_start_week + 3L)
soe_weeks_last4  <- seq.int(soe_end_week - 3L,   soe_end_week)

# -- Volume diagnostics --
wk_vol <- panel_est %>%
  group_by(week_seq, week_start) %>%
  summarise(total_vol = sum(upc_week_volume, na.rm = TRUE), .groups = "drop")

pre_vol        <- wk_vol %>% filter(week_seq %in% pre_weeks)        %>% summarise(v = sum(total_vol)) %>% pull(v)
soe_vol_first4 <- wk_vol %>% filter(week_seq %in% soe_weeks_first4) %>% summarise(v = sum(total_vol)) %>% pull(v)
pct_chg_vol_onset <- 100 * (soe_vol_first4 - pre_vol) / pre_vol
message(sprintf("Volume: 4-week pre vs first 4 SOE weeks: %+.2f%%", pct_chg_vol_onset))

# -- Nominal price diagnostics --
wk_price_nom <- panel_est %>%
  group_by(week_seq) %>%
  summarise(mean_p_nom = mean(p_ist, na.rm = TRUE), .groups = "drop")
pre_p_nom        <- wk_price_nom %>% filter(week_seq %in% pre_weeks)        %>% summarise(v = mean(mean_p_nom)) %>% pull(v)
soe_p_nom_first4 <- wk_price_nom %>% filter(week_seq %in% soe_weeks_first4) %>% summarise(v = mean(mean_p_nom)) %>% pull(v)
soe_p_nom_last4  <- wk_price_nom %>% filter(week_seq %in% soe_weeks_last4)  %>% summarise(v = mean(mean_p_nom)) %>% pull(v)
pct_chg_nom_onset <- 100 * (soe_p_nom_first4 - pre_p_nom) / pre_p_nom
pct_chg_nom_full  <- 100 * (soe_p_nom_last4  - pre_p_nom) / pre_p_nom

# -- Real price diagnostics --
wk_price_real <- panel_est %>%
  group_by(week_seq) %>%
  summarise(mean_p_real = mean(p_real, na.rm = TRUE), .groups = "drop")
pre_p_real        <- wk_price_real %>% filter(week_seq %in% pre_weeks)        %>% summarise(v = mean(mean_p_real)) %>% pull(v)
soe_p_real_first4 <- wk_price_real %>% filter(week_seq %in% soe_weeks_first4) %>% summarise(v = mean(mean_p_real)) %>% pull(v)
soe_p_real_last4  <- wk_price_real %>% filter(week_seq %in% soe_weeks_last4)  %>% summarise(v = mean(mean_p_real)) %>% pull(v)
pct_chg_real_onset <- 100 * (soe_p_real_first4 - pre_p_real) / pre_p_real
pct_chg_real_full  <- 100 * (soe_p_real_last4  - pre_p_real) / pre_p_real

# -- Wholesale cost diagnostics --
wk_cost <- panel_est %>%
  group_by(week_seq) %>%
  summarise(
    mean_w_nom  = mean(w_ist,  na.rm = TRUE),
    mean_w_real = mean(w_real, na.rm = TRUE),
    .groups = "drop"
  )
pre_w_nom        <- wk_cost %>% filter(week_seq %in% pre_weeks)        %>% summarise(v = mean(mean_w_nom))  %>% pull(v)
soe_w_nom_first4 <- wk_cost %>% filter(week_seq %in% soe_weeks_first4) %>% summarise(v = mean(mean_w_nom))  %>% pull(v)
soe_w_nom_last4  <- wk_cost %>% filter(week_seq %in% soe_weeks_last4)  %>% summarise(v = mean(mean_w_nom))  %>% pull(v)
pre_w_real        <- wk_cost %>% filter(week_seq %in% pre_weeks)        %>% summarise(v = mean(mean_w_real)) %>% pull(v)
soe_w_real_first4 <- wk_cost %>% filter(week_seq %in% soe_weeks_first4) %>% summarise(v = mean(mean_w_real)) %>% pull(v)
soe_w_real_last4  <- wk_cost %>% filter(week_seq %in% soe_weeks_last4)  %>% summarise(v = mean(mean_w_real)) %>% pull(v)
pct_chg_cost_nom_onset  <- 100 * (soe_w_nom_first4  - pre_w_nom)  / pre_w_nom
pct_chg_cost_nom_full   <- 100 * (soe_w_nom_last4   - pre_w_nom)  / pre_w_nom
pct_chg_cost_real_onset <- 100 * (soe_w_real_first4 - pre_w_real) / pre_w_real
pct_chg_cost_real_full  <- 100 * (soe_w_real_last4  - pre_w_real) / pre_w_real

# -- Weekly volume with mean nominal and real prices (dual axis) --
wk <- panel_est %>%
  group_by(week_start, week_seq) %>%
  summarise(
    total_volume  = sum(upc_week_volume, na.rm = TRUE),
    mean_p_retail = mean(p_ist,          na.rm = TRUE),
    mean_p_real   = mean(p_real,         na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(week_start)

vol_rng   <- range(wk$total_volume, na.rm = TRUE)
price_rng <- range(c(wk$mean_p_retail, wk$mean_p_real), na.rm = TRUE)
scale_fac <- (vol_rng[2] - vol_rng[1]) / (price_rng[2] - price_rng[1])
shift_fac <- vol_rng[1] - price_rng[1] * scale_fac

wk <- wk %>%
  mutate(
    p_retail_scaled = mean_p_retail * scale_fac + shift_fac,
    p_real_scaled   = mean_p_real   * scale_fac + shift_fac
  )

g_vol_price <- ggplot(wk, aes(x = week_start)) +
  annotate("rect",
           xmin = soe_start, xmax = soe_end,
           ymin = -Inf, ymax = Inf, alpha = 0.12) +
  geom_col(aes(y = total_volume), alpha = 0.55, width = 4) +
  geom_line(aes(y = p_retail_scaled, linetype = "Mean Retail Price (nominal)"), linewidth = 0.8) +
  geom_line(aes(y = p_real_scaled,   linetype = "Mean Retail Price (real)"),   linewidth = 0.8) +
  scale_y_continuous(
    name     = "Total weekly volume",
    sec.axis = sec_axis(transform = ~ (. - shift_fac) / scale_fac, name = "Mean price")
  ) +
  labs(
    title    = "Weekly volume with mean prices (dual axis)",
    subtitle = paste0(
      "Products: bananas, cabbage, cucumbers, lettuce, tomatoes.\n",
      "Shaded = SOE period (", soe_start, " to ", soe_end, ").\n",
      sprintf("Pre to first 4 SOE weeks -- volume: %+.1f%%, nom. price: %+.1f%%, real price: %+.1f%%, nom. cost: %+.1f%%.",
              pct_chg_vol_onset, pct_chg_nom_onset, pct_chg_real_onset, pct_chg_cost_nom_onset)
    ),
    x = "Week", linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top", plot.subtitle = element_text(size = 8))

ggsave("figures/01_fig_volume_and_prices_dual_axis.png", g_vol_price,
       width = 11, height = 5.5, dpi = 300)
message("Saved: figures/01_fig_volume_and_prices_dual_axis.png")

# -- Mean wholesale cost over time --
cost_weekly <- panel_est %>%
  group_by(week_start) %>%
  summarise(
    avg_cost_nom  = mean(w_ist,   na.rm = TRUE),
    avg_cost_real = mean(w_real,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(week_start) %>%
  pivot_longer(c(avg_cost_nom, avg_cost_real), names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         avg_cost_nom  = "Nominal",
                         avg_cost_real = "Real (Jan 2018 base)"))

g_cost_weekly <- ggplot(cost_weekly, aes(x = week_start, y = value, linetype = series)) +
  annotate("rect",
           xmin = soe_start, xmax = soe_end,
           ymin = -Inf, ymax = Inf, alpha = 0.12) +
  geom_line(linewidth = 0.7) +
  labs(
    title    = "Mean wholesale cost over time",
    subtitle = paste0(
      "Unweighted mean across store-product-weeks. Shaded = pooled SOE window.\n",
      sprintf("Pre to first 4 SOE weeks: nom. cost %+.1f%%, real cost %+.1f%%.",
              pct_chg_cost_nom_onset, pct_chg_cost_real_onset)
    ),
    x = "Week", y = "Mean wholesale cost ($)", linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top", plot.subtitle = element_text(size = 8))

ggsave("figures/02_fig_cost_weekly.png", g_cost_weekly,
       width = 11, height = 5.5, dpi = 300)
message("Saved: figures/02_fig_cost_weekly.png")

# -- DecaData coverage by year --
panel_est %>%
  group_by(store_id) %>%
  summarise(
    first_year = min(as.integer(format(week_start, "%Y")), na.rm = TRUE),
    last_year  = max(as.integer(format(week_start, "%Y")), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  count(first_year)

tab_decadata_raw <- panel_est %>%
  mutate(year = as.integer(format(week_start, "%Y"))) %>%
  group_by(year) %>%
  summarise(
    n_banners = n_distinct(retailer_id),
    n_stores  = n_distinct(store_id),
    n_obs     = n(),
    .groups   = "drop"
  ) %>%
  arrange(year) %>%
  mutate(year = as.character(year)) %>%
  rename(
    Year                  = year,
    Banners               = n_banners,
    `Store locations`     = n_stores,
    `Store-product-weeks` = n_obs
  )

if (SAVE_CSV) write.csv(tab_decadata_raw, "tables_csv/01_tab_decadata_summary.csv", row.names = FALSE)

save_tex(
  kbl(tab_decadata_raw,
      format = "latex", booktabs = TRUE,
      caption = "DecaData coverage by year. Five Southeastern states, five fresh produce items.",
      label   = "decadata_summary",
      align   = "lrrrr",
      format.args = list(big.mark = ",")) %>%
    kable_styling(latex_options = c("hold_position")),
  "01_tab_decadata_summary.tex"
)

# -- DecaData coverage by year, retailer, and state (wide) --
tab_decadata_wide <- panel_est %>%
  mutate(year = as.integer(format(week_start, "%Y"))) %>%
  group_by(year, retailer_id, sst) %>%
  summarise(n_stores = n_distinct(store_id), .groups = "drop") %>%
  pivot_wider(names_from = sst, values_from = n_stores, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !c(year)), na.rm = TRUE)) %>%
  arrange(year, retailer_id) %>%
  mutate(year = as.character(year)) %>%
  rename(Year = year, Retailer = retailer_id)

if (SAVE_CSV) write.csv(tab_decadata_wide, "tables_csv/02_tab_decadata_summary_wide.csv", row.names = FALSE)

state_cols <- sort(setdiff(names(tab_decadata_wide), c("Year", "Retailer", "Total")))
n_states   <- length(state_cols)

save_tex(
  kbl(tab_decadata_wide %>% select(Year, Retailer, all_of(state_cols), Total),
      format = "latex", booktabs = TRUE,
      caption = paste0(
        "Store locations by year, retailer, and state. ",
        "The sample covers five Southeastern states and five fresh produce categories: ",
        "bananas, cabbage, cucumbers, lettuce, and tomatoes. ",
        "Product selection required a UPC to pass coverage screens in at least 75\\% of stores, ",
        "where a store-level pass required at least 80\\% weekly presence in windows around ",
        "both APG activation and deactivation and at least five pre- and post-APG weeks observed. ",
        "Within each category, UPCs are ranked by total net sales; up to five per category are retained. ",
        "The final panel uses one UPC each for bananas, cabbage, cucumbers, lettuce, and tomatoes."
      ),
      label = "decadata_summary_wide",
      align = paste0("ll", strrep("r", n_states + 1)),
      format.args = list(big.mark = ",")) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "top") %>%
    add_header_above(c(" " = 2, "Store locations by state" = n_states, " " = 1)) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")),
  "02_tab_decadata_summary_wide.tex"
)

# -- Five-product coverage --
tab_product_raw <- panel_est %>%
  group_by(product) %>%
  summarise(
    n_total      = n(),
    n_positive   = sum(upc_week_volume > 0, na.rm = TRUE),
    coverage     = n_positive / n_total,
    avg_price    = mean(p_ist[upc_week_volume > 0], na.rm = TRUE),
    total_volume = sum(upc_week_volume, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  arrange(product) %>%
  mutate(
    product      = stringr::str_to_title(as.character(product)),
    coverage     = round(coverage, 3),
    avg_price    = round(avg_price, 2),
    total_volume = round(total_volume, 0)
  ) %>%
  select(product, coverage, avg_price, total_volume) %>%
  rename(
    Product            = product,
    `Coverage (share)` = coverage,
    `Avg. price ($)`   = avg_price,
    `Total volume`     = total_volume
  )

if (SAVE_CSV) write.csv(tab_product_raw, "tables_csv/03_tab_product_coverage.csv", row.names = FALSE)

save_tex(
  kbl(tab_product_raw,
      format = "latex", booktabs = TRUE,
      caption = "Coverage and sales for the five fresh produce categories, 2018--2023. Coverage is the share of store-product-weeks with positive sales. Average price is in nominal dollars per unit or pound. Volume units: pounds (bananas, cabbage, cucumbers, tomatoes) and 8~oz bags (lettuce).",
      label   = "product_coverage",
      align   = "lrrr",
      format.args = list(big.mark = ",")) %>%
    kable_styling(latex_options = c("hold_position")),
  "03_tab_product_coverage.tex"
)


# ==============================================================================
# 6. PERIOD MEANS
# ==============================================================================

message("Building period means tables ...")

make_period_means_long <- function(df, price_col, cost_col, margin_col) {
  df_period <- df %>%
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
  
  # By period
  period_long <- df_period %>%
    group_by(product, period) %>%
    summarise(
      Volume = mean(upc_week_volume, na.rm = TRUE),
      Price  = mean(.data[[price_col]][upc_week_volume > 0],  na.rm = TRUE),
      Cost   = mean(.data[[cost_col]][upc_week_volume > 0],   na.rm = TRUE),
      Margin = mean(.data[[margin_col]][upc_week_volume > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(period = as.character(period)) %>%
    pivot_longer(c(Volume, Price, Cost, Margin), names_to = "metric", values_to = "value")
  
  # Pooled across all periods: total volume (sum), pooled avg price/cost/margin
  total_long <- df_period %>%
    group_by(product) %>%
    summarise(
      Volume = sum(upc_week_volume, na.rm = TRUE),
      Price  = mean(.data[[price_col]][upc_week_volume > 0],  na.rm = TRUE),
      Cost   = mean(.data[[cost_col]][upc_week_volume > 0],   na.rm = TRUE),
      Margin = mean(.data[[margin_col]][upc_week_volume > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(c(Volume, Price, Cost, Margin), names_to = "metric", values_to = "value") %>%
    mutate(period = if_else(metric == "Volume", "Total", "All"))
  
  bind_rows(period_long, total_long) %>%
    mutate(
      product = stringr::str_to_title(as.character(product)),
      metric  = factor(metric, levels = c("Volume", "Price", "Cost", "Margin")),
      period  = factor(period, levels = c("Pre-SOE", "During SOE", "Post-SOE", "Total", "All")),
      value   = if_else(metric == "Volume", round(value, 0), round(value, 2))
    ) %>%
    arrange(metric, period, product) %>%
    pivot_wider(names_from = product, values_from = value) %>%
    arrange(metric, period)
}

make_period_table_kbl <- function(df_wide, caption_str, label_str) {
  product_cols <- setdiff(names(df_wide), c("metric", "period"))
  
  pack_index <- df_wide %>%
    count(metric, .drop = FALSE) %>%
    tibble::deframe()
  
  tbl <- df_wide %>% select(period, all_of(product_cols))
  
  kbl(tbl,
      format = "latex", booktabs = TRUE,
      caption = caption_str, label = label_str,
      align   = paste0("l", strrep("r", length(product_cols))),
      col.names = c(" ", product_cols),
      format.args = list(big.mark = ",")) %>%
    pack_rows(index = pack_index) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
}

# Nominal (main)
means_nom <- make_period_means_long(panel_est, "p_ist", "w_ist", "margin_nom")

if (SAVE_CSV) write.csv(means_nom, "tables_csv/04_tab_period_means_nominal.csv", row.names = FALSE)

save_tex(
  make_period_table_kbl(
    means_nom,
    caption_str  = "Average weekly volume, nominal retail price, nominal wholesale cost, and nominal dollar margin by product and SOE period. ``Total'' is summed volume across the full sample; ``All'' rows are pooled averages across all periods. Averages computed across store-product-weeks with positive sales. Volume units: pounds (bananas, cabbage, cucumbers, tomatoes) and 8~oz bags (lettuce).",
    label_str    = "period_means_nominal"
  ),
  "04_tab_period_means_nominal.tex"
)

# Real (supplementary)
means_real <- make_period_means_long(panel_est, "p_real", "w_real", "margin_real")

if (SAVE_CSV) write.csv(means_real, "tables_csv/05_tab_period_means_real.csv", row.names = FALSE)

save_tex(
  make_period_table_kbl(
    means_real,
    caption_str  = "Average weekly volume, real retail price, real wholesale cost, and real dollar margin by product and SOE period (January 2018 = 1.00 base). ``Total'' is summed volume across the full sample; ``All'' rows are pooled averages across all periods. Averages computed across store-product-weeks with positive sales. Volume units: pounds (bananas, cabbage, cucumbers, tomatoes) and 8~oz bags (lettuce).",
    label_str    = "period_means_real"
  ),
  "05_tab_period_means_real.tex"
)


# ==============================================================================
# 7. FLAGGED-WEEKS TABLE
# ==============================================================================
# A store-product-week is flagged if the nominal retail price during the SOE
# exceeds the thirty-day pre-SOE average by more than threshold kappa.
# Cost justification: dollar price increase does not exceed dollar cost increase.
# ==============================================================================

message("Building flagged-weeks table ...")

THRESHOLDS <- c(0.10, 0.15, 0.20, 0.25, 0.30)
COST_MULT  <- 1.00

# NOTE: the baseline window and Alabama's statute reference a THIRTY-day pre-SOE window.
baseline_flag <- panel_est %>%
  filter(!is.na(apg_start_date)) %>%
  mutate(days_to_start = as.integer(apg_start_date - week_start)) %>%
  filter(days_to_start > 0, days_to_start <= 30) %>%
  group_by(store_id, product) %>%
  summarise(
    p_base = mean(p_ist, na.rm = TRUE),
    w_base = mean(w_ist, na.rm = TRUE),
    .groups = "drop"
  )

flag_data <- panel_est %>%
  filter(SoE == 1) %>%
  left_join(baseline_flag, by = c("store_id", "product")) %>%
  filter(!is.na(p_base), p_base > 0) %>%
  mutate(
    pct_above_base = (p_ist / p_base) - 1,
    dp_from_base   = p_ist - p_base,
    dw_from_base   = w_ist - w_base,
    cost_justified = case_when(
      is.na(dp_from_base) | is.na(dw_from_base) ~ NA_integer_,
      dp_from_base <= 0                          ~ 0L,
      dw_from_base <= 0                          ~ 0L,
      dp_from_base <= COST_MULT * dw_from_base   ~ 1L,
      TRUE                                       ~ 0L
    )
  ) %>%
  filter(!is.na(pct_above_base))

flag_long <- flag_data %>%
  tidyr::crossing(threshold = THRESHOLDS) %>%
  mutate(
    thresh_lbl      = paste0("T", as.integer(threshold * 100)),
    flagged         = as.integer(pct_above_base >= threshold),
    flagged_just    = as.integer(flagged == 1L & cost_justified == 1L),
    flagged_notjust = as.integer(flagged == 1L & cost_justified == 0L)
  )

# All-thresholds table (pooled)
tab_flagged_all_raw <- flag_long %>%
  group_by(thresh_lbl, threshold) %>%
  summarise(
    n_soe              = n(),
    share_flagged      = mean(flagged, na.rm = TRUE),
    share_just_flagged = if_else(
      sum(flagged, na.rm = TRUE) > 0,
      sum(flagged_just, na.rm = TRUE) / sum(flagged, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  arrange(threshold) %>%
  mutate(
    Threshold               = paste0(as.integer(threshold * 100), "%"),
    `SOE store-weeks`       = n_soe,
    `% flagged`             = round(100 * share_flagged, 1),
    `% flagged, cost-just.` = round(100 * share_just_flagged, 1)
  ) %>%
  select(Threshold, `SOE store-weeks`, `% flagged`, `% flagged, cost-just.`)

if (SAVE_CSV) write.csv(tab_flagged_all_raw, "tables_csv/06_tab_flagged_weeks_all.csv", row.names = FALSE)

save_tex(
  kbl(tab_flagged_all_raw,
      format = "latex", booktabs = TRUE,
      caption = "Hypothetical flagged-weeks across thresholds. A store-product-week is flagged if the nominal retail price exceeds the thirty-day pre-SOE average by more than the listed threshold. Cost justification: dollar price increase does not exceed dollar cost increase (COST\\_MULT = 1.00). Pooled across all five products and retailers 2, 3, and 5.",
      label   = "flagged_weeks_all",
      align   = "lrrr",
      format.args = list(big.mark = ",")) %>%
    kable_styling(latex_options = c("hold_position")),
  "06_tab_flagged_weeks_all.tex"
)

# T25 table by product
tab_flagged_T25_raw <- flag_long %>%
  filter(thresh_lbl == "T25") %>%
  group_by(product) %>%
  summarise(
    n_soe              = n(),
    share_flagged      = mean(flagged, na.rm = TRUE),
    share_just_flagged = if_else(
      sum(flagged, na.rm = TRUE) > 0,
      sum(flagged_just, na.rm = TRUE) / sum(flagged, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Product                 = stringr::str_to_title(as.character(product)),
    `SOE store-weeks`       = n_soe,
    `% flagged`             = round(100 * share_flagged, 1),
    `% flagged, cost-just.` = round(100 * share_just_flagged, 1)
  ) %>%
  select(Product, `SOE store-weeks`, `% flagged`, `% flagged, cost-just.`)

if (SAVE_CSV) write.csv(tab_flagged_T25_raw, "tables_csv/07_tab_flagged_weeks_T25.csv", row.names = FALSE)

save_tex(
  kbl(tab_flagged_T25_raw,
      format = "latex", booktabs = TRUE,
      caption = "Hypothetical flagged-weeks at the 25\\% threshold (matches Alabama's statutory limit). A store-product-week is flagged if the nominal retail price exceeds the thirty-day pre-SOE average by more than 25\\%. Cost justification: dollar price increase does not exceed dollar cost increase. Pooled across retailers 2, 3, and 5.",
      label   = "flagged_weeks_T25",
      align   = "lrrr",
      format.args = list(big.mark = ",")) %>%
    kable_styling(latex_options = c("hold_position")),
  "07_tab_flagged_weeks_T25.tex"
)

# -- Flagged weeks stacked bar by threshold and retailer --
thresh_lbls_ordered <- paste0("T", as.integer(THRESHOLDS * 100))

plot_df <- flag_long %>%
  mutate(retailer_id = as.character(retailer_id)) %>%
  group_by(thresh_lbl, retailer_id, product) %>%
  summarise(
    pct_just = 100 * mean(flagged_just,    na.rm = TRUE),
    pct_not  = 100 * mean(flagged_notjust, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  group_by(thresh_lbl, retailer_id) %>%
  summarise(
    pct_just = mean(pct_just, na.rm = TRUE),
    pct_not  = mean(pct_not,  na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    thresh_lbl  = factor(thresh_lbl,  levels = thresh_lbls_ordered),
    retailer_id = factor(retailer_id, levels = sort(unique(retailer_id))),
    x_group     = interaction(thresh_lbl, retailer_id, sep = " : ")
  )

plot_long <- plot_df %>%
  pivot_longer(c(pct_just, pct_not), names_to = "component", values_to = "pct") %>%
  mutate(
    component = recode(component,
                       pct_just = "Flagged & cost-justified",
                       pct_not  = "Flagged & not cost-justified"),
    component = factor(component,
                       levels = c("Flagged & not cost-justified",
                                  "Flagged & cost-justified"))
  )

g_flag_cluster <- ggplot(plot_long,
                         aes(x = x_group, y = pct, fill = component, pattern = component)) +
  geom_col_pattern(
    width           = 0.85,
    pattern_density = 0.4,
    pattern_spacing = 0.03,
    pattern_fill    = "black",
    pattern_colour  = "black",
    colour          = "grey30",
    linewidth       = 0.2
  ) +
  scale_fill_manual(values = c(
    "Flagged & not cost-justified" = "grey85",
    "Flagged & cost-justified"     = "grey40"
  )) +
  scale_pattern_manual(values = c(
    "Flagged & not cost-justified" = "none",
    "Flagged & cost-justified"     = "stripe"
  )) +
  labs(
    title    = "APG flag rates during SOE (stacked by cost justification)",
    subtitle = "Bar height = average % of SOE store-weeks flagged across products. Textured = cost-justified. Untextured = not cost-justified.",
    x = "Threshold : Retailer", y = "% of SOE store-weeks",
    fill = NULL, pattern = NULL
  ) +
  guides(
    fill    = guide_legend(override.aes = list(pattern = c("none", "stripe"))),
    pattern = "none"
  ) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 35, hjust = 1),
        plot.subtitle = element_text(size = 8))

ggsave("figures/03_fig_flag_cluster_stacked.png", g_flag_cluster,
       width = 12, height = 6, dpi = 300)
message("Saved: figures/03_fig_flag_cluster_stacked.png")

# ==============================================================================
# SUMMARY STATISTICS TABLE
# ==============================================================================

sum_stat <- function(x) {
  x <- x[is.finite(x)]
  q <- quantile(x, c(0.25, 0.5, 0.75))
  c(Mean = mean(x), SD = sd(x),
    P25 = q[[1]], Median = q[[2]], P75 = q[[3]],
    Min = min(x), Max = max(x))
}

dur_by_state <- panel_est %>%
  distinct(sst, Dur_st) %>%
  filter(Dur_st > 0)          # keep only states that had an SOE

vars <- list(
  list(sym = "$p_{ist}$",       desc = "Retail price $p_{ist}$ (\\$/unit or lb)",              col = "p_ist"),
  list(sym = "$w_{ist}$",       desc = "Wholesale cost $w_{ist}$ (\\$/unit or lb)",             col = "w_ist"),
  list(sym = "$m_{ist}$",       desc = "Margin $m_{ist} = p_{ist} - w_{ist}$",                 col = "margin_nom"),
  list(sym = "$\\Delta p$",     desc = "Weekly change in retail price $\\Delta p_{ist}$",      col = "dP"),
  list(sym = "$\\Delta w$",     desc = "Weekly change in wholesale cost $\\Delta w_{ist}$",    col = "dW"),
  list(sym = "$SoE$",           desc = "State of emergency indicator $SoE_{st}$",              col = "SoE"),
  list(sym = "$k$",             desc = "Event time $k$ relative to end of $SoE_{st}$",        col = "k_end"),
  list(sym = "$Dur_s$",
       desc = "APG enforcement duration $Dur_s$ (weeks)\\textsuperscript{a}",
       col  = NULL,
       vals = dur_by_state$Dur_st)
)

sumstat_rows <- purrr::map_dfr(vars, function(v) {
  x <- if (!is.null(v$vals)) v$vals else panel_est[[v$col]]
  s <- sum_stat(x)
  tibble::tibble(
    Variable    = v$sym,
    Description = v$desc,
    Mean        = round(s["Mean"],   3),
    `Std. Dev.` = round(s["SD"],     3),
    `25th`      = round(s["P25"],    3),
    Median      = round(s["Median"], 3),
    `75th`      = round(s["P75"],    3),
    Min         = round(s["Min"],    3),
    Max         = round(s["Max"],    3)
  )
})

sumstat_notes <- paste0(
  "\\vspace{0.5em}\n",
  "\\begin{minipage}{\\linewidth}\n",
  "\\footnotesize\n",
  "\\textit{Notes:} Statistics computed at the store--product--week level over 2018--2023. ",
  "Prices and costs are in dollars per unit or per pound depending on whether the item is sold by count or weight. ",
  "$\\Delta p_{ist}$ and $\\Delta w_{ist}$ are weekly first differences of the corresponding levels. ",
  "$SoE_{st}$ equals one in weeks when a COVID-19 state of emergency (and associated APG protections) is active in state $s$, zero otherwise. ",
  "$k$ is event time in weeks relative to the end of the state of emergency. ",
  "\\textsuperscript{a}\\,$Dur_s$ is the total duration of the state of emergency in each state; statistics are computed across the five states in the sample.",
  "\n\\end{minipage}"
)

save_tex(
  kbl(sumstat_rows,
      format    = "latex", booktabs = TRUE, escape = FALSE,
      caption   = "Summary statistics: store--product--week panel",
      label     = "summary_stats",
      align     = paste0("ll", strrep("r", 7)),
      col.names = c("Variable", "Description", "Mean", "Std.\\ Dev.",
                    "25th pctl", "Median", "75th pctl", "Min", "Max")) %>%
    row_spec(5, extra_latex_after = "\\addlinespace") %>%
    row_spec(6, extra_latex_after = "\\addlinespace") %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(
      general = c(
        "Statistics computed at the store--product--week level over 2018--2023.",
        "Prices and costs are in dollars per unit or per pound.",
        "$\\\\Delta p_{ist}$ and $\\\\Delta w_{ist}$ are weekly first differences.",
        "$SoE_{st}$ equals one when a COVID-19 state of emergency is active in state $s$.",
        "$k$ is event time in weeks relative to the end of the state of emergency.",
        "\\\\textsuperscript{a}$Dur_s$ is the total duration of the state of emergency in each state; statistics are computed across the five states in the sample."
      ),
      general_title = "\\\\textit{Notes:}",
      escape = FALSE
    ),
  "00_tab_summary_stats.tex"
)

message("Saved: tables_latex/00_tab_summary_stats.tex")

message("Descriptive tables complete.")

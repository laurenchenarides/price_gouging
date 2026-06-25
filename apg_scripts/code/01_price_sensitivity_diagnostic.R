# ==============================================================================
# 01_price_sensitivity_diagnostic.R
#
# Diagnostic: sensitivity of period means and margins to price definition.
# Compares three price measures:
#   p_ist_net      : volume-weighted net price (after promotional discounts)
#   p_ist_gross    : volume-weighted gross price (posted shelf price)
#   avg_unit_price : simple daily average of posted shelf price (equal weight
#                    across days within the week, immune to volume artifacts)
#
# Key motivation: share of transactions on sale was higher during the SOE
# (~30%) than pre-SOE (~8%), which means p_ist_net is pulled down more
# during the SOE than in other periods. This creates a mechanical downward
# bias in p_ist_net during the SOE relative to p_ist_gross, and may
# overstate the price decline during the emergency period.
#
# Data corrections applied in this script before producing outputs:
#   - Peppers, week of 2019-05-27: p_ist_gross and p_ist_net replaced with
#     avg_unit_price (~$1.02). volume-weighted prices were inflated to
#     $200-$430/lb due to near-zero volume denominators in a subset of stores.
#     avg_unit_price is unaffected because it does not use volume as a
#     denominator. Confirmed via spot check that posted shelf price was normal.
#   - Peppers, week of 2019-05-27: w_ist replaced with the median w_ist for
#     peppers across the immediately adjacent weeks (2019-05-20 and
#     2019-06-03). The replacement value of $0.52/lb is consistent with the
#     five-week window around the spike (range: $0.49 to $0.55 mean).
#     Corrections are applied to panel_diag only and do not affect
#     panel_upc_week or the main estimation panel.
#
# Outputs:
#   tables_csv/diag_01_sale_share_by_period.csv
#   tables_csv/diag_02_price_comparison_by_period.csv
#   tables_csv/diag_03_margin_comparison_by_period.csv
#   figures/diag_01_sale_share_over_time.png
#   figures/diag_02_price_series_comparison.png
#   figures/diag_03_price_gap_over_time.png
#   figures/diag_04_margin_series_comparison.png
#   figures/diag_05_margin_gap_over_time.png
# ==============================================================================

# ------------------------------------------------------------------------------
# Assign period labels
# ------------------------------------------------------------------------------

names(panel_upc_week)

panel_diag <- panel_upc_week %>%
  filter(retailer_id %in% RETAILERS_KEEP) %>%
  mutate(
    # Period based on SOE indicator; pre/post derived from apg dates
    period = case_when(
      SoE == 1L                                    ~ "During SOE",
      !is.na(apg_start_date) &
        week_start < apg_start_date                ~ "Pre-SOE",
      !is.na(apg_end_date) &
        week_start > apg_end_date                  ~ "Post-SOE",
      TRUE                                         ~ NA_character_
    ),
    period = factor(period, levels = c("Pre-SOE", "During SOE", "Post-SOE")),
    # Margins for each price definition
    margin_net   = p_ist_net   - w_ist,
    margin_gross = p_ist_gross - w_ist,
    margin_avg   = avg_unit_price - w_ist
  ) %>%
  filter(!is.na(period))

# Identify the source of the price spike visible in diag_02
# Find the week with the highest mean gross price
spike_week <- panel_diag %>%
  filter(p_ist_gross > 0) %>%
  group_by(week_start) %>%
  summarise(mean_p_gross = mean(p_ist_gross, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_p_gross)) %>%
  slice(1) %>%
  pull(week_start)

message("Spike week: ", spike_week)

# Within that week, find which product and stores are driving it
spike_detail <- panel_diag %>%
  filter(week_start == spike_week, p_ist_gross > 0) %>%
  arrange(desc(p_ist_gross)) %>%
  select(week_start, product, store_id, retailer_id, sst,
         p_ist_gross, p_ist_net, avg_unit_price, w_ist,
         upc_week_volume, upc_week_gross_sales)

print(spike_detail)

# Summary by product for that week
spike_by_product <- panel_diag %>%
  filter(week_start == spike_week, p_ist_gross > 0) %>%
  group_by(product) %>%
  summarise(
    n_stores      = n(),
    mean_p_gross  = mean(p_ist_gross, na.rm = TRUE),
    max_p_gross   = max(p_ist_gross,  na.rm = TRUE),
    mean_volume   = mean(upc_week_volume, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_p_gross))

print(spike_by_product)

# Check w_ist for peppers in the spike week
panel_diag %>%
  filter(product == "peppers", week_start == as.Date("2019-05-27")) %>%
  arrange(desc(w_ist)) %>%
  select(week_start, product, store_id, retailer_id, sst,
         p_ist_gross, p_ist_net, avg_unit_price, w_ist,
         upc_week_volume, upc_week_total_cost) %>%
  print(n = 20)

# ------------------------------------------------------------------------------
# Price artifact correction: peppers, week of 2019-05-27
#
# volume-weighted prices (p_ist_gross, p_ist_net) are inflated for peppers
# in this week due to near-zero volume denominators in a subset of stores.
# The posted shelf price (avg_unit_price) is ~$1.02 and is unaffected by
# the volume artifact. For this week and product only, replace p_ist_gross
# and p_ist_net with avg_unit_price.
#
# This correction is applied to panel_diag only and does not affect
# panel_upc_week or the main estimation panel. If the main regressions
# are also sensitive to this artifact, apply the same correction there.
# ------------------------------------------------------------------------------

panel_diag <- panel_diag %>%
  mutate(
    p_ist_gross = if_else(
      product == "peppers" & week_start == as.Date("2019-05-27"),
      avg_unit_price,
      p_ist_gross
    ),
    p_ist_net = if_else(
      product == "peppers" & week_start == as.Date("2019-05-27"),
      avg_unit_price,
      p_ist_net
    ),
    # Recompute margins after price correction
    margin_gross = p_ist_gross - w_ist,
    margin_net   = p_ist_net   - w_ist
  )

# Verify correction: mean gross price for peppers in spike week should now
# be close to avg_unit_price (~$1.02)
panel_diag %>%
  filter(product == "peppers", week_start == as.Date("2019-05-27")) %>%
  summarise(
    mean_p_gross   = mean(p_ist_gross,   na.rm = TRUE),
    mean_p_net     = mean(p_ist_net,     na.rm = TRUE),
    mean_avg_price = mean(avg_unit_price, na.rm = TRUE)
  ) %>%
  print()

# ------------------------------------------------------------------------------
# Cost artifact correction: peppers, week of 2019-05-27
#
# w_ist is inflated by near-zero volume denominators (e.g., w_ist = $226/lb
# when upc_week_volume = 0.36 lbs). The posted price (avg_unit_price ~$1.02)
# is unaffected because it does not use volume as a denominator.
# There is no direct posted-cost equivalent to substitute, so w_ist is
# replaced with the median w_ist for peppers in the immediately adjacent
# weeks (one week before and one week after 2019-05-27).
# ------------------------------------------------------------------------------

spike_date <- as.Date("2019-05-27")

# Identify the adjacent week_start values
adjacent_weeks <- panel_diag %>%
  filter(product == "peppers") %>%
  distinct(week_start) %>%
  arrange(week_start) %>%
  mutate(rank = row_number()) %>%
  filter(week_start == spike_date |
           week_start == lag(week_start,  1) & lead(week_start, 1) == spike_date |
           week_start == lead(week_start, 1) & lag(week_start,  1) == spike_date)

# Simpler: just find the week before and after directly
week_before <- panel_diag %>%
  filter(product == "peppers", week_start < spike_date) %>%
  summarise(week_start = max(week_start)) %>%
  pull(week_start)

week_after <- panel_diag %>%
  filter(product == "peppers", week_start > spike_date) %>%
  summarise(week_start = min(week_start)) %>%
  pull(week_start)

message("Replacing spike week cost using adjacent weeks: ", week_before, " and ", week_after)

# Compute median w_ist for peppers in the two adjacent weeks
w_replacement <- panel_diag %>%
  filter(product == "peppers",
         week_start %in% c(week_before, week_after),
         w_ist > 0, !is.na(w_ist)) %>%
  summarise(w_median = median(w_ist, na.rm = TRUE)) %>%
  pull(w_median)

message("Replacement w_ist value: ", round(w_replacement, 4))

# Apply correction
panel_diag <- panel_diag %>%
  mutate(
    w_ist = if_else(
      product == "peppers" & week_start == spike_date,
      w_replacement,
      w_ist
    ),
    # Recompute margins after cost correction
    margin_gross = p_ist_gross - w_ist,
    margin_net   = p_ist_net   - w_ist
  )

# Verify
panel_diag %>%
  filter(product == "peppers", week_start == spike_date) %>%
  summarise(
    mean_w        = mean(w_ist,        na.rm = TRUE),
    mean_margin_g = mean(margin_gross, na.rm = TRUE),
    mean_margin_n = mean(margin_net,   na.rm = TRUE)
  ) %>%
  print()

# Check w_ist for peppers in the five weeks before and after the spike week
# to confirm the replacement value is reasonable relative to adjacent observations

weeks_around_spike <- panel_diag %>%
  filter(product == "peppers") %>%
  distinct(week_start) %>%
  arrange(week_start) %>%
  mutate(rank = row_number()) %>%
  filter(rank >= (which(.$week_start == spike_date) - 5) &
           rank <= (which(.$week_start == spike_date) + 5)) %>%
  pull(week_start)

panel_diag %>%
  filter(product == "peppers",
         week_start %in% weeks_around_spike) %>%
  group_by(week_start) %>%
  summarise(
    n_stores    = n(),
    mean_w      = mean(w_ist,  na.rm = TRUE),
    median_w    = median(w_ist, na.rm = TRUE),
    min_w       = min(w_ist,   na.rm = TRUE),
    max_w       = max(w_ist,   na.rm = TRUE),
    is_spike_wk = first(week_start == spike_date),
    .groups     = "drop"
  ) %>%
  arrange(week_start) %>%
  print()

# ------------------------------------------------------------------------------
# Table 1: Sale share by period and product
# This is the most direct evidence that the net/gross distinction matters.
# ------------------------------------------------------------------------------

tab_sale_share <- panel_diag %>%
  group_by(period) %>%
  summarise(
    n_store_weeks         = n(),
    sum_total_transactions = sum(weekly_transactions_total, na.rm = TRUE),
    sum_transactions_on_sale = sum(weekly_transactions_on_sale, na.rm = TRUE),
    avg_share_on_sale = sum_transactions_on_sale / sum_total_transactions,
    .groups = "drop"
  ) %>%
  mutate(
    avg_share_on_sale_pct = round(100 * avg_share_on_sale, 1)
  ) %>%
  select(period, n_store_weeks, avg_share_on_sale_pct) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(n_store_weeks, avg_share_on_sale_pct)
  )

print(tab_sale_share)

tab_sale_share <- panel_diag %>%
  group_by(product, period) %>%
  summarise(
    n_store_weeks         = n(),
    sum_total_transactions = sum(weekly_transactions_total, na.rm = TRUE),
    sum_transactions_on_sale = sum(weekly_transactions_on_sale, na.rm = TRUE),
    avg_share_on_sale = sum_transactions_on_sale / sum_total_transactions,
    .groups = "drop"
  ) %>%
  mutate(
    product              = stringr::str_to_title(as.character(product)),
    avg_share_on_sale_pct = round(100 * avg_share_on_sale, 1)
  ) %>%
  select(product, period, n_store_weeks, avg_share_on_sale_pct) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(n_store_weeks, avg_share_on_sale_pct)
  )

print(tab_sale_share)
write.csv(tab_sale_share,
          "tables_csv/diag_01_sale_share_by_period.csv",
          row.names = FALSE)

# Check the share of weeks with no promotional activity by period
# A week is defined as having no deals if avg_sale_price is zero or NA,
# or if share_on_sale is zero (no transactions completed at a sale price)

weeks_no_deals <- panel_diag %>%
  group_by(week_start, period) %>%
  summarise(
    avg_sale_price_pooled = mean(avg_sale_price,  na.rm = TRUE),
    avg_share_on_sale     = mean(share_on_sale,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    no_deal_week_sale_price  = avg_sale_price_pooled == 0 | is.na(avg_sale_price_pooled),
    no_deal_week_share       = avg_share_on_sale == 0     | is.na(avg_share_on_sale)
  )

# Summary by period: share of weeks with no deals
weeks_no_deals %>%
  group_by(period) %>%
  summarise(
    total_weeks              = n(),
    n_no_deal_sale_price     = sum(no_deal_week_sale_price, na.rm = TRUE),
    n_no_deal_share          = sum(no_deal_week_share,      na.rm = TRUE),
    pct_no_deal_sale_price   = round(100 * mean(no_deal_week_sale_price, na.rm = TRUE), 1),
    pct_no_deal_share        = round(100 * mean(no_deal_week_share,      na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  print()

# Count of store-product-weeks where avg_sale_price is zero by product and period
# Not collapsed to a binary: shows the distribution of zero-sale-price weeks
# to assess whether pre-SOE had more weeks with no promotional activity

weeks_zero_sale_price <- panel_diag %>%
  group_by(product, period, week_start) %>%
  summarise(
    n_stores_zero_sale  = sum(avg_sale_price == 0 | is.na(avg_sale_price), na.rm = TRUE),
    n_stores_total      = n(),
    share_stores_zero   = n_stores_zero_sale / n_stores_total,
    .groups = "drop"
  ) %>%
  group_by(product, period) %>%
  summarise(
    total_product_weeks       = n(),
    n_weeks_any_zero          = sum(n_stores_zero_sale > 0),
    n_weeks_all_zero          = sum(n_stores_zero_sale == n_stores_total),
    avg_share_stores_zero     = round(mean(share_stores_zero, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  mutate(product = stringr::str_to_title(as.character(product))) %>%
  arrange(product, period)

print(weeks_zero_sale_price)

# ------------------------------------------------------------------------------
# Table 2: Period means for each price definition
# Shows how much the three price measures differ by period.
# If net and gross are close, the deal-mix concern is empirically small.
# If they diverge during SOE, the gross price is the cleaner measure.
# ------------------------------------------------------------------------------

tab_price_comparison <- panel_diag %>%
  filter(p_ist_net > 0, p_ist_gross > 0) %>%
  group_by(product, period) %>%
  summarise(
    mean_p_net   = mean(p_ist_net,     na.rm = TRUE),
    mean_p_gross = mean(p_ist_gross,   na.rm = TRUE),
    mean_p_avg   = mean(avg_unit_price, na.rm = TRUE),
    # Gap between gross and net: positive means net is below gross
    gap_gross_net = mean(p_ist_gross - p_ist_net, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    product = stringr::str_to_title(as.character(product)),
    across(where(is.numeric), ~round(.x, 3))
  )

print(tab_price_comparison)
write.csv(tab_price_comparison,
          "tables_csv/diag_02_price_comparison_by_period.csv",
          row.names = FALSE)

# ------------------------------------------------------------------------------
# Table 3: Period means for each margin definition
# ------------------------------------------------------------------------------

tab_margin_comparison <- panel_diag %>%
  filter(p_ist_net > 0, p_ist_gross > 0, w_ist > 0) %>%
  group_by(product, period) %>%
  summarise(
    mean_margin_net   = mean(margin_net,   na.rm = TRUE),
    mean_margin_gross = mean(margin_gross, na.rm = TRUE),
    mean_margin_avg   = mean(margin_avg,   na.rm = TRUE),
    gap_gross_net     = mean(margin_gross - margin_net, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    product = stringr::str_to_title(as.character(product)),
    across(where(is.numeric), ~round(.x, 3))
  )

print(tab_margin_comparison)
write.csv(tab_margin_comparison,
          "tables_csv/diag_03_margin_comparison_by_period.csv",
          row.names = FALSE)

# ------------------------------------------------------------------------------
# Figure 1: Sale share over time (weekly mean across store-product cells)
# The key figure for the coauthor email: shows that deal frequency
# spiked during the SOE, which is why net and gross prices diverge.
# ------------------------------------------------------------------------------

sale_share_weekly <- panel_diag %>%
  group_by(week_start) %>%
  summarise(
    sum_total_transactions = sum(weekly_transactions_total, na.rm = TRUE),
    sum_transactions_on_sale = sum(weekly_transactions_on_sale, na.rm = TRUE),
    mean_share_on_sale = sum_transactions_on_sale / sum_total_transactions,
    .groups = "drop"
  )

soe_window_diag <- panel_diag %>%
  filter(SoE == 1) %>%
  summarise(
    soe_start = min(week_start, na.rm = TRUE),
    soe_end   = max(week_start, na.rm = TRUE)
  )

g_sale_share <- ggplot(sale_share_weekly,
                       aes(x = week_start, y = mean_share_on_sale)) +
  annotate("rect",
           xmin = soe_window_diag$soe_start,
           xmax = soe_window_diag$soe_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey50") +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title    = "Share of transactions on sale over time",
    subtitle = paste0(
      "Shaded region = pooled SOE window."
    ),
    x = "Week",
    y = "Share of transactions on sale"
  ) +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 8))

g_sale_share

ggsave("figures/diag_01_sale_share_over_time.png", g_sale_share,
       width = 10, height = 5, dpi = 300)
message("Saved: figures/diag_01_sale_share_over_time.png")

# ------------------------------------------------------------------------------
# Figure 2: Net vs gross price over time (pooled across products)
# Shows the two series together so the coauthor can see how much they diverge.
# ------------------------------------------------------------------------------

price_series_weekly <- panel_diag %>%
  filter(p_ist_net > 0, p_ist_gross > 0) %>%
  group_by(week_start) %>%
  summarise(
    mean_p_net   = mean(p_ist_net,   na.rm = TRUE),
    mean_p_gross = mean(p_ist_gross, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(mean_p_net, mean_p_gross),
    names_to  = "series",
    values_to = "price"
  ) %>%
  mutate(
    series = recode(series,
                    mean_p_net   = "Net price (after discounts)",
                    mean_p_gross = "Gross price (posted shelf price)")
  )

g_price_series <- ggplot(price_series_weekly,
                         aes(x = week_start, y = price, linetype = series)) +
  annotate("rect",
           xmin = soe_window_diag$soe_start,
           xmax = soe_window_diag$soe_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey50") +
  geom_line(linewidth = 0.7) +
  labs(
    title    = "Net vs gross retail price over time (pooled across products)",
    subtitle = "Shaded region = pooled SOE window.",
    x        = "Week",
    y        = "Mean price (nominal $)",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.subtitle   = element_text(size = 8))

g_price_series

ggsave("figures/diag_02_price_series_comparison.png", g_price_series,
       width = 10, height = 5, dpi = 300)
message("Saved: figures/diag_02_price_series_comparison.png")


# ------------------------------------------------------------------------------
# Figure 2: Net vs gross price over time (pooled across products)
# Compares four price measures:
#   Gross price (p_ist_gross)    : volume-weighted posted shelf price
#   Avg unit price (avg_unit_price): simple daily average of posted shelf price
#   Net price (p_ist_net)        : volume-weighted price after discounts
#   Avg sale price (avg_sale_price): simple daily average of sale price
#
# Gross and avg_unit_price use the same color (posted shelf price family).
# Net and avg_sale_price use the same color (discounted price family).
# Solid = volume-weighted; dashed = simple daily average.
# ------------------------------------------------------------------------------

price_series_weekly <- panel_diag %>%
  filter(p_ist_net > 0, p_ist_gross > 0) %>%
  group_by(week_start) %>%
  summarise(
    mean_p_gross  = mean(p_ist_gross,   na.rm = TRUE),
    mean_p_net    = mean(p_ist_net,     na.rm = TRUE),
    mean_avg_unit = mean(avg_unit_price, na.rm = TRUE),
    mean_avg_sale = mean(avg_sale_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(mean_p_gross, mean_p_net, mean_avg_unit, mean_avg_sale),
    names_to  = "series",
    values_to = "price"
  ) %>%
  mutate(
    series = recode(series,
                    mean_p_gross  = "Gross price (volume-weighted)",
                    mean_avg_unit = "Weekly avg unit price (unweighted)",
                    mean_p_net    = "Net price (volume-weighted)",
                    mean_avg_sale = "Weekly avg sale price (unweighted)"),
    series = factor(series, levels = c(
      "Gross price (volume-weighted)",
      "Weekly avg unit price (unweighted)",
      "Net price (volume-weighted)",
      "Weekly avg sale price (unweighted)"
    ))
  )

g_price_series <- ggplot(price_series_weekly,
                         aes(x        = week_start,
                             y        = price,
                             color    = series,
                             linetype = series)) +
  annotate("rect",
           xmin = soe_window_diag$soe_start,
           xmax = soe_window_diag$soe_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey50") +
  geom_line(linewidth = 0.7) +
  scale_color_manual(
    values = c(
      "Gross price (volume-weighted)"     = "steelblue",
      "Weekly avg unit price (unweighted)" = "steelblue",
      "Net price (volume-weighted)"       = "firebrick",
      "Weekly avg sale price (unweighted)" = "firebrick"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Gross price (volume-weighted)"     = "solid",
      "Weekly avg unit price (unweighted)" = "dashed",
      "Net price (volume-weighted)"       = "solid",
      "Weekly avg sale price (unweighted)" = "dashed"
    )
  ) +
  labs(
    title    = "Four price measures over time (pooled across products)",
    subtitle = paste0(
      "Shaded region = pooled SOE window.\n",
      "Blue = posted shelf price family. Red = discounted price family.\n",
      "Solid = volume-weighted. Dashed = unweighted weekly average."
    ),
    x        = "Week",
    y        = "Mean price (nominal $)",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.subtitle   = element_text(size = 8)
  )

g_price_series

ggsave("figures/diag_02_price_series_comparison.png", g_price_series,
       width = 10, height = 5, dpi = 300)
message("Saved: figures/diag_02_price_series_comparison.png")


# ------------------------------------------------------------------------------
# Figure 3: Gap between gross and net price over time
# Isolates the deal-discount component of price to show directly when and
# how much the two series diverge.
# ------------------------------------------------------------------------------

gap_weekly <- panel_diag %>%
  filter(p_ist_net > 0, p_ist_gross > 0) %>%
  group_by(week_start) %>%
  summarise(
    mean_gap = mean(p_ist_gross - p_ist_net, na.rm = TRUE),
    .groups  = "drop"
  )

g_gap <- ggplot(gap_weekly, aes(x = week_start, y = mean_gap)) +
  annotate("rect",
           xmin = soe_window_diag$soe_start,
           xmax = soe_window_diag$soe_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  labs(
    title    = "Gap between gross and net price over time (gross minus net)",
    subtitle = paste0(
      "Positive values indicate promotional discounts are reducing the volume-weighted price ",
      "below the posted shelf price.\n",
      "A widening gap during SOE would indicate that deal intensity increased ",
      "and p_ist_net understates the true shelf price movement."
    ),
    x = "Week",
    y = "Mean gap: gross minus net price (nominal $)"
  ) +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 8))

g_gap

ggsave("figures/diag_03_price_gap_over_time.png", g_gap,
       width = 10, height = 5, dpi = 300)
message("Saved: figures/diag_03_price_gap_over_time.png")

# ------------------------------------------------------------------------------
# Figure 4: Net vs gross margin over time (pooled across products)
# Shows how the margin series differ depending on price definition.
# A wider gap during SOE indicates that deal intensity is compressing
# the net margin relative to the gross margin during the emergency period.
# ------------------------------------------------------------------------------

margin_series_weekly <- panel_diag %>%
  filter(p_ist_net > 0, p_ist_gross > 0, w_ist > 0) %>%
  group_by(week_start) %>%
  summarise(
    mean_margin_net   = mean(margin_net,   na.rm = TRUE),
    mean_margin_gross = mean(margin_gross, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(mean_margin_net, mean_margin_gross),
    names_to  = "series",
    values_to = "margin"
  ) %>%
  mutate(
    series = recode(series,
                    mean_margin_net   = "Net margin (p_net - w)",
                    mean_margin_gross = "Gross margin (p_gross - w)")
  )

g_margin_series <- ggplot(margin_series_weekly,
                          aes(x = week_start, y = margin, linetype = series)) +
  annotate("rect",
           xmin = soe_window_diag$soe_start,
           xmax = soe_window_diag$soe_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey50") +
  geom_line(linewidth = 0.7) +
  labs(
    title    = "Net vs gross dollar margin over time (pooled across products)",
    subtitle = "Shaded region = pooled SOE window. Gross margin uses posted shelf price; net margin uses price after discounts.",
    x        = "Week",
    y        = "Mean dollar margin (nominal $)",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.subtitle   = element_text(size = 8))

g_margin_series

ggsave("figures/diag_04_margin_series_comparison.png", g_margin_series,
       width = 10, height = 5, dpi = 300)
message("Saved: figures/diag_04_margin_series_comparison.png")

# ------------------------------------------------------------------------------
# Figure 5: Gap between gross and net margin over time
# ------------------------------------------------------------------------------

margin_gap_weekly <- panel_diag %>%
  filter(p_ist_net > 0, p_ist_gross > 0, w_ist > 0) %>%
  group_by(week_start) %>%
  summarise(
    mean_margin_gap = mean(margin_gross - margin_net, na.rm = TRUE),
    .groups = "drop"
  )

g_margin_gap <- ggplot(margin_gap_weekly,
                       aes(x = week_start, y = mean_margin_gap)) +
  annotate("rect",
           xmin = soe_window_diag$soe_start,
           xmax = soe_window_diag$soe_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  labs(
    title    = "Gap between gross and net margin over time (gross minus net)",
    subtitle = paste0(
      "Positive values indicate promotional discounts are reducing the net margin ",
      "below the gross margin.\n",
      "A widening gap during SOE reflects higher deal intensity compressing ",
      "the net margin relative to the posted-price margin."
    ),
    x = "Week",
    y = "Mean gap: gross minus net margin (nominal $)"
  ) +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 8))

g_margin_gap

ggsave("figures/diag_05_margin_gap_over_time.png", g_margin_gap,
       width = 10, height = 5, dpi = 300)
message("Saved: figures/diag_05_margin_gap_over_time.png")


# ------------------------------------------------------------------------------
# Figure: Wholesale cost over time (pooled across products)
# w_ist is the volume-weighted unit wholesale cost: total weekly acquisition
# cost divided by total weekly volume. Plotted alongside the gross price
# to show the evolution of the retail-wholesale spread over time.
# ------------------------------------------------------------------------------

cost_series_weekly <- panel_diag %>%
  filter(p_ist_gross > 0, w_ist > 0) %>%
  group_by(week_start) %>%
  summarise(
    mean_p_gross = mean(p_ist_gross, na.rm = TRUE),
    mean_w_ist   = mean(w_ist,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(mean_p_gross, mean_w_ist),
    names_to  = "series",
    values_to = "value"
  ) %>%
  mutate(
    series = recode(series,
                    mean_p_gross = "Gross price (volume-weighted)",
                    mean_w_ist   = "Wholesale cost (volume-weighted)"),
    series = factor(series, levels = c(
      "Gross price (volume-weighted)",
      "Wholesale cost (volume-weighted)"
    ))
  )

g_cost_series <- ggplot(cost_series_weekly,
                        aes(x        = week_start,
                            y        = value,
                            color    = series,
                            linetype = series)) +
  annotate("rect",
           xmin = soe_window_diag$soe_start,
           xmax = soe_window_diag$soe_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey50") +
  geom_line(linewidth = 0.7) +
  scale_color_manual(
    values = c(
      "Gross price (volume-weighted)"     = "steelblue",
      "Wholesale cost (volume-weighted)"  = "forestgreen"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Gross price (volume-weighted)"     = "solid",
      "Wholesale cost (volume-weighted)"  = "solid"
    )
  ) +
  labs(
    title    = "Gross retail price and wholesale cost over time (pooled across products)",
    subtitle = paste0(
      "Shaded region = pooled SOE window.\n",
      "Both series are volume-weighted: total weekly sales or cost divided by total weekly volume."
    ),
    x        = "Week",
    y        = "Mean price / cost (nominal $)",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.subtitle   = element_text(size = 8)
  )

g_cost_series

ggsave("figures/diag_06_cost_series.png", g_cost_series,
       width = 10, height = 5, dpi = 300)
message("Saved: figures/diag_06_cost_series.png")


# Compare wholesale cost measures over time

cost_series_weekly <- panel_diag %>%
  filter(w_ist > 0) %>%
  group_by(week_start) %>%
  summarise(
    mean_w_ist          = mean(w_ist,               na.rm = TRUE),
    mean_avg_cost_min   = mean(avg_unit_cost_min,   na.rm = TRUE),
    mean_avg_cost_max   = mean(avg_unit_cost_max,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(mean_w_ist, mean_avg_cost_min, mean_avg_cost_max),
    names_to  = "series",
    values_to = "cost"
  ) %>%
  mutate(
    series = recode(series,
                    mean_w_ist        = "Wholesale cost (volume-weighted)",
                    mean_avg_cost_min = "Weekly avg unit cost - min (unweighted)",
                    mean_avg_cost_max = "Weekly avg unit cost - max (unweighted)"),
    series = factor(series, levels = c(
      "Wholesale cost (volume-weighted)",
      "Weekly avg unit cost - min (unweighted)",
      "Weekly avg unit cost - max (unweighted)"
    ))
  )

g_cost_series <- ggplot(cost_series_weekly,
                        aes(x        = week_start,
                            y        = cost,
                            color    = series,
                            linetype = series)) +
  annotate("rect",
           xmin = soe_window_diag$soe_start,
           xmax = soe_window_diag$soe_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey50") +
  geom_line(linewidth = 0.7) +
  scale_color_manual(
    values = c(
      "Wholesale cost (volume-weighted)"       = "forestgreen",
      "Weekly avg unit cost - min (unweighted)" = "forestgreen",
      "Weekly avg unit cost - max (unweighted)" = "forestgreen"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Wholesale cost (volume-weighted)"       = "solid",
      "Weekly avg unit cost - min (unweighted)" = "dashed",
      "Weekly avg unit cost - max (unweighted)" = "dotted"
    )
  ) +
  labs(
    title    = "Wholesale cost measures over time (pooled across products)",
    subtitle = paste0(
      "Shaded region = pooled SOE window.\n",
      "All series in forestgreen. Solid = volume-weighted. ",
      "Dashed = unweighted daily min. Dotted = unweighted daily max.\n",
      "Min and max track closely if cost is stable within store-UPC-date; ",
      "divergence indicates genuine two-rate days."
    ),
    x        = "Week",
    y        = "Mean cost (nominal $)",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.subtitle   = element_text(size = 8)
  )

g_cost_series

ggsave("figures/diag_07_cost_series_comparison.png", g_cost_series,
       width = 10, height = 5, dpi = 300)
message("Saved: figures/diag_07_cost_series_comparison.png")


message("Price sensitivity diagnostic complete.")


# ==============================================================================
# 01c_COMPARE_all_df_select_df.R
#
# Purpose: Diagnostic comparisons of price series for all perishables vs the
#   five selected produce UPCs, using various weighting schemes and a fixed-
#   weight price index. Also includes residualized trend plots (UPC + week FE)
#   to check whether selected UPCs behave differently from the full sample.
#
# Depends on: store_upc_week and cpi_rebased (from apg_analysis.R Secs 1-2),
#   fixest, ggplot2, dplyr, tidyr, stringr.
#
# Selected UPCs: 4011 (bananas), 7143001065 (lettuce), 4065 (peppers),
#   4062 (cucumbers), 4087 (tomatoes).
#
# Outputs (images/):
#   price_plot_meat_v_perish.png
#   price_plot_chicken_v_other.png
#   price_plot_all.png
#   price_plot_select.png
#   price_plot_fixed_all_rp.png   price_plot_fixed_all_wp.png
#   price_plot_fixed_select_rp.png price_plot_fixed_select_wp.png
#   price_plot_fixed_compare_rp.png
#   price_plot_resid_select_v_all.png
#   price_plot_resid_select_v_no_meat.png
#   price_plot_resid_meat_v_non.png
# ==============================================================================

# ---- pull stg.store_upc_week ----
store_upc_week <- dplyr::tbl(con, dbplyr::in_schema("stg", "store_upc_week")) %>%
  collect() %>% 
  filter(retailer_id != 1) 

# Note: these are only categories that appear in at least 85%?? of stores -- go back to SQL code to check

store_upc_week <- store_upc_week %>%
  left_join(
    dwi %>% select(week_seq, week_start, month, month_name, year_week),
    by = "week_seq"
  ) %>%
  mutate(month_start = lubridate::floor_date(week_start, unit = "month")) %>%
  rename(p_retail = p_ijst, w_retail = w_ijst) %>%
  left_join(cpi_rebased, by = "month_start") %>%
  mutate(
    p_real = p_retail / P_t,
    w_real = w_retail / P_t,
    margin_real = p_real - w_real
  ) 

store_upc_week <- store_upc_week %>%
  # Keep only observations that support logs
  filter(p_real > 0, w_real > 0) %>%
  arrange(sst, store_id, category, week_seq) %>%
  group_by(sst, store_id, category) %>%
  mutate(
    lnp = log(p_real),
    lnw = log(w_real),
    
    dlnp = lnp - lag(lnp),
    dlnw = lnw - lag(lnw)
  ) %>%
  ungroup() %>%
  filter(is.finite(dlnp), is.finite(dlnw)) %>%
  group_by(category) %>%
  mutate(
    q_dlnp = ntile(dlnp, 100),
    q_dlnw = ntile(dlnw, 100)
  ) %>%
  filter(
    q_dlnp > 1, q_dlnp < 100,
    q_dlnw > 1, q_dlnw < 100
  ) %>%
  select(-q_dlnp, -q_dlnw) %>%
  ungroup()

names(store_upc_week)


# ---------------------------------------------
# Keep only SoE/APG-active observations
# ---------------------------------------------
soe_df <- store_upc_week %>%
  filter(SoE_apg_active == 1, retailer_id != 4)

soe_df_select <- store_upc_week %>%
  filter(SoE_apg_active == 1, retailer_id != 4) %>%
  filter(upc %in% c("4011", "7143001065", "4065", "4062", "4087"))

# ---------------------------------------------
# Safe weighted mean
# ---------------------------------------------
safe_wmean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & (w > 0)
  
  if (!any(keep)) return(NA_real_)
  if (sum(w[keep], na.rm = TRUE) <= 0) return(NA_real_)
  
  weighted.mean(x[keep], w = w[keep], na.rm = TRUE)
}


# ---------------------------------------------
# Helper function
# ---------------------------------------------
make_price_series <- function(df, by_retailer = FALSE) {
  
  group_vars <- c("week_start")
  if (by_retailer) group_vars <- c("retailer_id", group_vars)
  
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      # Retail
      p_retail_unweighted = mean(p_retail, na.rm = TRUE),
      p_retail_sales_wt   = safe_wmean(p_retail, upc_week_net_sales),
      p_retail_volume_wt  = safe_wmean(p_retail, upc_week_volume),
      
      p_real_unweighted   = mean(p_real, na.rm = TRUE),
      p_real_sales_wt     = safe_wmean(p_real, upc_week_net_sales),
      p_real_volume_wt    = safe_wmean(p_real, upc_week_volume),
      
      # Wholesale
      w_retail_unweighted = mean(w_retail, na.rm = TRUE),
      w_retail_sales_wt   = safe_wmean(w_retail, upc_week_net_sales),
      w_retail_volume_wt  = safe_wmean(w_retail, upc_week_volume),
      
      w_real_unweighted   = mean(w_real, na.rm = TRUE),
      w_real_sales_wt     = safe_wmean(w_real, upc_week_net_sales),
      w_real_volume_wt    = safe_wmean(w_real, upc_week_volume),
      
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -all_of(group_vars),
      names_to = c("price_type", "weighting"),
      names_pattern = "^(p_retail|p_real|w_retail|w_real)_(unweighted|sales_wt|volume_wt)$",
      values_to = "value"
    ) %>%
    mutate(
      price_type = recode(
        price_type,
        p_retail = "Retail price (nominal)",
        p_real   = "Retail price (real)",
        w_retail = "Wholesale price (nominal)",
        w_real   = "Wholesale price (real)"
      ),
      weighting = recode(
        weighting,
        unweighted = "Unweighted mean",
        sales_wt   = "Sales-weighted mean",
        volume_wt  = "Volume-weighted mean"
      ),
      weighting = factor(
        weighting,
        levels = c("Unweighted mean", "Sales-weighted mean", "Volume-weighted mean")
      ),
      price_type = factor(
        price_type,
        levels = c(
          "Retail price (nominal)",
          "Retail price (real)",
          "Wholesale price (nominal)",
          "Wholesale price (real)"
        )
      )
    ) %>%
    filter(is.finite(value), !is.na(value))
}

# ---------------------------------------------
# Build series
# ---------------------------------------------
plot_df_agg <- make_price_series(soe_df, by_retailer = FALSE)
plot_df_ret <- make_price_series(soe_df, by_retailer = TRUE)


plot_df_agg_select <- make_price_series(soe_df_select, by_retailer = FALSE)
plot_df_ret_select <- make_price_series(soe_df_select, by_retailer = TRUE)

# ---------------------------------------------
# Aggregate plot
# ---------------------------------------------
g_agg <- ggplot(
  plot_df_agg,
  aes(x = week_start, y = value, linetype = price_type)
) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ weighting, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail and wholesale prices during SoE/APG-active weeks",
    subtitle = "Aggregate weekly means across all products and stores",
    x = "Week",
    y = "Price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 10)
  )

print(g_agg)

# ---------------------------------------------
# By-retailer plot
# ---------------------------------------------
g_ret <- ggplot(
  plot_df_ret,
  aes(x = week_start, y = value, linetype = price_type)
) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  facet_grid(weighting ~ retailer_id, scales = "free_y") +
  labs(
    title = "Retail and wholesale prices during SoE/APG-active weeks, by retailer",
    subtitle = "Weekly means across all products within retailer",
    x = "Week",
    y = "Price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 9)
  )

print(g_ret)

# =========================================================
# 6) Optional: separate retail-only and wholesale-only versions
#    if the 4-line plots feel too crowded
# =========================================================

categories_included <- soe_df %>%
  distinct(category) %>%
  arrange(category) %>%
  pull(category)

caption_txt <- paste0(
  "Included categories: ",
  paste(categories_included, collapse = ", "),
  "."
) %>%
  str_wrap(width = 90)

caption_txt

plot_df_agg_retail <- plot_df_agg %>%
  filter(price_type %in% c("Retail price (nominal)", "Retail price (real)"))

plot_df_agg_wholesale <- plot_df_agg %>%
  filter(price_type %in% c("Wholesale price (nominal)", "Wholesale price (real)"))

g_agg_retail <- ggplot(
  plot_df_agg_retail,
  aes(x = week_start, y = value, linetype = price_type)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail prices during SoE/APG-active weeks",
    subtitle = paste0("Aggregate weekly means across all stores.\n", caption_txt),
    x = "Week",
    y = "Price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(g_agg_retail)

g_agg_wholesale <- ggplot(
  plot_df_agg_wholesale,
  aes(x = week_start, y = value, linetype = price_type)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, scales = "free_y", ncol = 1) +
  labs(
    title = "Wholesale prices during SoE/APG-active weeks",
    subtitle = paste0("Aggregate weekly means across all stores.\n", caption_txt),
    x = "Week",
    y = "Price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(g_agg_wholesale)


# Select products

caption_txt_select <- paste(
  "Products (PLU) included:",
  "bananas (4011; Yellow, includes Cavendish),",
  "cucumbers (4062; Green/Ridge/Short),",
  "\nlettuce (7143001065; Dole salad shredded lettuce),",
  "tomatoes (4087; Red, Plum/Italian/Saladette/Roma),",
  "peppers (4065; Green, Bell, Field Grow).",
  "\nExcludes retail chain 4. SoE weeks only."
)

plot_df_agg_retail_select <- plot_df_agg_select %>%
  filter(price_type %in% c("Retail price (nominal)", "Retail price (real)"))

plot_df_agg_wholesale_select <- plot_df_agg_select %>%
  filter(price_type %in% c("Wholesale price (nominal)", "Wholesale price (real)"))

g_agg_retail_select <- ggplot(
  plot_df_agg_retail_select,
  aes(x = week_start, y = value, linetype = price_type)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail prices during SoE/APG-active weeks",
    subtitle = paste0("Aggregate weekly means across all stores.\n", caption_txt_select),
    x = "Week",
    y = "Price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(g_agg_retail_select)

g_agg_wholesale_select <- ggplot(
  plot_df_agg_wholesale_select,
  aes(x = week_start, y = value, linetype = price_type)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, scales = "free_y", ncol = 1) +
  labs(
    title = "Wholesale prices during SoE/APG-active weeks",
    subtitle = paste0("Aggregate weekly means across all stores.\n", caption_txt_select),
    x = "Week",
    y = "Price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(g_agg_wholesale_select)


# ------------------- Expanded version ----------------------

cat_week <- all_df %>%
  group_by(category, week_start) %>%
  summarise(
    p_real_unweighted = mean(p_real, na.rm = TRUE),
    p_real_sales_wt   = safe_wmean(p_real, upc_week_net_sales),
    p_real_volume_wt  = safe_wmean(p_real, upc_week_volume),
    soe_active = as.integer(any(SoE_apg_active == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(p_real_unweighted, p_real_sales_wt, p_real_volume_wt),
    names_to = "weighting",
    values_to = "value"
  ) %>%
  mutate(
    weighting = recode(
      weighting,
      p_real_unweighted = "Unweighted mean",
      p_real_sales_wt   = "Sales-weighted mean",
      p_real_volume_wt  = "Volume-weighted mean"
    )
  )

ggplot(cat_week, aes(x = week_start, y = value)) +
  geom_line(linewidth = 0.5) +
  facet_grid(weighting ~ category, scales = "free_y") +
  labs(
    title = "Category-level real retail prices over time",
    x = "Week",
    y = "Real retail price"
  ) +
  theme_minimal()

# If only a handful of categories are rising sharply, that is probably what is pulling up the all-products line => Looks like this is being driven by meat categories.

all_df <- all_df %>%
  mutate(
    group_simple = case_when(
      category %in% c("CHICKEN", "BEEF", "PORK") ~ "Meat",
      TRUE ~ "Other perishables"
    )
  )

group_week <- all_df %>%
  group_by(group_simple, week_start) %>%
  summarise(
    p_real_unweighted = mean(p_real, na.rm = TRUE),
    p_real_sales_wt   = safe_wmean(p_real, upc_week_net_sales),
    p_real_volume_wt  = safe_wmean(p_real, upc_week_volume),
    soe_active = as.integer(any(SoE_apg_active == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(p_real_unweighted, p_real_sales_wt, p_real_volume_wt),
    names_to = "weighting",
    values_to = "value"
  ) %>%
  mutate(
    weighting = recode(
      weighting,
      p_real_unweighted = "Unweighted mean",
      p_real_sales_wt   = "Sales-weighted mean",
      p_real_volume_wt  = "Volume-weighted mean"
    ),
    weighting = factor(
      weighting,
      levels = c("Unweighted mean", "Sales-weighted mean", "Volume-weighted mean")
    )
  )

price_plot_meat_v_perish <- ggplot(group_week, aes(x = week_start, y = value, linetype = group_simple)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, scales = "free_y", ncol = 1) +
  labs(
    title = "Real retail prices over time: meat versus other perishables",
    x = "Week",
    y = "Real retail price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(price_plot_meat_v_perish)

ggsave("images/price_plot_meat_v_perish.png", price_plot_meat_v_perish, width = 12, height = 8, dpi = 300)


# Chicken specific
series_all <- all_df %>%
  group_by(week_start) %>%
  summarise(
    value = safe_wmean(p_real, upc_week_net_sales),
    .groups = "drop"
  ) %>%
  mutate(sample = "All perishables")

series_no_chicken <- all_df %>%
  filter(category != "CHICKEN") %>%
  group_by(week_start) %>%
  summarise(
    value = safe_wmean(p_real, upc_week_net_sales),
    .groups = "drop"
  ) %>%
  mutate(sample = "Excluding chicken")

price_plot_chicken_v_other <- bind_rows(series_all, series_no_chicken) %>%
  ggplot(aes(x = week_start, y = value, linetype = sample)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Sales-weighted real retail price: contribution of chicken",
    x = "Week",
    y = "Real retail price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(price_plot_chicken_v_other)

ggsave("images/price_plot_chicken_v_other.png", price_plot_chicken_v_other, width = 12, height = 8, dpi = 300)


# Look at product composition - is it changing?

cat_share <- all_df %>%
  group_by(week_start, category) %>%
  summarise(
    sales = sum(upc_week_net_sales, na.rm = TRUE),
    volume = sum(upc_week_volume, na.rm = TRUE),
    soe_active = as.integer(any(SoE_apg_active == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  group_by(week_start) %>%
  mutate(
    sales_share  = sales / sum(sales, na.rm = TRUE),
    volume_share = volume / sum(volume, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(cat_share, aes(x = week_start, y = sales_share, color = category)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Category sales shares over time",
    x = "Week",
    y = "Sales share"
  ) +
  theme_minimal()


baseline_shares <- all_df %>%
  filter(week_start < as.Date("2020-03-01")) %>%
  group_by(category) %>%
  summarise(
    base_sales = sum(upc_week_net_sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(base_share = base_sales / sum(base_sales, na.rm = TRUE)) %>%
  select(category, base_share)

cat_price <- all_df %>%
  group_by(category, week_start) %>%
  summarise(
    cat_price = safe_wmean(p_real, upc_week_net_sales),
    .groups = "drop"
  )

fixed_weight_series <- cat_price %>%
  left_join(baseline_shares, by = "category") %>%
  group_by(week_start) %>%
  summarise(
    fixed_weight_price = sum(cat_price * base_share, na.rm = TRUE),
    .groups = "drop"
  )

time_varying_series <- all_df %>%
  group_by(week_start) %>%
  summarise(
    actual_sales_weight_price = safe_wmean(p_real, upc_week_net_sales),
    .groups = "drop"
  )

compare_comp <- fixed_weight_series %>%
  left_join(time_varying_series, by = "week_start") %>%
  pivot_longer(
    cols = c(fixed_weight_price, actual_sales_weight_price),
    names_to = "series",
    values_to = "value"
  )

ggplot(compare_comp, aes(x = week_start, y = value, linetype = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Actual vs fixed-weight aggregate real retail price",
    subtitle = "Difference between the two reflects changing category mix",
    x = "Week",
    y = "Real retail price",
    linetype = NULL
  ) +
  theme_minimal()

# If those two lines diverge, the increase is being driven partly by changing mix. => They do not diverge.

all_df_select <- all_df %>%
  filter(upc %in% c("4011", "7143001065", "4065", "4062", "4087"))

make_price_series_allweeks <- function(df, by_retailer = FALSE) {
  
  group_vars <- c("week_start")
  if (by_retailer) group_vars <- c("retailer_id", group_vars)
  
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      soe_active = as.integer(any(SoE_apg_active == 1, na.rm = TRUE)),
      
      p_retail_unweighted = mean(p_retail, na.rm = TRUE),
      p_retail_sales_wt   = safe_wmean(p_retail, upc_week_net_sales),
      p_retail_volume_wt  = safe_wmean(p_retail, upc_week_volume),
      
      p_real_unweighted   = mean(p_real, na.rm = TRUE),
      p_real_sales_wt     = safe_wmean(p_real, upc_week_net_sales),
      p_real_volume_wt    = safe_wmean(p_real, upc_week_volume),
      
      w_retail_unweighted = mean(w_retail, na.rm = TRUE),
      w_retail_sales_wt   = safe_wmean(w_retail, upc_week_net_sales),
      w_retail_volume_wt  = safe_wmean(w_retail, upc_week_volume),
      
      w_real_unweighted   = mean(w_real, na.rm = TRUE),
      w_real_sales_wt     = safe_wmean(w_real, upc_week_net_sales),
      w_real_volume_wt    = safe_wmean(w_real, upc_week_volume),
      
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -c(all_of(group_vars), soe_active),
      names_to = c("price_type", "weighting"),
      names_pattern = "^(p_retail|p_real|w_retail|w_real)_(unweighted|sales_wt|volume_wt)$",
      values_to = "value"
    ) %>%
    mutate(
      price_type = recode(
        price_type,
        p_retail = "Retail price (nominal)",
        p_real   = "Retail price (real)",
        w_retail = "Wholesale price (nominal)",
        w_real   = "Wholesale price (real)"
      ),
      weighting = recode(
        weighting,
        unweighted = "Unweighted mean",
        sales_wt   = "Sales-weighted mean",
        volume_wt  = "Volume-weighted mean"
      ),
      weighting = factor(
        weighting,
        levels = c("Unweighted mean", "Sales-weighted mean", "Volume-weighted mean")
      )
    ) %>%
    filter(is.finite(value), !is.na(value))
}

make_soe_shading <- function(df, by_retailer = FALSE) {
  
  group_vars <- c("week_start")
  if (by_retailer) group_vars <- c("retailer_id", group_vars)
  
  base <- df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      soe_active = as.integer(any(SoE_apg_active == 1, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(across(all_of(group_vars))) 
  
  if (!by_retailer) {
    base %>%
      arrange(week_start) %>%
      mutate(block = cumsum(soe_active != lag(soe_active, default = first(soe_active)))) %>%
      filter(soe_active == 1) %>%
      group_by(block) %>%
      summarise(
        xmin = min(week_start),
        xmax = max(week_start) + 7,
        .groups = "drop"
      )
  } else {
    base %>%
      arrange(retailer_id, week_start) %>%
      group_by(retailer_id) %>%
      mutate(block = cumsum(soe_active != lag(soe_active, default = first(soe_active)))) %>%
      ungroup() %>%
      filter(soe_active == 1) %>%
      group_by(retailer_id, block) %>%
      summarise(
        xmin = min(week_start),
        xmax = max(week_start) + 7,
        .groups = "drop"
      )
  }
}

plot_all_agg <- make_price_series_allweeks(all_df, by_retailer = FALSE) %>%
  filter(price_type %in% c("Retail price (nominal)", "Retail price (real)"))

shade_agg <- make_soe_shading(all_df, by_retailer = FALSE)

price_plot_all <- ggplot(plot_all_agg, aes(x = week_start, y = value, linetype = price_type)) +
  geom_rect(
    data = shade_agg,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail prices over all weeks",
    subtitle = paste0("Grey shading marks SoE/APG-active periods. ", "Aggregate weekly means across all stores.\n", caption_txt),
    x = "Week",
    y = "Price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(price_plot_all)

ggsave("images/price_plot_all.png", price_plot_all, width = 12, height = 8, dpi = 300)

plot_all_agg_select <- make_price_series_allweeks(all_df_select, by_retailer = FALSE) %>%
  filter(price_type %in% c("Retail price (nominal)", "Retail price (real)"))

shade_agg_select <- make_soe_shading(all_df_select, by_retailer = FALSE)

price_plot_select <- ggplot(plot_all_agg_select, aes(x = week_start, y = value, linetype = price_type)) +
  geom_rect(
    data = shade_agg_select,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail prices over all weeks: selected products",
    subtitle = paste0("Grey shading marks SoE/APG-active periods. ", "Aggregate weekly means across all stores.\n", caption_txt_select),
    x = "Week",
    y = "Price",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(price_plot_select)

ggsave("images/price_plot_select.png", price_plot_select, width = 12, height = 8, dpi = 300)


# --------------- Fixed base period ---------------

# =========================================================
# 0) SETTINGS
# =========================================================

base_start <- as.Date("2019-01-01")
base_end   <- as.Date("2019-12-31")

selected_upcs <- c("4011", "7143001065", "4065", "4062", "4087")

# ---------------------------------------------------------
# Make sure UPC is character if your selected vector is character
# ---------------------------------------------------------
# store_upc_week <- store_upc_week %>%
#   mutate(upc = as.character(upc))

# =========================================================
# 1) ANALYSIS SAMPLES
# =========================================================

all_df_select <- all_df %>%
  filter(upc %in% selected_upcs)

# =========================================================
# 3) FIXED-WEIGHT INDEX BUILDER
#
# This creates a fixed-basket price index from UPC-level prices.
#
# Step 1:
#   For each UPC i, compute its average price in the base period:
#     p_i0
#
# Step 2:
#   For each week t, normalize the UPC's price by its base-period price:
#     rel_price_it = p_it / p_i0
#
# Step 3:
#   Aggregate rel_price_it across UPCs using one of three schemes:
#
#   (a) "equal_weight"
#       Index_t = mean_i(rel_price_it)
#
#   (b) "sales_wt"
#       Index_t = sum_i(s_i0 * rel_price_it)
#       where s_i0 is UPC i's share of base-period sales
#
#   (c) "volume_wt"
#       Index_t = sum_i(v_i0 * rel_price_it)
#       where v_i0 is UPC i's share of base-period volume
#
# Notes:
#   - The sales and volume weights are fixed in the base period.
#   - The equal-weight version is not weighted by expenditure or volume;
#     it is just the simple average across UPCs in the base basket.
#   - The resulting index equals 1 in the base period on average.
# =========================================================

make_fixed_weight_index <- function(df, price_var,
                                    base_start, base_end,
                                    weight_type = c("equal_weight", "sales_wt", "volume_wt"),
                                    by_retailer = FALSE) {
  
  weight_type <- match.arg(weight_type)
  
  id_vars <- c("upc")
  if (by_retailer) id_vars <- c("retailer_id", id_vars)
  
  panel_group_vars <- c(setdiff(id_vars, "upc"), "week_start")
  basket_group_vars <- setdiff(id_vars, "upc")
  
  # -------------------------------------------------------
  # Weekly UPC-level panel
  # One row per UPC-week (or retailer-UPC-week if by_retailer = TRUE)
  # -------------------------------------------------------
  weekly_upc <- df %>%
    group_by(across(all_of(c(id_vars, "week_start")))) %>%
    summarise(
      price  = mean(.data[[price_var]], na.rm = TRUE),
      sales  = sum(upc_week_net_sales, na.rm = TRUE),
      volume = sum(upc_week_volume, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(price), !is.na(price), price > 0)
  
  # -------------------------------------------------------
  # Base-period UPC statistics
  # -------------------------------------------------------
  base_upc <- weekly_upc %>%
    filter(week_start >= base_start, week_start <= base_end) %>%
    group_by(across(all_of(id_vars))) %>%
    summarise(
      base_price  = mean(price, na.rm = TRUE),
      base_sales  = sum(sales, na.rm = TRUE),
      base_volume = sum(volume, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(base_price), !is.na(base_price), base_price > 0)
  
  # -------------------------------------------------------
  # Join base-period UPC price onto full weekly panel
  # rel_price_it = p_it / p_i0
  # -------------------------------------------------------
  index_input <- weekly_upc %>%
    inner_join(
      base_upc %>% select(all_of(id_vars), base_price, base_sales, base_volume),
      by = id_vars
    ) %>%
    mutate(
      rel_price = price / base_price
    )
  
  # -------------------------------------------------------
  # Build index
  # -------------------------------------------------------
  
  if (weight_type == "equal_weight") {
    
    index_df <- index_input %>%
      group_by(across(all_of(panel_group_vars))) %>%
      summarise(
        index_value = mean(rel_price, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(weighting = "Equal-weight")
  }
  
  if (weight_type == "sales_wt") {
    
    weights_df <- base_upc %>%
      group_by(across(all_of(basket_group_vars))) %>%
      mutate(weight = base_sales / sum(base_sales, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(is.finite(weight), !is.na(weight))
    
    index_df <- index_input %>%
      inner_join(
        weights_df %>% select(all_of(id_vars), weight),
        by = id_vars
      ) %>%
      group_by(across(all_of(panel_group_vars))) %>%
      summarise(
        index_value = sum(weight * rel_price, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(weighting = "Sales-weighted")
  }
  
  if (weight_type == "volume_wt") {
    
    weights_df <- base_upc %>%
      group_by(across(all_of(basket_group_vars))) %>%
      mutate(weight = base_volume / sum(base_volume, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(is.finite(weight), !is.na(weight))
    
    index_df <- index_input %>%
      inner_join(
        weights_df %>% select(all_of(id_vars), weight),
        by = id_vars
      ) %>%
      group_by(across(all_of(panel_group_vars))) %>%
      summarise(
        index_value = sum(weight * rel_price, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(weighting = "Volume-weighted")
  }
  
  index_df
}

# =========================================================
# 4) WRAPPER:
#    Build indices for p_retail, p_real, w_retail, w_real
# =========================================================

make_index_panel <- function(df, base_start, base_end, by_retailer = FALSE) {
  
  weight_types <- c("equal_weight", "sales_wt", "volume_wt")
  price_vars   <- c("p_retail", "p_real", "w_retail", "w_real")
  
  out <- list()
  k <- 1
  
  for (pv in price_vars) {
    for (wt in weight_types) {
      tmp <- make_fixed_weight_index(
        df = df,
        price_var = pv,
        base_start = base_start,
        base_end = base_end,
        weight_type = wt,
        by_retailer = by_retailer
      ) %>%
        mutate(
          price_type = recode(
            pv,
            p_retail = "Retail price (nominal)",
            p_real   = "Retail price (real)",
            w_retail = "Wholesale price (nominal)",
            w_real   = "Wholesale price (real)"
          )
        )
      
      out[[k]] <- tmp
      k <- k + 1
    }
  }
  
  bind_rows(out) %>%
    mutate(
      weighting = factor(
        weighting,
        levels = c("Equal-weight", "Sales-weighted", "Volume-weighted")
      ),
      price_type = factor(
        price_type,
        levels = c(
          "Retail price (nominal)",
          "Retail price (real)",
          "Wholesale price (nominal)",
          "Wholesale price (real)"
        )
      )
    )
}

# =========================================================
# 5) BUILD INDEX PANELS
# =========================================================

index_all <- make_index_panel(
  df = all_df,
  base_start = base_start,
  base_end = base_end,
  by_retailer = FALSE
)

index_select <- make_index_panel(
  df = all_df_select,
  base_start = base_start,
  base_end = base_end,
  by_retailer = FALSE
)

shade_all <- make_soe_shading(all_df, by_retailer = FALSE)
shade_select <- make_soe_shading(all_df_select, by_retailer = FALSE)

# =========================================================
# 6) OPTIONAL CAPTIONS
# =========================================================

wrap_caption <- function(x, width = 100) str_wrap(x, width = width)

cats_all <- all_df %>%
  distinct(category) %>%
  arrange(category) %>%
  pull(category)

caption_all <- wrap_caption(
  paste0(
    "Fixed-weight index with 2019 as the base period (2019 average = 1 within each UPC before aggregation). ",
    "Included categories: ",
    paste(cats_all, collapse = ", "),
    "."
  ),
  width = 100
)

cats_select <- all_df_select %>%
  distinct(category) %>%
  arrange(category) %>%
  pull(category)

caption_select <- wrap_caption(
  paste0(
    "Fixed-weight index with 2019 as the base period (2019 average = 1 within each UPC before aggregation). ",
    "Selected UPCs: ",
    paste(selected_upcs, collapse = ", "),
    ". Included categories: ",
    paste(cats_select, collapse = ", "),
    "."
  ),
  width = 100
)

# =========================================================
# 7) PLOTS: ALL PERISHABLES
# =========================================================

g_all_retail <- ggplot(
  index_all %>% filter(price_type %in% c("Retail price (nominal)", "Retail price (real)")),
  aes(x = week_start, y = index_value, linetype = price_type)
) +
  geom_rect(
    data = shade_all,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, ncol = 1) +
  labs(
    title = "Fixed-weight retail price index over all weeks: all perishables",
    subtitle = "Grey shading marks SoE/APG-active periods",
    caption = caption_all,
    x = "Week",
    y = "Index (2019 base = 1)",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8)
  )

print(g_all_retail)

ggsave("images/price_plot_fixed_all_rp.png", g_all_retail, width = 12, height = 8, dpi = 300)


g_all_wholesale <- ggplot(
  index_all %>% filter(price_type %in% c("Wholesale price (nominal)", "Wholesale price (real)")),
  aes(x = week_start, y = index_value, linetype = price_type)
) +
  geom_rect(
    data = shade_all,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, ncol = 1) +
  labs(
    title = "Fixed-weight wholesale price index over all weeks: all perishables",
    subtitle = "Grey shading marks SoE/APG-active periods",
    caption = caption_all,
    x = "Week",
    y = "Index (2019 base = 1)",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8)
  )

print(g_all_wholesale)

ggsave("images/price_plot_fixed_all_wp.png", g_all_wholesale, width = 12, height = 8, dpi = 300)

# =========================================================
# 8) PLOTS: SELECTED UPCS
# =========================================================

g_select_retail <- ggplot(
  index_select %>% filter(price_type %in% c("Retail price (nominal)", "Retail price (real)")),
  aes(x = week_start, y = index_value, linetype = price_type)
) +
  geom_rect(
    data = shade_select,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, ncol = 1) +
  labs(
    title = "Fixed-weight retail price index over all weeks: selected UPCs",
    subtitle = "Grey shading marks SoE/APG-active periods",
    caption = caption_select,
    x = "Week",
    y = "Index (2019 base = 1)",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8)
  )

print(g_select_retail)

ggsave("images/price_plot_fixed_select_rp.png", g_select_retail, width = 12, height = 8, dpi = 300)

g_select_wholesale <- ggplot(
  index_select %>% filter(price_type %in% c("Wholesale price (nominal)", "Wholesale price (real)")),
  aes(x = week_start, y = index_value, linetype = price_type)
) +
  geom_rect(
    data = shade_select,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, ncol = 1) +
  labs(
    title = "Fixed-weight wholesale price index over all weeks: selected UPCs",
    subtitle = "Grey shading marks SoE/APG-active periods",
    caption = caption_select,
    x = "Week",
    y = "Index (2019 base = 1)",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8)
  )

print(g_select_wholesale)

ggsave("images/price_plot_fixed_select_wp.png", g_select_wholesale, width = 12, height = 8, dpi = 300)


# =========================================================
# 9) OPTIONAL: DIRECT COMPARISON OF ALL vs SELECTED
#    Retail real only, to keep it simple for slides
# =========================================================

compare_retail_real <- bind_rows(
  index_all %>%
    filter(price_type == "Retail price (real)") %>%
    mutate(sample = "All perishables"),
  index_select %>%
    filter(price_type == "Retail price (real)") %>%
    mutate(sample = "Selected UPCs")
) %>%
  mutate(
    sample = factor(sample, levels = c("All perishables", "Selected UPCs"))
  )


caption_compare <- str_wrap(
  paste0(
    "Each series is a fixed-basket index of real retail prices with 2019 as the base period. ",
    "For each UPC, weekly real price is normalized by that UPC’s 2019 average price before aggregation. ",
    "The equal-weight panel gives each UPC the same weight in the base basket; ",
    "the sales-weighted panel uses fixed 2019 sales shares; ",
    "the volume-weighted panel uses fixed 2019 volume shares. ",
    "The 'All perishables' series includes all UPCs in the analysis sample after filters; ",
    "the 'Selected UPCs' series includes UPCs 4011, 7143001065, 4065, 4062, and 4087. ",
    "Grey shading marks weeks in which anti-price gouging rules were active."
  ),
  width = 110
)

g_compare_retail_real <- ggplot(
  compare_retail_real,
  aes(x = week_start, y = index_value, linetype = sample)
) +
  geom_rect(
    data = shade_all,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.20
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ weighting, ncol = 1) +
  labs(
    title = "Fixed-weight real retail price index: all perishables vs selected UPCs",
    subtitle = "Grey shading marks SoE/APG-active periods",
    caption = caption_compare,
    x = "Week",
    y = "Index (2019 base = 1)",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8)
  )

print(g_compare_retail_real)

ggsave("images/price_plot_fixed_compare_rp.png", g_compare_retail_real, width = 12, height = 8, dpi = 300)

# ------------- Residual plots ----------------

# =========================================================
# 1) Build sample with explicit comparison groups
# =========================================================

selected_upcs <- c("4011", "7143001065", "4065", "4062", "4087")

all_df <- all_df %>%
  mutate(
    upc = as.character(upc),

    # Explicit selected-vs-not-selected comparison
    selected_group = case_when(
      upc %in% selected_upcs ~ "Selected UPCs",
      TRUE ~ "All other included UPCs"
    ),
    
    # Explicit meat comparison
    meat_group = case_when(
      category %in% c("CHICKEN", "BEEF", "PORK") ~ "Meat categories",
      TRUE ~ "Non-meat categories"
    )
  )

# Optional: inspect what is actually in each group
selected_group_counts <- all_df %>%
  distinct(upc, category, selected_group) %>%
  count(selected_group, category, sort = TRUE)

meat_group_counts <- all_df %>%
  distinct(upc, category, meat_group) %>%
  count(meat_group, category, sort = TRUE)

selected_group_counts
meat_group_counts

non_meat_categories <- all_df %>%
  filter(meat_group == "Non-meat categories") %>%
  distinct(category) %>%
  arrange(category) %>%
  pull(category)

non_meat_categories

# Estimate model with product and week FE

# Keep only valid prices
reg_df <- all_df %>%
  filter(is.finite(p_real), !is.na(p_real), p_real > 0)

# Product FE + week FE
m_twfe <- feols(p_real ~ 1 | upc + week_seq, data = reg_df)

reg_df <- reg_df %>%
  mutate(resid_twfe = resid(m_twfe))

# Aggregate residuals by week and group
resid_week_selected <- reg_df %>%
  group_by(week_start, selected_group) %>%
  summarise(
    resid_mean = mean(resid_twfe, na.rm = TRUE),
    soe_active = as.integer(any(SoE_apg_active == 1, na.rm = TRUE)),
    .groups = "drop"
  )

resid_week_selected_no_meat <- reg_df %>%
  filter(meat_group == "Non-meat categories") %>%
  group_by(week_start, selected_group) %>%
  summarise(
    resid_mean = mean(resid_twfe, na.rm = TRUE),
    soe_active = as.integer(any(SoE_apg_active == 1, na.rm = TRUE)),
    .groups = "drop"
  )

resid_week_meat <- reg_df %>%
  group_by(week_start, meat_group) %>%
  summarise(
    resid_mean = mean(resid_twfe, na.rm = TRUE),
    soe_active = as.integer(any(SoE_apg_active == 1, na.rm = TRUE)),
    .groups = "drop"
  )

shade_all <- make_soe_shading(reg_df)

# Plot selected UPCs vs all others
caption_selected <- str_wrap(
  paste0(
    "Residuals from p_real ~ UPC FE + week FE. ",
    "Selected UPCs: ", paste(selected_upcs, collapse = ", "), ". ",
    "Comparison group includes all other UPCs remaining in the analysis sample after filters."
  ),
  width = 100
)

g_resid_week_selected <- ggplot(resid_week_selected, aes(x = week_start, y = resid_mean, linetype = selected_group)) +
  geom_rect(
    data = shade_all,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Average residual by week: selected UPCs versus all other included UPCs",
    subtitle = "Residuals from real retail price model with UPC and week fixed effects; grey shading marks SoE/APG-active periods",
    caption = caption_selected,
    x = "Week",
    y = "Mean residual",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8)
  )

print(g_resid_week_selected)

ggsave("images/price_plot_resid_select_v_all.png", g_resid_week_selected, width = 12, height = 8, dpi = 300)

# Plot selected UPCs vs all other non-meat
caption_selected <- str_wrap(
  paste0(
    "Residuals from p_real ~ UPC FE + week FE. ",
    "Selected UPCs: ", paste(selected_upcs, collapse = ", "), ". ",
    "Comparison group includes all other UPCs remaining in the analysis sample after filters."
  ),
  width = 100
)

g_resid_week_selected_no_meat <- ggplot(resid_week_selected_no_meat, aes(x = week_start, y = resid_mean, linetype = selected_group)) +
  geom_rect(
    data = shade_all,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Average residual by week: selected UPCs versus all other included UPCs",
    subtitle = "Residuals from real retail price model with UPC and week fixed effects; grey shading marks SoE/APG-active periods",
    caption = caption_selected,
    x = "Week",
    y = "Mean residual",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8)
  )

print(g_resid_week_selected_no_meat)

ggsave("images/price_plot_resid_select_v_no_meat.png", g_resid_week_selected_no_meat, width = 12, height = 8, dpi = 300)


# Plot meat vs. non meat
non_meat_categories <- reg_df %>%
  filter(meat_group == "Non-meat categories") %>%
  distinct(category) %>%
  arrange(category) %>%
  pull(category)

caption_meat <- str_wrap(
  paste0(
    "Residuals from p_real ~ UPC FE + week FE. ",
    "Meat categories are CHICKEN, BEEF, and PORK. ",
    "Non-meat categories included here: ",
    paste(non_meat_categories, collapse = ", "),
    "."
  ),
  width = 100
)

g_resid_week_meat <- ggplot(resid_week_meat, aes(x = week_start, y = resid_mean, linetype = meat_group)) +
  geom_rect(
    data = shade_all,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Average residual by week: meat versus non-meat categories",
    subtitle = "Residuals from real retail price model with UPC and week fixed effects; grey shading marks SoE/APG-active periods",
    caption = caption_meat,
    x = "Week",
    y = "Mean residual",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8)
  )

print(g_resid_week_meat)

ggsave("images/price_plot_resid_meat_v_non.png", g_resid_week_meat, width = 12, height = 8, dpi = 300)


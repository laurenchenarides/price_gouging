# ==============================================================================
# 08_demand_rotation.R
#
# Purpose: Mechanism 3 -- Countercyclical promotional pricing (Butters 2025).
#
# The Butters (2025) demand rotation framework: optimal retail pricing depends
# not only on the level of demand but on how price-sensitive consumers are.
# When a state of emergency shifts the composition of shoppers (e.g., toward
# more inelastic panic buyers) or rotates the demand curve, the profit-
# maximizing retailer responds by adjusting both the posted price and the
# depth/frequency of promotional discounts. APG laws constrain the posted price
# but not necessarily the promotional environment, so retailers may substitute
# between margin-restoration channels.
#
# Empirical strategy:
#   M3a. Gross vs net price SOE gap -- does the gross-net spread widen during
#        the SOE? A widening spread means deal frequency rose, consistent with
#        countercyclical promotional pricing.
#   M3b. Promotional intensity regressions -- did share_on_sale and discount
#        depth change during / after the SOE, conditional on product and store FEs?
#   M3c. Price dispersion regressions -- did within-store-day price variation
#        (pct_days_dispersion, avg_price_spread) change during the SOE?
#   M3d. Loyalty vs non-loyalty split -- did share_loyalty change, indicating
#        a shift in which types of shoppers purchased?
#   M3e. IV pass-through (demand rotation) -- instrument for Delta_w using
#        distance-weighted cross-market wholesale prices (Z_ist) constructed
#        from store lat/lon. Identifies how price sensitivity changed.
#
# Data inputs:
#   panel_est            -- from 02_build_panel.R (store-product-week)
#   promo_panel          -- from stg.pd_store_upc_week (read via SQL connection)
#   store_info           -- from stg.pos_store_master (lat/lon for IV)
#
# Outputs (tables_latex/):
#   21_tab_gross_net_gap.tex
#   22_tab_promo_intensity.tex
#   23_tab_price_dispersion.tex
#   24_tab_iv_passthrough.tex
#
# Outputs (figures/):
#   19_fig_gross_net_gap.png
#   20_fig_promo_intensity.png
#   21_fig_price_dispersion.png
#
# Depends on: panel_est, save_tex(), store_info, SAVE_CSV
#   Also requires stg.pd_store_upc_week in SQL (run
#   BuildMarkupsNew_PriceDiscrimination.sql first).
# ==============================================================================

message("Estimating Mechanism 3: countercyclical promotional pricing ...")

# ==============================================================================
# M3a. GROSS VS NET PRICE GAP OVER TIME
# ==============================================================================
# The gross-net gap equals the volume-weighted discount per unit.
# A wider gap during SOE means retailers ran deeper or more frequent promotions.
# This is the first, descriptive test of demand rotation.
# ==============================================================================

gap_data <- panel_est %>%
  filter(p_ist_gross > 0, p_ist_net > 0,
         is.finite(p_ist_gross), is.finite(p_ist_net)) %>%
  mutate(gross_net_gap = p_ist_gross - p_ist_net)

# Regression: does the gross-net gap change during / after SOE?
m_gap_base <- feols(
  gross_net_gap ~ SoE + postSoE | product + store_id,
  data    = gap_data,
  cluster = ~ store_id
)

m_gap_state <- feols(
  gross_net_gap ~ i(sst, SoE, ref = "AL") +
                  i(sst, postSoE, ref = "AL") | product + store_id,
  data    = gap_data,
  cluster = ~ store_id
)

etable(list("(1) Pooled" = m_gap_base, "(2) By state" = m_gap_state))

etable(
  list("(1) Pooled" = m_gap_base, "(2) By state" = m_gap_state),
  tex    = TRUE,
  file   = "tables_latex/21_tab_gross_net_gap.tex",
  title  = "Effect of SOE on gross--net price gap (promotional discount per unit)",
  label  = "tab:gross_net_gap",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict   = c(
    "SoE"     = "SOE$_{st}$",
    "postSoE" = "Post-SOE$_{st}$"
  ),
  headers = list("$p^{gross}_{ist} - p^{net}_{ist}$ (\\$/unit)" = 2),
  notes   = c(
    "Dependent variable: gross price minus net price (dollars per unit or lb).",
    "A positive SOE coefficient means the discount deepened during the emergency.",
    "FEs: product and store. Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/21_tab_gross_net_gap.tex")

# Figure: gross-net gap over time with SOE shading
gap_weekly <- gap_data %>%
  group_by(week_start) %>%
  summarise(mean_gap = mean(gross_net_gap, na.rm = TRUE), .groups = "drop")

soe_w <- panel_est %>%
  filter(SoE == 1) %>%
  summarise(soe_start = min(week_start), soe_end = max(week_start))

g_gap <- ggplot(gap_weekly, aes(x = week_start, y = mean_gap)) +
  annotate("rect",
           xmin = soe_w$soe_start, xmax = soe_w$soe_end,
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  labs(
    title    = "Gross-to-net price gap over time (pooled across products)",
    subtitle = "Gap = posted shelf price minus transaction price. Shaded = SOE window. Positive = promotional discount active.",
    x = "Week", y = "Mean gap: gross minus net (nominal $/unit)"
  ) +
  theme_bw() +
  theme(plot.subtitle = element_text(size = 8))

ggsave("figures/19_fig_gross_net_gap.png", g_gap, width = 10, height = 5, dpi = 300)
message("Saved: figures/19_fig_gross_net_gap.png")


# ==============================================================================
# M3b. PROMOTIONAL INTENSITY REGRESSIONS
# ==============================================================================
# Outcome 1: share_on_sale (fraction of transactions at promotional price)
# Outcome 2: avg_discount_depth (regular price minus sale price, when on sale)
#
# Data: stg.pd_store_upc_week, read via SQL connection.
# This table must exist before running this script (run
# BuildMarkupsNew_PriceDiscrimination.sql first).
#
# If the SQL table is not yet available, the week-level share_on_sale from
# panel_est (pulled in 00_read_in_data.R) is used as a fallback for the
# share_on_sale regression only.
# ==============================================================================

# -- Attempt to load promotional panel from SQL --------------------------------
promo_panel <- tryCatch({
  message("Attempting to load stg.pd_store_upc_week from SQL ...")
  con_promo <- dbConnect(
    odbc::odbc(),
    Driver             = "SQL Server",
    Server             = "Orchard",
    Database           = "DecaData",
    Trusted_Connection = "Yes"
  )
  df <- dplyr::tbl(con_promo, dbplyr::in_schema("stg", "pd_store_upc_week")) %>%
    collect() %>%
    mutate(
      store_id    = as.factor(store_id),
      product     = recode(upc,
                           "4011"        = "bananas",
                           "4069"        = "cabbage",
                           "4062"        = "cucumbers",
                           "7143001065"  = "lettuce",
                           "4087"        = "tomatoes"),
      product     = as.factor(product),
      week_fe     = as.factor(week_seq),
      soe_period  = factor(soe_period, levels = c("Pre-SOE", "During SOE", "Post-SOE"))
    ) %>%
    left_join(
      panel_est %>%
        select(store_id, product, week_seq, SoE, postSoE, preSoE, sst) %>%
        distinct() %>%
        mutate(store_id = as.factor(store_id), product = as.factor(product)),
      by = c("store_id", "product", "week_seq")
    ) %>%
    filter(!is.na(SoE))
  DBI::dbDisconnect(con_promo)
  message("Loaded promo_panel: ", nrow(df), " rows.")
  df
}, error = function(e) {
  message("Could not load stg.pd_store_upc_week: ", conditionMessage(e))
  message("Falling back to share_on_sale from panel_est.")
  NULL
})

# -- Share on sale regression --------------------------------------------------
if (!is.null(promo_panel)) {

  m_share_base <- feols(
    share_on_sale ~ SoE + postSoE | product + store_id,
    data    = promo_panel,
    cluster = ~ store_id
  )

  m_share_week_fe <- feols(
    share_on_sale ~ SoE + postSoE | product + store_id + week_fe,
    data    = promo_panel,
    cluster = ~ store_id
  )

  # Discount depth: conditional on a sale occurring
  m_depth_base <- feols(
    avg_discount_depth ~ SoE + postSoE | product + store_id,
    data    = promo_panel %>% filter(!is.na(avg_discount_depth)),
    cluster = ~ store_id
  )

  etable(
    list(
      "(1) Share, no week FE"   = m_share_base,
      "(2) Share, week FE"      = m_share_week_fe,
      "(3) Discount depth"      = m_depth_base
    ),
    tex    = TRUE,
    file   = "tables_latex/22_tab_promo_intensity.tex",
    title  = "Effect of SOE on promotional intensity: share on sale and discount depth",
    label  = "tab:promo_intensity",
    digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
    dict   = c(
      "SoE"     = "SOE$_{st}$",
      "postSoE" = "Post-SOE$_{st}$"
    ),
    headers = list(
      "Share on sale" = 2,
      "Discount depth (\\$/unit)" = 1
    ),
    notes = c(
      "Columns (1)--(2): dependent variable is share of transactions at promotional price.",
      "Column (3): dependent variable is regular price minus sale price, conditional on a sale.",
      "FEs: product and store. Column (2) adds week FEs.",
      "Standard errors clustered at the store level."
    )
  )
  message("Saved: tables_latex/22_tab_promo_intensity.tex")

} else {
  # Fallback: use share_on_sale from panel_est
  m_share_fallback <- feols(
    share_on_sale ~ SoE + postSoE | product + store_id,
    data    = panel_est %>% filter(is.finite(share_on_sale)),
    cluster = ~ store_id
  )
  etable(list("(1) Share on sale" = m_share_fallback))
  message("Note: promo_panel unavailable; ran fallback share_on_sale regression on panel_est.")
}

# -- Promotional intensity figure ----------------------------------------------
if (!is.null(promo_panel)) {

  promo_weekly <- promo_panel %>%
    group_by(week_start) %>%
    summarise(
      mean_share_on_sale = mean(share_on_sale, na.rm = TRUE),
      mean_discount_depth = mean(avg_discount_depth, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(c(mean_share_on_sale, mean_discount_depth),
                 names_to = "series", values_to = "value") %>%
    mutate(series = recode(series,
                           mean_share_on_sale   = "Share on sale (left axis)",
                           mean_discount_depth  = "Avg discount depth, $/unit (right axis)"))

  share_weekly <- promo_panel %>%
    group_by(week_start) %>%
    summarise(mean_share = mean(share_on_sale, na.rm = TRUE), .groups = "drop")

  depth_weekly <- promo_panel %>%
    filter(!is.na(avg_discount_depth)) %>%
    group_by(week_start) %>%
    summarise(mean_depth = mean(avg_discount_depth, na.rm = TRUE), .groups = "drop")

  scale_fac <- max(depth_weekly$mean_depth, na.rm = TRUE) /
               max(share_weekly$mean_share, na.rm = TRUE)

  g_promo <- ggplot(share_weekly, aes(x = week_start)) +
    annotate("rect",
             xmin = soe_w$soe_start, xmax = soe_w$soe_end,
             ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey50") +
    geom_line(aes(y = mean_share, linetype = "Share on sale"), linewidth = 0.7) +
    geom_line(data = depth_weekly,
              aes(y = mean_depth / scale_fac, linetype = "Discount depth (scaled)"),
              linewidth = 0.7, color = "firebrick") +
    scale_y_continuous(
      name     = "Share of transactions on sale",
      labels   = scales::label_percent(),
      sec.axis = sec_axis(~ . * scale_fac, name = "Mean discount depth ($/unit)")
    ) +
    scale_linetype_manual(values = c("Share on sale" = "solid",
                                     "Discount depth (scaled)" = "dashed")) +
    labs(
      title    = "Promotional intensity over time",
      subtitle = "Shaded = SOE window. Share on sale (left axis) and discount depth (right axis).",
      x = NULL, linetype = NULL
    ) +
    theme_bw() +
    theme(legend.position = "top", plot.subtitle = element_text(size = 8))

  ggsave("figures/20_fig_promo_intensity.png", g_promo, width = 10, height = 5, dpi = 300)
  message("Saved: figures/20_fig_promo_intensity.png")
}


# ==============================================================================
# M3c. PRICE DISPERSION REGRESSIONS
# ==============================================================================
# Outcome: pct_days_dispersion -- fraction of store-days where both sale and
#   regular transactions coexist (i.e., different shoppers pay different prices
#   on the same day at the same store).
# Outcome: avg_price_spread -- mean (max - min) effective price within a store-day.
#
# A positive SOE coefficient on pct_days_dispersion means price discrimination
# via promotional pricing intensified during the SOE. A negative coefficient
# means the mix became more uniform (fewer deal-days).
# ==============================================================================

if (!is.null(promo_panel)) {

  m_disp_base <- feols(
    pct_days_dispersion ~ SoE + postSoE | product + store_id,
    data    = promo_panel %>% filter(is.finite(pct_days_dispersion)),
    cluster = ~ store_id
  )

  m_spread_base <- feols(
    avg_price_spread ~ SoE + postSoE | product + store_id,
    data    = promo_panel %>% filter(is.finite(avg_price_spread), avg_price_spread > 0),
    cluster = ~ store_id
  )

  etable(
    list(
      "(1) Pct days w/ dispersion" = m_disp_base,
      "(2) Avg within-day spread"  = m_spread_base
    ),
    tex    = TRUE,
    file   = "tables_latex/23_tab_price_dispersion.tex",
    title  = "Effect of SOE on within-store price dispersion",
    label  = "tab:price_dispersion",
    digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
    dict   = c(
      "SoE"     = "SOE$_{st}$",
      "postSoE" = "Post-SOE$_{st}$"
    ),
    headers = list(
      "\\% days with mixed pricing" = 1,
      "Price spread (\\$/unit)"      = 1
    ),
    notes = c(
      "Column (1): share of store-days within the week where both sale and regular",
      "price transactions coexist (i.e., different shoppers paid different prices).",
      "Column (2): mean within-day (max -- min) effective price, conditional on dispersion > 0.",
      "FEs: product and store. Standard errors clustered at the store level."
    )
  )
  message("Saved: tables_latex/23_tab_price_dispersion.tex")

  # Figure: dispersion over time
  disp_weekly <- promo_panel %>%
    group_by(week_start) %>%
    summarise(
      mean_disp   = mean(pct_days_dispersion, na.rm = TRUE),
      mean_spread = mean(avg_price_spread[avg_price_spread > 0], na.rm = TRUE),
      .groups = "drop"
    )

  g_disp <- ggplot(disp_weekly, aes(x = week_start, y = mean_disp)) +
    annotate("rect",
             xmin = soe_w$soe_start, xmax = soe_w$soe_end,
             ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey50") +
    geom_line(linewidth = 0.7) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      title    = "Within-store price dispersion over time",
      subtitle = "Share of store-days where both sale and regular-price transactions occur. Shaded = SOE window.",
      x = "Week", y = "Mean share of days with price dispersion"
    ) +
    theme_bw() +
    theme(plot.subtitle = element_text(size = 8))

  ggsave("figures/21_fig_price_dispersion.png", g_disp, width = 10, height = 5, dpi = 300)
  message("Saved: figures/21_fig_price_dispersion.png")
}


# ==============================================================================
# M3e. IV PASS-THROUGH: DEMAND ROTATION
# ==============================================================================
# Instrument: distance-weighted cross-market wholesale price (Z_ist).
#   For each store i in state s, Z_ist is the inverse-distance-weighted average
#   of w_jt for the same product in all stores j outside state s. This
#   instrument shifts the supply curve (cost) without directly affecting local
#   demand conditions.
#
# Store lat/lon: available to 2 decimal places from stg.pos_store_master
#   (already in store_info object from 00_read_in_data.R).
#
# Specification:
#   Stage 1: dW_ist = pi0 + pi1*Z_ist + pi2*(Z_ist*SoE) + gamma_i + tau_t
#   Stage 2: dP_ist = alpha + beta1*dW_hat + beta2*(dW_hat*SoE) +
#                     beta3*(dW_hat*postSoE) + gamma_i + delta_j + tau_t
#
# The SOE interaction on the instrument (Z*SoE) tests whether the demand
# rotation affected the price-cost pass-through rate specifically during the
# emergency period.
# ==============================================================================

message("Building distance-weighted IV ...")

# Confirm store_info has lat/lon
if (!all(c("latitude", "longitude") %in% names(store_info))) {
  warning("store_info missing latitude/longitude. Check stg.pos_store_master column names.")
  message("Skipping IV pass-through (M3e). Run with lat/lon columns available.")
} else {

  # Join lat/lon to panel_est
  pt_iv_data <- panel_est %>%
    filter(is.finite(dP), is.finite(dW)) %>%
    left_join(
      store_info %>%
        select(store_id, latitude, longitude, sst_store = sst) %>%
        distinct() %>%
        mutate(store_id = as.factor(store_id)),
      by = "store_id"
    ) %>%
    filter(!is.na(latitude), !is.na(longitude))

  # Build store-level reference table (one row per store)
  store_coords <- pt_iv_data %>%
    select(store_id, latitude, longitude, sst) %>%
    distinct()

  # For each store i, compute the inverse-distance-weighted mean dW across
  # all stores j in OTHER states in the same week.
  # Distance in degrees (adequate for cross-state weighting across SE states).

  message("Computing cross-market IV (this may take a moment) ...")

  # Collapse dW to store-product-week (already at that grain in panel_est)
  dw_store_week <- pt_iv_data %>%
    select(store_id, product, week_seq, dW, sst, latitude, longitude) %>%
    distinct()

  # Self-join: for each focal store, get dW from all OTHER-state stores
  iv_raw <- dw_store_week %>%
    rename(lat_i = latitude, lon_i = longitude, sst_i = sst) %>%
    inner_join(
      dw_store_week %>%
        rename(store_j = store_id, dW_j = dW, sst_j = sst,
               lat_j = latitude, lon_j = longitude),
      by = c("product", "week_seq")
    ) %>%
    filter(sst_i != sst_j) %>%   # only cross-state stores
    mutate(
      dist_ij = sqrt((lat_i - lat_j)^2 + (lon_i - lon_j)^2),
      weight  = 1 / pmax(dist_ij, 0.01)   # floor at 0.01 degrees to avoid Inf
    ) %>%
    group_by(store_id, product, week_seq) %>%
    summarise(
      Z_ist = weighted.mean(dW_j, w = weight, na.rm = TRUE),
      .groups = "drop"
    )

  pt_iv_data <- pt_iv_data %>%
    left_join(iv_raw, by = c("store_id", "product", "week_seq")) %>%
    filter(is.finite(Z_ist))

  message(sprintf("IV panel: %d obs, %d stores.", nrow(pt_iv_data), n_distinct(pt_iv_data$store_id)))

  # Two-stage IV: instrument dW and dW:SoE with Z_ist and Z_ist:SoE
  m_iv <- feols(
    dP ~ 1 | product + store_id + week_fe |
      dW + dW:SoE + dW:postSoE ~ Z_ist + Z_ist:SoE + Z_ist:postSoE,
    data    = pt_iv_data,
    cluster = ~ store_id
  )

  # OLS benchmark for comparison
  m_ols_iv_sample <- feols(
    dP ~ dW + dW:SoE + dW:postSoE | product + store_id + week_fe,
    data    = pt_iv_data,
    cluster = ~ store_id
  )

  etable(list("(1) OLS" = m_ols_iv_sample, "(2) IV" = m_iv))

  etable(
    list("(1) OLS" = m_ols_iv_sample, "(2) IV" = m_iv),
    tex    = TRUE,
    file   = "tables_latex/24_tab_iv_passthrough.tex",
    title  = "Pass-through: OLS vs IV (demand rotation instrument)",
    label  = "tab:iv_passthrough",
    digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2 + ivf,
    dict   = c(
      "fit_dW"         = "$\\widehat{\\Delta w}_{ist}$",
      "fit_dW:SoE"     = "$\\widehat{\\Delta w}_{ist} \\times SOE_{st}$",
      "fit_dW:postSoE" = "$\\widehat{\\Delta w}_{ist} \\times postSOE_{st}$",
      "dW"             = "$\\Delta w_{ist}$",
      "dW:SoE"         = "$\\Delta w_{ist} \\times SOE_{st}$",
      "dW:postSoE"     = "$\\Delta w_{ist} \\times postSOE_{st}$"
    ),
    headers = list("Pass-through: $\\Delta p_{ist}$" = 2),
    notes = c(
      "IV instrument: $Z_{ist}$ = inverse-distance-weighted mean $\\Delta w_{jt}$",
      "across stores $j$ in other states, using store lat/lon (2 decimal places).",
      "Column (2) instruments $\\Delta w$, $\\Delta w \\times SOE$, and $\\Delta w \\times postSOE$",
      "with $Z_{ist}$, $Z_{ist} \\times SOE$, and $Z_{ist} \\times postSOE$.",
      "FEs: product, store, week. Standard errors clustered at store level."
    )
  )
  message("Saved: tables_latex/24_tab_iv_passthrough.tex")

}  # end IV block

message("Mechanism 3 (demand rotation) complete.")

# ==============================================================================
# 08_demand_rotation.R
#
# Food Retailer Pricing Behavior Under Anti-Price Gouging Laws: Evidence from
# Wholesale and Retail Scanner Data -- Chenarides, Richards, and Dong
#
# Purpose: Mechanism 3 -- Countercyclical promotional pricing (Butters 2025).
#
# --------------------------------------------------------------------------
# KEY FINDING FROM TRANSACTION-LEVEL DATA (stg.pd_store_upc_day):
#
#   Intra-day price discrimination does NOT occur in this data. Across all
#   five focal products and all three SOE periods, both_types_present = 0
#   for every store-UPC-day observation. That is, no store-day observation contains
#   a mix of regular-price and sale-price transactions. On any given day at
#   any given store, all shoppers pay the same price for the same product:
#   either the store runs a promotion (everyone gets the sale price) or it
#   does not (everyone pays the shelf price).
#
#   Loyalty cards (share_loyalty = 96-99%) do not generate intra-day price
#   variation. Sale prices appear to be store-wide promotions applied to all
#   shoppers, not loyalty-card-exclusive discounts.
#
# COUNTERCYCLICAL PRICING MECHANISM (Butters 2025):
#
#   The mechanism is inter-temporal, not within-day. Retailers choose on any
#   given day (or week) whether to run a promotion. The price-discriminating
#   retailer serves two types of shoppers: price-elastic deal-hunters (who
#   only buy when a promotion runs) and price-inelastic habitual buyers (who
#   buy at the regular price regardless). The optimal promotion frequency
#   depends on the composition of demand. If the COVID-19 demand shock
#   shifted the shopper mix toward more inelastic buyers (e.g., captive
#   essential-goods shoppers), the retailer's optimal response is to run
#   promotions MORE often to recapture elastic shoppers, not LESS. The net
#   price falls because more transaction-weeks are in "promotion mode," even
#   though the posted shelf price and the sale price themselves are
#   unchanged. This is confirmed by the sharp rise in share_on_sale during
#   the SOE (Pre-SOE: 0.5-20%, During SOE: 37-91% depending on product)
#   while the shelf price (p_gross_weekly) is flat.
#
# EMPIRICAL STRATEGY:
#   M3a. Gross vs net price SOE gap -- does the gross-net spread widen during
#        the SOE? A widening spread means deal frequency rose, consistent with
#        countercyclical promotional pricing.
#   M3b. Promotional intensity regressions -- did share_on_sale and discount
#        depth change during / after the SOE, conditional on product and store
#        FEs? This is the main test of the countercyclical pricing channel.
#   M3c. Gross price stability -- was the posted shelf price flat during the
#        SOE? A near-zero SOE coefficient on p_gross_weekly, alongside a
#        significant coefficient on share_on_sale, confirms that the net price
#        decline was driven by promotion frequency rather than shelf price cuts.
#   M3d. IV pass-through (demand rotation) -- instrument for Delta_w using
#        distance-weighted cross-market wholesale prices (Z_ist) constructed
#        from store lat/lon. Identifies how price sensitivity changed.
#
# DATA INPUTS:
#   panel_est            -- from 02_build_panel.R (store-product-week)
#   promo_panel          -- from stg.pd_store_upc_week (read via SQL connection)
#   store_info           -- from stg.pos_store_master (lat/lon for IV)
#
# OUTPUTS (tables_latex/):
#   21_tab_gross_net_gap.tex
#   22_tab_promo_intensity.tex
#   23_tab_gross_price_stability.tex
#   24_tab_iv_passthrough.tex
#
# OUTPUTS (figures/):
#   19_fig_gross_net_gap.png
#   20_fig_promo_intensity.png
#
# DEPENDS ON: panel_est, save_tex(), store_info, SAVE_CSV
#   Also requires stg.pd_store_upc_week in SQL (run
#   BuildMarkupsNew_PriceDiscrimination.sql first).
# ==============================================================================

message("Estimating Mechanism 3: countercyclical promotional pricing ...")

# ==============================================================================
# M3a. GROSS VS NET PRICE GAP OVER TIME
# ==============================================================================
# The gross-net gap equals the volume-weighted discount per unit.
# A wider gap during SOE means retailers ran more frequent promotions.
# The gap is zero when no promotion is running (everyone pays shelf price).
# ==============================================================================

gap_data <- panel_est %>%
  filter(p_ist_gross > 0, p_ist_net > 0,
         is.finite(p_ist_gross), is.finite(p_ist_net)) %>%
  mutate(gross_net_gap = p_ist_gross - p_ist_net)

# Regression: does the gross-net gap change during / after SOE?
m_gap_base <- feols(
  gross_net_gap ~ SoE + postSoE | product + store_id,
  data    = gap_data,
  cluster = ~ sst
)

etable(list("(1) Pooled" = m_gap_base))

etable(
  list("(1) Pooled" = m_gap_base),
  tex    = TRUE,
  file   = "tables_latex/21_tab_gross_net_gap.tex",
  title  = "Effect of SOE on gross--net price gap (promotional discount per unit)",
  label  = "tab:gross_net_gap",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict   = c(
    "SoE"     = "SOE$_{st}$",
    "postSoE" = "Post-SOE$_{st}$"
  ),
  headers = list("$p^{gross}_{ist} - p^{net}_{ist}$ (\\$/unit)" = 1),
  notes   = c(
    "Dependent variable: gross price minus net price (dollars per unit or lb).",
    "A positive SOE coefficient means more transactions occurred at promotional prices during the emergency.",
    "When no promotion is running, gross equals net and the gap is zero.",
    "FEs: product and store. Standard errors clustered at the state level."
  )
)
message("Saved: tables_latex/21_tab_gross_net_gap.tex")

# Descriptive means by state x period
gap_state_summary <- gap_data %>%
  group_by(sst, soe_period = case_when(
    SoE == 1    ~ "During SOE",
    postSoE == 1 ~ "Post-SOE",
    TRUE         ~ "Pre-SOE"
  )) %>%
  summarise(
    mean_gap = round(mean(gross_net_gap, na.rm = TRUE), 3),
    n        = n(),
    .groups  = "drop"
  ) %>%
  pivot_wider(names_from = soe_period, values_from = c(mean_gap, n),
              names_glue = "{soe_period}_{.value}")

gap_state_summary

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
    subtitle = "Gap = posted shelf price minus net transaction price. Shaded = SOE window. Gap > 0 means promotions were running.",
    x = "Week", y = "Mean gap: gross minus net (nominal $/unit)"
  ) +
  theme_bw() +
  theme(plot.subtitle = element_text(size = 8))

g_gap

ggsave("figures/19_fig_gross_net_gap.png", g_gap, width = 10, height = 5, dpi = 300)
message("Saved: figures/19_fig_gross_net_gap.png")


# ==============================================================================
# M3b. PROMOTIONAL INTENSITY REGRESSIONS
# ==============================================================================
# Main test of countercyclical pricing. Retailers serve elastic shoppers by
# running promotions; the SOE shifted the frequency of promotion weeks.
#
# Outcome 1: share_on_sale -- fraction of transactions at the promotional price.
#   Pre-SOE: 0.5-20% by product. During SOE: 37-91%. This is the primary
#   evidence that retailers expanded promotion frequency, not that they cut
#   the posted shelf price.
#
# Outcome 2: avg_discount_depth -- regular price minus sale price when on sale.
#   Tests whether the SIZE of the markdown also changed, or only the frequency.
#
# Data: stg.pd_store_upc_week, read via SQL connection.
# If SQL unavailable, falls back to share_on_sale from panel_est.
# ==============================================================================

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
      store_id   = as.factor(store_id),
      product    = recode(upc,
                          "4011"       = "bananas",
                          "4069"       = "cabbage",
                          "4062"       = "cucumbers",
                          "7143001065" = "lettuce",
                          "4087"       = "tomatoes"),
      product    = as.factor(product),
      week_fe    = as.factor(week_seq),
      soe_period = factor(soe_period, levels = c("Pre-SOE", "During SOE", "Post-SOE"))
    ) %>%
    left_join(
      panel_est %>%
        select(store_id, product, week_seq, SoE, postSoE, preSoE) %>%
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

# -- Share on sale and discount depth regressions ------------------------------
if (!is.null(promo_panel)) {
  
  m_share_base <- feols(
    share_on_sale ~ SoE + postSoE | product + store_id,
    data    = promo_panel,
    cluster = ~ sst
  )
  
  m_share_week_fe <- feols(
    share_on_sale ~ SoE + postSoE | product + store_id + week_fe,
    data    = promo_panel,
    cluster = ~ sst
  )
  
  # Discount depth: conditional on a sale occurring this week
  m_depth_base <- feols(
    avg_discount_depth ~ SoE + postSoE | product + store_id,
    data    = promo_panel %>% filter(!is.na(avg_discount_depth)),
    cluster = ~ sst
  )
  
  etable(
    list(
      "(1) Share, no week FE" = m_share_base,
      "(2) Share, week FE"    = m_share_week_fe,
      "(3) Discount depth"    = m_depth_base
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
      "Share on sale"             = 2,
      "Discount depth (\\$/unit)" = 1
    ),
    notes = c(
      "Columns (1)--(2): dependent variable is share of transactions at promotional price.",
      "Column (3): dependent variable is regular price minus sale price, conditional on a promotion running.",
      "Promotion frequency (share on sale) rose sharply during the SOE (from 0.5--20\\% pre-SOE",
      "to 37--91\\% during SOE depending on product), consistent with countercyclical promotional pricing.",
      "Intra-day price variation is absent: on any store-day, all shoppers pay the same price.",
      "The mechanism is inter-temporal -- retailers switched more weeks into promotion mode.",
      "FEs: product and store. Column (2) adds week FEs. Standard errors clustered at the state level."
    )
  )
  message("Saved: tables_latex/22_tab_promo_intensity.tex")
  
} else {
  m_share_fallback <- feols(
    share_on_sale ~ SoE + postSoE | product + store_id,
    data    = panel_est %>% filter(is.finite(share_on_sale)),
    cluster = ~ sst
  )
  etable(list("(1) Share on sale" = m_share_fallback))
  message("Note: promo_panel unavailable; ran fallback share_on_sale regression on panel_est.")
}

# -- Promotional intensity figure ----------------------------------------------
if (!is.null(promo_panel)) {
  
  share_weekly <- promo_panel %>%
    group_by(week_start) %>%
    summarise(mean_share = mean(share_on_sale, na.rm = TRUE), .groups = "drop")
  
  depth_weekly <- promo_panel %>%
    filter(!is.na(avg_discount_depth)) %>%
    group_by(week_start) %>%
    summarise(mean_depth = mean(avg_discount_depth, na.rm = TRUE), .groups = "drop")
  
  # Make sure dates are Date objects
  share_weekly <- share_weekly %>%
    mutate(week_start = as.Date(week_start))
  
  depth_weekly <- depth_weekly %>%
    mutate(week_start = as.Date(week_start))
  
  soe_start <- as.Date(soe_w$soe_start)
  soe_end   <- as.Date(soe_w$soe_end)
  
  scale_fac <- max(depth_weekly$mean_depth, na.rm = TRUE) /
    max(share_weekly$mean_share, na.rm = TRUE)
  
  g_promo <- ggplot(share_weekly, aes(x = week_start)) +
    annotate("rect",
             xmin = soe_w$soe_start, 
             xmax = soe_w$soe_end,
             ymin = -Inf, 
             ymax = Inf, 
             alpha = 0.12, 
             fill = "grey50") +
    geom_line(aes(y = mean_share, linetype = "Share on sale"), linewidth = 0.7) +
    geom_line(data = depth_weekly,
              aes(y = mean_depth / scale_fac, linetype = "Discount depth (scaled)"),
              linewidth = 0.7, color = "firebrick") +
    scale_y_continuous(
      name     = "Share of transactions on sale",
      labels   = scales::label_percent(),
      sec.axis = sec_axis(~ . * scale_fac, name = "Mean discount depth ($/unit)")
    ) +
    scale_linetype_manual(values = c("Share on sale"           = "solid",
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

g_promo

# ==============================================================================
# M3c. GROSS PRICE STABILITY DURING SOE
# ==============================================================================
# If the countercyclical pricing channel explains the net price decline, the
# posted shelf price (p_gross_weekly) should be flat during the SOE. A near-
# zero SOE coefficient on the gross price, alongside a significant positive
# coefficient on share_on_sale (M3b), confirms that the net price decline was
# driven entirely by more transactions occurring at already-existing promotional
# prices -- not by cuts to the shelf price itself.
#
# This test is run on promo_panel (from stg.pd_store_upc_week) so that
# p_gross_weekly and share_on_sale come from the same data source.
# ==============================================================================

if (!is.null(promo_panel)) {
  
  promo_gross <- promo_panel %>%
    filter(p_gross_weekly > 0, is.finite(p_gross_weekly))
  
  # Gross price level regression
  m_gross_base <- feols(
    p_gross_weekly ~ SoE + postSoE | product + store_id,
    data    = promo_gross,
    cluster = ~ sst
  )
  
  m_gross_week_fe <- feols(
    p_gross_weekly ~ SoE + postSoE | product + store_id + week_fe,
    data    = promo_gross,
    cluster = ~ sst
  )
  
  # Net price regression on same sample for direct comparison
  m_net_base <- feols(
    p_net_weekly ~ SoE + postSoE | product + store_id,
    data    = promo_panel %>% filter(p_net_weekly > 0, is.finite(p_net_weekly)),
    cluster = ~ sst
  )
  
  etable(
    list(
      "(1) Gross, no week FE" = m_gross_base,
      "(2) Gross, week FE"    = m_gross_week_fe,
      "(3) Net price"         = m_net_base
    ),
    tex    = TRUE,
    file   = "tables_latex/23_tab_gross_price_stability.tex",
    title  = "Gross price stability during SOE: posted shelf price vs.\\ net transaction price",
    label  = "tab:gross_price_stability",
    digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
    dict   = c(
      "SoE"     = "SOE$_{st}$",
      "postSoE" = "Post-SOE$_{st}$"
    ),
    headers = list(
      "Gross price $p^{gross}$ (\\$/unit)" = 2,
      "Net price $p^{net}$ (\\$/unit)"     = 1
    ),
    notes = c(
      "Columns (1)--(2): dependent variable is the revenue-weighted gross (shelf) price.",
      "Column (3): dependent variable is the revenue-weighted net (transaction) price.",
      "A near-zero SOE coefficient on the gross price alongside a negative coefficient",
      "on the net price confirms that the net price decline was driven by promotional",
      "expansion (more transactions at the existing sale price), not shelf price reductions.",
      "FEs: product and store. Column (2) adds week FEs. Standard errors clustered at the state level."
    )
  )
  message("Saved: tables_latex/23_tab_gross_price_stability.tex")
  
}


# ==============================================================================
# M3d. IV PASS-THROUGH: DEMAND ROTATION
# ==============================================================================
# Instrument: distance-weighted cross-market wholesale price (Z_ist).
#   For each store i in state s, Z_ist is the inverse-distance-weighted average
#   of dW_jt for the same product in all stores j outside state s. This
#   instrument shifts the supply curve (cost) without directly affecting local
#   demand conditions.
#
# Store lat/lon: available from stg.pos_store_master (store_info object,
#   loaded in 00_read_in_data.R).
#
# Specification:
#   Stage 1: dW_ist = pi0 + pi1*Z_ist + pi2*(Z_ist*SoE) + gamma_i + tau_t
#   Stage 2: dP_ist = alpha + beta1*dW_hat + beta2*(dW_hat*SoE) +
#                     beta3*(dW_hat*postSoE) + gamma_i + delta_j + tau_t
#
# The SOE interaction on the instrument (Z*SoE) tests whether pass-through
# changed during the emergency period -- a shift in pass-through is consistent
# with a rotation in demand elasticity.
# ==============================================================================

message("Building distance-weighted IV ...")

store_info <- store_info %>%
  rename(latitude = lat, longitude = lon)
  
if (!all(c("latitude", "longitude") %in% names(store_info))) {
  warning("store_info missing latitude/longitude. Check stg.pos_store_master column names.")
  message("Skipping IV pass-through (M3d). Run with lat/lon columns available.")
} else {
  
  pt_iv_data <- panel_est %>%
    filter(is.finite(dP), is.finite(dW)) %>%
    left_join(
      store_info %>%
        select(store_id, latitude, longitude) %>%
        distinct() %>%
        mutate(store_id = as.factor(store_id)),
      by = "store_id"
    ) %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  store_coords <- pt_iv_data %>%
    select(store_id, latitude, longitude, sst) %>%
    distinct()
  
  message("Computing cross-market IV (this may take a moment) ...")
  
  dw_store_week <- pt_iv_data %>%
    select(store_id, product, week_seq, dW, sst, latitude, longitude) %>%
    distinct()
  
  iv_raw <- dw_store_week %>%
    rename(lat_i = latitude, lon_i = longitude, sst_i = sst) %>%
    inner_join(
      dw_store_week %>%
        rename(store_j = store_id, dW_j = dW, sst_j = sst,
               lat_j = latitude, lon_j = longitude),
      by = c("product", "week_seq"),
      relationship = "many-to-many"
    ) %>%
    filter(sst_i != sst_j) %>%
    mutate(
      dist_ij = sqrt((lat_i - lat_j)^2 + (lon_i - lon_j)^2),
      weight  = 1 / pmax(dist_ij, 0.01)
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
  
  m_iv <- feols(
    dP ~ 1 | product + store_id + week_fe |
      dW + dW:SoE + dW:postSoE ~ Z_ist + Z_ist:SoE + Z_ist:postSoE,
    data    = pt_iv_data,
    cluster = ~ sst
  )
  
  m_ols_iv_sample <- feols(
    dP ~ dW + dW:SoE + dW:postSoE | product + store_id + week_fe,
    data    = pt_iv_data,
    cluster = ~ sst
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
      "across stores $j$ in other states, using store latitude and longitude.",
      "Column (2) instruments $\\Delta w$, $\\Delta w \\times SOE$, and $\\Delta w \\times postSOE$",
      "with $Z_{ist}$, $Z_{ist} \\times SOE$, and $Z_{ist} \\times postSOE$.",
      "FEs: product, store, week. Standard errors clustered at the state level."
    )
  )
  message("Saved: tables_latex/24_tab_iv_passthrough.tex")
  
}

message("Mechanism 3 (countercyclical promotional pricing) complete.")

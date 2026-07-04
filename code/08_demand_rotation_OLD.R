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
#   M3d. 
#        
#        
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
  con_promo <- open_decadata_connection()   # server/database set in code/config.R
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

  g_promo

}

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
# M3d. CONTROL FUNCTION DEMAND ESTIMATION (DEMAND ROTATION)
# ==============================================================================
# Tests whether the COVID-19 demand shock rotated the demand curve during the
# SOE. Following Butters et al. (2025), we estimate a reduced-form demand
# equation with SOE-period indicators and SOE-by-price interaction terms.
#
# The key coefficient is eta_SOE: the interaction of demeaned log price with
# the SOE indicator.
#   - Positive eta_SOE: demand became less elastic during SOE (captive shopper
#     story -- inelastic essential-goods buyers added produce to basket)
#   - Negative eta_SOE: demand became more elastic during SOE (Butters et al.
#     story -- occasional buyers entered the category)
#
# ENDOGENEITY: price appears in three places on the RHS (demeaned log price,
# its interaction with SOE, its interaction with postSOE). 2SLS is not
# applicable when the endogenous variable appears as an interaction term.
# We use a control function (CF) approach: run three first stages, save
# residuals, include residuals as regressors in the second stage.
#
# INSTRUMENT: inverse-distance-weighted average log price of the same product
# in stores outside state s during week t (Z_jist). Following Gandhi and
# Houde (2019). Interacted with SOE and postSOE for the three interaction terms.
#
# STANDARD ERRORS: bootstrapped (200 replications, clustered at store level)
# because second-stage regressors include generated variables (first-stage
# residuals) and analytic SEs do not account for first-stage uncertainty.
#
# DATA: panel_est (store-product-week). Requires store lat/lon from store_info.
# ==============================================================================

message("Estimating M3d: control function demand rotation ...")

# ------------------------------------------------------------------------------
# Step 0: check that store lat/lon is available
# ------------------------------------------------------------------------------

if (all(c("lat", "lon") %in% names(store_info))) {
  store_info <- store_info %>% rename(latitude = lat, longitude = lon)
}
if (!all(c("latitude", "longitude") %in% names(store_info))) {
  warning("store_info missing latitude/longitude. Skipping M3d.")
  M3D_OK <- FALSE
} else {
  message("OK to continue with M3d.")
  M3D_OK <- TRUE
}

if (M3D_OK) {
  
# ----------------------------------------------------------------------------
# Step 1: build the CF data frame with log price and log quantity
# ----------------------------------------------------------------------------

cf_data <- panel_est %>%
  rename(vol = upc_week_volume) %>%
  filter(
    p_ist > 0, vol > 0,
    is.finite(p_ist), is.finite(vol)
  ) %>%
  mutate(
    lnQ = log(vol),
    lnP = log(p_ist)
  ) %>%
  group_by(product) %>%
  mutate(lnP_mean = mean(lnP, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    lnP_dm      = lnP - lnP_mean,
    lnP_dm_soe  = lnP_dm * SoE,
    lnP_dm_post = lnP_dm * postSoE
  ) %>%
  left_join(
    store_info %>%
      select(store_id, latitude, longitude) %>%
      distinct() %>%
      mutate(store_id = as.factor(store_id)),
    by = "store_id"
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

message(sprintf("CF data: %d obs across %d stores and %d products.",
                nrow(cf_data), n_distinct(cf_data$store_id),
                n_distinct(cf_data$product)))

# ----------------------------------------------------------------------------
# Step 2: construct the distance-weighted cross-market log price instrument
# ----------------------------------------------------------------------------
# For each store i in state s, product j, week t:
#   Z_jist = sum_{i' not in s} [d(i,i')^{-1} * lnP_{ji't}]
#            / sum_{i' not in s} [d(i,i')^{-1}]
# "Other market" = other states. Weights = inverse Euclidean distance.
# ----------------------------------------------------------------------------

message("Building distance-weighted log price instrument ...")

store_lnP <- cf_data %>%
  select(store_id, product, week_seq, lnP, sst, latitude, longitude) %>%
  distinct()

iv_cf_raw <- store_lnP %>%
  rename(lat_i = latitude, lon_i = longitude, sst_i = sst) %>%
  inner_join(
    store_lnP %>%
      rename(store_j = store_id, lnP_j = lnP, sst_j = sst,
             lat_j = latitude, lon_j = longitude),
    by           = c("product", "week_seq"),
    relationship = "many-to-many"
  ) %>%
  filter(sst_i != sst_j) %>%
  mutate(
    dist_ij = sqrt((lat_i - lat_j)^2 + (lon_i - lon_j)^2),
    weight  = 1 / pmax(dist_ij, 0.01)
  ) %>%
  group_by(store_id, product, week_seq) %>%
  summarise(
    Z_jist = weighted.mean(lnP_j, w = weight, na.rm = TRUE),
    .groups = "drop"
  )

cf_data <- cf_data %>%
  left_join(iv_cf_raw, by = c("store_id", "product", "week_seq")) %>%
  filter(is.finite(Z_jist)) %>%
  mutate(
    Z_soe  = Z_jist * SoE,
    Z_post = Z_jist * postSoE
  )

message(sprintf("CF data after IV merge: %d obs.", nrow(cf_data)))

# ----------------------------------------------------------------------------
# Step 3: three first-stage regressions
# ----------------------------------------------------------------------------

message("Running three first-stage regressions ...")

fs1 <- feols(
  lnP_dm ~ Z_jist + Z_soe + Z_post + SoE + postSoE | product + store_id,
  data = cf_data, cluster = ~ store_id
)
fs2 <- feols(
  lnP_dm_soe ~ Z_jist + Z_soe + Z_post + SoE + postSoE | product + store_id,
  data = cf_data, cluster = ~ store_id
)
fs3 <- feols(
  lnP_dm_post ~ Z_jist + Z_soe + Z_post + SoE + postSoE | product + store_id,
  data = cf_data, cluster = ~ store_id
)

etable(
  list("FS1: lnP_dm" = fs1, "FS2: lnP_dm x SOE" = fs2,
       "FS3: lnP_dm x postSOE" = fs3),
  fitstat = ~ n + r2 + f
)

message("First stage F-statistics:")
message(sprintf("  FS1 (lnP_dm):      F = %.1f", fitstat(fs1, ~ f)$f[1]))
message(sprintf("  FS2 (lnP_dm*SOE):  F = %.1f", fitstat(fs2, ~ f)$f[1]))
message(sprintf("  FS3 (lnP_dm*post): F = %.1f", fitstat(fs3, ~ f)$f[1]))

# Stock-Yogo (2005) critical values: 1 endogenous variable, K = 3 excluded instruments.
# Source: Stock & Yogo (2005), Table 1 (maximal size) and Table 2 (relative bias).
# "Maximal size" = worst-case 5% Wald test size distortion <= threshold.
# "Relative bias" = IV bias as share of OLS bias <= threshold.
sy_k3 <- list(
  maxsize_10pct = 22.30,  # 10% maximal size (most commonly cited)
  maxsize_15pct = 15.09,
  relbias_10pct = 12.83,  # 10% relative bias
  relbias_05pct = 22.30   # 5% relative bias (same as 10% maxsize for K=3)
)

fs_stats <- c(
  "FS1: lnP_dm"      = fitstat(fs1, ~ f)$f[1],
  "FS2: lnP_dm*SOE"  = fitstat(fs2, ~ f)$f[1],
  "FS3: lnP_dm*post" = fitstat(fs3, ~ f)$f[1]
)

message("\nFirst-stage F-statistics vs Stock-Yogo (2005) critical values (K=3 instruments):")
message(sprintf("  %-22s  %6s  %6s  %6s  %6s",
                "", "F-stat", "SY 10%", "SY 15%", "Bias10"))
message(sprintf("  %-22s  %6s  %6s  %6s  %6s",
                "", "      ", "maxsz", "maxsz", "relbias"))
for (nm in names(fs_stats)) {
  message(sprintf("  %-22s  %6.1f  %6.2f  %6.2f  %6.2f  %s",
                  nm,
                  fs_stats[nm],
                  sy_k3$maxsize_10pct,
                  sy_k3$maxsize_15pct,
                  sy_k3$relbias_10pct,
                  ifelse(fs_stats[nm] > sy_k3$maxsize_10pct, "PASS", "FAIL")))
}

cf_data <- cf_data %>%
  mutate(
    resid_fs1 = resid(fs1),
    resid_fs2 = resid(fs2),
    resid_fs3 = resid(fs3)
  )

etable(
  list(
    "(1) $\\ln p_{jist} - \\overline{\\ln p}_{j}$"               = fs1,
    "(2) $(\\ln p_{jist} - \\overline{\\ln p}_{j}) \\times SOE$"  = fs2,
    "(3) $(\\ln p_{jist} - \\overline{\\ln p}_{j}) \\times post$" = fs3
  ),
  tex     = TRUE,
  file    = "tables_latex/25_tab_demand_firststage.tex",
  title   = "First-stage regressions for control function demand estimation",
  label   = "tab:demand_firststage",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2 + f,
  dict    = c(
    "Z_jist" = "$Z_{jist}$",
    "Z_soe"  = "$Z_{jist} \\times SOE_{st}$",
    "Z_post" = "$Z_{jist} \\times postSOE_{st}$"
  ),
  notes = c(
    "Dependent variables are the three endogenous terms in equation~\\ref{eq:demand_butters}.",
    "Instrument: $Z_{jist}$ = inverse-distance-weighted average log net price of",
    "the same product in stores outside state $s$ during week $t$.",
    "Fixed effects: product and store. Standard errors clustered at the store level.",
    "F-statistic tests joint significance of instruments in each first stage."
  )
)
message("Saved: tables_latex/25_tab_demand_firststage.tex")

# ----------------------------------------------------------------------------
# Step 4: second stage (OLS on augmented demand equation)
# ----------------------------------------------------------------------------
# Residuals absorb the endogenous component of each price term.
# Do not interpret analytic SEs -- use bootstrap SEs from Step 5.
# ----------------------------------------------------------------------------

message("Running second stage (control function) ...")

m_demand_cf_ols <- feols(
  lnQ ~ SoE + postSoE +
    lnP_dm + lnP_dm_soe + lnP_dm_post +
    resid_fs1 + resid_fs2 + resid_fs3 |
    product + store_id,
  data    = cf_data,
  cluster = ~ store_id
)

etable(list("CF demand (analytic SE)" = m_demand_cf_ols))

message("Second-stage key coefficients (analytic SE -- bootstrap pending):")
message(sprintf("  Baseline elasticity (lnP_dm):    %.3f",
                coef(m_demand_cf_ols)["lnP_dm"]))
message(sprintf("  SOE rotation (lnP_dm_soe):       %.3f",
                coef(m_demand_cf_ols)["lnP_dm_soe"]))
message(sprintf("  Post-SOE rotation (lnP_dm_post): %.3f",
                coef(m_demand_cf_ols)["lnP_dm_post"]))


# ----------------------------------------------------------------------------
# Robustness checks
# ----------------------------------------------------------------------------

# 1. Restrict to pre-SOE and during-SOE only (drop post) to check stability
m_demand_cf_soe_only <- feols(
  lnQ ~ SoE + lnP_dm + lnP_dm_soe + resid_fs1 + resid_fs2 | product + store_id,
  data    = cf_data %>% filter(postSoE == 0),
  cluster = ~ store_id
)

# 2. Run plain OLS (no CF) on same sample for comparison
m_demand_ols <- feols(
  lnQ ~ SoE + postSoE + lnP_dm + lnP_dm_soe + lnP_dm_post | product + store_id,
  data    = cf_data,
  cluster = ~ store_id
)

etable(list("OLS" = m_demand_ols, "CF" = m_demand_cf_ols))

# 3. Drop first 8 SOE weeks to check if stockpiling drives rotation
cf_data_nosurge <- cf_data %>%
  left_join(
    panel_est %>%
      filter(SoE == 1) %>%
      group_by(store_id, product) %>%
      mutate(soe_week_num = row_number()) %>%
      select(store_id, product, week_seq, soe_week_num),
    by = c("store_id", "product", "week_seq")
  ) %>%
  filter(is.na(soe_week_num) | soe_week_num > 8)

m_demand_nosurge <- feols(
  lnQ ~ SoE + postSoE +
    lnP_dm + lnP_dm_soe + lnP_dm_post +
    resid_fs1 + resid_fs2 + resid_fs3 |
    product + store_id,
  data    = cf_data_nosurge,
  cluster = ~ store_id
)

etable(list(
  "Full SOE"      = m_demand_cf_ols,
  "Drop first 8w" = m_demand_nosurge
))

# ----------------------------------------------------------------------------
# Step 5: bootstrap standard errors
# ----------------------------------------------------------------------------
# Resample stores with replacement (cluster bootstrap). Re-runs all three
# first stages + second stage on each bootstrap sample. B = 200 replications.
# ----------------------------------------------------------------------------

if (RUN_CF_BOOTSTRAP) {
  
message("Bootstrapping standard errors (B = 200, clustered at store level) ...")
message("This may take several hours. Consider running overnight.")

set.seed(42)
B          <- 200
store_ids  <- unique(cf_data$store_id)
boot_coefs <- matrix(NA, nrow = B, ncol = length(coef(m_demand_cf_ols)))
colnames(boot_coefs) <- names(coef(m_demand_cf_ols))

t0_boot <- proc.time()

for (b in seq_len(B)) {
  
  sampled_stores <- sample(store_ids, length(store_ids), replace = TRUE)
  
  boot_df <- map_dfr(
    seq_along(sampled_stores),
    ~ cf_data %>%
      filter(store_id == sampled_stores[.x]) %>%
      mutate(store_boot = paste0("s", .x))
  ) %>%
    mutate(store_id_boot = as.factor(store_boot))
  
  tryCatch({
    bfs1 <- feols(
      lnP_dm ~ Z_jist + Z_soe + Z_post + SoE + postSoE | product + store_id_boot,
      data = boot_df, warn = FALSE, notes = FALSE
    )
    bfs2 <- feols(
      lnP_dm_soe ~ Z_jist + Z_soe + Z_post + SoE + postSoE | product + store_id_boot,
      data = boot_df, warn = FALSE, notes = FALSE
    )
    bfs3 <- feols(
      lnP_dm_post ~ Z_jist + Z_soe + Z_post + SoE + postSoE | product + store_id_boot,
      data = boot_df, warn = FALSE, notes = FALSE
    )
    
    boot_df <- boot_df %>%
      mutate(
        resid_fs1 = resid(bfs1),
        resid_fs2 = resid(bfs2),
        resid_fs3 = resid(bfs3)
      )
    
    bss <- feols(
      lnQ ~ SoE + postSoE +
        lnP_dm + lnP_dm_soe + lnP_dm_post +
        resid_fs1 + resid_fs2 + resid_fs3 |
        product + store_id_boot,
      data = boot_df, warn = FALSE, notes = FALSE
    )
    
    boot_coefs[b, names(coef(bss))] <- coef(bss)
    
  }, error = function(e) {
    message(sprintf("Bootstrap rep %d failed: %s", b, conditionMessage(e)))
  })
  
  if (b %% 20 == 0) {
    elapsed <- (proc.time() - t0_boot)["elapsed"]
    message(sprintf("  Bootstrap rep %d / %d  (%.1f min elapsed)", b, B, elapsed / 60))
  }
}

message(sprintf("Bootstrap done in %.1f minutes.",
                (proc.time() - t0_boot)["elapsed"] / 60))

boot_se <- apply(boot_coefs, 2, sd,       na.rm = TRUE)
boot_ci <- apply(boot_coefs, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

demand_results <- tibble(
  term     = names(coef(m_demand_cf_ols)),
  estimate = coef(m_demand_cf_ols),
  boot_se  = boot_se[names(coef(m_demand_cf_ols))],
  ci_low   = boot_ci["2.5%",  names(coef(m_demand_cf_ols))],
  ci_high  = boot_ci["97.5%", names(coef(m_demand_cf_ols))]
) %>%
  mutate(
    z        = estimate / boot_se,
    p_val    = 2 * pnorm(-abs(z)),
    stars    = case_when(
      p_val < 0.01 ~ "***",
      p_val < 0.05 ~ "**",
      p_val < 0.10 ~ "*",
      TRUE         ~ ""
    ),
    coef_str = paste0(formatC(estimate, digits = 3, format = "f"), stars),
    se_str   = paste0("(", formatC(boot_se, digits = 3, format = "f"), ")")
  )

print(demand_results %>% select(term, estimate, boot_se, ci_low, ci_high, stars))

if (SAVE_CSV) {
  write.csv(demand_results, "tables_csv/25_tab_demand_cf.csv",      row.names = FALSE)
  write.csv(boot_coefs,     "tables_csv/25_boot_coefs_raw.csv",     row.names = FALSE)
  message("Saved: tables_csv/25_tab_demand_cf.csv")
}

# ----------------------------------------------------------------------------
# Step 6: LaTeX output -- OLS | CF (analytic SE) | CF (bootstrap SE)
# ----------------------------------------------------------------------------

display_terms <- c(
  "SoE"         = "$SOE_{st}$",
  "postSoE"     = "$postSOE_{st}$",
  "lnP_dm"      = "$\\ln p_{jist} - \\overline{\\ln p}_{j}$",
  "lnP_dm_soe"  = "$(\\ln p_{jist} - \\overline{\\ln p}_{j}) \\times SOE_{st}$",
  "lnP_dm_post" = "$(\\ln p_{jist} - \\overline{\\ln p}_{j}) \\times postSOE_{st}$"
)

# Extract display-term coef and SE strings from a feols object
extract_display <- function(model, terms) {
  cf  <- coef(model)
  ses <- se(model)
  map_dfr(names(terms), function(nm) {
    est   <- if (nm %in% names(cf))  cf[nm]  else NA_real_
    s     <- if (nm %in% names(ses)) ses[nm] else NA_real_
    pv    <- if (!is.na(est) && !is.na(s) && s > 0) 2 * pnorm(-abs(est / s)) else NA_real_
    stars <- case_when(
      is.na(pv)  ~ "",
      pv < 0.01  ~ "***",
      pv < 0.05  ~ "**",
      pv < 0.10  ~ "*",
      TRUE       ~ ""
    )
    tibble(
      term     = nm,
      coef_str = if (!is.na(est)) paste0(formatC(est, digits = 3, format = "f"), stars) else "",
      se_str   = if (!is.na(s))   paste0("(", formatC(s,   digits = 3, format = "f"), ")") else ""
    )
  })
}

ols_disp  <- extract_display(m_demand_ols,    display_terms)
cf_disp   <- extract_display(m_demand_cf_ols, display_terms)
boot_disp <- demand_results %>%
  filter(term %in% names(display_terms)) %>%
  arrange(match(term, names(display_terms))) %>%
  select(term, coef_str, se_str)

# Build alternating coef / SE rows for all five display terms
coef_rows <- bind_rows(lapply(names(display_terms), function(nm) {
  bind_rows(
    tibble(label = display_terms[[nm]],
           col1  = ols_disp$coef_str[ ols_disp$term  == nm],
           col2  = cf_disp$coef_str[  cf_disp$term   == nm],
           col3  = boot_disp$coef_str[boot_disp$term == nm]),
    tibble(label = "",
           col1  = ols_disp$se_str[ ols_disp$term  == nm],
           col2  = cf_disp$se_str[  cf_disp$term   == nm],
           col3  = boot_disp$se_str[boot_disp$term == nm])
  )
}))

# Fit-stat rows (R2 is same for CF analytic and CF bootstrap -- same model)
fit_rows <- tibble(
  label = c("Observations", "$R^2$"),
  col1  = c(formatC(nobs(m_demand_ols),    format = "d", big.mark = ","),
            formatC(r2(m_demand_ols)["r2"],    digits = 3, format = "f")),
  col2  = c(formatC(nobs(m_demand_cf_ols), format = "d", big.mark = ","),
            formatC(r2(m_demand_cf_ols)["r2"], digits = 3, format = "f")),
  col3  = c(formatC(nobs(m_demand_cf_ols), format = "d", big.mark = ","),
            formatC(r2(m_demand_cf_ols)["r2"], digits = 3, format = "f"))
)

demand_tbl3 <- bind_rows(coef_rows, fit_rows)
n_coef_rows <- nrow(coef_rows)  # row after which to insert \midrule

save_tex(
  kbl(demand_tbl3,
      format    = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("", "(1) OLS", "(2) CF", "(3) CF bootstrap"),
      caption   = "Demand rotation: OLS, control function with analytic SEs, and control function with bootstrap SEs",
      label     = "tab:demand_cf",
      align     = "lrrr") %>%
    kable_styling(latex_options = "hold_position") %>%
    row_spec(n_coef_rows, extra_latex_after = "\\midrule") %>%
    footnote(
      general = c(
        "Dependent variable: $\\ln Q_{jist}$ (log quantity sold per store-product-week).",
        "Price variable: demeaned log net retail price, $\\ln p_{jist} - \\overline{\\ln p}_{j}$.",
        "Column (1): OLS with no endogeneity correction.",
        "Columns (2)--(3): control function approach; residuals from three first stages",
        "included as regressors to absorb endogenous price variation.",
        "Column (2) reports analytic standard errors (inconsistent -- shown for comparison only).",
        "Column (3) reports bootstrap standard errors ($B = 200$ replications, store-clustered).",
        "Instrument: $Z_{jist}$ = inverse-distance-weighted average log net price",
        "of the same product in stores outside state $s$ during week $t$.",
        "First-stage F-statistics reported in Table~\\ref{demand_firststage}.",
        "Fixed effects: product and store.",
        "Signif. codes: ***: 0.01, **: 0.05, *: 0.1"
      ),
      general_title = "", escape = FALSE
    ),
  "26_tab_demand_cf.tex"
)
message("Saved: tables_latex/26_tab_demand_cf.tex")

} else {
  message("RUN_CF_BOOTSTRAP = FALSE: skipping CF bootstrap and Table 26 (set flag in code/config.R).")
}

message("M3d (control function demand estimation) complete.")

}  # end if (M3D_OK)

message("Mechanism 3 (countercyclical promotional pricing) complete.")

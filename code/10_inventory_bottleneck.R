# ==============================================================================
# 10_inventory_bottleneck.R
#
# Supply side: did inventory pile up relative to sales during the SOE, and did
# inventory gluts precede promotions? Responds to Tim (Sept 16, Sept 29: the
# foodservice push / store bottleneck story) and Claude Major 4 (shrink and
# spoilage make wholesale cost an incomplete marginal-cost measure).
#
# Uses the DecaData inventory tables only (store-day on-hand UNITS). Argo
# shipment data are not used: Argo reports cases, which are not comparable to
# inventory units or POS volume without a conversion we do not have.
#
#   10.1 Coverage check (the UPC-count check). The 2021 extract of
#        stg.combined_inventory has far fewer distinct UPCs than 2020
#        (8,883 vs 128,200). This step checks whether coverage of the three
#        focal items changed, by month, before any series is built, and stops
#        the analysis window at a sustained coverage drop.
#   10.2 Store-product-week inventory and weeks of supply (WOS = average daily
#        on-hand units / weekly volume sold).
#   10.3 SOE effects on log inventory, log volume, and log WOS
#        (ln WOS = ln inventory - ln volume, so the coefficients add up).
#   10.4 Do inventory gluts precede promotions? Lagged log WOS and share on sale,
#        at the store and chain level.
#   10.5 Back-of-envelope shrink bound: how much of the SOE margin decline could
#        lower shrink from faster turnover explain, under assumed baseline
#        shrink rates.
#
# SCOPE (state in the manuscript)
#   - Cabbage (4069), cucumbers (4062), lettuce (7143001065) only. Bananas
#     (4011) and tomatoes (4087) have no inventory records under their codes.
#   - 2019-01-01 to 2021-12-31 only. 2018 is nearly empty for these items and
#     no inventory table extends past December 2021, so the post-SOE window is
#     partial (Mississippi has only a few post-SOE weeks).
#   - Inventory unit of measure for random-weight items may differ from POS
#     volume (pounds). Regressions use logs with store-product FEs, so a constant
#     unit factor within store-product drops out. WOS levels are reported only
#     as a sanity check.
#
# DATA SOURCE: stg.combined_inventory (store_id, date_key, upc, INT_CD,
#   inventory_units, inv_unit_cost, source). The stg.inventory view replaces
#   zero or missing inventory_units with inventory_selling_units, which would
#   turn true stock-outs into sales volume, so this script reads
#   combined_inventory directly and keeps inventory_units as recorded.
#   Duplicate store-day-UPC rows are collapsed with MAX(inventory_units).
#   All queries are read-only SELECTs.
#
# CACHING: the focal-item pull and the diagnostics are saved to data/
#   (git-ignored) and reused when INV_USE_CACHE = TRUE. Delete the .rds files
#   to force a fresh pull.
#
# Depends on: panel_est, save_tex(), SAVE_CSV, open_decadata_connection()
#
# Outputs (tables_latex/):
#   30_tab_inventory_coverage.tex
#   31_tab_inventory_soe.tex
#   32_tab_inventory_promo.tex
#   33_tab_shrink_bound.tex
# Outputs (figures/):
#   23_fig_inventory_coverage.png
#   24_fig_weeks_of_supply.png
# Outputs (tables_csv/, always written; small diagnostics):
#   30_inventory_coverage_monthly.csv
#   30_inventory_upc_universe_monthly.csv
# ==============================================================================

message("Estimating inventory and supply-side checks (10_inventory_bottleneck.R) ...")

if (!exists("INV_UPCS"))              INV_UPCS <- c(cabbage = "4069", cucumbers = "4062",
                                                    lettuce = "7143001065")
if (!exists("INV_START"))             INV_START <- 20190101L
if (!exists("INV_END"))               INV_END   <- 20211231L
if (!exists("INV_TABLE"))             INV_TABLE <- "stg.combined_inventory"
if (!exists("INV_USE_CACHE"))         INV_USE_CACHE <- TRUE
if (!exists("RUN_INV_DIAGNOSTICS"))   RUN_INV_DIAGNOSTICS <- TRUE
if (!exists("INV_MIN_COVERAGE"))      INV_MIN_COVERAGE <- 0.5   # share of pre-SOE coverage
if (!exists("INV_IGNORE_COVERAGE_DROP")) INV_IGNORE_COVERAGE_DROP <- FALSE
if (!exists("SHRINK_BASE_RATES"))     SHRINK_BASE_RATES <- c(0.05, 0.10, 0.15)  # assumptions

dir.create("data", showWarnings = FALSE)
CACHE_DAILY <- "data/inventory_focal_daily.rds"
CACHE_DIAG  <- "data/inventory_diagnostics.rds"

upc_sql <- paste(INV_UPCS, collapse = ", ")


# ==============================================================================
# 10.1 PULL AND COVERAGE CHECK
# ==============================================================================

run_inv_query <- function(sql) {
  con_inv <- open_decadata_connection()
  on.exit(DBI::dbDisconnect(con_inv), add = TRUE)
  DBI::dbGetQuery(con_inv, sql)
}

# ---- Focal-item store-day inventory -----------------------------------------
if (INV_USE_CACHE && file.exists(CACHE_DAILY)) {
  message("Reading cached focal-item inventory: ", CACHE_DAILY)
  inv_daily <- readRDS(CACHE_DAILY)
} else {
  message("Pulling focal-item inventory from ", INV_TABLE, " (one-time scan) ...")
  inv_daily <- run_inv_query(sprintf("
    SELECT store_id, CAST(upc AS varchar(20)) AS upc, date_key,
           MAX(inventory_units) AS inv_units,
           COUNT(*)             AS n_rows,
           SUM(CASE WHEN inventory_units IS NULL THEN 1 ELSE 0 END) AS n_null
    FROM %s
    WHERE upc IN (%s) AND date_key BETWEEN %d AND %d
    GROUP BY store_id, upc, date_key", INV_TABLE, upc_sql, INV_START, INV_END))
  saveRDS(inv_daily, CACHE_DAILY)
  message("Saved cache: ", CACHE_DAILY)
}

# ---- Diagnostics: item universe by month (explains the 2021 UPC drop) --------
inv_diag <- NULL
if (RUN_INV_DIAGNOSTICS) {
  if (INV_USE_CACHE && file.exists(CACHE_DIAG)) {
    inv_diag <- readRDS(CACHE_DIAG)
  } else {
    message("Pulling item-universe diagnostics from ", INV_TABLE, " (full scan of the window) ...")
    inv_diag <- run_inv_query(sprintf("
      SELECT date_key / 100 AS ym, source,
             COUNT(*)                 AS n_rows,
             COUNT(DISTINCT upc)      AS n_upc,
             COUNT(DISTINCT store_id) AS n_stores
      FROM %s
      WHERE date_key BETWEEN %d AND %d
      GROUP BY date_key / 100, source", INV_TABLE, INV_START, INV_END))
    saveRDS(inv_diag, CACHE_DIAG)
  }
  write.csv(inv_diag %>% arrange(ym, source),
            "tables_csv/30_inventory_upc_universe_monthly.csv", row.names = FALSE)
  message("Item universe by month and source (tables_csv/30_inventory_upc_universe_monthly.csv):")
  print(inv_diag %>% arrange(ym, source), n = Inf)
}

inv_daily <- inv_daily %>%
  mutate(
    store_id = as.character(store_id),
    upc      = as.character(upc),
    product  = names(INV_UPCS)[match(upc, INV_UPCS)],
    date     = as.Date(as.character(date_key), format = "%Y%m%d"),
    ym       = as.integer(date_key %/% 100)
  ) %>%
  filter(!is.na(product), !is.na(date))

# Panel stores selling each focal product, by month: the coverage denominator
panel_store_months <- panel_est %>%
  filter(as.character(product) %in% names(INV_UPCS)) %>%
  transmute(store_id = as.character(store_id), product = as.character(product),
            ym = as.integer(format(week_start, "%Y%m"))) %>%
  filter(ym >= INV_START %/% 100, ym <= INV_END %/% 100) %>%
  distinct()

inv_store_months <- inv_daily %>%
  filter(store_id %in% unique(panel_store_months$store_id)) %>%
  group_by(product, ym) %>%
  summarise(n_inv_stores = n_distinct(store_id),
            n_store_days = n(),
            dup_share    = mean(n_rows > 1),
            null_share   = sum(n_null) / sum(n_rows),
            zero_share   = mean(inv_units == 0, na.rm = TRUE),
            .groups = "drop")

coverage <- panel_store_months %>%
  count(product, ym, name = "n_panel_stores") %>%
  left_join(inv_store_months, by = c("product", "ym")) %>%
  mutate(n_inv_stores = tidyr::replace_na(n_inv_stores, 0L),
         coverage     = n_inv_stores / n_panel_stores,
         month        = as.Date(paste0(ym, "01"), format = "%Y%m%d")) %>%
  group_by(product) %>%
  mutate(pre_cov  = median(coverage[month < as.Date("2020-03-01")], na.rm = TRUE),
         low      = coverage < INV_MIN_COVERAGE * pre_cov) %>%
  ungroup() %>%
  arrange(product, month)

write.csv(coverage, "tables_csv/30_inventory_coverage_monthly.csv", row.names = FALSE)

# A sustained drop = two or more consecutive low-coverage months. The analysis
# window ends at the first sustained drop in any focal product.
first_sustained_drop <- coverage %>%
  group_by(product) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(run = low & dplyr::lead(low, default = FALSE)) %>%
  filter(run) %>%
  summarise(first_drop = min(month), .groups = "drop")

inv_end_date <- as.Date(as.character(INV_END), format = "%Y%m%d")
if (nrow(first_sustained_drop) > 0) {
  message("Sustained coverage drop (coverage < ", INV_MIN_COVERAGE,
          " x pre-SOE median for 2+ months):")
  print(first_sustained_drop)
  if (INV_IGNORE_COVERAGE_DROP) {
    warning("INV_IGNORE_COVERAGE_DROP = TRUE: using the full window despite the drop.")
  } else {
    inv_end_date <- min(first_sustained_drop$first_drop) - 1
    warning(sprintf("Inventory analysis window truncated to end %s at the first sustained coverage drop. ",
                    format(inv_end_date)),
            "Set INV_IGNORE_COVERAGE_DROP = TRUE to override.")
  }
} else {
  message("No sustained coverage drop for the focal items through ",
          format(inv_end_date), ".")
}

coverage_summary <- coverage %>%
  mutate(year = as.integer(format(month, "%Y"))) %>%
  group_by(product, year) %>%
  summarise(
    `Panel stores`        = round(mean(n_panel_stores)),
    `Stores w/ inventory` = round(mean(n_inv_stores)),
    Coverage              = mean(coverage),
    `Duplicate rows`      = mean(dup_share, na.rm = TRUE),
    `Null units`          = mean(null_share, na.rm = TRUE),
    `Zero units`          = mean(zero_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(Coverage, `Duplicate rows`, `Null units`, `Zero units`),
                ~ formatC(.x, digits = 3, format = "f"))) %>%
  rename(Product = product, Year = year)

save_tex(
  kbl(coverage_summary, format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Inventory coverage for the focal items, by year",
      label   = "inventory_coverage", align = "llrrrrrr") %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(general = c(
      "Source: stg.combined_inventory, store-day on-hand units. Monthly averages within each year.",
      "Coverage: stores with any inventory record in the month divided by panel stores selling the product that month.",
      "Duplicate rows: share of store-days with more than one record (collapsed with MAX).",
      "Null and zero units: shares of records with missing or zero on-hand units.",
      "Bananas and tomatoes have no inventory records and are excluded."),
      general_title = "", escape = FALSE),
  "30_tab_inventory_coverage.tex"
)

g_cov <- ggplot(coverage, aes(x = month, y = coverage, color = product)) +
  annotate("rect", xmin = as.Date("2020-03-09"), xmax = as.Date("2021-11-20"),
           ymin = -Inf, ymax = Inf, alpha = 0.10) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = NULL, y = "Panel stores with inventory records",
       title = "Inventory coverage of the focal items by month",
       subtitle = "Shaded: earliest SOE start to latest SOE end across states.",
       color = NULL) +
  theme_bw() + theme(legend.position = "top", plot.subtitle = element_text(size = 8))

ggsave("figures/23_fig_inventory_coverage.png", g_cov, width = 9, height = 5, dpi = 300)
message("Saved: figures/23_fig_inventory_coverage.png")


# ==============================================================================
# 10.2 STORE-PRODUCT-WEEK INVENTORY AND WEEKS OF SUPPLY
# ==============================================================================

week_map <- panel_est %>%
  distinct(week_seq, week_start) %>%
  arrange(week_start)

inv_daily <- inv_daily %>%
  filter(date <= inv_end_date) %>%
  mutate(w_idx = findInterval(as.numeric(date), as.numeric(week_map$week_start))) %>%
  filter(w_idx > 0) %>%
  mutate(week_seq = week_map$week_seq[w_idx],
         wk_start = week_map$week_start[w_idx]) %>%
  filter(as.numeric(date - wk_start) < 7)

inv_week <- inv_daily %>%
  group_by(store_id, product, week_seq) %>%
  arrange(date, .by_group = TRUE) %>%
  summarise(
    inv_avg  = mean(inv_units, na.rm = TRUE),
    inv_end  = dplyr::last(inv_units[!is.na(inv_units)], default = NA_real_),
    n_days   = sum(!is.na(inv_units)),
    .groups  = "drop"
  )

inv_panel <- panel_est %>%
  filter(as.character(product) %in% names(INV_UPCS)) %>%
  mutate(store_id_chr = as.character(store_id), product_chr = as.character(product)) %>%
  inner_join(inv_week, by = c("store_id_chr" = "store_id", "product_chr" = "product",
                              "week_seq" = "week_seq")) %>%
  filter(is.finite(inv_avg), inv_avg > 0, upc_week_volume > 0, n_days >= 4) %>%
  mutate(
    product      = droplevels(product),
    wos          = inv_avg / upc_week_volume,
    ln_inv       = log(inv_avg),
    ln_vol       = log(upc_week_volume),
    ln_wos       = ln_inv - ln_vol,
    product_woy  = interaction(product, week_of_year, drop = TRUE),
    retailer_product = interaction(retailer_id, product, drop = TRUE)
  )

message(sprintf("Inventory panel: %s store-product-weeks, %d stores, weeks %s to %s.",
                format(nrow(inv_panel), big.mark = ","), n_distinct(inv_panel$store_id),
                format(min(inv_panel$week_start)), format(max(inv_panel$week_start))))

wos_check <- inv_panel %>%
  group_by(product) %>%
  summarise(median_wos = median(wos), p10 = quantile(wos, 0.1),
            p90 = quantile(wos, 0.9), .groups = "drop")
message("Weeks of supply (inventory units / weekly volume), unit sanity check:")
print(wos_check)
if (any(wos_check$median_wos < 0.05 | wos_check$median_wos > 20)) {
  warning("Median weeks of supply is outside 0.05-20 for at least one product. ",
          "Inventory and POS volume may use different units for that item. ",
          "Log regressions with store-product FEs are unaffected by a constant unit factor, ",
          "but WOS levels and the shrink bound (10.5) are not interpretable for that item.")
}


# ==============================================================================
# 10.3 SOE EFFECTS ON INVENTORY, VOLUME, AND WEEKS OF SUPPLY
# ==============================================================================
# Positive ln WOS during the SOE = inventory rose relative to sales (product
# pushed into stores faster than it sold). Negative = stores sold through
# inventory faster (lean stock, faster turnover, less time for shrink).
# ==============================================================================

INV_FE <- "| store_product + product_woy"

inv_models <- list(
  "(1) ln inventory" = feols(as.formula(paste("ln_inv ~ SoE + postSoE", INV_FE)),
                             data = inv_panel, cluster = ~ sst),
  "(2) ln volume"    = feols(as.formula(paste("ln_vol ~ SoE + postSoE", INV_FE)),
                             data = inv_panel, cluster = ~ sst),
  "(3) ln WOS"       = feols(as.formula(paste("ln_wos ~ SoE + postSoE", INV_FE)),
                             data = inv_panel, cluster = ~ sst)
)

etable(inv_models)

etable(
  inv_models,
  tex     = TRUE,
  file    = "tables_latex/31_tab_inventory_soe.tex",
  title   = "Inventory, volume, and weeks of supply during and after the SOE",
  label   = "tab:inventory_soe",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$"),
  notes   = c(
    "Cabbage, cucumbers, and lettuce; 2019-2021. Inventory is mean daily on-hand units in the week.",
    "WOS (weeks of supply) = inventory / weekly volume, so column (3) = column (1) - column (2).",
    "FEs: store-product and product-by-week-of-year. Standard errors clustered at the state level.",
    "Post-SOE weeks end in December 2021 because no inventory table extends further."
  )
)
message("Saved: tables_latex/31_tab_inventory_soe.tex")

# Weekly path of ln WOS, demeaned within store-product
wos_weekly <- inv_panel %>%
  group_by(store_product) %>%
  mutate(ln_wos_dm = ln_wos - mean(ln_wos)) %>%
  ungroup() %>%
  group_by(product, week_start) %>%
  summarise(ln_wos_dm = mean(ln_wos_dm), .groups = "drop")

soe_rng <- panel_est %>% filter(SoE == 1L) %>%
  summarise(a = min(week_start), b = max(week_start))

g_wos <- ggplot(wos_weekly, aes(x = week_start, y = ln_wos_dm, color = product)) +
  annotate("rect", xmin = soe_rng$a, xmax = soe_rng$b, ymin = -Inf, ymax = Inf, alpha = 0.10) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.6) +
  labs(x = NULL, y = "log weeks of supply (store-product demeaned)",
       title = "Weeks of supply for the focal items",
       subtitle = "Inventory on hand relative to weekly sales. Shaded: pooled SOE window.",
       color = NULL) +
  theme_bw() + theme(legend.position = "top", plot.subtitle = element_text(size = 8))

ggsave("figures/24_fig_weeks_of_supply.png", g_wos, width = 9, height = 5, dpi = 300)
message("Saved: figures/24_fig_weeks_of_supply.png")


# ==============================================================================
# 10.4 DO INVENTORY GLUTS PRECEDE PROMOTIONS?
# ==============================================================================
# Lagged one week so a promotion that sells down inventory in week t does not
# mechanically drive the relationship. Three designs:
#   (1) Store level, store-product + product-by-week-of-year FEs.
#   (2) Store level, adding retailer-product-week FEs: within a chain and week,
#       does a store's own glut predict its own promotion share? Chains set
#       promotions centrally, so a coefficient near zero here is expected.
#   (3) Chain-state level (retailer-state-product-week means): does a glut
#       across a chain's stores in a state precede promotions there? SOE status
#       is constant within a state-week, so it aggregates cleanly.
# ==============================================================================

inv_panel <- inv_panel %>%
  arrange(store_product, week_seq) %>%
  group_by(store_product) %>%
  mutate(ln_wos_lag = if_else(week_seq - dplyr::lag(week_seq) == 1L,
                              dplyr::lag(ln_wos), NA_real_)) %>%
  ungroup()

chain_panel <- inv_panel %>%
  group_by(retailer_id, sst, product, week_seq, product_woy, SoE, postSoE) %>%
  summarise(share_on_sale = mean(share_on_sale, na.rm = TRUE),
            ln_wos        = mean(ln_wos),
            .groups = "drop") %>%
  mutate(chain_state_product = interaction(retailer_id, sst, product, drop = TRUE)) %>%
  arrange(chain_state_product, week_seq) %>%
  group_by(chain_state_product) %>%
  mutate(ln_wos_lag = if_else(week_seq - dplyr::lag(week_seq) == 1L,
                              dplyr::lag(ln_wos), NA_real_)) %>%
  ungroup()

promo_models <- list(
  "(1) Store" = feols(share_on_sale ~ SoE + postSoE + ln_wos_lag + ln_wos_lag:SoE |
                        store_product + product_woy,
                      data = inv_panel, cluster = ~ store_id),
  "(2) Store, within chain-week" = feols(share_on_sale ~ ln_wos_lag + ln_wos_lag:SoE |
                                           store_product + retailer_id^product^week_seq,
                                         data = inv_panel, cluster = ~ store_id),
  "(3) Chain-state" = feols(share_on_sale ~ SoE + postSoE + ln_wos_lag + ln_wos_lag:SoE |
                              chain_state_product + product_woy,
                            data = chain_panel, cluster = ~ chain_state_product)
)

etable(promo_models)

etable(
  promo_models,
  tex     = TRUE,
  file    = "tables_latex/32_tab_inventory_promo.tex",
  title   = "Do inventory gluts precede promotions?",
  label   = "tab:inventory_promo",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$",
              "ln_wos_lag" = "$\\ln WOS_{t-1}$",
              "ln_wos_lag:SoE" = "$\\ln WOS_{t-1} \\times SOE_{st}$"),
  notes   = c(
    "Dependent variable: share of transactions at a promotional price in week $t$.",
    "$\\ln WOS_{t-1}$: log weeks of supply in the previous week (consecutive weeks only).",
    "(1) store level, store-product and product-by-week-of-year FEs, store-clustered SEs.",
    "(2) adds retailer-product-week FEs: identified from stores of the same chain in the same week.",
    "(3) retailer-state-product-week means; retailer-state-product and product-by-week-of-year FEs;",
    "SEs clustered by retailer-state-product.",
    "Cabbage, cucumbers, and lettuce; 2019-2021."
  )
)
message("Saved: tables_latex/32_tab_inventory_promo.tex")


# ==============================================================================
# 10.5 BACK-OF-ENVELOPE SHRINK BOUND
# ==============================================================================
# If shrink is proportional to time on the shelf, the shrink rate in period r is
# s_r = s_0 * WOS_r / WOS_pre, and the effective cost per unit sold is
# w / (1 - s_r). The change in effective cost from pre-SOE to SOE,
#   w_pre / (1 - s_SOE) - w_pre / (1 - s_0),
# is the part of the measured margin decline that could reflect lower shrink
# rather than lower markups. s_0 (pre-SOE shrink rate) is NOT observed: the
# values in SHRINK_BASE_RATES are assumptions, reported as a range.
# WOS_SOE / WOS_pre is taken from the ln WOS regression (Table 31, col. 3).
# Not interpretable for a product whose WOS levels fail the unit check (10.2).
# ==============================================================================

wos_ratio_by_product <- purrr::map_dfr(levels(inv_panel$product), function(p) {
  m <- feols(ln_wos ~ SoE + postSoE | store_product + product_woy,
             data = inv_panel %>% filter(product == p), cluster = ~ sst)
  tibble::tibble(product = p, wos_ratio = exp(unname(coef(m)["SoE"])))
})

margin_chg_by_product <- purrr::map_dfr(levels(inv_panel$product), function(p) {
  d <- panel_est %>% filter(as.character(product) == p)
  m <- feols(margin_nom ~ SoE + postSoE | store_id, data = d, cluster = ~ sst)
  tibble::tibble(product = p,
                 margin_chg = unname(coef(m)["SoE"]),
                 w_pre      = mean(d$w_ist[d$preSoE == 1L], na.rm = TRUE))
})

shrink_tbl <- tidyr::crossing(wos_ratio_by_product, s0 = SHRINK_BASE_RATES) %>%
  left_join(margin_chg_by_product, by = "product") %>%
  mutate(
    s_soe        = pmin(s0 * wos_ratio, 0.9),
    cost_chg     = w_pre / (1 - s_soe) - w_pre / (1 - s0),
    share_margin = if_else(margin_chg < 0, cost_chg / margin_chg, NA_real_)
  )

print(shrink_tbl %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

shrink_out <- shrink_tbl %>%
  transmute(
    Product = product,
    `Assumed pre-SOE shrink` = scales::percent(s0, accuracy = 1),
    `WOS ratio (SOE / pre)`  = formatC(wos_ratio, digits = 3, format = "f"),
    `Implied SOE shrink`     = scales::percent(s_soe, accuracy = 0.1),
    `$\\Delta$ effective cost (\\$/unit)` = formatC(cost_chg, digits = 3, format = "f"),
    `SOE margin change (\\$/unit)`        = formatC(margin_chg, digits = 3, format = "f"),
    `Share of margin change` = if_else(is.na(share_margin), "",
                                       formatC(share_margin, digits = 2, format = "f"))
  )

save_tex(
  kbl(shrink_out, format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Back-of-envelope bound: margin decline attributable to lower shrink",
      label   = "shrink_bound", align = "llrrrrr") %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(general = c(
      "Pre-SOE shrink rates are assumptions, not estimates; shrink is not observed in the data.",
      "Shrink is assumed proportional to weeks of supply: $s_{SOE} = s_0 \\\\times WOS_{SOE}/WOS_{pre}$,",
      "with $WOS_{SOE}/WOS_{pre} = \\\\exp(\\\\hat\\\\beta_{SOE})$ from a product-specific version of Table~\\\\ref{tab:inventory_soe}, column (3).",
      "$\\\\Delta$ effective cost $= w_{pre}/(1 - s_{SOE}) - w_{pre}/(1 - s_0)$, with $w_{pre}$ the mean pre-SOE wholesale cost.",
      "SOE margin change: product-specific version of Table~\\\\ref{tab:margin_reg}, column (2).",
      "Share of margin change: the part of the measured margin decline that lower shrink could explain."),
      general_title = "", escape = FALSE),
  "33_tab_shrink_bound.tex"
)

message("Inventory and supply-side analysis complete.")

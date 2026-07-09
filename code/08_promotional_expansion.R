# ==============================================================================
# 08_promotional_expansion.R
#
# Food Retailer Pricing Behavior Under Anti-Price Gouging Laws: Evidence from
# Wholesale and Retail Scanner Data -- Chenarides, Richards, and Dong
#
# Purpose: Mechanism 3 -- Countercyclical promotional pricing.
#
# --------------------------------------------------------------------------
# SCOPE:
#   The net price decline during the SOE is due to PROMOTIONAL-FREQUENCY:
#   posted shelf prices were flat, the share of transactions on sale rose
#   sharply, and net prices fell. We document this channel and then decompose
#   the coincident rise in quantity sold into extensive vs. intensive margins,
#   following the extensive-margin logic of Butters et al. (2025).
#
#   Data limitations/outside scope:
#     - Control-function estimation of a change in demand elasticity (the SOE
#       elasticity change is not separately identified in a single, simultaneous
#       emergency).
#     - Within-store/within-day price-dispersion test (the data contain none:
#       on any store-day all shoppers pay the same price). This is retained only
#       as a one-line motivating fact for the inter-temporal framing.
#
# TESTS:
#   M3a. Gross-net price gap over time -- promotional discount per unit.
#   M3b. Promotional intensity -- share_on_sale and discount depth.
#   M3c. Gross price stability -- gross (shelf) flat vs net fell.
#   M3d. Extensive vs. intensive decomposition of the rise in quantity sold:
#        did Q rise through more purchase occasions (extensive) or larger
#        quantity per occasion (intensive)?  ln Q = ln(occasions) + ln(Q/occ),
#        so the SOE coefficients satisfy beta_Q = beta_ext + beta_int.
#
# DATA INPUTS:
#   panel_est    -- from 02_build_panel.R (store-product-week)
#   promo_panel  -- from stg.pd_store_upc_week (read via SQL connection)
#
# OUTPUTS (tables_latex/)
#   21_tab_gross_price_stability.tex   (Sec 5.3 body, table 1)
#   22_tab_promo_intensity.tex         (Sec 5.3 body, table 2)
#   23_tab_extensive_intensive.tex     (Sec 5.3 body, table 3)
#   24_tab_gross_net_gap.tex           (appendix)
#   25_tab_category_price_promo.tex    (appendix, category robustness)
#   26_tab_category_decomp.tex         (appendix, category robustness)
#
# OUTPUTS (figures/):
#   19_fig_gross_net_gap.png
#   20_fig_promo_intensity.png
#   21_fig_extensive_intensive.png
#
# DEPENDS ON: panel_est, save_tex(), SAVE_CSV
#   Requires stg.pd_store_upc_week in SQL (run
#   BuildMarkupsNew_PriceDiscrimination.sql first). The extensive/intensive
#   decomposition uses the exact distinct-basket occasion count
#   (weekly_occasions from stg.pd_ext_int_week) when USE_EXACT_OCCASIONS = TRUE,
#   which is the DEFAULT (see below). If stg.pd_ext_int_week is unavailable, set
#   USE_EXACT_OCCASIONS = FALSE to fall back to weekly_transactions_total
#   (item lines, ~ baskets for single-UPC produce).
# ==============================================================================

message("Estimating Mechanism 3: countercyclical promotional pricing ...")

if (!exists("USE_EXACT_OCCASIONS")) USE_EXACT_OCCASIONS <- TRUE

# ==============================================================================
# M3a. GROSS VS NET PRICE GAP OVER TIME
# ==============================================================================
# The gross-net gap equals the volume-weighted discount per unit.
# A wider gap during SOE means retailers ran more frequent promotions.
# The gap is zero when no promotion is running (everyone pays shelf price).
# (Regression table is an APPENDIX table: 24_tab_gross_net_gap.tex.)
# ==============================================================================

gap_data <- panel_est %>%
  filter(p_ist_gross > 0, p_ist_net > 0,
         is.finite(p_ist_gross), is.finite(p_ist_net)) %>%
  mutate(gross_net_gap = p_ist_gross - p_ist_net)

m_gap_base <- feols(
  gross_net_gap ~ SoE + postSoE | product + store_id,
  data    = gap_data,
  cluster = ~ sst
)

etable(list("(1) Pooled" = m_gap_base))

etable(
  list("(1) Pooled" = m_gap_base),
  tex    = TRUE,
  file   = "tables_latex/24_tab_gross_net_gap.tex",
  title  = "Effect of SOE on gross--net price gap (promotional discount per unit)",
  label  = "tab:gross_net_gap",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict   = c("SoE" = "SOE$_{st}$", "postSoE" = "Post-SOE$_{st}$"),
  headers = list("$p^{gross}_{ist} - p^{net}_{ist}$ (\\$/unit)" = 1),
  notes   = c(
    "Dependent variable: gross price minus net price (dollars per unit or lb).",
    "A positive SOE coefficient means more transactions occurred at promotional prices during the emergency.",
    "When no promotion is running, gross equals net and the gap is zero.",
    "FEs: product and store. Standard errors clustered at the state level."
  )
)
message("Saved: tables_latex/24_tab_gross_net_gap.tex")

gap_state_summary <- gap_data %>%
  group_by(sst, soe_period = case_when(
    SoE == 1     ~ "During SOE",
    postSoE == 1 ~ "Post-SOE",
    TRUE         ~ "Pre-SOE"
  )) %>%
  summarise(mean_gap = round(mean(gross_net_gap, na.rm = TRUE), 3),
            n = n(), .groups = "drop") %>%
  pivot_wider(names_from = soe_period, values_from = c(mean_gap, n),
              names_glue = "{soe_period}_{.value}")
print(gap_state_summary)

gap_weekly <- gap_data %>%
  group_by(week_start) %>%
  summarise(mean_gap = mean(gross_net_gap, na.rm = TRUE), .groups = "drop")

soe_w <- panel_est %>%
  filter(SoE == 1) %>%
  summarise(soe_start = min(week_start), soe_end = max(week_start))

g_gap <- ggplot(gap_weekly, aes(x = week_start, y = mean_gap)) +
  annotate("rect", xmin = soe_w$soe_start, xmax = soe_w$soe_end,
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  labs(title = "Gross-to-net price gap over time (pooled across products)",
       subtitle = "Gap = posted shelf price minus net transaction price. Shaded = SOE window. Gap > 0 means promotions were running.",
       x = "Week", y = "Mean gap: gross minus net (nominal $/unit)") +
  theme_bw() + theme(plot.subtitle = element_text(size = 8))

ggsave("figures/19_fig_gross_net_gap.png", g_gap, width = 10, height = 5, dpi = 300)
message("Saved: figures/19_fig_gross_net_gap.png")

# ==============================================================================
# Load promotional / transaction panel (stg.pd_store_upc_week)
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
  
  # Optional: join exact distinct-basket occasion count (stg.pd_ext_int_week)
  if (USE_EXACT_OCCASIONS) {
    ext <- dplyr::tbl(con_promo, dbplyr::in_schema("stg", "pd_ext_int_week")) %>%
      collect() %>%
      mutate(store_id = as.factor(store_id)) %>%
      select(store_id, upc, week_seq, weekly_occasions)
    df <- df %>% left_join(ext, by = c("store_id", "upc", "week_seq"))
  }
  
  DBI::dbDisconnect(con_promo)
  message("Loaded promo_panel: ", nrow(df), " rows.")
  df
}, error = function(e) {
  message("Could not load stg.pd_store_upc_week: ", conditionMessage(e))
  NULL
})

# ==============================================================================
# M3b. PROMOTIONAL INTENSITY REGRESSIONS
# ==============================================================================
# Outcome 1: share_on_sale -- fraction of transactions at the promotional price.
# Outcome 2: avg_discount_depth -- regular minus sale price, conditional on a sale.
# For share_on_sale and gross-price levels, the SOE effect is a common,
# simultaneous shift across states; week FEs absorb it (column 2), so the
# no-week-FE column is the relevant specification (parallel to Section 4).
# ==============================================================================

if (!is.null(promo_panel)) {
  
  m_share_base    <- feols(share_on_sale ~ SoE + postSoE | product + store_id,
                           data = promo_panel, cluster = ~ sst)
  m_share_week_fe <- feols(share_on_sale ~ SoE + postSoE | product + store_id + week_fe,
                           data = promo_panel, cluster = ~ sst)
  m_depth_base    <- feols(avg_discount_depth ~ SoE + postSoE | product + store_id,
                           data = promo_panel %>% filter(!is.na(avg_discount_depth)),
                           cluster = ~ sst)
  
  etable(
    list("(1) Share, no week FE" = m_share_base,
         "(2) Discount depth"    = m_depth_base),
    tex    = TRUE,
    file   = "tables_latex/22_tab_promo_intensity.tex",
    title  = "Effect of SOE on promotional intensity: share on sale and discount depth",
    label  = "tab:promo_intensity",
    digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
    dict   = c("SoE" = "SOE$_{st}$", "postSoE" = "Post-SOE$_{st}$"),
    headers = list("Share on sale" = 1, "Discount depth (\\$/unit)" = 1),
    notes = c(
      "Columns (1): dependent variable is share of transactions at promotional price.",
      "Column (2): dependent variable is regular price minus sale price, conditional on a promotion running.",
      "The share-on-sale SOE effect is a common, simultaneous shift across states;",
      "week fixed effects absorb it (column 2), so column 1 is the relevant specification.",
      "FEs: product and store. Column (2) adds week FEs. Standard errors clustered at the state level."
    )
  )
  message("Saved: tables_latex/22_tab_promo_intensity.tex")
  
  # -- Promotional intensity figure --
  share_weekly <- promo_panel %>%
    group_by(week_start) %>%
    summarise(mean_share = mean(share_on_sale, na.rm = TRUE), .groups = "drop") %>%
    mutate(week_start = as.Date(week_start))
  depth_weekly <- promo_panel %>%
    filter(!is.na(avg_discount_depth)) %>%
    group_by(week_start) %>%
    summarise(mean_depth = mean(avg_discount_depth, na.rm = TRUE), .groups = "drop") %>%
    mutate(week_start = as.Date(week_start))
  
  scale_fac <- max(depth_weekly$mean_depth, na.rm = TRUE) /
    max(share_weekly$mean_share, na.rm = TRUE)
  
  g_promo <- ggplot(share_weekly, aes(x = week_start)) +
    annotate("rect", xmin = soe_w$soe_start, xmax = soe_w$soe_end,
             ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey50") +
    geom_line(aes(y = mean_share, linetype = "Share on sale"), linewidth = 0.7) +
    geom_line(data = depth_weekly,
              aes(y = mean_depth / scale_fac, linetype = "Discount depth (scaled)"),
              linewidth = 0.7, color = "firebrick") +
    scale_y_continuous(name = "Share of transactions on sale",
                       labels = scales::label_percent(),
                       sec.axis = sec_axis(~ . * scale_fac, name = "Mean discount depth ($/unit)")) +
    scale_linetype_manual(values = c("Share on sale" = "solid",
                                     "Discount depth (scaled)" = "dashed")) +
    labs(title = "Promotional intensity over time",
         subtitle = "Shaded = SOE window. Share on sale (left axis) and discount depth (right axis).",
         x = NULL, linetype = NULL) +
    theme_bw() + theme(legend.position = "top", plot.subtitle = element_text(size = 8))
  
  ggsave("figures/20_fig_promo_intensity.png", g_promo, width = 10, height = 5, dpi = 300)
  message("Saved: figures/20_fig_promo_intensity.png")
}

# ==============================================================================
# M3c. GROSS PRICE STABILITY DURING SOE
# ==============================================================================
# Gross (shelf) price flat + net price fell => decline is promotional, not a
# shelf-price cut. Run on panel_est, which carries both p_ist_gross and
# p_ist (= p_ist_net), so column 2 reproduces the baseline net-price estimate
# exactly (same trimmed estimation panel as Table price_reg).
# ==============================================================================

gp <- panel_est %>% filter(p_ist_gross > 0, is.finite(p_ist_gross))

m_gross_base    <- feols(p_ist_gross ~ SoE + postSoE | product + store_id,           data = gp,        cluster = ~ sst)
m_gross_week_fe <- feols(p_ist_gross ~ SoE + postSoE | product + store_id + week_fe, data = gp,        cluster = ~ sst)
m_net_base      <- feols(p_ist       ~ SoE + postSoE | product + store_id,           data = panel_est, cluster = ~ sst)  # p_ist = p_ist_net

etable(
  list("(1) Gross, no week FE" = m_gross_base,
       "(2) Net price"         = m_net_base),
  tex    = TRUE,
  file   = "tables_latex/21_tab_gross_price_stability.tex",
  title  = "Gross price stability during SOE: posted shelf price vs.\\ net transaction price",
  label  = "tab:gross_price_stability",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict   = c("SoE" = "SOE$_{st}$", "postSoE" = "Post-SOE$_{st}$"),
  headers = list("Gross price $p^{gross}$ (\\$/unit)" = 1, "Net price $p^{net}$ (\\$/unit)" = 1),
  notes = c(
    "Column (1): dependent variable is the volume-weighted gross (shelf) price.",
    "Column (2): dependent variable is the volume-weighted net (transaction) price.",
    "A near-zero SOE coefficient on the gross price alongside a large negative coefficient",
    "on the net price indicates the net price decline was driven by promotional expansion,",
    "not shelf-price reductions. On any store-day all shoppers pay the same price, so the",
    "expansion is inter-temporal (which weeks are on promotion), not within-day discrimination.",
    "Standard errors clustered at the state level."
  )
)
message("Saved: tables_latex/21_tab_gross_price_stability.tex")


# ==============================================================================
# M3d. EXTENSIVE vs INTENSIVE DECOMPOSITION OF THE RISE IN QUANTITY SOLD
# ==============================================================================
# Following the extensive-margin logic of Butters et al. (2025): did the SOE
# rise in quantity sold arrive through MORE purchase occasions buying produce
# (extensive) or LARGER quantity per occasion (intensive)?
#
# A purchase occasion is a distinct basket (store x date x register x
# transaction). By default (USE_EXACT_OCCASIONS = TRUE) we use the exact
# distinct-basket count (weekly_occasions from stg.pd_ext_int_week). Set
# USE_EXACT_OCCASIONS = FALSE to fall back to weekly_transactions_total
# (item lines ~ baskets for single-UPC produce) when that table is unavailable.
#
# Identity: ln Q = ln(occasions) + ln(volume per occasion), so the SOE
# coefficients satisfy  beta_Q = beta_extensive + beta_intensive.
# All three outcomes come from promo_panel so the identity holds exactly.
# ==============================================================================

if (!is.null(promo_panel)) {
  
  decomp_panel <- promo_panel %>%
    mutate(
      n_occasions = if (USE_EXACT_OCCASIONS && "weekly_occasions" %in% names(.))
        weekly_occasions else weekly_transactions_total
    ) %>%
    filter(weekly_volume > 0, n_occasions > 0,
           is.finite(weekly_volume), is.finite(n_occasions)) %>%
    mutate(
      lnQ_pd    = log(weekly_volume),
      ln_occ    = log(n_occasions),
      ln_volper = log(weekly_volume / n_occasions)
    )
  
  m_total     <- feols(lnQ_pd    ~ SoE + postSoE | product + store_id,
                       data = decomp_panel, cluster = ~ sst)
  m_extensive <- feols(ln_occ    ~ SoE + postSoE | product + store_id,
                       data = decomp_panel, cluster = ~ sst)
  m_intensive <- feols(ln_volper ~ SoE + postSoE | product + store_id,
                       data = decomp_panel, cluster = ~ sst)
  
  etable(list("(1) Total ln Q"            = m_total,
              "(2) Extensive ln N"        = m_extensive,
              "(3) Intensive ln (Q/N)"    = m_intensive))
  
  # Identity check: beta_Q should equal beta_ext + beta_int
  message(sprintf(
    "Decomposition check (SOE):  total %.3f  =  ext %.3f  +  int %.3f  (%.3f)",
    coef(m_total)["SoE"], coef(m_extensive)["SoE"], coef(m_intensive)["SoE"],
    coef(m_extensive)["SoE"] + coef(m_intensive)["SoE"]))
  message(sprintf(
    "Decomposition check (post): total %.3f  =  ext %.3f  +  int %.3f  (%.3f)",
    coef(m_total)["postSoE"], coef(m_extensive)["postSoE"], coef(m_intensive)["postSoE"],
    coef(m_extensive)["postSoE"] + coef(m_intensive)["postSoE"]))
  
  etable(
    list("(1) Total: $\\ln Q$"       = m_total,
         "(2) Extensive: $\\ln N$"   = m_extensive,
         "(3) Intensive: $\\ln(Q/N)$" = m_intensive),
    tex    = TRUE,
    file   = "tables_latex/23_tab_extensive_intensive.tex",
    title  = "Extensive vs.\\ intensive decomposition of the SOE-period rise in quantity sold",
    label  = "tab:extensive_intensive",
    digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
    dict   = c("SoE" = "SOE$_{st}$", "postSoE" = "Post-SOE$_{st}$"),
    notes  = c(
      "A purchase occasion $N$ is a distinct basket (store $\\times$ date $\\times$ register $\\times$ transaction).",
      "Column (1): log weekly volume $\\ln Q$. Column (2): log purchase occasions $\\ln N$ (extensive margin).",
      "Column (3): log volume per occasion $\\ln(Q/N)$ (intensive margin).",
      "By the identity $\\ln Q = \\ln N + \\ln(Q/N)$, the SOE coefficients satisfy $\\beta_Q = \\beta_N + \\beta_{Q/N}$.",
      "FEs: product and store. Standard errors clustered at the state level."
    )
  )
  message("Saved: tables_latex/23_tab_extensive_intensive.tex")
  
  # Figure: SOE coefficient decomposed into extensive + intensive
  decomp_coef <- tibble::tibble(
    component = factor(c("Total (ln Q)", "Extensive (ln N)", "Intensive (ln Q/N)"),
                       levels = c("Total (ln Q)", "Extensive (ln N)", "Intensive (ln Q/N)")),
    estimate  = c(coef(m_total)["SoE"], coef(m_extensive)["SoE"], coef(m_intensive)["SoE"]),
    se        = c(se(m_total)["SoE"],   se(m_extensive)["SoE"],   se(m_intensive)["SoE"])
  ) %>%
    mutate(conf.low = estimate - 1.96 * se, conf.high = estimate + 1.96 * se)
  
  g_decomp <- ggplot(decomp_coef, aes(x = component, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_col(width = 0.6, fill = "grey70", color = "grey30") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_text(aes(label = round(estimate, 3)), vjust = -0.8, size = 3.5) +
    labs(title = "SOE-period rise in quantity sold: extensive vs. intensive margin",
         subtitle = "Log-quantity SOE coefficient decomposed. Extensive = more purchase occasions; intensive = more per occasion.",
         x = NULL, y = "SOE coefficient (log points)") +
    theme_bw() + theme(plot.subtitle = element_text(size = 8))
  
  ggsave("figures/21_fig_extensive_intensive.png", g_decomp, width = 8, height = 5, dpi = 300)
  message("Saved: figures/21_fig_extensive_intensive.png")
  
  if (SAVE_CSV) {
    write.csv(decomp_coef, "tables_csv/23_tab_extensive_intensive.csv", row.names = FALSE)
  }
}

message("Mechanism 3 (countercyclical promotional pricing) complete.")

# ==============================================================================
# CATEGORY-LEVEL ROBUSTNESS (substitution across varieties)
# Part of 08_promotional_expansion.R (runs after M3d).
#
# Rebuilds the Mechanism 3 outcomes at the store-CATEGORY-week level, aggregating
# ALL UPCs within each focal category (not just the five selected items), to test
# whether the promotional-expansion and demand results reflect substitution
# across varieties within a category.
#
# Requires stg.store_category_week (built by the category rollup SQL). SOE timing
# is joined from panel_est so it is identical to the main analysis.
#
# --------------------------------------------------------------------------
# ***  WHERE TO FILL IN [X] IN THE MANUSCRIPT  ***
#   The Section 5.3 footnote reads: "...aggregating over all [X] varieties the
#   retailers sell in these five categories." [X] = the total number of DISTINCT
#   UPCs across all five focal categories. The rolled-up store_category_week
#   table only stores n_upcs PER cell (per store-week), which is NOT the
#   sample-wide distinct count, so the breadth block below queries
#   stg.pos_weekly_presence directly and PRINTS the value to the console:
#       ">>> [X] = total distinct UPCs across the five categories = NNN <<<"
#   Use that NNN for [X]. (Equivalently, run the "Total distinct UPCs per
#   category" query at the bottom of sql_category_rollup.sql and sum the five
#   rows.)
# --------------------------------------------------------------------------
#
# OUTPUTS (tables_latex/):
#   25_tab_category_price_promo.tex   -- gross price, net price, share on sale
#   26_tab_category_decomp.tex        -- extensive vs intensive decomposition
# ==============================================================================

message("Category-level robustness (substitution check) ...")

cat_panel <- tryCatch({
  con_cat <- open_decadata_connection()   # server/database set in code/config.R
  df <- dplyr::tbl(con_cat, dbplyr::in_schema("stg", "store_category_week")) %>%
    collect() %>%
    mutate(
      store_id = as.factor(store_id),
      category = factor(stringr::str_to_title(category)),
      sst      = as.factor(sst)
    )
  DBI::dbDisconnect(con_cat)
  message("Loaded store_category_week: ", nrow(df), " rows.")
  df
}, error = function(e) {
  message("Could not load stg.store_category_week: ", conditionMessage(e))
  message("Run the category rollup SQL first; skipping category robustness.")
  NULL
})

if (!is.null(cat_panel)) {
  
  # --------------------------------------------------------------------------
  # BREADTH: total distinct UPCs across the five focal categories -> fills [X].
  # Same source and filters as the rollup SQL's "Total distinct UPCs per
  # category" query. Printed to the console; use the total for [X].
  # --------------------------------------------------------------------------
  cat_breadth <- tryCatch({
    con_b <- open_decadata_connection()
    # Select down each table before the join so retailer_id comes from only one
    # side (pos_weekly_presence also carries retailer_id, which otherwise clashes
    # and gets renamed retailer_id.x / retailer_id.y after the join).
    pres  <- dplyr::tbl(con_b, dbplyr::in_schema("stg", "pos_weekly_presence")) %>%
      select(store_id, upc, category)
    sdim  <- dplyr::tbl(con_b, dbplyr::in_schema("stg", "store_dim")) %>%
      select(store_id, retailer_id)
    by_cat <- pres %>%
      inner_join(sdim, by = "store_id") %>%
      filter(retailer_id %in% c(2, 3, 5),
             toupper(category) %in% c("BANANAS", "CABBAGE", "CUCUMBER",
                                      "LETTUCE", "TOMATOES")) %>%
      group_by(category = toupper(category)) %>%
      summarise(n_upcs_total = n_distinct(upc), .groups = "drop") %>%
      collect()
    DBI::dbDisconnect(con_b)
    by_cat
  }, error = function(e) {
    message("Breadth query failed (fill [X] from the SQL query instead): ",
            conditionMessage(e))
    NULL
  })
  
  if (!is.null(cat_breadth)) {
    message("Distinct UPCs per focal category (breadth actually captured):")
    print(cat_breadth)
    message(sprintf(
      ">>> [X] = total distinct UPCs across the five categories = %d <<<",
      sum(cat_breadth$n_upcs_total)))
    message("    Fill this value in for [X] in the Section 5.3 footnote.")
  }
  
  # SOE indicators from the main analysis (state-week level) -> identical timing
  soe_xwalk <- panel_est %>%
    distinct(sst, week_seq, SoE, postSoE, preSoE) %>%
    mutate(sst = as.factor(sst))
  
  cat_panel <- cat_panel %>%
    left_join(soe_xwalk, by = c("sst", "week_seq")) %>%
    filter(!is.na(SoE), cat_volume > 0, cat_transactions > 0,
           p_net_cat > 0, p_gross_cat > 0) %>%
    mutate(
      gross_net_gap_cat = p_gross_cat - p_net_cat,
      ln_Q   = log(cat_volume),
      ln_N   = log(cat_transactions),
      ln_QN  = log(cat_volume / cat_transactions)
    )
  
  message(sprintf("Category panel: %d store-category-weeks across %d categories.",
                  nrow(cat_panel), n_distinct(cat_panel$category)))
  message("Varieties aggregated per category (mean, max UPCs per store-cat-week):")
  cat_panel %>%
    group_by(category) %>%
    summarise(mean_upcs = round(mean(n_upcs), 1),
              max_upcs  = max(n_upcs),
              n_cells   = n(), .groups = "drop") %>%
    print()
  
  # ----------------------------------------------------------------------------
  # (A) Price & promotion at category grain  (mirrors Tables 21 and 22)
  #     Net fell + share up  ==>  promotional expansion is category-wide, not
  #     variety-switching. share_on_sale is a transaction-count fraction and is
  #     free of price-index composition bias; the volume-weighted category gross
  #     index can move with the variety mix and is not a clean shelf-price gauge.
  # ----------------------------------------------------------------------------
  cm_gross <- feols(p_gross_cat        ~ SoE + postSoE | category + store_id,
                    data = cat_panel, cluster = ~ sst)
  cm_net   <- feols(p_net_cat          ~ SoE + postSoE | category + store_id,
                    data = cat_panel, cluster = ~ sst)
  cm_share <- feols(share_on_sale_cat  ~ SoE + postSoE | category + store_id,
                    data = cat_panel, cluster = ~ sst)
  
  etable(list("(1) Gross price" = cm_gross, "(2) Net price" = cm_net,
              "(3) Share on sale" = cm_share))
  
  etable(
    list("(1) Gross price"    = cm_gross,
         "(2) Net price"      = cm_net,
         "(3) Share on sale"  = cm_share),
    tex    = TRUE,
    file   = "tables_latex/25_tab_category_price_promo.tex",
    title  = "Category-level robustness: gross price, net price, and promotional share",
    label  = "tab:category_price_promo",
    digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
    dict   = c("SoE" = "SOE$_{st}$", "postSoE" = "Post-SOE$_{st}$"),
    notes  = c(
      "Outcomes aggregated over ALL UPCs in each focal category (store-category-week grain).",
      "Column (1): volume-weighted gross (shelf) price. Column (2): volume-weighted net price.",
      "Column (3): share of transactions at a promotional price (a transaction-count fraction,",
      "free of the price-index composition bias that affects volume-weighted category prices).",
      "FEs: category and store. Standard errors clustered at the state level."
    )
  )
  message("Saved: tables_latex/25_tab_category_price_promo.tex")
  
  # ----------------------------------------------------------------------------
  # (B) Extensive/intensive decomposition at category grain (mirrors Table 23)
  #     Within-category variety-switching does not change the category-level
  #     occasion count, so if the category extensive margin also rises the rise
  #     in quantity sold is category breadth, not reallocation toward the focal
  #     items.
  # ----------------------------------------------------------------------------
  cm_total <- feols(ln_Q  ~ SoE + postSoE | category + store_id, data = cat_panel, cluster = ~ sst)
  cm_ext   <- feols(ln_N  ~ SoE + postSoE | category + store_id, data = cat_panel, cluster = ~ sst)
  cm_int   <- feols(ln_QN ~ SoE + postSoE | category + store_id, data = cat_panel, cluster = ~ sst)
  
  message(sprintf(
    "Category decomposition check (SOE): total %.3f = ext %.3f + int %.3f (%.3f)",
    coef(cm_total)["SoE"], coef(cm_ext)["SoE"], coef(cm_int)["SoE"],
    coef(cm_ext)["SoE"] + coef(cm_int)["SoE"]))
  
  etable(list("(1) Total ln Q" = cm_total, "(2) Extensive ln N" = cm_ext,
              "(3) Intensive ln(Q/N)" = cm_int))
  
  etable(
    list("(1) Total: $\\ln Q$"        = cm_total,
         "(2) Extensive: $\\ln N$"    = cm_ext,
         "(3) Intensive: $\\ln(Q/N)$" = cm_int),
    tex    = TRUE,
    file   = "tables_latex/26_tab_category_decomp.tex",
    title  = "Category-level robustness: extensive vs.\\ intensive decomposition of the rise in quantity sold",
    label  = "tab:category_decomp",
    digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
    dict   = c("SoE" = "SOE$_{st}$", "postSoE" = "Post-SOE$_{st}$"),
    notes  = c(
      "Aggregated over ALL UPCs in each focal category (store-category-week grain).",
      "Column (1): log category volume. Column (2): log purchase occasions (extensive margin).",
      "Column (3): log volume per occasion (intensive margin). $\\beta_Q = \\beta_N + \\beta_{Q/N}$.",
      "The occasion count sums per-variety transaction counts, so a basket buying two",
      "varieties of a category counts twice; this modestly inflates the extensive margin.",
      "FEs: category and store. Standard errors clustered at the state level."
    )
  )
  message("Saved: tables_latex/26_tab_category_decomp.tex")
}

message("Category robustness complete.")

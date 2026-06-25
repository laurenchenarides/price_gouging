# ==============================================================================
# apg_analysis.R  [DEPRECATED -- use run_all.R]
#
# This script has been split into numbered scripts for clarity.
# To reproduce all results, run:  source("code/run_all.R")
#
# Anti-Price Gouging Laws and Retailer Pricing Behavior During COVID-19
# Chenarides, Richards, and Dong
#
# Full analysis script. Run top to bottom to reproduce all tables and figures.
#
# Section map:
#   0.  Setup and paths
#   1.  Data import (SQL pull -- run once, then load from saved RDS)
#   2.  CPI deflation and variable construction
#   3.  SOE timing, duration, and event-time indices
#   4.  Weekly first differences and outlier trimming
#   5.  Section II tables: DecaData summary and product coverage
#   6.  Section III.A tables: period means (nominal main, real supplementary)
#   7.  Section III.B: flagged-weeks table
#   8.  Section III.C-E: residualized trend plots
#   9.  Section IV.A: price level regressions
#   10. Section IV.B: margin level regressions
#   11. Section IV.C: pass-through regressions (workplan specs)
#   11a. Optional extension: pass-through duration (Dur_wk)
#   12. Section IV.D: uniform pricing
#
# Folder structure (relative to project root):
#   code\           this script lives here
#   cpi\            CPI Excel file
#   figures\        all PNG outputs (time-series, residual plots, coef plots)
#   tables_csv\     intermediate CSV tables (summary stats)
#   tables_latex\   formatted LaTeX tables for the paper
#
# Subscript conventions:
#   i = store
#   j = product (sometimes implicit in single-product contexts)
#   t = week
#   g = state (implicit in store i since all stores in a state share SOE timing)
#
# Variable naming:
#   p_ist       retail price, product i, store s, week t
#   w_ist       wholesale cost, product i, store s, week t
#   margin_nom  nominal dollar margin = p_ist - w_ist
#   margin_real real dollar margin = p_real - w_real
#   SoE         state-of-emergency indicator (1 = APG enforcement active in state g, week t)
#   postSoE     indicator = 1 for weeks after SOE ended in state g
#   preSoE      indicator = 1 for weeks before SOE started in state g
#   Dur_st      weeks since SOE activation in state g (0 outside SOE)
#
# Retailer 4 is excluded from all analyses and summary tables.
# It closed partway through the sample period and is not representative.
# ==============================================================================


# ==============================================================================
# 0. SETUP AND PATHS
# ==============================================================================

rm(list = ls())

# Set the working directory to the project root if not using pg_project.Rproj.
# Opening pg_project.Rproj in RStudio sets this automatically.
# setwd("/path/to/price_gouging")

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  DBI, odbc, dbplyr,
  dplyr, tidyr, lubridate, stringr, purrr,
  ggplot2, scales,
  fixest, broom,
  knitr, kableExtra,
  readxl, rlang, ggpattern
)

options(dplyr.summarise.inform = FALSE)

# Create output directories if they do not exist
# for (d in c("figures", "tables_csv", "tables_latex")) {
#   dir.create(d, showWarnings = FALSE, recursive = TRUE)
# }
# dir.create("data", showWarnings = FALSE, recursive = TRUE)

# Set SAVE_OPTIONAL_PLOTS to TRUE to also produce state-specific and
# product-specific residualized trend plots (workplan III.D and III.E).
# These are useful as appendix or seminar materials.
SAVE_OPTIONAL_PLOTS <- TRUE

# Retailers to include (retailer 4 excluded: closed mid-sample)
RETAILERS_KEEP <- c(2, 3, 5)

# Pass-through duration extension (Section 11a): set to TRUE to run
# the Dur_wk follow-up after the main workplan pass-through specifications.
RUN_DUR_EXTENSION <- TRUE

# ==============================================================================
# 1. DATA IMPORT
# ==============================================================================
# The five product tables are pulled from stg schema in SQL Server (DecaData).
# Each table is already aggregated to store-product-week by the upstream SQL.
# The SQL-generated panel is treated as given for this script.
#
# Panel unit of observation: product i x store s x week t
# ==============================================================================

source("code/00_read_in_data.R")

# ==============================================================================
# 01_price_sensitivity_diagnostic.R
#
# Diagnostic: sensitivity of period means and margins to price definition.
# Compares three price measures:
#   p_ist_net   : revenue-weighted net price (after promotional discounts)
#   p_ist_gross : revenue-weighted gross price (posted shelf price)
#   avg_unit_price: simple daily average of posted shelf price
# ==============================================================================

source("code/01_price_sensitivity_diagnostics.R")

# ==============================================================================
# 1b. DATA CORRECTIONS AND PRIMARY PRICE VARIABLE ASSIGNMENT
# ==============================================================================
# Peppers, week of 2019-05-27: revenue-weighted prices and costs are inflated
# by near-zero volume denominators. See 01_price_sensitivity_diagnostic.R for
# full documentation of the artifact and the basis for these replacements.
# p_ist_gross and p_ist_net are replaced with avg_unit_price (~$1.02).
# w_ist is replaced with the median w_ist for peppers in the two adjacent weeks
# ($0.52/lb), which is consistent with the surrounding five-week window.
#
# Primary price variable: p_ist is set to p_ist_gross throughout.
# p_ist_gross uses posted shelf revenue and is not affected by deal-mix shifts
# during the SOE period. p_ist_net serves as a robustness check and is
# available in panel_upc_week for that purpose.
# ==============================================================================

panel_upc_week <- panel_upc_week %>%
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
    w_ist = if_else(
      product == "peppers" & week_start == as.Date("2019-05-27"),
      0.52,
      w_ist
    ),
    # Set primary price variable used in all main regressions and tables
    p_ist = p_ist_gross
  )

# ==============================================================================
# 2. CPI DEFLATION AND VARIABLE CONSTRUCTION
# ==============================================================================
# CPI source: national monthly CPI for all urban consumers (BLS, 1982-84 = 100).
# Rebased so that January 2018 = 1.00, producing deflator P_t.
# Each observation is assigned the deflator for its calendar month via week_start.
#
# Nominal series (p_ist, w_ist, margin_nom):
#   APG statutes reference nominal retail prices relative to a nominal
#   pre-emergency benchmark, so nominal levels are the correct outcome.
#
# Real series (p_real, w_real, margin_real):
#   Used only in a supplementary descriptive table (Section III.A).
# ==============================================================================

message("Reading and rebasing CPI ...")

cpi_path  <- "cpi/cpi_20152025.xlsx"

cpi_long <- readxl::read_excel(cpi_path, sheet = "Sheet1") %>%
  rename_with(~stringr::str_trim(.x)) %>%
  rename(year = Year) %>%
  pivot_longer(cols = Jan:Dec, names_to = "month_abbr", values_to = "cpi_8284") %>%
  mutate(
    month       = match(month_abbr, month.abb),
    month_start = make_date(year = as.integer(year), month = month, day = 1L)
  ) %>%
  arrange(month_start)

base_date  <- lubridate::ymd("2018-01-01")

base_value <- cpi_long %>%
  filter(month_start == base_date) %>%
  pull(cpi_8284)

if (length(base_value) == 0 || is.na(base_value)) {
  stop("No CPI value found for January 2018. Confirm cpi_20152025.xlsx includes that date.")
}

cpi_deflator <- cpi_long %>%
  mutate(
    P_t         = cpi_8284 / base_value,
    month_start = month_start
  ) %>%
  select(month_start, P_t)

rm(cpi_long)

message("Joining deflator and constructing price, cost, and margin variables ...")

panel_levels <- panel_upc_week %>%
  # Retailer 4 excluded from all analyses
  filter(retailer_id %in% RETAILERS_KEEP) %>%
  mutate(month_start = lubridate::floor_date(week_start, unit = "month")) %>%
  left_join(cpi_deflator, by = "month_start") %>%
  mutate(
    # Nominal series (main regressions)
    margin_nom = p_ist - w_ist,
    
    # Real series (supplementary descriptive table and uniform pricing)
    p_real      = p_ist / P_t,
    w_real      = w_ist / P_t,
    margin_real = p_real - w_real
  )

# CPI coverage check
missing_cpi <- mean(is.na(panel_levels$P_t))
if (missing_cpi > 0) {
  warning(sprintf(
    "%.2f%% of observations have no CPI match. Check CPI file date coverage.",
    100 * missing_cpi
  ))
}

# ==============================================================================
# 3. SOE TIMING, DURATION, AND EVENT-TIME INDICES
# ==============================================================================
# State-level SOE dates are identified from the panel itself.
# T_start and T_end are the first and last week_seq values with SoE == 1
# in each state. All stores in the same state share the same T_start and T_end.
#
# Constructed variables:
#   preSoE   = 1 if week_seq < T_start in state g
#   postSoE  = 1 if week_seq > T_end in state g
#   Dur_st   = weeks since SOE activation (0 outside SOE)
#              = k_start when SoE == 1, else 0
#   k_start  = week_seq - T_start (event time relative to SOE start)
#   k_end    = week_seq - T_end   (event time relative to SOE end)
# ==============================================================================

message("Constructing duration since activation and event-time indices ...")

soe_dates <- panel_levels %>%
  select(sst, week_seq, SoE) %>%
  distinct() %>%
  group_by(sst) %>%
  summarise(
    T_start = min(week_seq[SoE == 1], na.rm = TRUE),
    T_end   = max(week_seq[SoE == 1], na.rm = TRUE),
    .groups = "drop"
  )

panel_levels <- panel_levels %>%
  left_join(soe_dates, by = "sst") %>%
  mutate(
    k_start = if_else(!is.na(T_start), as.integer(week_seq - T_start), NA_integer_),
    k_end   = if_else(!is.na(T_end),   as.integer(week_seq - T_end),   NA_integer_),
    # Dur_st: weeks since SOE activation, 0 outside SOE
    Dur_st  = if_else(SoE == 1L, pmax(k_start, 0L), 0L),
    preSoE  = if_else(!is.na(T_start) & week_seq < T_start, 1L, 0L),
    postSoE = if_else(!is.na(T_end)   & week_seq > T_end,   1L, 0L)
  )

# Confirm SOE dates per state (print for log)
message("SOE start and end weeks by state:")
print(soe_dates)

panel_levels %>%
  select(sst, apg_start_date, apg_end_date) %>%
  distinct() %>%
  group_by(sst) %>%
  summarise(
    start = min(apg_start_date, na.rm = TRUE),
    end   = max(apg_end_date, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 4. WEEKLY FIRST DIFFERENCES AND OUTLIER TRIMMING
# ==============================================================================
# First differences are taken within each store-product cell (sorted by week_seq).
#
# Nominal first differences (dP, dW, dM):
#   Used in the pass-through regression (Section IV.C) and 
#   the uniform pricing log-difference analysis (Section IV.D).
#   dP = p_ist(t) - p_ist(t-1)
#   dW = w_ist(t) - w_ist(t-1)
#   dM = margin_nom(t) - margin_nom(t-1)
#
# Log first differences (dlnp, dlnw):
#   Constructed from real prices. Used for trimming only.
#
# Trimming: top and bottom 1% of dlnp and dlnw (real log changes) within each
#   product are removed. Trimming on real log changes is preferred because the
#   CPI deflator smooths out aggregate price-level drift, making the real series
#   a cleaner basis for identifying extreme within-product weekly movements.
#   The trim is applied to the estimation panel only. 
#   Observations dropped by the real log-change trim are also dropped
#   from the nominal difference series so both panels remain aligned.
# ==============================================================================

message("Constructing weekly first differences ...")

panel_est_raw <- panel_levels %>%
  filter(p_ist > 0, w_ist > 0, p_real > 0, w_real > 0) %>%
  arrange(sst, store_id, product, week_seq) %>%
  group_by(sst, store_id, product) %>%
  mutate(
    # Nominal first differences
    dP = p_ist      - lag(p_ist),
    dW = w_ist      - lag(w_ist),
    dM = margin_nom - lag(margin_nom),
    
    # Real log first differences (used for trimming)
    lnp  = log(p_real),
    lnw  = log(w_real),
    dlnp = lnp - lag(lnp),
    dlnw = lnw - lag(lnw)
  ) %>%
  ungroup()

message("Trimming extreme weekly log changes (within product, top and bottom 1%) ...")

panel_est <- panel_est_raw %>%
  filter(is.finite(dlnp), is.finite(dlnw)) %>%
  group_by(product) %>%
  mutate(
    q_dlnp = ntile(dlnp, 100),
    q_dlnw = ntile(dlnw, 100)
  ) %>%
  filter(q_dlnp > 1, q_dlnp < 100, q_dlnw > 1, q_dlnw < 100) %>%
  select(-q_dlnp, -q_dlnw) %>%
  ungroup()

message(sprintf(
  "Estimation panel: %d observations, %d products, %d stores.",
  nrow(panel_est),
  n_distinct(panel_est$product),
  n_distinct(panel_est$store_id)
))

# Factor variables for fixest FE specifications
# week_fe: factor week for week fixed effects in pass-through regressions
# store_product: store x product interaction for uniform pricing dispersion regs
panel_est <- panel_est %>%
  mutate(
    store_id      = as.factor(store_id),
    retailer_id   = as.factor(retailer_id),
    sst           = as.factor(sst),
    product       = as.factor(product),
    week_fe       = as.factor(week_seq),
    month_fe      = as.factor(format(
      lubridate::floor_date(week_start, unit = "month"), "%Y-%m"
    )),
    store_product = interaction(store_id, product, drop = TRUE)
  )

# Save panels for diagnostic use (not required)
# saveRDS(panel_levels, "data/panel_levels.rds")
# saveRDS(panel_est,    "data/panel_estimation.rds")

# Helper: save a kable object as a .tex file
save_tex <- function(kbl_obj, filename) {
  writeLines(as.character(kbl_obj),
             con = file.path("tables_latex", filename))
  message("Saved: tables_latex/", filename)
}


# ==============================================================================
# 5. SECTION II TABLES: DECADATA SUMMARY AND PRODUCT COVERAGE
# ==============================================================================

# ------------------------------------------------------------------------------
# Figure II.A: Weekly volume (bars) with mean nominal and real retail prices
# (dual axis, SOE shading)
#
# Source: panel_est (trimmed estimation panel), retailer 4 excluded.
# Bars: total weekly volume (left axis).
# Lines: unweighted mean nominal retail price (p_ist) and mean real retail
#   price (p_real) across store-product cells (right axis, rescaled linearly
#   to share the left axis range).
# Shaded region: pooled SOE window (min apg_start_date to max apg_end_date).
#
# Diagnostic percent-change comparisons (4-week pre vs first 4 SoE weeks)
# are printed to console.
# ------------------------------------------------------------------------------

# 4-week windows around SOE start and end
soe_start_week <- panel_est %>%
  filter(SoE == 1) %>%
  summarise(start_week = min(as.integer(week_seq), na.rm = TRUE)) %>%
  pull(start_week)

soe_end_week <- panel_est %>%
  filter(SoE == 1) %>%
  summarise(end_week = max(as.integer(week_seq), na.rm = TRUE)) %>%
  pull(end_week)

# Window definitions
pre_weeks        <- seq.int(soe_start_week - 4L, soe_start_week - 1L)  # 4 weeks before SOE
soe_weeks_first4 <- seq.int(soe_start_week,      soe_start_week + 3L)  # first 4 SOE weeks
soe_weeks_last4  <- seq.int(soe_end_week - 3L,   soe_end_week)         # last 4 SOE weeks

# ------------------------------------------------------------------------------
# Diagnostic 1: volume -- pre vs first 4 SOE weeks
# ------------------------------------------------------------------------------
wk_vol <- panel_est %>%
  group_by(week_seq, week_start) %>%
  summarise(total_vol = sum(upc_week_volume, na.rm = TRUE), .groups = "drop")

pre_vol        <- wk_vol %>% filter(week_seq %in% pre_weeks)        %>% summarise(v = sum(total_vol)) %>% pull(v)
soe_vol_first4 <- wk_vol %>% filter(week_seq %in% soe_weeks_first4) %>% summarise(v = sum(total_vol)) %>% pull(v)

pct_chg_vol_onset <- 100 * (soe_vol_first4 - pre_vol) / pre_vol

message(sprintf(
  "Volume: 4-week pre vs first 4 SOE weeks: %+.2f%%", pct_chg_vol_onset
))

# ------------------------------------------------------------------------------
# Diagnostic 2: nominal price -- pre vs first 4 SOE, and pre vs last 4 SOE
# ------------------------------------------------------------------------------
wk_price_nom <- panel_est %>%
  group_by(week_seq) %>%
  summarise(mean_p_nom = mean(p_ist, na.rm = TRUE), .groups = "drop")

pre_p_nom        <- wk_price_nom %>% filter(week_seq %in% pre_weeks)        %>% summarise(v = mean(mean_p_nom)) %>% pull(v)
soe_p_nom_first4 <- wk_price_nom %>% filter(week_seq %in% soe_weeks_first4) %>% summarise(v = mean(mean_p_nom)) %>% pull(v)
soe_p_nom_last4  <- wk_price_nom %>% filter(week_seq %in% soe_weeks_last4)  %>% summarise(v = mean(mean_p_nom)) %>% pull(v)

pct_chg_nom_onset <- 100 * (soe_p_nom_first4 - pre_p_nom) / pre_p_nom
pct_chg_nom_full  <- 100 * (soe_p_nom_last4  - pre_p_nom) / pre_p_nom

message(sprintf("Nominal price: pre vs first 4 SOE weeks: %+.2f%%", pct_chg_nom_onset))
message(sprintf("Nominal price: pre vs last 4 SOE weeks:  %+.2f%%", pct_chg_nom_full))

# ------------------------------------------------------------------------------
# Diagnostic 3: real price -- pre vs first 4 SOE, and pre vs last 4 SOE
# ------------------------------------------------------------------------------
wk_price_real <- panel_est %>%
  group_by(week_seq) %>%
  summarise(mean_p_real = mean(p_real, na.rm = TRUE), .groups = "drop")

pre_p_real        <- wk_price_real %>% filter(week_seq %in% pre_weeks)        %>% summarise(v = mean(mean_p_real)) %>% pull(v)
soe_p_real_first4 <- wk_price_real %>% filter(week_seq %in% soe_weeks_first4) %>% summarise(v = mean(mean_p_real)) %>% pull(v)
soe_p_real_last4  <- wk_price_real %>% filter(week_seq %in% soe_weeks_last4)  %>% summarise(v = mean(mean_p_real)) %>% pull(v)

pct_chg_real_onset <- 100 * (soe_p_real_first4 - pre_p_real) / pre_p_real
pct_chg_real_full  <- 100 * (soe_p_real_last4  - pre_p_real) / pre_p_real

message(sprintf("Real price: pre vs first 4 SOE weeks: %+.2f%%", pct_chg_real_onset))
message(sprintf("Real price: pre vs last 4 SOE weeks:  %+.2f%%", pct_chg_real_full))

# ------------------------------------------------------------------------------
# Diagnostic 4: wholesale cost -- pre vs first 4 SOE, and pre vs last 4 SOE
# ------------------------------------------------------------------------------
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

message(sprintf("Nominal cost: pre vs first 4 SOE weeks: %+.2f%%", pct_chg_cost_nom_onset))
message(sprintf("Nominal cost: pre vs last 4 SOE weeks:  %+.2f%%", pct_chg_cost_nom_full))
message(sprintf("Real cost:    pre vs first 4 SOE weeks: %+.2f%%", pct_chg_cost_real_onset))
message(sprintf("Real cost:    pre vs last 4 SOE weeks:  %+.2f%%", pct_chg_cost_real_full))

# ------------------------------------------------------------------------------
# SOE shading window
# ------------------------------------------------------------------------------
soe_window <- panel_est %>%
  filter(SoE == 1) %>%
  summarise(
    soe_start = min(apg_start_date, na.rm = TRUE),
    soe_end   = max(apg_end_date,   na.rm = TRUE)
  )

soe_start <- soe_window$soe_start
soe_end   <- soe_window$soe_end

# ------------------------------------------------------------------------------
# Weekly aggregates for plot
# ------------------------------------------------------------------------------
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

caption_txt <- paste0(
  "Products: bananas (4011), cucumbers (4062), lettuce (7143001065), ",
  "tomatoes (4087), peppers (4065). Excludes retailer 4.\n",
  "Shaded region = SOE period (", soe_start, " to ", soe_end, ").\n",
  sprintf(
    "Pre-SOE to first 4 SOE weeks -- volume: %+.1f%%, nominal price: %+.1f%%, real price: %+.1f%%, nominal cost: %+.1f%%.\n",
    pct_chg_vol_onset, pct_chg_nom_onset, pct_chg_real_onset, pct_chg_cost_nom_onset
  ),
  sprintf(
    "Pre-SOE to last 4 SOE weeks -- nominal price: %+.1f%%, real price: %+.1f%%, nominal cost: %+.1f%%.",
    pct_chg_nom_full, pct_chg_real_full, pct_chg_cost_nom_full
  )
)

g_vol_price <- ggplot(wk, aes(x = week_start)) +
  annotate(
    "rect",
    xmin = soe_start, xmax = soe_end,
    ymin = -Inf, ymax = Inf,
    alpha = 0.12
  ) +
  geom_col(aes(y = total_volume), alpha = 0.55, width = 4) +
  geom_line(aes(y = p_retail_scaled, linetype = "Mean Retail Price (nominal)"), linewidth = 0.8) +
  geom_line(aes(y = p_real_scaled,   linetype = "Mean Retail Price (real)"),   linewidth = 0.8) +
  scale_y_continuous(
    name     = "Total weekly volume",
    sec.axis = sec_axis(
      transform = ~ (. - shift_fac) / scale_fac,
      name      = "Mean price"
    )
  ) +
  labs(
    title    = "Weekly volume with mean prices (dual axis)",
    subtitle = caption_txt,
    x        = "Week",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.subtitle   = element_text(size = 8)
  )

g_vol_price

ggsave("figures/01_fig_volume_and_prices_dual_axis.png", g_vol_price,
       width = 11, height = 5.5, dpi = 300)
message("Saved: figures/01_fig_volume_and_prices_dual_axis.png")

# ==============================================================================
# 5b. WHOLESALE COST CHANGES WITHIN THE SOE WINDOW
# ==============================================================================
# This section documents how wholesale costs evolved within the SOE window,
# addressing the interpretive thread in Section IV.B: wholesale costs
# declined during the SOE period, meaning retailers could not justify price
# increases on cost grounds during most of the enforcement window.
#
# soe_weeks_first4, soe_weeks_last4, and wk_cost are already constructed
# in the Section 5 diagnostic block above and are reused here directly.
# pct_chg_cost_nom and pct_chg_cost_real use the pre-SOE baseline (pre_weeks)
# as the reference, consistent with the other diagnostics.
# ==============================================================================

message("Building wholesale cost figure ...")

# Within-SOE quarter definitions
# Quarters are defined relative to Dur_st (weeks since SOE activation).
# Quarter boundaries: [0,12], [13,25], [26,38], [39+]
# Pre-SOE and post-SOE are included as reference rows.
avg_cost_by_period <- panel_est %>%
  mutate(
    soe_quarter = case_when(
      preSoE  == 1L                 ~ "Pre-SOE (baseline)",
      SoE     == 1L & Dur_st <= 12L ~ "SOE Q1 (weeks 1-13)",
      SoE     == 1L & Dur_st <= 25L ~ "SOE Q2 (weeks 14-26)",
      SoE     == 1L & Dur_st <= 38L ~ "SOE Q3 (weeks 27-39)",
      SoE     == 1L & Dur_st >= 39L ~ "SOE Q4 (weeks 40+)",
      postSoE == 1L                 ~ "Post-SOE",
      TRUE                          ~ NA_character_
    ),
    soe_quarter = factor(soe_quarter, levels = c(
      "Pre-SOE (baseline)",
      "SOE Q1 (weeks 1-13)",
      "SOE Q2 (weeks 14-26)",
      "SOE Q3 (weeks 27-39)",
      "SOE Q4 (weeks 40+)",
      "Post-SOE"
    ))
  ) %>%
  filter(!is.na(soe_quarter)) %>%
  group_by(soe_quarter) %>%
  summarise(
    n_storeweeks   = n(),
    avg_cost_nom   = mean(w_ist,   na.rm = TRUE),
    avg_cost_real  = mean(w_real,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 3))) %>%
  rename(
    Period                = soe_quarter,
    `Store-product-weeks` = n_storeweeks,
    `Cost ($)`            = avg_cost_nom,
    `Cost (real $)`       = avg_cost_real
  )

avg_cost_by_period

# Weekly mean wholesale cost (nominal and real) over time
cost_weekly <- panel_est %>%
  group_by(week_start) %>%
  summarise(
    avg_cost_nom  = mean(w_ist,   na.rm = TRUE),
    avg_cost_real = mean(w_real,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(week_start) %>%
  pivot_longer(
    cols      = c(avg_cost_nom, avg_cost_real),
    names_to  = "series",
    values_to = "value"
  ) %>%
  mutate(
    series = recode(series,
                    avg_cost_nom  = "Nominal",
                    avg_cost_real = "Real (Jan 2018 base)")
  )

g_cost_weekly <- ggplot(cost_weekly, aes(x = week_start, y = value,
                                           linetype = series)) +
  annotate(
    "rect",
    xmin  = soe_start, xmax = soe_end,
    ymin  = -Inf, ymax = Inf,
    alpha = 0.12
  ) +
  geom_line(linewidth = 0.7) +
  labs(
    title    = "Mean wholesale cost over time",
    subtitle = paste0(
      "Unweighted mean across store-product-weeks. ",
      "Shaded region = pooled SOE window (", soe_start, " to ", soe_end, ").\n",
      # "Retailer 4 excluded.\n",
      sprintf(
        "Pre-SOE to first 4 SOE weeks: nominal cost %+.1f%%, real cost %+.1f%%.\n",
        pct_chg_cost_nom_onset, pct_chg_cost_real_onset
      ),
      sprintf(
        "Pre-SOE to last 4 SOE weeks: nominal cost %+.1f%%, real cost %+.1f%%.",
        pct_chg_cost_nom_full, pct_chg_cost_real_full
      )
    ),
    x        = "Week",
    y        = "Mean wholesale cost ($)",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.subtitle   = element_text(size = 8)
  )

g_cost_weekly

ggsave("figures/02_fig_cost_weekly.png", g_cost_weekly,
       width = 11, height = 5.5, dpi = 300)
message("Saved: figures/02_fig_cost_weekly.png")

# ------------------------------------------------------------------------------
# Table II.B: DecaData coverage by year, retailer, and state
#
# Product selection criteria (from upstream SQL):
#   - UPC-level coverage screen applied across the full store universe
#   - Store-level pass criteria: >= 80% weekly presence in the start window
#     (+-26 weeks around APG activation), >= 80% presence in the end window,
#     >= 5 pre-APG weeks present, and >= 5 post-APG weeks present
#   - UPC-level criterion: share_stores_pass >= 0.75 (share of stores in the
#     full store universe where the UPC passed all four store-level criteria)
#   - Within each category, UPCs ranked by total net sales then total volume;
#     up to 5 UPCs per category retained
#   - Candidate categories: fresh produce and meat (apples, bananas, cucumbers,
#     lettuce, peppers, tomatoes, beef, chicken, pork, turkey, and others)
#   - Final focal products: bananas, lettuce, peppers,
#     cucumbers, tomatoes (one UPC each)
#
# Table format: rows = year x retailer, columns = states (wide).
# Metric shown: number of store locations per cell.
# A separate column shows total stores across all states.
# ------------------------------------------------------------------------------
message("Building Section II tables ...")

tab_decadata_raw <- panel_est %>%
  mutate(year = as.integer(format(week_start, "%Y"))) %>%
  group_by(year) %>%
  summarise(
    n_banners = n_distinct(retailer_id),
    n_stores  = n_distinct(store_id),
    # n_upcs    = n_distinct(upc),
    n_obs     = n(),
    .groups   = "drop"
  ) %>%
  arrange(year) %>%
  rename(
    Year                   = year,
    Banners                = n_banners,
    `Store locations`      = n_stores,
    # `Product items (UPCs)` = n_upcs,
    `Store-product-weeks`  = n_obs
  )

tab_decadata_raw

write.csv(tab_decadata_raw, "tables_csv/01_tab_decadata_summary.csv", row.names = FALSE)

save_tex(
  kbl(tab_decadata_raw,
      format   = "latex", booktabs = TRUE,
      caption  = "DecaData coverage by year. Five Southeastern states, five fresh produce items. Retailer 4 excluded (closed mid-sample).",
      label    = "tab:decadata_summary",
      align    = "lrrrr",
      format.args = list(big.mark = ",")) %>%
    kable_styling(latex_options = c("hold_position")),
  "01_tab_decadata_summary.tex"
)

# By retailer ID and state
tab_decadata_wide <- panel_est %>%
  mutate(year = as.integer(format(week_start, "%Y"))) %>%
  group_by(year, retailer_id, sst) %>%
  summarise(
    n_stores = n_distinct(store_id),
    .groups  = "drop"
  ) %>%
  pivot_wider(
    names_from  = sst,
    values_from = n_stores,
    values_fill = 0
  ) %>%
  # Add a total column across states
  mutate(Total = rowSums(across(where(is.numeric) & !c(year)), na.rm = TRUE)) %>%
  arrange(year, retailer_id) %>%
  rename(Year = year, Retailer = retailer_id)

tab_decadata_wide

write.csv(tab_decadata_wide, "tables_csv/02_tab_decadata_summary_wide.csv", row.names = FALSE)

# State columns are whatever appears in the panel; sort them alphabetically
state_cols <- sort(setdiff(names(tab_decadata_wide), c("Year", "Retailer", "Total")))
n_states   <- length(state_cols)

save_tex(
  kbl(tab_decadata_wide %>% select(Year, Retailer, all_of(state_cols), Total),
      format      = "latex",
      booktabs    = TRUE,
      caption = paste0(
        "Store locations by year, retailer, and state. ",
        "The sample covers five Southeastern states and five fresh produce categories: ",
        "bananas, cucumbers, lettuce, peppers, and tomatoes. Retailer 4 excluded.",
        "Product selection required a UPC to pass coverage screens in at least 75\\% of stores, ",
        "where a store-level pass required at least 80\\% weekly presence in windows around ",
        "both APG activation and deactivation and at least five pre- and post-APG weeks observed. ",
        "Within each category, UPCs are ranked by total net sales; up to five per category are retained. ",
        "The final panel uses one UPC each for bananas, cumcumbers, lettuce, peppers, and tomatoes."
      ),
      label       = "tab:decadata_summary_wide",
      align       = paste0("ll", strrep("r", n_states + 1)),
      format.args = list(big.mark = ",")
  ) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "top") %>%
    add_header_above(c(" " = 2, "Store locations by state" = n_states, " " = 1)) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")),
  "02_tab_decadata_summary_wide.tex"
)

# ------------------------------------------------------------------------------
# Table II.C: Five-product coverage table
# Coverage = share of all store-product-weeks with positive volume.
# Average price is nominal. Total volume is in the natural unit of each product.
# ------------------------------------------------------------------------------
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

tab_product_raw

write.csv(tab_product_raw, "tables_csv/03_tab_product_coverage.csv", row.names = FALSE)

save_tex(
  kbl(tab_product_raw,
      format      = "latex", booktabs = TRUE,
      caption     = "Coverage and sales for the five fresh produce categories, 2018--2022. Coverage is the share of store-product-weeks with positive sales. Average price is in nominal dollars per unit or pound. Volume units: pounds (bananas, cucumbers, peppers, tomatoes) and 8~oz bags (lettuce).",
      label       = "tab:product_coverage",
      align       = "lrrr",
      format.args = list(big.mark = ",")) %>%
    kable_styling(latex_options = c("hold_position")),
  "03_tab_product_coverage.tex"
)

# ==============================================================================
# 6. SECTION III.A TABLES: PERIOD MEANS
# ==============================================================================
# Main table uses nominal prices. Supplementary table uses real prices.
# Both tables have the same structure: product x period, four outcomes.
# Only observations with positive volume are included in price, cost, and
# margin averages. Volume averages use all observations.
# ==============================================================================

message("Building Section III.A period means tables ...")

make_period_means <- function(df, price_col, cost_col, margin_col) {
  df %>%
    mutate(
      period = case_when(
        preSoE  == 1L ~ "Pre-SOE",
        SoE     == 1L ~ "During SOE",
        postSoE == 1L ~ "Post-SOE",
        TRUE          ~ NA_character_
      ),
      period = factor(period, levels = c("Pre-SOE", "During SOE", "Post-SOE"))
    ) %>%
    filter(!is.na(period)) %>%
    group_by(product, period) %>%
    summarise(
      avg_volume = mean(upc_week_volume, na.rm = TRUE),
      avg_price  = mean(.data[[price_col]][upc_week_volume > 0],  na.rm = TRUE),
      avg_cost   = mean(.data[[cost_col]][upc_week_volume > 0],   na.rm = TRUE),
      avg_margin = mean(.data[[margin_col]][upc_week_volume > 0], na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    pivot_wider(
      names_from  = period,
      values_from = c(avg_volume, avg_price, avg_cost, avg_margin),
      names_glue  = "{period}_{.value}"
    ) %>%
    arrange(product) %>%
    mutate(product = stringr::str_to_title(as.character(product)))
}

# Column order: cluster by outcome, three periods within each outcome
period_col_order <- c(
  "product",
  "Pre-SOE_avg_volume",   "During SOE_avg_volume",   "Post-SOE_avg_volume",
  "Pre-SOE_avg_price",    "During SOE_avg_price",    "Post-SOE_avg_price",
  "Pre-SOE_avg_cost",     "During SOE_avg_cost",     "Post-SOE_avg_cost",
  "Pre-SOE_avg_margin",   "During SOE_avg_margin",   "Post-SOE_avg_margin"
)

make_period_table_kbl <- function(df_wide, caption_str, label_str,
                                  price_label  = "Price (\\$)",
                                  cost_label   = "Cost (\\$)",
                                  margin_label = "Margin (\\$)") {
  
  # Rename columns to clean three-level labels before passing to kbl
  # Names follow the structure: Pre / During / Post within each outcome block
  period_labels <- c("Pre-SOE", "During SOE", "Post-SOE")
  
  df_renamed <- df_wide %>%
    setNames(c(
      "Product",
      paste0("Vol. (", period_labels, ")"),
      paste0("Price (", period_labels, ")"),
      paste0("Cost (", period_labels, ")"),
      paste0("Margin (", period_labels, ")")
    ))
  
  kbl(df_renamed,
      format   = "latex", booktabs = TRUE,
      caption  = caption_str,
      label    = label_str,
      align    = paste0("l", strrep("r", ncol(df_renamed) - 1)),
      escape   = FALSE) %>%
    add_header_above(c(
      " "            = 1,
      "Volume"       = 3,
      price_label    = 3,
      cost_label     = 3,
      margin_label   = 3
    ), escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
}

# Nominal (main)
means_nom <- make_period_means(panel_est, "p_ist", "w_ist", "margin_nom")
means_nom <- means_nom %>%
  select(all_of(intersect(period_col_order, names(means_nom)))) %>%
  rename(Product = product) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

means_nom

write.csv(means_nom, "tables_csv/04_tab_period_means_nominal.csv", row.names = FALSE)

save_tex(
  make_period_table_kbl(
    means_nom,
    caption_str  = "Average weekly volume, nominal retail price, nominal wholesale cost, and nominal dollar margin by product and SOE period. Averages computed across store-product-weeks with positive sales, retailer 4 excluded. Volume units: pounds (bananas, cucumbers, peppers, tomatoes) and 8~oz bags (lettuce).",
    label_str    = "tab:period_means_nominal",
    price_label  = "Price (\\$)",
    cost_label   = "Cost (\\$)",
    margin_label = "Margin (\\$)"
  ),
  "04_tab_period_means_nominal.tex"
)

# Real (supplementary)
means_real <- make_period_means(panel_est, "p_real", "w_real", "margin_real")
means_real <- means_real %>%
  select(all_of(intersect(period_col_order, names(means_real)))) %>%
  rename(Product = product) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

means_real

write.csv(means_real, "tables_csv/05_tab_period_means_real.csv", row.names = FALSE)

save_tex(
  make_period_table_kbl(
    means_real,
    caption_str  = "Average weekly volume, real retail price, real wholesale cost, and real dollar margin by product and SOE period (January 2018 = 1.00 base). Averages computed across store-product-weeks with positive sales, retailer 4 excluded. Volume units: pounds (bananas, cucumbers, peppers, tomatoes) and 8~oz bags (lettuce).",
    label_str    = "tab:period_means_real",
    price_label  = "Price (real \\$)",
    cost_label   = "Cost (real \\$)",
    margin_label = "Margin (real \\$)"
  ),
  "05_tab_period_means_real.tex"
)


# ==============================================================================
# 7. SECTION III.B: FLAGGED-WEEKS TABLE
# ==============================================================================
# A store-product-week is flagged if the nominal retail price during the SOE
# exceeds the four-week pre-SOE average by more than threshold kappa.
#
# Baseline: mean nominal retail price and cost in the 28 days before
# apg_start_date for each store-product cell. Four weeks approximates the
# 30-day benchmark window referenced in several state statutes.
#
# Cost justification: flagged week is cost-justified if the dollar price
# increase from the baseline does not exceed the dollar cost increase
# (dollar-for-dollar criterion, COST_MULT = 1.00).
#
# Change baseline window (e.g., 10 days per Georgia's statute), by altering
# the filter below from <= 28 to <= 10.
# ==============================================================================

message("Building Section III.B flagged-weeks table ...")

THRESHOLDS <- c(0.10, 0.15, 0.20, 0.25, 0.30)
COST_MULT  <- 1.00

# 4-week pre-SOE baseline per store-product
baseline_flag <- panel_est %>%
  filter(!is.na(apg_start_date)) %>%
  mutate(days_to_start = as.integer(apg_start_date - week_start)) %>%
  filter(days_to_start > 0, days_to_start <= 28) %>%
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

# All-thresholds table (main output), pooled across products and retailers
tab_flagged_all_raw <- flag_long %>%
  group_by(thresh_lbl, threshold) %>%
  summarise(
    n_soe             = n(),
    share_flagged     = mean(flagged, na.rm = TRUE),
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

write.csv(tab_flagged_all_raw, "tables_csv/06_tab_flagged_weeks_all.csv", row.names = FALSE)

save_tex(
  kbl(tab_flagged_all_raw,
      format   = "latex", booktabs = TRUE,
      caption  = "Hypothetical flagged-weeks across thresholds. A store-product-week is flagged if the nominal retail price exceeds the four-week pre-SOE average by more than the listed threshold. Cost justification: dollar price increase does not exceed dollar cost increase (COST\\_MULT = 1.00). Pooled across all five products and retailers 2, 3, and 5.",
      label    = "tab:flagged_weeks_all",
      align    = "lrrr",
      format.args = list(big.mark = ",")) %>%
    kable_styling(latex_options = c("hold_position")),
  "06_tab_flagged_weeks_all.tex"
)

# T25 table by product (reference threshold matching Alabama's statute)
tab_flagged_T25_raw <- flag_long %>%
  filter(thresh_lbl == "T25") %>%
  group_by(product) %>%
  summarise(
    n_soe             = n(),
    share_flagged     = mean(flagged, na.rm = TRUE),
    share_just_flagged = if_else(
      sum(flagged, na.rm = TRUE) > 0,
      sum(flagged_just, na.rm = TRUE) / sum(flagged, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Product             = stringr::str_to_title(as.character(product)),
    `SOE store-weeks`   = n_soe,
    `% flagged`         = round(100 * share_flagged, 1),
    `% flagged, cost-just.` = round(100 * share_just_flagged, 1)
  ) %>%
  select(Product, `SOE store-weeks`, `% flagged`, `% flagged, cost-just.`)

write.csv(tab_flagged_T25_raw, "tables_csv/07_tab_flagged_weeks_T25.csv", row.names = FALSE)

save_tex(
  kbl(tab_flagged_T25_raw,
      format   = "latex", booktabs = TRUE,
      caption  = "Hypothetical flagged-weeks at the 25\\% threshold (matches Alabama's statutory limit). A store-product-week is flagged if the nominal retail price exceeds the four-week pre-SOE average by more than 25\\%. Cost justification: dollar price increase does not exceed dollar cost increase. Pooled across retailers 2, 3, and 5.",
      label    = "tab:flagged_weeks_T25",
      align    = "lrrr",
      format.args = list(big.mark = ",")) %>%
    kable_styling(latex_options = c("hold_position")),
  "07_tab_flagged_weeks_T25.tex"
)


# ------------------------------------------------------------------------------
# Figure III.B: Flagged weeks by threshold and retailer (stacked bar)
#
# For each (threshold, retailer) cell, bars show the average share of SOE
# store-weeks flagged, stacked into cost-justified and not-cost-justified
# components. Averages are taken across products within each cell.
#
# x-axis: retailer within threshold cluster.
# Bar height: average % of SOE store-weeks flagged.
# Fill: cost-justified (price increase within dollar cost increase) vs not.
# ------------------------------------------------------------------------------

thresh_lbls_ordered <- paste0("T", as.integer(THRESHOLDS * 100))

# Collapse to threshold x retailer x product, then average across products
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
  pivot_longer(
    cols      = c(pct_just, pct_not),
    names_to  = "component",
    values_to = "pct"
  ) %>%
  mutate(
    component = recode(component,
                       pct_just = "Flagged & cost-justified",
                       pct_not  = "Flagged & not cost-justified"),
    component = factor(component,
                       levels = c("Flagged & not cost-justified",
                                  "Flagged & cost-justified"))
  )

g_flag_cluster <- ggplot(plot_long,
                         aes(x = x_group, y = pct,
                             fill    = component,
                             pattern = component)) +
  geom_col_pattern(
    width                = 0.85,
    pattern_density      = 0.4,
    pattern_spacing      = 0.03,
    pattern_fill         = "black",
    pattern_colour       = "black",
    colour               = "grey30",    # bar border
    linewidth            = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "Flagged & not cost-justified" = "grey85",
      "Flagged & cost-justified"     = "grey40"
    )
  ) +
  scale_pattern_manual(
    values = c(
      "Flagged & not cost-justified" = "none",
      "Flagged & cost-justified"     = "stripe"
    )
  ) +
  labs(
    title    = "APG flag rates during SOE (stacked by cost justification)",
    subtitle = paste0(
      "Bar height = average % of SOE store-weeks flagged across products. ",
      "Textured = cost-justified (dollar price increase does not exceed dollar cost increase). ",
      "Untextured = not cost-justified. Nominal prices."
    ),
    x       = "Threshold : Retailer",
    y       = "% of SOE store-weeks",
    fill    = NULL,
    pattern = NULL
  ) +
  guides(
    fill    = guide_legend(override.aes = list(pattern = c("none", "stripe"))),
    pattern = "none"   # suppress duplicate legend
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x     = element_text(angle = 35, hjust = 1),
    plot.subtitle   = element_text(size = 8)
  )

g_flag_cluster

ggsave("figures/03_fig_flag_cluster_stacked.png", g_flag_cluster,
       width = 12, height = 6, dpi = 300)
message("Saved: figures/03_fig_flag_cluster_stacked.png")


# ==============================================================================
# 8. SECTION III.C-E: RESIDUALIZED TREND PLOTS
# ==============================================================================
# For each outcome y_ist, regress on product and store fixed effects:
#   y_ist = gamma_j + delta_i + e_ist
# Recover residuals. Compute the weekly mean residual across all
# store-product cells. Plot the mean residual over time.
#
# Week FE are excluded so that time variation remains visible in the residuals.
# Shaded region indicates the pooled SOE window (min start to max end).
#
# Pooled plots are the main outputs (Section III.C).
# State-specific (III.D) and product-specific (III.E) are extra.
# ==============================================================================

message("Building Section III.C-E residualized trend plots ...")

panel_resid <- panel_est %>%
  filter(upc_week_volume > 0,
         is.finite(p_ist), is.finite(w_ist), is.finite(margin_nom)) %>%
  mutate(
    store_id  = as.factor(store_id),
    product   = as.factor(product),
    sst       = as.factor(sst)
  )

# Pooled SOE shading window
soe_shade <- panel_resid %>%
  filter(SoE == 1) %>%
  summarise(
    soe_start = min(week_start, na.rm = TRUE),
    soe_end   = max(week_start, na.rm = TRUE)
  )

# Helper: residualize an outcome on product + store FE; return weekly mean residual
resid_weekly <- function(df, outcome_col) {
  fml <- as.formula(paste0(outcome_col, " ~ 1 | product + store_id"))
  m   <- feols(fml, data = df, warn = FALSE, notes = FALSE)
  df %>%
    mutate(.resid = resid(m)) %>%
    group_by(week_start) %>%
    summarise(mean_resid = mean(.resid, na.rm = TRUE), .groups = "drop") %>%
    arrange(week_start)
}

# Helper: same but split by a grouping column (state or product)
resid_weekly_by <- function(df, outcome_col, by_col) {
  groups <- sort(unique(as.character(df[[by_col]])))
  purrr::map_dfr(groups, function(g) {
    df_g <- df %>% filter(as.character(.data[[by_col]]) == g)
    if (nrow(df_g) < 50) return(NULL)
    fml <- as.formula(paste0(outcome_col, " ~ 1 | product + store_id"))
    m   <- feols(fml, data = df_g, warn = FALSE, notes = FALSE)
    df_g %>%
      mutate(.resid = resid(m)) %>%
      group_by(week_start) %>%
      summarise(mean_resid = mean(.resid, na.rm = TRUE), .groups = "drop") %>%
      mutate(group = g)
  })
}

# Helper: produce and save a pooled residual trend figure
plot_resid_pooled <- function(resid_df, outcome_label, filename) {
  g <- ggplot(resid_df, aes(x = week_start, y = mean_resid)) +
    annotate("rect",
             xmin = soe_shade$soe_start, xmax = soe_shade$soe_end,
             ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "grey50") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_line(linewidth = 0.7) +
    labs(
      title    = paste0("Residualized trend: ", outcome_label),
      subtitle = "Residuals from product and store fixed effects. Shaded region = pooled SOE window.",
      x = "Week", y = paste0("Mean residual (", outcome_label, ")")
    ) +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 8))
  ggsave(file.path("figures", filename), g, width = 10, height = 5, dpi = 300)
  message("Saved: figures/", filename)
  invisible(g)
}

# Helper: produce and save a grouped residual trend figure
plot_resid_grouped <- function(resid_df, outcome_label, group_label, filename) {
  g <- ggplot(resid_df, aes(x = week_start, y = mean_resid)) +
    annotate("rect",
             xmin = soe_shade$soe_start, xmax = soe_shade$soe_end,
             ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "grey50") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~ group, scales = "free_y") +
    labs(
      title    = paste0("Residualized trend by ", group_label, ": ", outcome_label),
      subtitle = "Residuals from product and store fixed effects within each group. Shaded = pooled SOE window.",
      x = "Week", y = paste0("Mean residual (", outcome_label, ")")
    ) +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 8), strip.text = element_text(size = 9))
  ggsave(file.path("figures", filename), g, width = 12, height = 8, dpi = 300)
  message("Saved: figures/", filename)
  invisible(g)
}

# Outcomes for residual plots
resid_outcomes <- list(
  list(col = "upc_week_volume", label = "Volume",        file = "04_fig_resid_volume_pooled.png"),
  list(col = "p_ist",           label = "Price (nom.)",  file = "05_fig_resid_price_pooled.png"),
  list(col = "w_ist",           label = "Cost (nom.)",   file = "06_fig_resid_cost_pooled.png"),
  list(col = "margin_nom",      label = "Margin (nom.)", file = "07_fig_resid_margin_pooled.png")
)

# Pooled plots (Section III.C -- main)
for (o in resid_outcomes) {
  rd <- resid_weekly(panel_resid, o$col)
  plot_resid_pooled(rd, o$label, o$file)
}

# Extra: by-state (Section III.D) and by-product (Section III.E)
if (SAVE_OPTIONAL_PLOTS) {
  for (o in resid_outcomes) {
    stem <- gsub("[^a-z]", "", tolower(o$label))
    
    rs <- resid_weekly_by(panel_resid, o$col, "sst")
    plot_resid_grouped(rs, o$label, "state",
                       paste0("fig_resid_", stem, "_by_state.png"))
    
    rp <- resid_weekly_by(panel_resid, o$col, "product")
    plot_resid_grouped(rp, o$label, "product",
                       paste0("fig_resid_", stem, "_by_product.png"))
  }
}


# ==============================================================================
# 9. SECTION IV.A: PRICE LEVEL REGRESSIONS
# ==============================================================================
# Specifications (workplan IV.A):
#
#   Baseline:
#     P_ist = alpha + beta1*SOE_st + beta2*postSOE_st + gamma_i + delta_s
#
#   State heterogeneity:
#     P_ist = alpha + sum_g beta1g*(SOE_st*state_g) + sum_g beta2g*(postSOE_st*state_g)
#             + gamma_i + delta_s
#
# Outcome: p_ist (nominal retail price, dollars per unit or pound).
#
# Fixed effects: product (gamma_i) and store (delta_s).
# Week FEs are excluded. SOE timing is common across states so the SOE
# indicator is collinear with week FEs in level specifications. Identification
# comes from within-store-product variation over time after conditioning on
# product and store means, with cross-state variation in SOE end dates.
#
# Standard errors: clustered at the store level.
# ==============================================================================

message("Estimating Section IV.A price level regressions ...")

reg_data <- panel_est %>%
  filter(is.finite(p_ist), is.finite(margin_nom))

# IV.A.i Baseline price regression
m_price_soe <- feols(
  p_ist ~ SoE | product + store_id,
  data    = reg_data,
  cluster = ~ store_id
)

# IV.A.ii Baseline price regression
m_price_prepost <- feols(
  p_ist ~ SoE + postSoE | product + store_id,
  data    = reg_data,
  cluster = ~ store_id
)

etable(list("(1)" = m_price_soe, "(2)" = m_price_prepost))
  
etable(
  list("(1)" = m_price_soe, "(2)" = m_price_prepost),
  tex      = TRUE,
  file     = "tables_latex/08_tab_price_reg.tex",
  title    = "Price regressions: SOE and post-SOE on nominal prices",
  label    = "tab:price_reg",
  digits   = 3, se.below = TRUE, depvar = FALSE,
  fitstat  = ~ n + r2,
  dict     = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$"),
  notes    = c(
    "Dependent variable: nominal retail price $p_{ist}$ (dollars per unit or pound).",
    "Specification: $p_{ist} = \\alpha + \\beta_1 SOE_{st} + \\beta_2 postSOE_{st} + \\gamma_i + \\delta_s + \\varepsilon_{ist}$.",
    "Fixed effects: product ($\\gamma_j$) and store ($\\delta_i$).",
    "Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/08_tab_price_reg.tex")

# IV.A.iii State heterogeneity price regression
m_price_state_het <- feols(
  p_ist ~ 0 + i(sst, SoE) + i(sst, postSoE) | product + store_id,
  data    = reg_data,
  cluster = ~ store_id
)

etable(
  list("(1)" = m_price_state_het),
  tex      = TRUE,
  file     = "tables_latex/09_tab_price_reg_state_heterog.tex",
  title    = "State heterogeneity in SOE and post-SOE price effects (nominal retail price)",
  label    = "tab:price_reg_state_heterog",
  digits   = 3, se.below = TRUE, depvar = FALSE,
  fitstat  = ~ n + r2,
  notes    = c(
    "Dependent variable: nominal retail price $p_{ist}$.",
    "Each coefficient is a state-specific SOE or post-SOE effect.",
    "Fixed effects: product and store.",
    "Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/09_tab_price_reg_state_heterog.tex")

# Custom wide-format table for state heterogeneity price regression
# Rows = states, Columns = During SOE and Post-SOE coefficients
price_state_wide <- broom::tidy(m_price_state_het, conf.int = TRUE) %>%
  filter(grepl(":SoE$|:postSoE$", term)) %>%
  mutate(
    state  = sub("sst::([A-Z]+):.*", "\\1", term),
    period = if_else(grepl(":SoE$", term), "During SOE", "Post-SOE"),
    # Format: coefficient with stars, SE in parentheses below
    stars  = case_when(
      p.value < 0.01  ~ "***",
      p.value < 0.05  ~ "**",
      p.value < 0.1   ~ "*",
      TRUE            ~ ""
    ),
    coef_str = paste0(formatC(estimate, digits = 3, format = "f"), stars),
    se_str   = paste0("(", formatC(std.error, digits = 4, format = "f"), ")")
  ) %>%
  select(state, period, coef_str, se_str) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(coef_str, se_str),
    names_glue  = "{period}_{.value}"
  ) %>%
  arrange(state) %>%
  # Interleave coef and SE rows manually via a long reshape
  rename(State = state)

# Build interleaved coef/SE rows for LaTeX presentation
# Each state gets two rows: estimate row and SE row
price_state_interleaved <- price_state_wide %>%
  mutate(row_type = "coef") %>%
  bind_rows(
    price_state_wide %>% mutate(row_type = "se")
  ) %>%
  arrange(State, row_type) %>%
  mutate(
    display_state = if_else(row_type == "coef", State, ""),
    `During SOE`  = if_else(row_type == "coef",
                            `During SOE_coef_str`,
                            `During SOE_se_str`),
    `Post-SOE`    = if_else(row_type == "coef",
                            `Post-SOE_coef_str`,
                            `Post-SOE_se_str`)
  ) %>%
  select(display_state, `During SOE`, `Post-SOE`) %>%
  rename(State = display_state)

save_tex(
  kbl(price_state_interleaved,
      format   = "latex",
      booktabs = TRUE,
      caption  = "State heterogeneity in SOE and post-SOE price effects (nominal retail price)",
      label    = "tab:price_reg_state_heterog",
      align    = "lrr",
      escape   = FALSE) %>%
    add_header_above(c(" " = 1, "Nominal retail price $p_{ist}$" = 2),
                     escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = c(
        "Dependent variable: nominal retail price $p_{ist}$.",
        "Fixed effects: product and store.",
        "Standard errors clustered at the store level in parentheses.",
        "Signif. codes: ***: 0.01, **: 0.05, *: 0.1"
      ),
      general_title = "",
      escape = FALSE
    ),
  "09_tab_price_reg_state_heterog.tex"
)


# Coefficient plot for price regressions: baseline
price_coef_df <- bind_rows(
  broom::tidy(m_price_prepost, conf.int = TRUE) %>%
    filter(term %in% c("SoE", "postSoE")) %>%
    mutate(model = "Baseline", term = recode(term, SoE = "During SOE", postSoE = "Post-SOE")),
  broom::tidy(m_price_state_het, conf.int = TRUE) %>%
    filter(grepl(":SoE$|:postSoE$", term)) %>%
    mutate(
      state = sub("sst::([A-Z]+):.*", "\\1", term),
      period = if_else(grepl(":SoE$", term), "During SOE", "Post-SOE"),
      model = "State heterogeneity",
      term  = paste0(state, " (", period, ")")
    )
)

g_price_coef <- ggplot(
  price_coef_df %>% filter(model == "Baseline"),
  aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  geom_text(aes(label = round(estimate, 3)), vjust = -1.0, size = 3.5) +
  labs(x = NULL, y = "Coefficient (nominal price, $)",
       title = "SOE and post-SOE effects on nominal retail price") +
  theme_bw()

g_price_coef

ggsave("figures/08_fig_price_coef_baseline.png", g_price_coef,
       width = 7, height = 5, dpi = 300)
message("Saved: figures/08_fig_price_coef_baseline.png")

# Coefficient plot for price regressions: State heterogeneity
g_price_state <- ggplot(
  price_coef_df %>% filter(model == "State heterogeneity"),
  aes(x = state, y = estimate, ymin = conf.low, ymax = conf.high, color = period)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_text(aes(label = round(estimate, 3)),
            position = position_dodge(width = 0.5), vjust = -1.0, size = 3.2,
            show.legend = FALSE) +
  labs(x = "State", y = "Coefficient (nominal price, $)",
       title = "State-specific SOE and post-SOE effects on nominal retail price",
       color = NULL, shape = NULL) +
  theme_bw()

g_price_state

ggsave("figures/09_fig_price_coef_state_heterog.png", g_price_state,
       width = 9, height = 5, dpi = 300)
message("Saved: figures/09_fig_price_coef_state_heterog.png")



# ==============================================================================
# 10. SECTION IV.B: MARGIN LEVEL REGRESSIONS
# ==============================================================================
# Specifications (workplan IV.B):
#
#   (i)   M_ist = alpha + beta1*SOE_st + gamma_i + delta_s
#   (ii)  M_ist = alpha + beta1*SOE_st + beta2*postSOE_st + gamma_i + delta_s
#   (iii) State heterogeneity version (state-interacted SOE and postSOE)
#
# Outcome: margin_nom = p_ist - w_ist (nominal dollar margin).
#
# Interpretive note: wholesale costs declined during the SOE period in this
# sample. Retailers could have held retail prices fixed and allowed margins to
# rise more sharply. The margin coefficient on SOE captures the net movement
# in the retail-wholesale spread conditional on product and store FE.
# A positive coefficient indicates margins expanded on average during SOE;
# a negative coefficient indicates compression. The pass-through regression
# in Section IV.C provides the corresponding dynamic evidence.
#
# Same FE and clustering as Section IV.A.
# ==============================================================================

message("Estimating Section IV.B margin level regressions ...")

# IV.B.i SOE only
m_margin_soe <- feols(
  margin_nom ~ SoE | product + store_id,
  data    = reg_data,
  cluster = ~ store_id
)

# IV.B.ii SOE and postSOE
m_margin_prepost <- feols(
  margin_nom ~ SoE + postSoE | product + store_id,
  data    = reg_data,
  cluster = ~ store_id
)

etable(list("(1)" = m_margin_soe, "(2)" = m_margin_prepost))

etable(
  list("(1)" = m_margin_soe, "(2)" = m_margin_prepost),
  tex      = TRUE,
  file     = "tables_latex/10_tab_margin_reg.tex",
  title    = "Margin regressions: SOE and post-SOE effects on nominal dollar margin",
  label    = "tab:margin_reg",
  digits   = 3, se.below = TRUE, depvar = FALSE,
  fitstat  = ~ n + r2,
  dict     = c("SoE" = "$SOE_{st}$", "postSoE" = "$postSOE_{st}$"),
  headers  = list("Nominal margin $M_{ist}$" = 2),
  notes    = c(
    "Dependent variable: nominal dollar margin $M_{ist} = p_{ist} - w_{ist}$.",
    "Column (1): SOE only. Column (2): SOE and post-SOE.",
    "Fixed effects: product and store.",
    "Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/10_tab_margin_reg.tex")

# IV.B.iii State heterogeneity margin regression
m_margin_state_het <- feols(
  margin_nom ~ 0 + i(sst, SoE) + i(sst, postSoE) | product + store_id,
  data    = reg_data,
  cluster = ~ store_id
)

etable(
  list("(1)" = m_margin_state_het),
  tex      = TRUE,
  file     = "tables_latex/11_tab_margin_reg_state_heterog.tex",
  title    = "State heterogeneity in SOE and post-SOE margin effects (nominal dollar margin)",
  label    = "tab:margin_reg_state_heterog",
  digits   = 3, se.below = TRUE, depvar = FALSE,
  fitstat  = ~ n + r2,
  notes    = c(
    "Dependent variable: nominal dollar margin $M_{ist}$.",
    "Each coefficient is a state-specific SOE or post-SOE margin effect.",
    "Fixed effects: product and store.",
    "Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/11_tab_margin_reg_state_heterog.tex")

# Custom wide-format table for state heterogeneity margin regression
# Rows = states, Columns = During SOE and Post-SOE coefficients
margin_state_wide <- broom::tidy(m_margin_state_het, conf.int = TRUE) %>%
  filter(grepl(":SoE$|:postSoE$", term)) %>%
  mutate(
    state  = sub("sst::([A-Z]+):.*", "\\1", term),
    period = if_else(grepl(":SoE$", term), "During SOE", "Post-SOE"),
    stars  = case_when(
      p.value < 0.01  ~ "***",
      p.value < 0.05  ~ "**",
      p.value < 0.1   ~ "*",
      TRUE            ~ ""
    ),
    coef_str = paste0(formatC(estimate, digits = 3, format = "f"), stars),
    se_str   = paste0("(", formatC(std.error, digits = 4, format = "f"), ")")
  ) %>%
  select(state, period, coef_str, se_str) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(coef_str, se_str),
    names_glue  = "{period}_{.value}"
  ) %>%
  arrange(state) %>%
  rename(State = state)

margin_state_interleaved <- margin_state_wide %>%
  mutate(row_type = "coef") %>%
  bind_rows(
    margin_state_wide %>% mutate(row_type = "se")
  ) %>%
  arrange(State, row_type) %>%
  mutate(
    display_state = if_else(row_type == "coef", State, ""),
    `During SOE`  = if_else(row_type == "coef",
                            `During SOE_coef_str`,
                            `During SOE_se_str`),
    `Post-SOE`    = if_else(row_type == "coef",
                            `Post-SOE_coef_str`,
                            `Post-SOE_se_str`)
  ) %>%
  select(display_state, `During SOE`, `Post-SOE`) %>%
  rename(State = display_state)

save_tex(
  kbl(margin_state_interleaved,
      format   = "latex",
      booktabs = TRUE,
      caption  = "State heterogeneity in SOE and post-SOE margin effects (nominal dollar margin)",
      label    = "tab:margin_reg_state_heterog",
      align    = "lrr",
      escape   = FALSE) %>%
    add_header_above(c(" " = 1, "Nominal dollar margin $M_{ist}$" = 2),
                     escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = c(
        "Dependent variable: nominal dollar margin $M_{ist} = p_{ist} - w_{ist}$.",
        "Fixed effects: product and store.",
        "Standard errors clustered at the store level in parentheses.",
        "Signif. codes: ***: 0.01, **: 0.05, *: 0.1"
      ),
      general_title = "",
      escape = FALSE
    ),
  "11_tab_margin_reg_state_heterog.tex"
)

# Coefficient plot: SOE and postSOE margin effects
margin_coef_df <- broom::tidy(m_margin_prepost, conf.int = TRUE) %>%
  filter(term %in% c("SoE", "postSoE")) %>%
  mutate(term = recode(term, SoE = "During SOE", postSoE = "Post-SOE"))

g_margin_coef <- ggplot(margin_coef_df,
                        aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  geom_text(aes(label = round(estimate, 3)), vjust = -1.0, size = 3.5) +
  labs(x = NULL, y = "Coefficient (nominal margin, $)",
       title = "SOE and post-SOE effects on nominal dollar margin") +
  theme_bw()

g_margin_coef

ggsave("figures/10_fig_margin_coef_prepost.png", g_margin_coef,
       width = 7, height = 5, dpi = 300)
message("Saved: figures/10_fig_margin_coef_prepost.png")

# Coefficient plot: state heterogeneity in SOE and postSOE margin effects
margin_state_coef_df <- broom::tidy(m_margin_state_het, conf.int = TRUE) %>%
  filter(grepl(":SoE$|:postSoE$", term)) %>%
  mutate(
    state  = sub("sst::([^:]+):.*", "\\1", term),
    period = if_else(grepl(":SoE$", term), "During SOE", "Post-SOE"),
    period = factor(period, levels = c("During SOE", "Post-SOE"))
  )

g_margin_state_coef <- ggplot(margin_state_coef_df,
                              aes(x = state, y = estimate, ymin = conf.low, ymax = conf.high,
                                  color = period, shape = period)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_text(aes(label = round(estimate, 3)),
            position = position_dodge(width = 0.5),
            vjust = -1.0, size = 3.2, show.legend = FALSE) +
  labs(x = "State", y = "Coefficient (nominal margin, $)",
       title = "State-specific SOE and post-SOE effects on nominal dollar margin",
       color = NULL, shape = NULL) +
  theme_bw()

g_margin_state_coef

ggsave("figures/11_fig_margin_coef_state_heterog.png", g_margin_state_coef,
       width = 9, height = 5, dpi = 300)
message("Saved: figures/11_fig_margin_coef_state_heterog.png")

# ==============================================================================
# 11. SECTION IV.C: PASS-THROUGH REGRESSIONS
# ==============================================================================
# Specification:
#   Delta_P_ist = alpha + beta1*Delta_w_ist + beta2*(Delta_w_ist * SOE_st) +
#                 beta3*(Delta_w_ist * postSOE_st) + gamma_i + delta_s [+ tau_t]
#
# Outcomes are nominal first differences (dP, dW).
#
# beta1: baseline pass-through (dollar change in retail per dollar change in
#        wholesale, pre-SOE period)
# beta2: change in pass-through during SOE. A negative estimate indicates
#        retail prices respond less to wholesale cost changes during enforcement,
#        consistent with APG rules dampening transmission of cost increases.
# beta3: change in pass-through after SOE ends relative to baseline.
#
# Two specifications:
#   (1) Without week FEs
#   (2) With week FEs
#
# Preferred specification is (1) because all five states have overlapping
# SOE windows, so within-week cross-sectional variation in SOE status across
# stores is limited. The no-FE specification identifies from within-store-
# product variation over time, which is the more transparent source of
# variation in this sample.
# ==============================================================================

message("Estimating Section IV.C pass-through regressions ...")

pt_data <- reg_data %>%
  filter(is.finite(dP), is.finite(dW))

# (1) Without week FEs
m_pt_no_fe <- feols(
  dP ~ dW + dW:SoE + dW:postSoE | product + store_id,
  data    = pt_data,
  cluster = ~ store_id
)

# (2) With week FEs
m_pt_with_fe <- feols(
  dP ~ dW + dW:SoE + dW:postSoE | product + store_id + week_fe,
  data    = pt_data,
  cluster = ~ store_id
)

etable(
  list(
    "(1) No week FE" = m_pt_no_fe,
    "(2) With week FE" = m_pt_with_fe
  ))

etable(
  list(
    "(1) No week FE" = m_pt_no_fe,
    "(2) With week FE" = m_pt_with_fe
  ),
  tex      = TRUE,
  file     = "tables_latex/12_tab_passthrough_reg.tex",
  title    = "Pass-through regressions with and without week fixed effects",
  label    = "tab:passthrough_reg",
  digits   = 3, se.below = TRUE, depvar = FALSE,
  fitstat  = ~ n + r2,
  dict     = c(
    "dW"         = "$\\Delta w_{ist}$",
    "dW:SoE"     = "$\\Delta w_{ist} \\times SOE_{st}$",
    "dW:postSoE" = "$\\Delta w_{ist} \\times postSOE_{st}$"
  ),
  headers  = list("Pass-through: $\\Delta p_{ist}$" = 2),
  notes    = c(
    "Dependent variable: nominal $\\Delta p_{ist}$ (dollars per unit or pound).",
    "Column (2) adds week fixed effects ($\\tau_t$).",
    "Standard errors clustered at the store level."
  )
)
message("Saved: tables_latex/12_tab_passthrough_reg.tex")

# Coefficient plot for pass-through
pt_coef_df <- bind_rows(
  broom::tidy(m_pt_no_fe, conf.int = TRUE) %>%
    mutate(spec = "No week FE"),
  broom::tidy(m_pt_with_fe, conf.int = TRUE) %>%
    mutate(spec = "With week FE")
) %>%
  filter(term %in% c("dW", "dW:SoE", "dW:postSoE")) %>%
  mutate(
    term = recode(term,
                  "dW"         = "Baseline (Delta w)",
                  "dW:SoE"     = "Delta w x SOE",
                  "dW:postSoE" = "Delta w x postSOE"),
    term = factor(term, levels = c("Baseline (Delta w)",
                                   "Delta w x SOE",
                                   "Delta w x postSOE")),
    spec = factor(spec, levels = c("No week FE", "With week FE"))
  )

g_pt_coef <- ggplot(pt_coef_df,
                    aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = spec)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_text(aes(label = round(estimate, 3)),
            position = position_dodge(width = 0.5),
            vjust = -1.0, size = 3.2, show.legend = FALSE) +
  labs(x = NULL, y = "Coefficient estimate",
       title = "Pass-through coefficients: with and without week fixed effects",
       color = NULL) +
  theme_bw() +
  theme(legend.position = "top")

g_pt_coef

ggsave("figures/12_fig_passthrough_coef.png", g_pt_coef,
       width = 9, height = 5, dpi = 300)
message("Saved: figures/12_fig_passthrough_coef.png")


# ==============================================================================
# 11a. EXTENSION: PASS-THROUGH BY DURATION (Dur_wk)
# ==============================================================================
# This section extends the workplan pass-through specification to ask how
# many weeks it takes for retailers to return to their pre-SOE pass-through
# rate after the emergency begins.
#
# Specification:
#   Delta_P_ist = alpha + beta1*Delta_w + beta2*(Delta_w*SOE) +
#                 beta3*(Delta_w*SOE*Dur_wk) + beta4*(Delta_w*postSOE) +
#                 gamma_j + delta_i
#
# Dur_wk = Dur_st = weeks since SOE activation in state g, set to 0 outside SOE.
# (Panel is weekly; Dur_st already counts weeks, not days.)
#
# beta2: change in pass-through at the start of the SOE (Dur_wk = 0)
# beta3: weekly rate of change in pass-through during SOE. If beta3 > 0,
#        retailers gradually return toward baseline pass-through as the
#        emergency continues.
# The implied SOE pass-through at duration d = beta1 + beta2 + beta3*d.
# Return to baseline occurs when beta2 + beta3*d = 0, i.e., d = -beta2/beta3.
# ==============================================================================

if (RUN_DUR_EXTENSION) {
  
  message("Estimating optional duration extension (Section 11a) ...")
  
  pt_dur_data <- pt_data %>%
    filter(is.finite(Dur_st)) %>%
    mutate(Dur_wk = as.numeric(Dur_st))
  
  m_pt_dur <- feols(
    dP ~ dW + dW:SoE + dW:SoE:Dur_wk + dW:postSoE | product + store_id,
    data    = pt_dur_data,
    cluster = ~ store_id
  )
  
  etable(
    list("(1)" = m_pt_dur),
    tex      = TRUE,
    file     = "tables_latex/13_tab_passthrough_duration.tex",
    title    = "Pass-through by duration: how pass-through evolves over the emergency window",
    label    = "tab:passthrough_duration",
    digits   = 3, se.below = TRUE, depvar = FALSE,
    fitstat  = ~ n + r2,
    dict     = c(
      "dW"              = "$\\Delta w_{ist}$",
      "dW:SoE"          = "$\\Delta w_{ist} \\times SOE_{st}$",
      "dW:SoE:Dur_wk"   = "$\\Delta w_{ist} \\times SOE_{st} \\times Dur^{wk}_{st}$",
      "dW:postSoE"      = "$\\Delta w_{ist} \\times postSOE_{st}$"
    ),
    notes = c(
      "Dependent variable: nominal $\\Delta p_{ist}$. No week FEs.",
      "$Dur^{wk}_{st}$ = weeks since SOE activation in state $g$, set to 0 outside SOE.",
      "The implied SOE pass-through at duration $d$ is $\\hat{\\beta}_1 + \\hat{\\beta}_2 + \\hat{\\beta}_3 d$.",
      "Retailers return to baseline pass-through at $d^* = -\\hat{\\beta}_2 / \\hat{\\beta}_3$ weeks.",
      "Standard errors clustered at the store level."
    )
  )
  message("Saved: tables_latex/13_tab_passthrough_duration.tex")
  
  # Linear combination table: implied SOE pass-through at selected durations
  lincombo_ci <- function(model, L_vec, alpha = 0.05) {
    b  <- coef(model)
    V  <- vcov(model)
    cn <- names(b)
    L  <- setNames(rep(0, length(cn)), cn)
    for (nm in names(L_vec)) if (nm %in% cn) L[nm] <- L_vec[[nm]]
    est  <- as.numeric(t(L) %*% b)
    se   <- sqrt(as.numeric(t(L) %*% V %*% L))
    z    <- qnorm(1 - alpha / 2)
    data.frame(estimate = est, se = se,
               conf.low = est - z * se, conf.high = est + z * se)
  }
  
  d_vals <- seq(0, 70, by = 4)
  
  pt_implied <- purrr::map_dfr(d_vals, function(d) {
    L <- list("dW" = 1, "dW:SoE" = 1, "dW:SoE:Dur_wk" = d)
    out <- lincombo_ci(m_pt_dur, L)
    cbind(data.frame(Dur_wk = d), round(out, 3))
  })
  
  write.csv(pt_implied, "tables_csv/08_tab_passthrough_implied_soe.csv", row.names = FALSE)
  
  save_tex(
    kbl(pt_implied,
        format   = "latex", booktabs = TRUE,
        caption  = "Implied SOE pass-through at selected enforcement durations. Computed as $\\hat{\\beta}_1 + \\hat{\\beta}_2 + \\hat{\\beta}_3 d$ from the duration model. $Dur^{wk}$ = weeks since SOE activation.",
        label    = "tab:passthrough_implied_soe",
        align    = "rrrrr") %>%
      kable_styling(latex_options = c("hold_position")),
    "14_tab_passthrough_implied_soe.tex"
  )
  
  # Plot: implied SOE pass-through by duration
  g_pt_dur <- ggplot(pt_implied, aes(x = Dur_wk, y = estimate)) +
    geom_hline(yintercept = coef(m_pt_dur)[["dW"]], linetype = "dashed", color = "grey40") +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.8) +
    geom_text(aes(label = round(estimate, 2)), vjust = -0.8, size = 3.5) +
    annotate("text", x = max(d_vals) * 0.85,
             y = coef(m_pt_dur)[["dW"]] + 0.01,
             label = "Baseline (pre-SOE)", size = 3, color = "grey40") +
    labs(
      title    = "Implied SOE pass-through by enforcement duration",
      subtitle = "Dashed line = pre-SOE baseline pass-through. Points are linear combinations from the duration model.",
      x        = "Weeks since SOE activation",
      y        = "Implied pass-through (Δ p / Δ w)"
    ) +
    theme_bw() +
    theme(plot.subtitle = element_text(size = 8))
  
  ggsave("figures/13_fig_passthrough_duration.png", g_pt_dur,
         width = 8, height = 5, dpi = 300)
  message("Saved: figures/13_fig_passthrough_duration.png")
  
}


# ==============================================================================
# 12. SECTION IV.D: UNIFORM PRICING
# ==============================================================================
# Method: for each (retailer, product, week) cell, compute all pairwise
# absolute log price differences across stores within the same chain.
# |log(p_s) - log(p_k)| for each unique store pair (s, k) within a retailer.
# Repeat for wholesale costs.
#
# A distribution tightly concentrated near zero indicates nearly uniform
# pricing within the chain. Comparing distributions across Pre-SOE, During SOE,
# and Post-SOE periods tests whether within-chain price uniformity changed
# during enforcement.
#
# Regression outcome: mean absolute log price difference at the
# (retailer, product, week) level. Regressed on During-SOE and Post-SOE
# indicators with various FE combinations.
#
# Nominal prices are used for log differences throughout, consistent with
# the rest of the paper's main specifications.
# ==============================================================================

message("Estimating Section IV.D uniform pricing ...")

unif_panel <- panel_est %>%
  filter(p_ist > 0, w_ist > 0) %>%  #change to p_real and w_real if we want to run this using real prices
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

# Pairwise log-difference function
# price_col: "p_ist" or "w_ist"  #change to p_real and w_real if we want to run this using real prices
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

pairs_retail    <- make_pairwise_logdiff(unif_panel, "p_ist") #change to p_real if we want to run this using real prices
pairs_wholesale <- make_pairwise_logdiff(unif_panel, "w_ist") #change to w_real if we want to run this using real prices

# -- Figures: pooled histogram and by-period histogram ---
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

# ------------------------------------------------------------------------------
# Summary tables: mean absolute log price difference by retailer and period
# These describe the distribution of within-chain price uniformity before,
# during, and after the SOE, separately for retail and wholesale costs.
# Built from pairs_retail and pairs_wholesale before the uniformity regressions.
# ------------------------------------------------------------------------------

make_disp_summary <- function(pairs_df, caption_str, label_str,
                              filename_csv, filename_tex) {
  tbl <- pairs_df %>%
    mutate(
      retailer = paste0("Retailer ", retailer),
      period   = factor(period, levels = c("Pre-SOE", "During SOE", "Post-SOE"))
    ) %>%
    group_by(retailer, period) %>%
    summarise(
      Count      = n(),
      Mean       = mean(diff,   na.rm = TRUE),
      Median     = median(diff, na.rm = TRUE),
      `Std. dev.` = sd(diff,   na.rm = TRUE),
      Variance   = var(diff,   na.rm = TRUE),
      Max        = max(diff,   na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    arrange(retailer, period) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))
  
  write.csv(tbl, file.path("tables_csv", filename_csv), row.names = FALSE)
  
  save_tex(
    kbl(tbl,
        format      = "latex", booktabs = TRUE,
        caption     = caption_str,
        label       = label_str,
        align       = "llrrrrrr",
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

# -- Regression data: collapse to retailer-product-week mean absolute log diff --
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

# -- Pooled uniformity regressions --
# Each FE structure is run with and without month FE.
# The no-FE versions absorb only the grouping structure; the month FE
# versions additionally control for common aggregate time variation.
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

# Retail: all six specifications in one table
etable(
  list(
    "(1)" = regs_retail$A,
    "(2)" = regs_retail$A2,
    "(3)" = regs_retail$B,
    "(4)" = regs_retail$B2,
    "(5)" = regs_retail$C,
    "(6)" = regs_retail$C2
  ),
  tex = TRUE, file = "tables_latex/17_tab_uniformity_retail.tex",
  title   = "Within-chain retail price uniformity during and after SOE",
  label   = "tab:uniformity_retail",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict  = c("during" = "During SOE", "post" = "Post-SOE"),
  notes = c(
    "Dependent variable: mean absolute log retail price difference across store pairs within retailer-product-week cells.",
    "Omitted category: pre-SOE period.",
    "Standard errors clustered at the retailer-product level."
  )
)
message("Saved: tables_latex/17_tab_uniformity_retail.tex")

# Wholesale: all six specifications in one table
etable(
  list(
    "(1)" = regs_wholesale$A,
    "(2)" = regs_wholesale$A2,
    "(3)" = regs_wholesale$B,
    "(4)" = regs_wholesale$B2,
    "(5)" = regs_wholesale$C,
    "(6)" = regs_wholesale$C2
  ),
  tex = TRUE, file = "tables_latex/18_tab_uniformity_wholesale.tex",
  title   = "Within-chain wholesale cost uniformity during and after SOE",
  label   = "tab:uniformity_wholesale",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict  = c("during" = "During SOE", "post" = "Post-SOE"),
  notes = c(
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
  list(
    "(1)" = heterog_retail$A, 
    "(2)" = heterog_retail$A2, 
    "(3)" = heterog_retail$B, 
    "(4)" = heterog_retail$B2 
  ),
  tex = TRUE, file = "tables_latex/19_tab_uniformity_heterog_retail.tex",
  title   = "Retailer heterogeneity in within-chain retail price uniformity",
  label   = "tab:uniformity_heterog_retail",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes   = heterog_notes
)
message("Saved: tables_latex/19_tab_uniformity_heterog_retail.tex")

etable(
  list(
    "(1)" = heterog_wholesale$A, 
    "(2)" = heterog_wholesale$A2, 
    "(3)" = heterog_wholesale$B, 
    "(4)" = heterog_wholesale$B2 
  ),
  tex = TRUE, file = "tables_latex/20_tab_uniformity_heterog_wholesale.tex",
  title   = "Retailer heterogeneity in within-chain wholesale cost uniformity",
  label   = "tab:uniformity_heterog_wholesale",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes   = heterog_notes
)
message("Saved: tables_latex/20_tab_uniformity_heterog_wholesale.tex")

# -- Coefficient plot for retailer heterogeneity uniformity regressions --
# Extracts retailer-specific during and post coefficients from heterogeneity
# models, pairing each no-FE specification with its month-FE counterpart.

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
    fe_spec  = factor(fe_spec,  levels = c("No additional FEs",
                                           "Product FE")),
    month_fe = factor(month_fe, levels = c("No month FE", "Month FE")),
    period   = factor(period,   levels = c("During SOE", "Post-SOE")),
    outcome  = factor(outcome,  levels = c("Retail", "Wholesale"))
  )

# One plot per outcome, faceted by FE specification, colored by period,
# shaped by month FE inclusion. This allows direct visual comparison of
# A vs A2, B vs B2 within each retailer.
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
    x       = "Retailer",
    y       = "Coefficient (mean absolute log price diff)",
    title   = "Retailer heterogeneity in within-chain price uniformity during and after SOE",
    subtitle = "Rows: retail vs wholesale. Columns: FE specification. Shape: with vs without month FE.",
    color   = NULL,
    shape   = NULL
  ) +
  theme_bw() +
  theme(
    legend.position  = "top",
    plot.subtitle    = element_text(size = 8),
    strip.text       = element_text(size = 9)
  )

ggsave("figures/18_fig_uniformity_heterog_coef.png", g_heterog_coef,
       width = 13, height = 8, dpi = 300)
message("Saved: figures/18_fig_uniformity_heterog_coef.png")





message("=== Analysis complete. ===")
message("Tables (LaTeX): tables_latex/")
message("Tables (CSV):   tables_csv/")
message("Figures (PNG):  figures/")

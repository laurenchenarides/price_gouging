# ==============================================================================
# 02_build_panel.R
#
# Purpose: Build the estimation panel from the raw SQL pull.
#   1.  Data corrections and primary price variable assignment
#   2.  CPI deflation and variable construction
#   3.  SOE timing, duration, and event-time indices
#   4.  Weekly first differences and outlier trimming
#
# Depends on: panel_upc_week (from 00_read_in_data.R),
#   RETAILERS_KEEP, cpi/cpi_20152025.xlsx
#
# Produces: panel_levels, panel_est
#   panel_est is the trimmed estimation panel used by all downstream scripts.
#
# Also defines: save_tex() helper used in all table-writing scripts.
# ==============================================================================


# ==============================================================================
# 1. PRIMARY PRICE VARIABLE ASSIGNMENT
# ==============================================================================
# p_ist is set to p_ist_net (volume-weighted transaction price) for all main
# regressions. This is the consumer-relevant price and matches the paper's
# estimating equations. p_ist_gross (posted shelf price) is retained for the
# promotional expansion robustness check in Mechanism 3.
# ==============================================================================

panel_upc_week <- panel_upc_week %>%
  mutate(p_ist = p_ist_net)

# ==============================================================================
# 2. CPI DEFLATION AND VARIABLE CONSTRUCTION
# ==============================================================================
# CPI source: national monthly CPI-U (BLS, 1982-84 = 100), rebased so that
# January 2018 = 1.00 (deflator P_t). Each observation is assigned the
# deflator for its calendar month via week_start.
#
# Nominal series (p_ist, w_ist, margin_nom):
#   APG statutes reference nominal retail prices so nominal levels are the
#   correct outcome for main regressions.
#
# Real series (p_real, w_real, margin_real):
#   Used only in the supplementary descriptive table (Section III.A).
# ==============================================================================

message("Reading and rebasing CPI ...")

cpi_long <- readxl::read_excel("cpi/cpi_20152025.xlsx", sheet = "Sheet1") %>%
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
  mutate(P_t = cpi_8284 / base_value) %>%
  select(month_start, P_t)

rm(cpi_long)

message("Joining deflator and constructing price, cost, and margin variables ...")

panel_levels <- panel_upc_week %>%
  filter(retailer_id %in% RETAILERS_KEEP) %>%
  mutate(month_start = lubridate::floor_date(week_start, unit = "month")) %>%
  left_join(cpi_deflator, by = "month_start") %>%
  mutate(
    margin_nom  = p_ist - w_ist,
    p_real      = p_ist / P_t,
    w_real      = w_ist / P_t,
    margin_real = p_real - w_real
  )

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
# T_start and T_end are the first and last week_seq values with SoE == 1
# in each state. All stores in the same state share the same T_start/T_end.
#
# Constructed variables:
#   preSoE   = 1 if week_seq < T_start in state g
#   postSoE  = 1 if week_seq > T_end in state g
#   Dur_st   = weeks since SOE activation (0 outside SOE)
#   k_start  = week_seq - T_start (event time relative to SOE start)
#   k_end    = week_seq - T_end   (event time relative to SOE end)
# ==============================================================================

message("Constructing SOE duration and event-time indices ...")

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
    Dur_st  = if_else(SoE == 1L, pmax(k_start, 0L), 0L),
    preSoE  = if_else(!is.na(T_start) & week_seq < T_start, 1L, 0L),
    postSoE = if_else(!is.na(T_end)   & week_seq > T_end,   1L, 0L)
  )

message("SOE start and end weeks by state:")
print(soe_dates)

# ==============================================================================
# 4. WEEKLY FIRST DIFFERENCES AND OUTLIER TRIMMING
# ==============================================================================
# First differences are taken within each store-product cell (sorted by week_seq).
#
# Nominal first differences (dP, dW, dM): used in pass-through and uniform
#   pricing regressions.
#
# Log first differences (dlnp, dlnw): constructed from real prices; used
#   only for trimming.
#
# Trimming: top and bottom 1% of dlnp and dlnw within each product are removed.
#   The trim is applied identically to both the log and level panels so they
#   remain aligned for Section IV.C comparisons.
# ==============================================================================

message("Constructing weekly first differences ...")

panel_est_raw <- panel_levels %>%
  filter(p_ist > 0, w_ist > 0, p_real > 0, w_real > 0) %>%
  arrange(sst, store_id, product, week_seq) %>%
  group_by(sst, store_id, product) %>%
  mutate(
    dP   = p_ist      - lag(p_ist),
    dW   = w_ist      - lag(w_ist),
    dM   = margin_nom - lag(margin_nom),
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

# Helper: save a kableExtra kable object as a .tex file
save_tex <- function(kbl_obj, filename) {
  writeLines(as.character(kbl_obj),
             con = file.path("tables_latex", filename))
  message("Saved: tables_latex/", filename)
}

message("Panel build complete.")

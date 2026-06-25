# Food Retailer Pricing Behavior Under Anti-Price Gouging Laws: Evidence from Wholesale and Retail Scanner Data
## Last updated: June 24, 2026

**Chenarides, Richards, and Dong**

This repository contains the replication code for the analysis of anti-price gouging (APG) laws and grocery retail pricing during COVID-19 state-of-emergency (SOE) periods. The analysis uses a store-product-week panel of scanner data for five fresh produce products across multiple retail chains.

---

## Repository Structure

```
price_gouging/
├── code/
│   ├── run_all.R                          # Master script — source this to reproduce everything
│   ├── 00_read_in_data.R                  # Step 1: SQL pull and panel assembly
│   ├── 01_price_sensitivity_diagnostic.R  # Step 2: Price measure diagnostics
│   ├── 01b_whisker_plots_logs_vs_levels.R # Optional: log vs level pass-through
│   ├── 01c_COMPARE_all_df_select_df.R     # Optional: full vs selected UPC comparison
│   ├── 02_build_panel.R                   # CPI deflation, SOE timing, first diffs
│   ├── 03_descriptive_tables.R            # Section II-III tables and figures
│   ├── 04_residual_plots.R                # Section III.C-E residualized trends
│   ├── 05_regressions.R                   # Section IV.A-B price and margin regs
│   ├── 06_passthrough.R                   # Section IV.C pass-through regressions
│   ├── 07_uniform_pricing.R               # Section IV.D uniform pricing
│   └── apg_analysis.R                     # Original monolithic script [deprecated]
├── cpi/
│   ├── cpi_20152025.xlsx                  # BLS CPI-U raw data (not tracked in git)
│   └── cpi_rebase.R                       # Standalone CPI diagnostic and deflator plots
├── tables_latex/                          # LaTeX-formatted output tables
│   └── net_price/                         # Robustness variant using net (post-discount) prices
├── pg_project.Rproj                       # RStudio project file
└── README.md                              # This file
```

---

## How to Run

Open `pg_project.Rproj` in RStudio. This sets the working directory automatically.

**To reproduce all tables and figures, run a single script:**

```r
source("code/run_all.R")
```

`run_all.R` calls all numbered scripts in order. Global flags at the top of `run_all.R` control optional outputs:

| Flag | Default | Effect |
|------|---------|--------|
| `SAVE_OPTIONAL_PLOTS` | `TRUE` | Produce by-state and by-product residual trend plots (III.D, III.E) |
| `RETAILERS_KEEP` | `c(2, 3, 5)` | Retailers included (4 excluded: closed mid-sample) |
| `RUN_DUR_EXTENSION` | `TRUE` | Run pass-through duration extension (Section 11a) |
| `SAVE_CSV` | `FALSE` | Also write intermediate CSV files alongside LaTeX tables |

---

## Script Guide

### Execution order (called automatically by `run_all.R`)

| Script | Purpose | Key outputs |
|--------|---------|-------------|
| `00_read_in_data.R` | Pull five product tables from SQL Server; assemble `panel_upc_week` | `panel_upc_week` in memory |
| `01_price_sensitivity_diagnostics.R` | Compare p_ist_net vs p_ist_gross; flag peppers spike week 2019-05-27 | Diagnostics to console |
| `02_build_panel.R` | Data corrections, CPI deflation, SOE timing, first diffs, trimming | `panel_levels`, `panel_est`, `save_tex()` |
| `03_descriptive_tables.R` | Section II–III summary tables and figures | Tables 01–07, Figures 01–03 |
| `04_residual_plots.R` | Section III.C–E residualized trend plots | Figures 04–07 (pooled + optional by-group) |
| `05_regressions.R` | Section IV.A–B price and margin level regressions | Tables 08–11, Figures 08–11 |
| `06_passthrough.R` | Section IV.C pass-through regressions and duration extension | Tables 12–14, Figures 12–13 |
| `07_uniform_pricing.R` | Section IV.D within-chain price uniformity | Tables 15–20, Figures 14–18 |

### Optional standalone scripts

These require `panel_est` (from `02_build_panel.R`) to be in memory:

| Script | Purpose |
|--------|---------|
| `01b_whisker_plots_logs_vs_levels.R` | Whisker plots comparing log vs level pass-through slopes across event time |
| `01c_COMPARE_all_df_select_df.R` | Fixed-weight price indices and residualized trends: all perishables vs five selected UPCs |
| `cpi/cpi_rebase.R` | Standalone CPI diagnostic: YoY inflation plot and deflator series |

---

## Output Inventory

### Tables (`tables_latex/`)

| File | Section | Content |
|------|---------|---------|
| `01_tab_decadata_summary.tex` | II | Coverage by year (banners, stores, obs) |
| `02_tab_decadata_summary_wide.tex` | II | Coverage by year × retailer × state |
| `03_tab_product_coverage.tex` | II | Coverage and sales by product |
| `04_tab_period_means_nominal.tex` | III.A | Period means: nominal price, cost, margin, volume |
| `05_tab_period_means_real.tex` | III.A | Period means: real price, cost, margin (supplementary) |
| `06_tab_flagged_weeks_all.tex` | III.B | Flag rates across five thresholds (10–30%) |
| `07_tab_flagged_weeks_T25.tex` | III.B | Flag rates at 25% threshold by product |
| `08_tab_price_reg.tex` | IV.A | Price level regressions (SOE, post-SOE) |
| `09_tab_price_reg_state_heterog.tex` | IV.A | State-specific price effects |
| `10_tab_margin_reg.tex` | IV.B | Margin level regressions |
| `11_tab_margin_reg_state_heterog.tex` | IV.B | State-specific margin effects |
| `12_tab_passthrough_reg.tex` | IV.C | Pass-through: no week FE vs week FE |
| `13_tab_passthrough_duration.tex` | IV.C | Pass-through duration extension |
| `14_tab_passthrough_implied_soe.tex` | IV.C | Implied SOE pass-through at selected durations |
| `15_tab_uniformity_summary_retail.tex` | IV.D | Retail log-diff summary by retailer and period |
| `16_tab_uniformity_summary_wholesale.tex` | IV.D | Wholesale log-diff summary by retailer and period |
| `17_tab_uniformity_retail.tex` | IV.D | Uniformity regressions: retail price (6 specs) |
| `18_tab_uniformity_wholesale.tex` | IV.D | Uniformity regressions: wholesale cost (6 specs) |
| `19_tab_uniformity_heterog_retail.tex` | IV.D | Retailer heterogeneity: retail uniformity |
| `20_tab_uniformity_heterog_wholesale.tex` | IV.D | Retailer heterogeneity: wholesale uniformity |

Robustness tables using `p_ist_net` (net price) are in `tables_latex/net_price/`.

### Figures (`figures/`)

| File | Section | Content |
|------|---------|---------|
| `01_fig_volume_and_prices_dual_axis.png` | II | Weekly volume + mean nominal and real prices (dual axis) |
| `02_fig_cost_weekly.png` | II | Mean wholesale cost over time (nominal and real) |
| `03_fig_flag_cluster_stacked.png` | III.B | APG flag rates: stacked bar by threshold and retailer |
| `04_fig_resid_volume_pooled.png` | III.C | Residualized volume trend (pooled) |
| `05_fig_resid_price_pooled.png` | III.C | Residualized nominal price trend (pooled) |
| `06_fig_resid_cost_pooled.png` | III.C | Residualized nominal cost trend (pooled) |
| `07_fig_resid_margin_pooled.png` | III.C | Residualized nominal margin trend (pooled) |
| `fig_resid_*_by_state.png` | III.D | By-state residualized trends (optional) |
| `fig_resid_*_by_product.png` | III.E | By-product residualized trends (optional) |
| `08_fig_price_coef_baseline.png` | IV.A | Price regression coefficients (baseline) |
| `09_fig_price_coef_state_heterog.png` | IV.A | Price coefficients by state |
| `10_fig_margin_coef_prepost.png` | IV.B | Margin regression coefficients (baseline) |
| `11_fig_margin_coef_state_heterog.png` | IV.B | Margin coefficients by state |
| `12_fig_passthrough_coef.png` | IV.C | Pass-through coefficients: no FE vs week FE |
| `13_fig_passthrough_duration.png` | IV.C | Implied SOE pass-through by enforcement duration |
| `14_fig_logdiff_retail_pooled.png` | IV.D | Retail log-diff histogram (all periods) |
| `15_fig_logdiff_wholesale_pooled.png` | IV.D | Wholesale log-diff histogram (all periods) |
| `16_fig_logdiff_retail_by_period.png` | IV.D | Retail log-diff by SOE period |
| `17_fig_logdiff_wholesale_by_period.png` | IV.D | Wholesale log-diff by SOE period |
| `18_fig_uniformity_heterog_coef.png` | IV.D | Retailer heterogeneity in uniformity |

---

## Data Requirements

### SQL Server connection

`00_read_in_data.R` connects to a SQL Server instance named `Orchard`, database `DecaData`, using Windows authentication. To configure for a different environment, edit the `dbConnect()` call at the top of that file:

```r
con <- dbConnect(
  odbc::odbc(),
  Driver   = "SQL Server",        # adjust ODBC driver string if needed
  Server   = "Orchard",           # your server name or IP
  Database = "DecaData",
  Trusted_Connection = "Yes"      # or supply UID/PWD for SQL auth
)
```

The underlying tables are in the `stg` schema:

| Table | Content |
|-------|---------|
| `stg.pos_bananas_4011` | Bananas (PLU 4011) |
| `stg.pos_lettuce` | Lettuce (Dole shredded, UPC 7143001065) |
| `stg.pos_peppers` | Peppers (PLU 4065) |
| `stg.pos_cucumbers` | Cucumbers (PLU 4062) |
| `stg.pos_tomatoes` | Tomatoes (PLU 4087) |
| `stg.date_week_index` | Week sequence to calendar date mapping |
| `stg.pos_store_master` | Store metadata |

### CPI data

Place the BLS CPI-U Excel file at `cpi/cpi_20152025.xlsx`. The file should be in wide format with a `Year` column and monthly columns `Jan` through `Dec`. The deflator is rebased so that January 2018 = 1.

---

## Key Variables

| Variable | Description |
|----------|-------------|
| `p_ist` | Primary retail price (set to `p_ist_gross`: posted shelf price, volume-weighted) |
| `p_ist_gross` | Volume-weighted gross (posted) retail price |
| `p_ist_net` | Volume-weighted net retail price (after promotional discounts) |
| `w_ist` | Volume-weighted wholesale unit cost |
| `margin_nom` | Nominal dollar margin = `p_ist` − `w_ist` |
| `p_real`, `w_real` | Real prices, deflated by CPI (Jan 2018 = 1) |
| `SoE` | 1 if APG enforcement active in state g, week t |
| `Dur_st` | Weeks since SOE activation (0 outside SOE) |
| `k_start` | Event time relative to SOE start (`week_seq` − T_start) |
| `k_end` | Event time relative to SOE end (`week_seq` − T_end) |
| `preSoE` | 1 for weeks before SOE start in state g |
| `postSoE` | 1 for weeks after SOE end in state g |

**Subscript conventions:** i = store, j = product, s = state, t = week

**Sample note:** Retailer 4 is excluded from all analyses (closed mid-sample). Included retailers: 2, 3, 5.

---

## R Packages

```r
# Install all dependencies via pacman (handled automatically in run_all.R)
install.packages("pacman")
pacman::p_load(
  DBI, odbc, dbplyr,
  dplyr, tidyr, lubridate, stringr, purrr,
  ggplot2, scales,
  fixest, broom,
  knitr, kableExtra,
  readxl, rlang, ggpattern
)
```

---

## Price Measure Note

Net and gross retail prices diverge during the SOE because the share of transactions on promotional discount rose from ~8% pre-SOE to ~30% during the SOE. Using `p_ist_net` during the SOE would mechanically understate posted shelf prices. The main analysis uses `p_ist_gross` (posted shelf price, volume-weighted). `p_ist_net` is retained as a robustness check; results are in `tables_latex/net_price/`. See `01_price_sensitivity_diagnostic.R` for full documentation.

# Code Directory — Script Reference

This folder contains all R and SQL scripts for the analysis. Run everything via `run_all.R` from the project root. See the root `README.md` for the full output inventory and embedded figures.

---

## Execution Order

Scripts are sourced automatically by `run_all.R` in the order below.

---

### Data Construction

**`00_read_in_data.R`**
Pulls five product tables from SQL Server (DecaData, `stg` schema) and assembles `panel_upc_week` at the store-product-week level. Also pulls `dwi` (date index) and `store_info` (store metadata with lat/lon). Products: bananas (4011), cabbage (4069), cucumbers (4062), lettuce (7143001065), tomatoes (4087).

**`01_price_sensitivity_diagnostic.R`**
Compares three price measures — `p_ist_net`, `p_ist_gross`, `avg_unit_price` — and documents the promotional environment. Produces step charts (gross, net, wholesale) by product around SOE activation. The volume filter `upc_week_volume >= 2` guards against near-zero-volume artifacts in `diag_08`.

**`02_build_panel.R`**
Sets `p_ist = p_ist_net` (primary price variable). Reads and rebases BLS CPI-U. Joins deflator; constructs `margin_nom`, `p_real`, `w_real`. Builds SOE timing variables (`SoE`, `postSoE`, `preSoE`, `Dur_st`, `k_start`, `k_end`). Takes weekly first differences (`dP`, `dW`, `dM`). Trims top/bottom 1% of log price changes within product. Defines `save_tex()` helper used by all downstream table-writing scripts.

---

### Variable Construction (diagnostic)

**`01_price_sensitivity_diagnostic.R`**
Compares three price measures — p_ist_net, p_ist_gross, avg_unit_price — and documents the promotional environment. Produces step charts (gross, net, wholesale) by product around SOE activation. The volume filter upc_week_volume >= 2 guards against near-zero-volume artifacts in diag_08.

---

### Variable Construction

**`02_build_panel.R`**
Sets p_ist = p_ist_net (primary price variable). Reads and rebases BLS CPI-U. Joins deflator; constructs margin_nom, p_real, w_real. Builds SOE timing variables (SoE, postSoE, preSoE, Dur_st, k_start, k_end). Takes weekly first differences (dP, dW, dM). Trims top/bottom 1% of log price changes within product. Defines save_tex() helper used by all downstream table-writing scripts.

Output objects: `panel_levels`, `panel_est`

---

### Results: Descriptive Evidence

**`03_descriptive_tables.R`** → Tables 01–05, Figures 01–02

- **Table 01** (`01_tab_decadata_summary.tex`): Coverage by year (banners, stores, obs)
- **Table 02** (`02_tab_decadata_summary_wide.tex`): Coverage by year × retailer × state
- **Table 03** (`03_tab_product_coverage.tex`): Coverage and average sales by product
- **Table 04** (`04_tab_period_means_nominal.tex`): Period means — nominal price, cost, margin, volume by product
- **Table 05** (`05_tab_period_means_real.tex`): Period means — real prices (supplementary)
- **Figure 01** (`01_fig_volume_and_prices_dual_axis.png`): Weekly volume and prices (dual axis)
- **Figure 02** (`02_fig_cost_weekly.png`): Mean wholesale cost over time

### Results: Incidence of Price Gouging

**`03_descriptive_tables.R`** → Tables 06–07, Figure 03

- **Table 06** (`06_tab_flagged_weeks_all.tex`): APG flag rates across five thresholds
- **Table 07** (`07_tab_flagged_weeks_T25.tex`): Flag rates at 25% threshold by product
- **Figure 03** (`03_fig_flag_cluster_stacked.png`): Stacked bar of flag rates by retailer

`04_residual_plots.R` → Figures 04–07 (pooled); optional by-state and by-product

Regresses each outcome on product + store FEs; plots weekly mean residuals. Outcomes: volume, price, cost, margin. By-group plots produced when `SAVE_OPTIONAL_PLOTS = TRUE`.

---

### Empirical Model: Price and Margin Level Regressions

**`05_regressions.R`** → Tables 08–11, Figures 08–11

Specification: `P_ist = α + β₁·SOE_st + β₂·postSOE_st + γ_j + δ_i + ε_ist`

Fixed effects: product and store. Standard errors clustered at the store level.

- **Table 08** (`08_tab_price_reg.tex`): Baseline price regressions
- **Table 09** (`09_tab_price_reg_state_heterog.tex`): State-heterogeneous price effects
- **Table 10** (`10_tab_margin_reg.tex`): Baseline margin regressions
- **Table 11** (`11_tab_margin_reg_state_heterog.tex`): State-heterogeneous margin effects

---

### Mechanism 1: Constant Retail Prices

**`07_uniform_pricing.R`** → Tables 15–20, Figures 14–18

Outcome: mean absolute log price difference across store pairs within retailer-product-week. Tests whether within-chain price uniformity changed during and after the SOE.

- **Tables 15–16**: Summary of retail and wholesale log-differences by period
- **Tables 17–18**: Uniformity regressions (retail and wholesale)
- **Tables 19–20**: Retailer heterogeneity in uniformity response

---

### Mechanism 2: Variation in Pass-Through

**`06_passthrough.R`** → Tables 12–14, Figures 12–13

Specification: `ΔP_ist = α + β₁·Δw + β₂·(Δw×SOE) + β₃·(Δw×postSOE) + γ_j + δ_i + τ_t`

Preferred specification includes week FEs. The identifying variation (`Δw × SOE`) varies across stores within a week and is not absorbed by week FEs.

- **Table 12** (`12_tab_passthrough_reg.tex`): Pass-through with and without week FEs
- **Table 13** (`13_tab_passthrough_duration.tex`): Duration extension (`dW × SOE × Dur_wk`)
- **Table 14** (`14_tab_passthrough_implied_soe.tex`): Implied SOE pass-through at selected enforcement durations
- **Figure 12** (`12_fig_passthrough_coef.png`): Coefficient estimates, both specs
- **Figure 13** (`13_fig_passthrough_duration.png`): Implied pass-through by weeks since SOE

Duration extension runs when `RUN_DUR_EXTENSION = TRUE` (default).

---

### Mechanism 3: Countercyclical Promotional Pricing

**`08_demand_rotation.R`** → Tables 21–24, Figures 19–21

Tests the Butters (2025) demand rotation hypothesis: retailers use promotional discounts to price discriminate between price-elastic and inelastic consumers. APG laws constrain the posted price; retailers may adjust deal frequency or depth as a substitute margin channel.

Requires `stg.pd_store_upc_week` in SQL (run `BuildMarkupsNew_PriceDiscrimination.sql` first). Falls back to `panel_est`-level `share_on_sale` if unavailable.

- **M3a** (`21_tab_gross_net_gap.tex`, `19_fig_gross_net_gap.png`): Did the promotional discount per unit widen during the SOE?
- **M3b** (`22_tab_promo_intensity.tex`, `20_fig_promo_intensity.png`): Did `share_on_sale` and discount depth change?
- **M3c** (`23_tab_price_dispersion.tex`, `21_fig_price_dispersion.png`): Did within-store-day price dispersion change?
- **M3e** (`24_tab_iv_passthrough.tex`): IV pass-through using inverse-distance-weighted cross-market costs (`Z_ist`) from store lat/lon.

---

## SQL Scripts

| File | Purpose |
|------|---------|
| `BuildMarkupsNew_2026_04.sql` | Builds `stg.store_upc_week` and `stg.pos_*` product tables. Run before R scripts. |
| `BuildMarkupsNew_Consumer_Tables.sql` | Diagnostic queries: distinct shoppers, shopping frequency by period, sale/regular coexistence within store-UPC-date. |
| `BuildMarkupsNew_PriceDiscrimination.sql` | Builds `stg.pd_store_upc_day` and `stg.pd_store_upc_week` for Mechanism 3. Uses `REWARD_CARD_NUMBER` as shopper ID. |

---

## Optional Standalone Scripts

Not called by `run_all.R`; require `panel_est` in memory:

| Script | Purpose |
|--------|---------|
| `01b_whisker_plots_logs_vs_levels.R` | Whisker plots: log vs level pass-through across event time |
| `01c_COMPARE_all_df_select_df.R` | Fixed-weight price indices: all perishables vs five selected UPCs |
| `cpi/cpi_rebase.R` | CPI diagnostic: YoY inflation and deflator series |

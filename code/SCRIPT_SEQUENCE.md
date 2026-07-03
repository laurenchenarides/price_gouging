# Code Directory — Script Reference

This folder contains all R and SQL scripts for the analysis. Run everything via `run_all.R` from the project root (after the server-side SQL build). See the root `README.md` for the full output inventory.

---

## Execution Order

Scripts are sourced automatically by `run_all.R` in the order below. Connection details and global flags live in `config.R`.

---

### Data Construction

**`00_read_in_data.R`**
Pulls five product tables from SQL Server (DecaData, `stg` schema) and assembles `panel_upc_week` at the store-product-week level. Also pulls `dwi` (date index) and `store_info` (store metadata with lat/lon; retailer 1 excluded, mirroring the SQL build). Products: bananas (4011), cabbage (4069), cucumbers (4062), lettuce (7143001065), tomatoes (4087).

**`01_price_sensitivity_diagnostic.R`**
Compares three price measures — `p_ist_net`, `p_ist_gross`, `avg_unit_price` — and documents the promotional environment. Produces step charts (gross, net, wholesale) by product around SOE activation (`diag_08_price_step_by_product_with_cost.png` = paper Figure 3). The volume filter `upc_week_volume >= 2` guards against near-zero-volume artifacts in `diag_08`.

**`02_build_panel.R`**
Sets `p_ist = p_ist_net` (primary price variable). Reads and rebases BLS CPI-U. Joins deflator; constructs `margin_nom`, `p_real`, `w_real`. Builds SOE timing variables (`SoE`, `postSoE`, `preSoE`, `Dur_st`, `k_start`, `k_end`). Takes weekly first differences (`dP`, `dW`, `dM`). Trims top/bottom 1% of weekly log changes within product. **Note:** the resulting `panel_est` is the shared estimation sample for ALL downstream scripts, including descriptives and level regressions (N = 604,121 in the current draft). Defines `save_tex()` used by all table-writing scripts.
Output objects: `panel_levels`, `panel_est`.

---

### Results: Descriptive Evidence & Incidence of Price Gouging

**`03_descriptive_tables.R`** → tables `00`–`07`, figures `01`–`03`

- `00_tab_summary_stats.tex` — paper Table A.15
- `01_tab_decadata_summary.tex` — paper Table 2 (coverage by year)
- `02_tab_decadata_summary_wide.tex` — paper Table A.16
- `03_tab_product_coverage.tex` — not currently in paper
- `04_tab_period_means_nominal.tex` — paper Table 3
- `05_tab_period_means_real.tex` — supplementary (not in paper)
- `06_tab_flagged_weeks_all.tex` — paper Table 4
- `07_tab_flagged_weeks_T25.tex` — paper Table 5
- `01_fig_volume_and_prices_dual_axis.png` — paper Figure 2
- `02_fig_cost_weekly.png` — not currently in paper
- `03_fig_flag_cluster_stacked.png` — paper Figure 4

**`04_residual_plots.R`** → `04–07_fig_resid_*_pooled.png` (+ by-state/by-product when `SAVE_OPTIONAL_PLOTS = TRUE`). Not currently placed in the paper.

---

### Empirical Model: Price and Margin Level Regressions

**`05_regressions.R`** → tables `08`–`11` + `08b`, `10b`, `10c`, `10d`; figures `08`–`11`

Specification: `P_ist = α + β₁·SOE_st + β₂·postSOE_st + γ_j + δ_i + ε_ist`.
Fixed effects: product and store. **Standard errors clustered at the state level (G = 5), complemented by wild cluster bootstrap with Webb weights (B = 9,999, seeded).**

- `08_tab_price_reg.tex` — paper Table 6; `08b_tab_price_wcb.tex` — paper Table B.17
- `09_tab_price_reg_state_heterog.tex` — paper Table 7
- `10_tab_margin_reg.tex` — paper Table 8; `10b_tab_margin_wcb.tex` — paper Table B.18
- `11_tab_margin_reg_state_heterog.tex` — paper Table 9
- `10c/10d_tab_cost_reg*.tex` — paper Tables C.19–C.20 (accounting check)

---

### Mechanism 1: Constant Retail Prices

**`06_uniform_pricing.R`** → tables `15`, `16`, `17_main`, `17b`, `18_main`, `18b`, `19`, `20`; figures `14`–`18`

Outcome: mean absolute log price (cost) difference across store pairs within retailer-product-week. Main regressions = paper Tables 10–11 (`17_tab_uniformity_retail_main.tex`, `18_tab_uniformity_wholesale_main.tex`); month-FE robustness = paper Tables D.23–D.24; summaries = D.21–D.22; retailer heterogeneity = D.25–D.26; figures 14–17 = paper Figures 9–12; figure 18 = paper Figure D.15.

---

### Mechanism 2: Variation in Pass-Through

**`07_passthrough.R`** → tables `12`, `12b`, `13`, `14`, `24`; figures `12`–`13`

- `12_tab_passthrough_reg.tex` — paper Table 12 (store-clustered SEs)
- `13/14_...` — paper Tables 13–14 (duration extension; runs when `RUN_DUR_EXTENSION = TRUE`)
- `12b_tab_passthrough_log.tex` — paper Table E.27 (Sangani levels-vs-logs check)
- `24_tab_iv_passthrough.tex` — paper Table E.28 (IV; runs when `RUN_IV = TRUE`; note this lives HERE, not in script 08)
- figures 12–13 = paper Figures 13–14

---

### Mechanism 3: Countercyclical Promotional Pricing

**`08_demand_rotation.R`** → tables `21`, `22`, `23`, `25`, `26`; figures `19`–`20` (paper §5.3.1 — results section not yet written)

Requires `stg.pd_store_upc_week` (run `BuildMarkupsNew_PriceDiscrimination.sql` first). Falls back to `panel_est` `share_on_sale` if unavailable.

- **M3a** `21_tab_gross_net_gap.tex`, `19_fig_gross_net_gap.png` — gross–net gap during SOE
- **M3b** `22_tab_promo_intensity.tex`, `20_fig_promo_intensity.png` — share on sale, discount depth
- **M3c** `23_tab_gross_price_stability.tex` — gross (shelf) price stability. NOTE: the transaction data show no intra-day price dispersion (`both_types_present = 0` everywhere), so the earlier within-store dispersion test was replaced by this gross-price-stability test. Requires a `p_gross_weekly` column in `stg.pd_store_upc_week` — see audit item C2.
- **M3d** `25_tab_demand_firststage.tex`, `26_tab_demand_cf.tex` — control-function demand rotation (distance-weighted price IV; bootstrap gated by `RUN_CF_BOOTSTRAP`).

---

## SQL Scripts

| File | Purpose |
|------|---------|
| `BuildMarkupsNew_2026_04.sql` | Builds `stg.store_upc_week` and the five `stg.pos_*` product tables, plus `stg.date_week_index`, `stg.pos_store_master`, `stg.store_dim`. Run FIRST. Assumes raw server objects `dbo.tempPOS_retailer_2_5`, `stg.pd`, `stg.category_key`, `stg.date_key`, and store metadata tables exist. |
| `BuildMarkupsNew_PriceDiscrimination.sql` | Builds `stg.pd_store_upc_day` and `stg.pd_store_upc_week` for Mechanism 3. Run SECOND. |

Archived: `_archive/BuildMarkupsNew_Consumer_Tables.sql` (shopper diagnostics).

---

## Optional Standalone Scripts

Not called by `run_all.R`. **These depend on objects from the deprecated `apg_analysis.R` pipeline** (`panel_est_trim`, `store_upc_week`, `cpi_rebased`) and will not run against the current pipeline without porting:

| Script | Purpose |
|--------|---------|
| `cpi/cpi_rebase.R` | CPI diagnostic: YoY inflation and deflator series (works standalone) |

# Code Directory — Script Reference

This folder contains all R and SQL scripts for the analysis. Run everything via
`run_all.R` from the project root (open `pg_project.Rproj` first so the working
directory is set). See the root `README.md` for the full output inventory and
embedded figures.

---

## Execution Order

`run_all.R` sources the numbered scripts in this order:

```
00 → 01 → 02 → 03 → 04 → 05 → 06 → 07 → 08
```

Scripts 06–08 correspond to Mechanisms 1, 2, and 3 respectively.

---

### Data Construction

**`00_read_in_data.R`**
Pulls five product tables from SQL Server (DecaData, `stg` schema) and assembles
`panel_upc_week` at the store-product-week level. Also pulls `dwi` (date index)
and `store_info` (store metadata with lat/lon). Products: bananas (4011),
cabbage (4069), cucumbers (4062), lettuce (7143001065), tomatoes (4087).

**`01_price_sensitivity_diagnostic.R`** → `diag_01`–`diag_08` figures
Compares three price measures — `p_ist_net`, `p_ist_gross`, `avg_unit_price` —
and documents the promotional environment. Produces step charts (gross, net,
wholesale) by product around SOE activation. The volume filter
`upc_week_volume >= 2` guards against near-zero-volume artifacts in `diag_08`
(`diag_08_price_step_by_product_with_cost.png`).

**`02_build_panel.R`** → `panel_est`, `save_tex()`
Sets `p_ist = p_ist_net` (primary price variable). Reads and rebases BLS CPI-U;
joins the deflator; constructs `margin_nom`, `p_real`, `w_real`. Builds SOE
timing variables (`SoE`, `postSoE`, `preSoE`, `Dur_st`, `k_start`, `k_end`).
Takes weekly first differences (`dP`, `dW`, `dM`). Trims the top/bottom 1% of
within-product weekly log price changes. Defines `save_tex()`, used by all
downstream table-writing scripts.

---

### Results: Descriptive Evidence and Incidence

**`03_descriptive_tables.R`** → Tables 00–07, Figures 01–03

- **Table 00** (`00_tab_summary_stats.tex`): Summary statistics for estimation variables
- **Table 01** (`01_tab_decadata_summary.tex`): Coverage by year (banners, stores, obs)
- **Table 02** (`02_tab_decadata_summary_wide.tex`): Coverage by year × retailer × state
- **Table 03** (`03_tab_product_coverage.tex`): Coverage and average sales by product
- **Table 04** (`04_tab_period_means_nominal.tex`): Period means — nominal price, cost, margin, volume
- **Table 05** (`05_tab_period_means_real.tex`): Period means — real prices (supplementary)
- **Table 06** (`06_tab_flagged_weeks_all.tex`): Flag rates across five thresholds
- **Table 07** (`07_tab_flagged_weeks_T25.tex`): Flag rates at the 25% threshold by product
- **Figure 01** (`01_fig_volume_and_prices_dual_axis.png`): Weekly volume and prices (dual axis)
- **Figure 02** (`02_fig_cost_weekly.png`): Mean wholesale cost over time
- **Figure 03** (`03_fig_flag_cluster_stacked.png`): Stacked bar of flag rates by retailer

**`04_residual_plots.R`** → Figures 04–07 (pooled); optional by-state/by-product
Regresses each outcome on product + store FEs and plots weekly mean residuals.
Outcomes: volume, price, cost, margin. By-group plots are produced when
`SAVE_OPTIONAL_PLOTS = TRUE`.

---

### Empirical Model: Price and Margin Level Regressions

**`05_regressions.R`** → Tables 08, 08b, 09, 10, 10b, 10c, 10d, 11; Figures 08–11

Specification: `P_ist = α + β₁·SOE_st + β₂·postSOE_st + γ_j + δ_i + ε_ist`.
Fixed effects: product and store. Standard errors clustered at the state level,
with wild cluster bootstrap inference (Webb weights, *B* = 9,999, *G* = 5).

- **Table 08** (`08_tab_price_reg.tex`): Baseline price regressions
- **Table 08b** (`08b_tab_price_wcb.tex`): Wild cluster bootstrap inference (price)
- **Table 09** (`09_tab_price_reg_state_heterog.tex`): State-heterogeneous price effects
- **Table 10** (`10_tab_margin_reg.tex`): Baseline margin regressions
- **Table 10b** (`10b_tab_margin_wcb.tex`): Wild cluster bootstrap inference (margin)
- **Table 10c** (`10c_tab_cost_reg.tex`): Wholesale cost regressions (appendix)
- **Table 10d** (`10d_tab_cost_reg_state_heterog.tex`): State-heterogeneous cost effects
- **Table 11** (`11_tab_margin_reg_state_heterog.tex`): State-heterogeneous margin effects
- **Figures 08–11**: Coefficient plots for price and margin, baseline and by state

---

### Mechanism 1: Constant Retail Prices

**`06_uniform_pricing.R`** → Tables 15–20, Figures 14–18

Outcome: mean absolute log price difference across store pairs within a
retailer-product-week. Store pairs are classified by joint enforcement status
(pre / during / post / mixed). Tests whether within-chain uniformity changed
during the SOE and whether mixed (cross-state) enforcement status broke uniform
pricing.

- **Tables 15–16** (`15_..._summary_retail`, `16_..._summary_wholesale`): log-diff summaries by period
- **Tables 17 / 17b / 17c** (`17_..._retail_main`, `17b_..._retail_robust`, `17c_..._retail_crossstate`): retail uniformity — main, month-FE robustness, cross-state (geography) control
- **Tables 18 / 18b / 18c** (`18_..._wholesale_main`, `18b_..._wholesale_robust`, `18c_..._wholesale_crossstate`): wholesale uniformity — main, robustness, cross-state control
- **Tables 19–20** (`19_..._heterog_retail`, `20_..._heterog_wholesale`): retailer heterogeneity
- **Figures 14–15**: pooled log-diff distributions (retail, wholesale)
- **Figures 16–17**: log-diff distributions by enforcement status
- **Figure 18**: retailer heterogeneity coefficients

---

### Mechanism 2: Variation in Pass-Through

**`07_passthrough.R`** → Tables 12, 12b, 12c, 13, 14; Figures 12–13

Specification:
`ΔP_ist = α + β₁·Δw + β₂·(Δw×SOE) + β₃·(Δw×postSOE) + γ_j + δ_i + τ_t`.
The preferred specification includes week FEs. The identifying variation
(`Δw × SOE`) varies across stores within a week and is not absorbed by week FEs.

- **Table 12** (`12_tab_passthrough_reg.tex`): Pass-through, no week FE vs. week FE
- **Table 12b** (`12b_tab_passthrough_log.tex`): Pass-through in logs (robustness)
- **Table 13** (`13_tab_passthrough_duration.tex`): Duration extension (`Δw × SOE × Dur_wk`)
- **Table 14** (`14_tab_passthrough_implied_soe.tex`): Implied SOE pass-through at selected durations
- **Table 12c** (`12c_tab_iv_passthrough.tex`): IV robustness — inverse-distance-weighted cross-state cost instrument `Z_ist` (from store lat/lon). Runs when `RUN_IV = TRUE` and `store_info` has lat/lon. 
- **Figure 12** (`12_fig_passthrough_coef.png`): Coefficients, both specs
- **Figure 13** (`13_fig_passthrough_duration.png`): Implied pass-through by weeks since SOE

The duration extension runs when `RUN_DUR_EXTENSION = TRUE` (default).

---

### Mechanism 3: Countercyclical Pricing

**`08_promotional_expansion.R`** → Tables 21–26; Figures 19–21

The net price decline is a promotional-frequency phenomenon: posted shelf prices
were flat, the share of transactions on sale rose sharply, and net prices fell.
The script documents this channel and decomposes the coincident rise in quantity
sold into extensive and intensive margins, following Butters, Sacks, and Seo
(2025). Requires `stg.pd_store_upc_week` and `stg.pd_ext_int_week` (run
`BuildMarkupsNew_PromoExpansion.sql` first) and, for the category robustness,
`stg.store_category_week` (built by the category rollup at the end of
`BuildMarkupsNew_2026_04.sql`).

Implemented (file numbers match the order tables appear in the paper's Section 6.3):

- **M3a — gross–net gap** (`24_tab_gross_net_gap.tex`, `19_fig_gross_net_gap.png`): Did the per-unit promotional discount widen during the SOE?
- **M3b — promotional intensity** (`22_tab_promo_intensity.tex`, `20_fig_promo_intensity.png`): Did `share_on_sale` and discount depth change?
- **M3c — gross price stability** (`21_tab_gross_price_stability.tex`): Was the posted shelf price flat while the net price fell?
- **M3d — extensive/intensive** (`23_tab_extensive_intensive.tex`, `21_fig_extensive_intensive.png`): Decompose the SOE quantity increase into extensive (purchase occasions) and intensive (volume per occasion) margins; ln *Q* = ln *N* + ln(*Q*/*N*).
- **Category robustness** (`25_tab_category_price_promo.tex`, `26_tab_category_decomp.tex`): Re-estimate at the store-category-week level over all UPCs in each focal category (requires `stg.store_category_week`).

---

## SQL Scripts

| File | Purpose |
|------|---------|
| `BuildMarkupsNew_2026_04.sql` | Builds the `stg.pos_*` product weekly panels and supporting tables. Run before the R scripts. |
| `BuildMarkupsNew_PromoExpansion.sql` | Builds `stg.pd_transactions`, `stg.pd_store_upc_day`, and `stg.pd_store_upc_week` for Mechanism 3.|

---

## Standalone / Non-Pipeline Scripts

Not called by `run_all.R`.

| Script | Purpose |
|--------|---------|
| `apg_analysis.R` | **Deprecated** monolithic script; superseded by the numbered scripts. Use `run_all.R`. |
| `cpi/cpi_rebase.R` | CPI diagnostic: YoY inflation and deflator series (visualization/verification) |

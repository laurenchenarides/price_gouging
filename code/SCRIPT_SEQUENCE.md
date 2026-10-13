# Code Directory — Script Reference

This folder contains all R and SQL scripts for the analysis. Run everything via
`run_all.R` from the project root (open `pg_project.Rproj` first so the working
directory is set). See the root `README.md` for the full output inventory and
embedded figures.

---

## Execution Order

`run_all.R` sources the numbered scripts in this order:

```
00 → 01 → 02 → 03 → 04 → 05 → 06 → 07 → 08 → 09 → 10
```

Scripts 06–08 correspond to Mechanisms 1, 2, and 3 respectively. Script 09
estimates demand elasticity and curvature; script 10 covers inventory and the
supply side. Neither is in the manuscript yet. Script 10 runs only when
`RUN_INVENTORY = TRUE` (default), because its first run scans
`stg.combined_inventory`.

Section labels inside each script follow the script number (Sections 2.1–2.4
in `02`, 5.1–5.4 in `05`, and so on). The three mechanism scripts use mechanism
labels instead: M1a–M1i in `06`, M2a–M2e in `07`, M3a–M3e in `08`.

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
Takes weekly first differences (`dP`, `dW`, `dM`), plus the shelf-price change
`dP_gross` and the lagged promotion share `share_on_sale_lag`, which are used
only in the non-promotional pass-through robustness in `07_passthrough.R`.
Trims the top/bottom 1% of within-product weekly log price and cost changes.
Defines `save_tex()`, used by all downstream table-writing scripts.

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

**`05_regressions.R`** → Tables 08, 08b, 08c, 08d, 09, 09b, 10, 10b–10e, 11, 11b; Figures 08, 08b, 09–11

Specification: `P_ist = α + β₁·SOE_st + β₂·postSOE_st + γ_j + δ_i + ε_ist`.
Fixed effects: product and store. Standard errors clustered at the state level,
with wild cluster bootstrap inference (Webb weights, *B* = 9,999, *G* = 5).
Tables 08 and 10 report within-*R*² alongside *R*².

- **Table 08** (`08_tab_price_reg.tex`): Baseline price regressions
- **Table 08b** (`08b_tab_price_wcb.tex`): Wild cluster bootstrap inference (price)
- **Table 08c** (`08c_tab_price_robust.tex`): Price robustness — excluding bananas; post-SOE window capped at 26 and 52 weeks after expiry (later post-SOE weeks dropped, not recoded)
- **Table 08d** (`08d_tab_loso_jackknife.tex`): Leave-one-state-out price and margin estimates, with jackknife SE
- **Table 09** (`09_tab_price_reg_state_heterog.tex`): State-heterogeneous price effects
- **Table 09b** (`09b_tab_price_state_heterog_capped.tex`): State-heterogeneous price effects with capped post-SOE windows (checks the Mississippi post-SOE outlier)
- **Table 10** (`10_tab_margin_reg.tex`): Baseline margin regressions
- **Table 10b** (`10b_tab_margin_wcb.tex`): Wild cluster bootstrap inference (margin)
- **Table 10c** (`10c_tab_cost_reg.tex`): Wholesale cost regressions (appendix)
- **Table 10d** (`10d_tab_cost_reg_state_heterog.tex`): State-heterogeneous cost effects
- **Table 10e** (`10e_tab_margin_robust.tex`): Margin robustness — excluding bananas; capped post-SOE windows
- **Table 11** (`11_tab_margin_reg_state_heterog.tex`): State-heterogeneous margin effects
- **Table 11b** (`11b_tab_margin_state_heterog_capped.tex`): State-heterogeneous margin effects with capped post-SOE windows
- **Figures 08–11**: Coefficient plots for price and margin, baseline and by state
- **Figure 08b** (`08b_fig_price_event_study.png`): Event study of price around SOE activation (weeks −26 to +52, reference week −1). All states activated in the same week, so week FEs cannot be included; this checks timing, not a difference-in-differences. The joint pre-trend test uses store-clustered SEs because 5 state clusters cannot support a joint test of 25 leads.

---

### Mechanism 1: Constant Retail Prices

**`06_uniform_pricing.R`** → Tables 15–20 (incl. 17b–17d, 18b–18c), Figures 14–18

Outcome: mean absolute log price difference across store pairs within a
retailer-product-week. Store pairs are classified by joint enforcement status
(pre / during / post / mixed). Tests whether within-chain uniformity changed
during the SOE and whether mixed (cross-state) enforcement status broke uniform
pricing.

- **Tables 15–16** (`15_..._summary_retail`, `16_..._summary_wholesale`): log-diff summaries by period
- **Tables 17 / 17b / 17c** (`17_..._retail_main`, `17b_..._retail_robust`, `17c_..._retail_crossstate`): retail uniformity — main, month-FE robustness, cross-state (geography) control
- **Tables 18 / 18b / 18c** (`18_..._wholesale_main`, `18b_..._wholesale_robust`, `18c_..._wholesale_crossstate`): wholesale uniformity — main, robustness, cross-state control
- **Table 17d** (`17d_tab_mixed_mde.tex`): minimum detectable effect (5% size, 80% power) for the mixed-status coefficient in each retail specification, also expressed relative to pre-SOE price dispersion
- **Tables 19–20** (`19_..._heterog_retail`, `20_..._heterog_wholesale`): retailer heterogeneity
- **Figures 14–15**: pooled log-diff distributions (retail, wholesale)
- **Figures 16–17**: log-diff distributions by enforcement status
- **Figure 18**: retailer heterogeneity coefficients

---

### Mechanism 2: Variation in Pass-Through

**`07_passthrough.R`** → Tables 12, 12b, 12c, 12d, 13, 14; Figures 12–13

Specification:
`ΔP_ist = α + β₁·Δw + β₂·(Δw×SOE) + β₃·(Δw×postSOE) + γ_j + δ_i + τ_t`.
The preferred specification includes week FEs. The identifying variation
(`Δw × SOE`) varies across stores within a week and is not absorbed by week FEs.

- **Table 12** (`12_tab_passthrough_reg.tex`): Pass-through, no week FE vs. week FE
- **Table 12b** (`12b_tab_passthrough_log.tex`): Pass-through in logs (robustness)
- **Table 13** (`13_tab_passthrough_duration.tex`): Duration extension (`Δw × SOE × Dur_wk`)
- **Table 14** (`14_tab_passthrough_implied_soe.tex`): Implied SOE pass-through at selected durations
- **Table 12c** (`12c_tab_iv_passthrough.tex`): IV robustness — inverse-distance-weighted cross-state cost instrument `Z_ist` (from store lat/lon). Runs when `RUN_IV = TRUE` and `store_info` has lat/lon.
- **Table 12d** (`12d_tab_passthrough_robust.tex`): Preferred specification re-estimated on (2) the posted shelf price change, (3)–(4) bananas and lettuce only (`PT_NONPROMO_PRODUCTS`), on all weeks and on non-promotional weeks (current and previous week both at or below `PT_NONPROMO_MAX_SHARE`, default 0), (5) excluding bananas, and (6)–(7) post-SOE windows capped at 26 and 52 weeks. Column (2) is the main test of whether the SOE attenuation is separate from the rise in promotions: if shelf-price pass-through did not fall, the attenuation comes from promotions. The non-promotional check is limited to bananas and lettuce because 90–95% of store-weeks for cabbage, cucumbers, and tomatoes had a promotion during the SOE. The script prints how much of each product-period survives the filter.
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
- **M3e — category robustness** (`25_tab_category_price_promo.tex`, `26_tab_category_decomp.tex`): Re-estimate at the store-category-week level over all UPCs in each focal category (requires `stg.store_category_week`).

---

### Demand Elasticity and Curvature

**`09_elasticities.R`** → Tables 27, 27b, 28, 28b, 29; Figure 22

Estimates how price sensitivity and the curvature of demand changed before,
during, and after the SOE, and links the curvature change to the pass-through
attenuation in Mechanism 2 through Weyl and Fabinger (2013). Analogue of
Butters, Sacks, and Seo (2025, eq. 3):
`ln Q_ist = a_SOE·SOE + a_post·postSOE + η·x + η_SOE·(x×SOE) + η_post·(x×postSOE) + θ_store×product×year + φ_product×week-of-year`,
where `x` is the log real net price minus its store-product-year mean.
Standard errors are clustered by store (`ELAS_CLUSTER`).

- **Table 27** (`27_tab_elasticity_period.tex`): Elasticity regressions — (1) OLS; (2) IV with own wholesale cost; (3) IV with the same chain's mean price in other states, same product and week (skipped if `ELAS_USE_HAUSMAN_IV = FALSE`); (4) OLS with product × week FEs, which absorb common weekly demand shocks
- **Table 27b** (`27b_tab_elasticity_by_period.tex`): Implied elasticity for pre-SOE, SOE, post-SOE, and the SOE change, delta-method SEs
- **Table 28** (`28_tab_elasticity_curvature.tex`): Adds `x²` and its period interactions (OLS and own-cost IV)
- **Table 28b** (`28b_tab_curvature_implied.tex`): For each period, the elasticity at the period's mean price *e*, curvature κ, the inverse elasticity of marginal surplus 1/ε_ms = (*e* − 2κ)/*e*² (positive = log-concave), and implied pass-through 1/(1 + 1/ε_ms) for a monopolist with constant marginal cost; plus the SOE change in 1/ε_ms
- **Table 29** (`29_tab_elasticity_state.tex`): State-specific period elasticities, OLS and own-cost IV
- **Figure 22** (`22_fig_elasticity_period.png`): Elasticity by period across specifications

Limitations stated in the script header: there are three regimes, not repeated
seasonal cycles, so the SOE elasticity rests on within-window price variation;
the two IVs rest on different exclusion restrictions; and conduct θ is held
constant across periods.

---

### Inventory and Supply Side

**`10_inventory_bottleneck.R`** → Tables 30–33; Figures 23–24; coverage CSVs

Uses store-day inventory on hand from `stg.combined_inventory` (read-only
queries; not Argo, which reports cases). Covers cabbage (4069), cucumbers (4062),
and lettuce (7143001065) for 2019–2021: bananas and tomatoes have no inventory
records, 2018 is nearly empty for these items, and no inventory table extends
past December 2021. The first run caches the pull to `data/` (`INV_USE_CACHE`);
delete the `.rds` files to force a fresh pull.

- **10.1 Coverage check**: Monthly share of panel stores with inventory records for each focal item. The analysis window ends at the first sustained drop (coverage below `INV_MIN_COVERAGE` × pre-SOE median for two or more months), unless `INV_IGNORE_COVERAGE_DROP = TRUE`. Also writes the item universe (distinct UPCs, rows, stores) by month and source, to explain the 2021 drop in distinct UPCs (`RUN_INV_DIAGNOSTICS`). Outputs: `30_tab_inventory_coverage.tex`, `23_fig_inventory_coverage.png`, `tables_csv/30_inventory_coverage_monthly.csv`, `tables_csv/30_inventory_upc_universe_monthly.csv`
- **10.2 Weeks of supply**: Mean daily on-hand units divided by weekly volume. Prints a unit sanity check; log regressions with store-product FEs are unaffected by a constant unit difference between inventory and POS volume
- **Table 31** (`31_tab_inventory_soe.tex`, `24_fig_weeks_of_supply.png`): SOE effects on log inventory, log volume, and log weeks of supply (the coefficients add up)
- **Table 32** (`32_tab_inventory_promo.tex`): Does last week's weeks of supply predict this week's share on sale? Store level, store level within chain-week, and chain-state level
- **Table 33** (`33_tab_shrink_bound.tex`): Back-of-envelope share of the SOE margin decline that lower shrink from faster turnover could explain, under assumed pre-SOE shrink rates (`SHRINK_BASE_RATES`, default 5/10/15%). Shrink is not observed; these are assumptions, not estimates

---

## SQL Scripts

| File | Purpose |
|------|---------|
| `BuildMarkupsNew_2026_04.sql` | Builds the `stg.pos_*` product weekly panels and supporting tables. Run before the R scripts. |
| `BuildMarkupsNew_PromoExpansion.sql` | Builds `stg.pd_transactions`, `stg.pd_store_upc_day`, `stg.pd_store_upc_week`, and `stg.pd_ext_int_week` for Mechanism 3.|

`10_inventory_bottleneck.R` reads the existing `stg.combined_inventory` table
directly and needs no build step.

---

## Standalone / Non-Pipeline Scripts

Not called by `run_all.R`.

| Script | Purpose |
|--------|---------|
| `apg_analysis.R` | **Deprecated** monolithic script; superseded by the numbered scripts. Use `run_all.R`. |
| `cpi/cpi_rebase.R` | CPI diagnostic: YoY inflation and deflator series (visualization/verification) |

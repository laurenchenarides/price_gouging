# ==============================================================================
# 06_uniform_pricing.R
#
# Mechanisms: Mechanism 1 (Constant Retail Prices / Uniform Pricing)
#
# ------------------------------------------------------------------------------
# CONSTRUCTION OF MATCHED PAIRS
#
#   Self-join keys reduced to c("retailer","product","week_seq","week_start").
#   Each pair carries BOTH stores' enforcement status and is classified as
#   "Pre-SOE" / "During SOE" / "Post-SOE" (both stores same) or "Mixed status"
#   (one During, one Post - the only feasible mix given simultaneous activation).
#   Regressions include a `mixed` dummy (omitted category: Pre-SOE pairs).
#
# CROSS-STATE (GEOGRAPHY) CONTROL
#
#   `mixed` pairs are ALWAYS cross-state, while the Pre/During/Post cells pool
#   within- and cross-state pairs. So the raw `mixed` coefficient conflates
#   "straddles an enforcement border" with "is in two different states"
#   (different distribution centers, geography, product mix). Because APG laws
#   do NOT constrain wholesale acquisition cost, a positive `mixed` coefficient
#   on WHOLESALE cost is a direct signal of this geography confound.
#
#   This version adds a `cross_state` indicator ( = 1 if the two stores are in
#   different states) and reports `mixed` NET of it, on a finer panel split by
#   cross_state. Interpretation of the new coefficients:
#     cross_state = extra dispersion of cross-state pairs vs within-state pairs
#                   of the SAME enforcement status (pure geography/composition).
#     mixed       = extra dispersion of enforcement-straddling pairs BEYOND
#                   being cross-state, i.e. the APG-border effect net of
#                   geography. mixed ~ 0 once cross_state is controlled supports
#                   the uniform-pricing conclusion.
#   `mixed` is separately identified from `cross_state` because cross_state = 1
#   also occurs with mixed = 0 (cross-state Pre/During/Post pairs).
#
#   New outputs (17c/18c) present the raw and net-of-geography models side by
#   side on the SAME finer-grain panel, so the only difference between columns
#   is the cross_state control. Point estimates in 17c/18c differ slightly from
#   Tables 17/18 because the cell grain differs; Tables 17/18 remain the primary,
#   paper-comparable specification and are UNCHANGED by this update.
#
# ------------------------------------------------------------------------------
# NOTE ON N:
#   The collapsed pair-status panel has more rows than unique retailer-product-
#   weeks, but now BY DESIGN: straddle weeks contribute up to three interpretable
#   pair-status cells (During, Post, Mixed) rather than two mislabeled ones.
#
# AUTHOR DECISION FLAG (default preserves closest comparability with original):
#   UNIF_WEIGHT_BY_PAIRS — weight cells by n_pairs. Default FALSE (unweighted,
#   as in the original). Retailer 2 contributes far more pairs per cell than
#   retailers 3/5, so consider TRUE as robustness.
#
# Because sample construction changed, ALL outputs of this script (paper Tables
# 10, 11, D.21-D.26; Figures 9-12, D.15) must be regenerated and re-transcribed.
#
# Depends on: panel_est, save_tex(), SAVE_CSV  (from run_all.R / 02_build_panel.R)
#
# Outputs (tables_latex/):
#   15_tab_uniformity_summary_retail.tex
#   16_tab_uniformity_summary_wholesale.tex
#   17_tab_uniformity_retail_main.tex
#   17b_tab_uniformity_retail_robust.tex
#   17c_tab_uniformity_retail_crossstate.tex      (NEW: mixed net of geography)
#   18_tab_uniformity_wholesale_main.tex
#   18b_tab_uniformity_wholesale_robust.tex
#   18c_tab_uniformity_wholesale_crossstate.tex   (NEW: mixed net of geography)
#   19_tab_uniformity_heterog_retail.tex
#   20_tab_uniformity_heterog_wholesale.tex
#
# Outputs (figures/):
#   14_fig_logdiff_retail_pooled.png
#   15_fig_logdiff_wholesale_pooled.png
#   16_fig_logdiff_retail_by_period.png     (4 facets incl. Mixed status)
#   17_fig_logdiff_wholesale_by_period.png  (4 facets incl. Mixed status)
#   18_fig_uniformity_heterog_coef.png
# ==============================================================================

message("Estimating Mechanism 1 uniform pricing (C1-fixed + cross-state control) ...")

# ---- Author-decision flag ----------------------------------------------------
if (!exists("UNIF_WEIGHT_BY_PAIRS")) UNIF_WEIGHT_BY_PAIRS <- FALSE

unif_panel <- panel_est %>%
  filter(p_ist > 0, w_ist > 0) %>%
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

# Week -> month lookup (month_fe was previously a join key; it is a function of
# week_seq, so we re-attach it at the cell level instead).
week_month_lookup <- unif_panel %>%
  distinct(week_seq, month_fe)

stopifnot(!anyDuplicated(week_month_lookup$week_seq))  # one month per week

PAIR_STATUS_LEVELS <- c("Pre-SOE", "During SOE", "Post-SOE", "Mixed status")

# ==============================================================================
# PAIRWISE LOG DIFFERENCES  (C1 FIX: period/month_fe removed from join keys)
# ==============================================================================

make_pairwise_logdiff <- function(df, price_col) {
  df_in <- df %>%
    select(
      retailer = retailer_id, store_id, sst, product,
      week_seq, week_start, period,
      price = !!rlang::sym(price_col)
    ) %>%
    group_by(retailer, week_seq) %>%
    mutate(storeno = row_number()) %>%
    ungroup()
  
  inner_join(
    df_in,
    df_in %>% rename(price2  = price,  storeno2 = storeno,
                     store_id2 = store_id, sst2 = sst, period2 = period),
    # C1 FIX: join on retailer-product-week ONLY, so pairs that straddle
    # enforcement status (period != period2) are RETAINED.
    by = c("retailer", "product", "week_seq", "week_start"),
    relationship = "many-to-many"
  ) %>%
    filter(storeno < storeno2) %>%
    mutate(
      diff = abs(log(price) - log(price2)),
      pair_status = if_else(period == period2,
                            as.character(period),
                            "Mixed status"),
      pair_status = factor(pair_status, levels = PAIR_STATUS_LEVELS),
      # Geography: are the two stores in different states?
      cross_state = if_else(as.character(sst) != as.character(sst2), 1L, 0L)
    ) %>%
    select(retailer, product, week_seq, week_start,
           pair_status, period, period2, sst, sst2, cross_state, diff)
}

pairs_retail    <- make_pairwise_logdiff(unif_panel, "p_ist")
pairs_wholesale <- make_pairwise_logdiff(unif_panel, "w_ist")

# ---- Diagnostics for the C1 fix ----------------------------------------------
# (1) What period combinations occur? With simultaneous activation, mixed pairs
#     should be exclusively During/Post. Anything else needs investigation.
message("Pair-status composition (retail):")
pairs_retail %>%
  count(pair_status, period, period2) %>%
  arrange(pair_status, desc(n)) %>%
  print(n = 20)

mixed_check <- pairs_retail %>%
  filter(pair_status == "Mixed status") %>%
  count(combo = paste(pmin(as.character(period), as.character(period2)),
                      pmax(as.character(period), as.character(period2)),
                      sep = " x "))
message("Mixed-status combinations (expect only 'During SOE x Post-SOE'):")
print(mixed_check)

# (2) Mixed pairs must be cross-state (enforcement varies at the state level).
n_bad_mixed <- pairs_retail %>%
  filter(pair_status == "Mixed status", cross_state == 0L) %>%
  nrow()
if (n_bad_mixed > 0) {
  warning(sprintf(
    "%d mixed-status pairs are WITHIN the same state — check SOE coding.",
    n_bad_mixed
  ))
} else {
  message("OK: all mixed-status pairs are cross-state, as expected.")
}

# (3) How many pairs does the fix recover?
message(sprintf(
  "Mixed-status pairs recovered by the fix: retail = %s, wholesale = %s (previously dropped).",
  format(sum(pairs_retail$pair_status    == "Mixed status"), big.mark = ","),
  format(sum(pairs_wholesale$pair_status == "Mixed status"), big.mark = ",")
))

# (4) Cross-state coverage by retailer: cross_state is identified only from
#     chains operating in >1 state. Single-state chains contribute cross_state=0
#     cells only (they still anchor the within-state baseline).
message("Cross-state pair share by retailer (retail):")
pairs_retail %>%
  group_by(retailer) %>%
  summarise(share_cross_state = mean(cross_state), n_pairs = n(), .groups = "drop") %>%
  print()

# ==============================================================================
# DISTRIBUTION PLOTS
# ==============================================================================

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

# C1 FIX: facet on pair_status (4 panels: Pre, During, Post, Mixed) so the
# recovered cross-status pairs are visible rather than dropped.
plot_logdiff_by_status <- function(df, title_str, filename) {
  g <- ggplot(df, aes(x = diff)) +
    geom_histogram(aes(y = after_stat(count / sum(count))),
                   binwidth = COMMON_BINWIDTH, fill = "steelblue", color = "white") +
    coord_cartesian(xlim = X_LIM) +
    scale_y_continuous(labels = scales::label_percent()) +
    facet_wrap(~ pair_status, nrow = 1) +
    labs(title = title_str,
         subtitle = "Pair status: both stores' enforcement state. 'Mixed status' = one store under active enforcement, one post-enforcement (cross-state pairs).",
         x = "Absolute log price difference", y = "Percent of store pairs") +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8))
  ggsave(file.path("figures", filename), g, width = 13, height = 5, dpi = 300)
  message("Saved: figures/", filename)
  invisible(g)
}

plot_logdiff_hist(pairs_retail,    "Within-chain retail price uniformity (all periods)",
                  "14_fig_logdiff_retail_pooled.png")
plot_logdiff_hist(pairs_wholesale, "Within-chain wholesale cost uniformity (all periods)",
                  "15_fig_logdiff_wholesale_pooled.png")
plot_logdiff_by_status(pairs_retail,    "Within-chain retail price uniformity by pair enforcement status",
                       "16_fig_logdiff_retail_by_period.png")
plot_logdiff_by_status(pairs_wholesale, "Within-chain wholesale cost uniformity by pair enforcement status",
                       "17_fig_logdiff_wholesale_by_period.png")

# ==============================================================================
# SUMMARY TABLES: mean absolute log diff by retailer and pair status
# ==============================================================================

make_disp_summary <- function(pairs_df, caption_str, label_str,
                              filename_csv, filename_tex) {
  tbl <- pairs_df %>%
    mutate(retailer = paste0("Retailer ", retailer)) %>%
    group_by(retailer, `Pair status` = pair_status) %>%
    summarise(
      Count       = n(),
      Mean        = mean(diff,   na.rm = TRUE),
      Median      = median(diff, na.rm = TRUE),
      `Std. dev.` = sd(diff,     na.rm = TRUE),
      Variance    = var(diff,    na.rm = TRUE),
      Max         = max(diff,    na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    arrange(retailer, `Pair status`) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))
  
  if (SAVE_CSV) write.csv(tbl, file.path("tables_csv", filename_csv), row.names = FALSE)
  
  save_tex(
    kbl(tbl,
        format = "latex", booktabs = TRUE,
        caption = caption_str, label = label_str,
        align   = "llrrrrrr",
        format.args = list(big.mark = ",")) %>%
      collapse_rows(columns = 1, latex_hline = "major", valign = "top") %>%
      kable_styling(latex_options = c("hold_position", "scale_down")),
    filename_tex
  )
  message("Saved: tables_latex/", filename_tex)
}

make_disp_summary(
  pairs_retail,
  caption_str  = "Within-chain retail price uniformity by retailer and pair enforcement status. Values are absolute log retail price differences across store pairs within the same chain, product, and week. Pair status reflects the enforcement state of both stores; ``Mixed status'' pairs have one store under active enforcement and one post-enforcement (necessarily cross-state).",
  label_str    = "uniformity_summary_retail",
  filename_csv = "15_tab_uniformity_summary_retail.csv",
  filename_tex = "15_tab_uniformity_summary_retail.tex"
)

make_disp_summary(
  pairs_wholesale,
  caption_str  = "Within-chain wholesale cost uniformity by retailer and pair enforcement status. Values are absolute log wholesale cost differences across store pairs within the same chain, product, and week. Pair status reflects the enforcement state of both stores; ``Mixed status'' pairs have one store under active enforcement and one post-enforcement (necessarily cross-state).",
  label_str    = "uniformity_summary_wholesale",
  filename_csv = "16_tab_uniformity_summary_wholesale.csv",
  filename_tex = "16_tab_uniformity_summary_wholesale.tex"
)

# ==============================================================================
# PRIMARY REGRESSION PANEL: retailer-product-week-PAIR_STATUS cells
# ==============================================================================
# Paper-comparable specification (Tables 17/18). Cells defined by pair status;
# straddle weeks contribute up to three cells (During, Post, Mixed).
# ==============================================================================

make_uniformity_panel <- function(pairs_df) {
  pairs_df %>%
    group_by(retailer, product, week_seq, pair_status) %>%
    summarise(
      Diff_bar = mean(diff, na.rm = TRUE),
      n_pairs  = n(),
      .groups  = "drop"
    ) %>%
    left_join(week_month_lookup, by = "week_seq") %>%
    mutate(
      during           = if_else(pair_status == "During SOE",   1L, 0L),
      post             = if_else(pair_status == "Post-SOE",     1L, 0L),
      mixed            = if_else(pair_status == "Mixed status", 1L, 0L),
      retailer_product = interaction(retailer, product, drop = TRUE)
    )
}

disp_retail    <- make_uniformity_panel(pairs_retail)
disp_wholesale <- make_uniformity_panel(pairs_wholesale)

message(sprintf(
  "Primary panel (retail): %s cells across %s unique retailer-product-weeks; %s Mixed-status cells.",
  format(nrow(disp_retail), big.mark = ","),
  format(nrow(distinct(disp_retail, retailer, product, week_seq)), big.mark = ","),
  format(sum(disp_retail$mixed), big.mark = ",")
))

# ==============================================================================
# POOLED UNIFORMITY REGRESSIONS (primary — paper Tables 17/18)
# ==============================================================================
# Diff_bar ~ during + post + mixed | FEs, omitted category = Pre-SOE pairs.
# Weights: n_pairs if UNIF_WEIGHT_BY_PAIRS (author decision; default FALSE).
# ==============================================================================

unif_weights <- if (UNIF_WEIGHT_BY_PAIRS) ~ n_pairs else NULL

run_uniformity_regs <- function(df) {
  list(
    main_ret      = feols(Diff_bar ~ during + post + mixed | retailer,
                          data = df, cluster = ~ retailer_product, weights = unif_weights),
    main_prod     = feols(Diff_bar ~ during + post + mixed | product,
                          data = df, cluster = ~ retailer_product, weights = unif_weights),
    main_ret_prod = feols(Diff_bar ~ during + post + mixed | retailer + product,
                          data = df, cluster = ~ retailer_product, weights = unif_weights),
    rob_ret       = feols(Diff_bar ~ during + post + mixed | retailer + month_fe,
                          data = df, cluster = ~ retailer_product, weights = unif_weights),
    rob_prod      = feols(Diff_bar ~ during + post + mixed | product + month_fe,
                          data = df, cluster = ~ retailer_product, weights = unif_weights),
    rob_ret_prod  = feols(Diff_bar ~ during + post + mixed | retailer + product + month_fe,
                          data = df, cluster = ~ retailer_product, weights = unif_weights)
  )
}

regs_retail    <- run_uniformity_regs(disp_retail)
regs_wholesale <- run_uniformity_regs(disp_wholesale)

UNIF_DICT <- c("during"      = "During SOE (both stores)",
               "post"        = "Post-SOE (both stores)",
               "mixed"       = "Mixed status (one during, one post)",
               "cross_state" = "Cross-state pair (geography)")
UNIF_NOTES_MAIN <- c(
  "Unit of observation: retailer-product-week-pair-status cell.",
  "Omitted category: pairs with both stores pre-SOE.",
  "``Mixed status'' cells contain cross-state store pairs with one store under",
  "active enforcement and one post-enforcement; these identify whether chains",
  "broke uniform pricing across enforcement borders.",
  if (UNIF_WEIGHT_BY_PAIRS) "Cells weighted by the number of store pairs." else
    "Cells unweighted (set UNIF_WEIGHT_BY_PAIRS = TRUE for pair-count weights).",
  "Standard errors clustered at the retailer-product level."
)
UNIF_NOTES_ROB <- c("Robustness checks add month fixed effects to each specification.",
                    UNIF_NOTES_MAIN)

# Retail — main
etable(
  list("(1) Retailer FE"           = regs_retail$main_ret,
       "(2) Product FE"            = regs_retail$main_prod,
       "(3) Retailer + Product FE" = regs_retail$main_ret_prod),
  tex = TRUE, file = "tables_latex/17_tab_uniformity_retail_main.tex",
  title   = "Within-chain retail price uniformity: main results (pair-status design)",
  label   = "tab:uniformity_retail_main",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = UNIF_DICT, notes = UNIF_NOTES_MAIN
)
message("Saved: tables_latex/17_tab_uniformity_retail_main.tex")

# Retail — robustness
etable(
  list("(1) Retailer + Month FE"          = regs_retail$rob_ret,
       "(2) Product + Month FE"           = regs_retail$rob_prod,
       "(3) Retailer + Product + Month FE" = regs_retail$rob_ret_prod),
  tex = TRUE, file = "tables_latex/17b_tab_uniformity_retail_robust.tex",
  title   = "Within-chain retail price uniformity: robustness (month FEs added)",
  label   = "tab:uniformity_retail_robust",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = UNIF_DICT, notes = UNIF_NOTES_ROB
)
message("Saved: tables_latex/17b_tab_uniformity_retail_robust.tex")

# Wholesale — main
etable(
  list("(1) Retailer FE"           = regs_wholesale$main_ret,
       "(2) Product FE"            = regs_wholesale$main_prod,
       "(3) Retailer + Product FE" = regs_wholesale$main_ret_prod),
  tex = TRUE, file = "tables_latex/18_tab_uniformity_wholesale_main.tex",
  title   = "Within-chain wholesale cost uniformity: main results (pair-status design)",
  label   = "tab:uniformity_wholesale_main",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = UNIF_DICT, notes = UNIF_NOTES_MAIN
)
message("Saved: tables_latex/18_tab_uniformity_wholesale_main.tex")

# Wholesale — robustness
etable(
  list("(1) Retailer + Month FE"          = regs_wholesale$rob_ret,
       "(2) Product + Month FE"           = regs_wholesale$rob_prod,
       "(3) Retailer + Product + Month FE" = regs_wholesale$rob_ret_prod),
  tex = TRUE, file = "tables_latex/18b_tab_uniformity_wholesale_robust.tex",
  title   = "Within-chain wholesale cost uniformity: robustness (month FEs added)",
  label   = "tab:uniformity_wholesale_robust",
  digits  = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict    = UNIF_DICT, notes = UNIF_NOTES_ROB
)
message("Saved: tables_latex/18b_tab_uniformity_wholesale_robust.tex")

# ==============================================================================
# CROSS-STATE (GEOGRAPHY) CONTROL: is `mixed` an enforcement effect or geography?
# ==============================================================================
# Finer panel: split each cell by cross_state. Then run the SAME model with and
# without the cross_state control so the only difference between columns is the
# geography control. `mixed` in the controlled column is the enforcement-border
# effect NET of being cross-state.
#
# Identification: cross_state = 1 also occurs with mixed = 0 (cross-state Pre/
# During/Post pairs), so `mixed` and `cross_state` are separately identified.
# cross_state is identified from chains operating in >1 state (retailers 2, 3);
# single-state chains (retailer 5) contribute only cross_state = 0 cells.
# ==============================================================================

make_uniformity_panel_xs <- function(pairs_df) {
  pairs_df %>%
    group_by(retailer, product, week_seq, pair_status, cross_state) %>%
    summarise(
      Diff_bar = mean(diff, na.rm = TRUE),
      n_pairs  = n(),
      .groups  = "drop"
    ) %>%
    left_join(week_month_lookup, by = "week_seq") %>%
    mutate(
      during           = if_else(pair_status == "During SOE",   1L, 0L),
      post             = if_else(pair_status == "Post-SOE",     1L, 0L),
      mixed            = if_else(pair_status == "Mixed status", 1L, 0L),
      retailer_product = interaction(retailer, product, drop = TRUE)
    )
}

disp_retail_xs    <- make_uniformity_panel_xs(pairs_retail)
disp_wholesale_xs <- make_uniformity_panel_xs(pairs_wholesale)

# Sanity: mixed cells must all be cross_state = 1.
stopifnot(all(disp_retail_xs$cross_state[disp_retail_xs$mixed == 1L]    == 1L))
stopifnot(all(disp_wholesale_xs$cross_state[disp_wholesale_xs$mixed == 1L] == 1L))

message(sprintf(
  "Cross-state panel (retail): %s cells (%s cross-state, %s within-state).",
  format(nrow(disp_retail_xs), big.mark = ","),
  format(sum(disp_retail_xs$cross_state == 1L), big.mark = ","),
  format(sum(disp_retail_xs$cross_state == 0L), big.mark = ",")
))

# Preferred FE = retailer + product (matches primary col 3); + month as robustness.
run_xs_pair <- function(df) {
  list(
    raw       = feols(Diff_bar ~ during + post + mixed | retailer + product,
                      data = df, cluster = ~ retailer_product, weights = unif_weights),
    net       = feols(Diff_bar ~ during + post + mixed + cross_state | retailer + product,
                      data = df, cluster = ~ retailer_product, weights = unif_weights),
    net_month = feols(Diff_bar ~ during + post + mixed + cross_state | retailer + product + month_fe,
                      data = df, cluster = ~ retailer_product, weights = unif_weights)
  )
}

xs_retail    <- run_xs_pair(disp_retail_xs)
xs_wholesale <- run_xs_pair(disp_wholesale_xs)

XS_NOTES <- c(
  "Unit of observation: retailer-product-week-pair-status-crossstate cell (finer",
  "than Tables 17/18; point estimates therefore differ slightly).",
  "Column (1) omits the cross-state control; columns (2)-(3) add it.",
  "``Cross-state pair'' = the two stores are in different states (geography).",
  "``Mixed status'' pairs are always cross-state, so the mixed coefficient in",
  "columns (2)-(3) is the enforcement-border effect NET of geography.",
  "Omitted category: within-state pairs with both stores pre-SOE.",
  "Standard errors clustered at the retailer-product level."
)

etable(
  list("(1) Raw (no geo. control)" = xs_retail$raw,
       "(2) + Cross-state"         = xs_retail$net,
       "(3) + Cross-state + Month FE" = xs_retail$net_month),
  tex = TRUE, file = "tables_latex/17c_tab_uniformity_retail_crossstate.tex",
  title  = "Within-chain retail price uniformity: mixed-status effect net of cross-state geography",
  label  = "tab:uniformity_retail_crossstate",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict   = UNIF_DICT, notes = XS_NOTES
)
message("Saved: tables_latex/17c_tab_uniformity_retail_crossstate.tex")

etable(
  list("(1) Raw (no geo. control)" = xs_wholesale$raw,
       "(2) + Cross-state"         = xs_wholesale$net,
       "(3) + Cross-state + Month FE" = xs_wholesale$net_month),
  tex = TRUE, file = "tables_latex/18c_tab_uniformity_wholesale_crossstate.tex",
  title  = "Within-chain wholesale cost uniformity: mixed-status effect net of cross-state geography",
  label  = "tab:uniformity_wholesale_crossstate",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  dict   = UNIF_DICT, notes = XS_NOTES
)
message("Saved: tables_latex/18c_tab_uniformity_wholesale_crossstate.tex")

# ---- Console comparison: does `mixed` shrink once geography is controlled? ----
get_coef <- function(m, nm) if (nm %in% names(coef(m))) unname(coef(m)[nm]) else NA_real_
mixed_compare <- tibble::tibble(
  outcome     = c("Retail", "Retail", "Wholesale", "Wholesale"),
  spec        = c("raw (no cross_state)", "net of cross_state",
                  "raw (no cross_state)", "net of cross_state"),
  mixed       = c(get_coef(xs_retail$raw, "mixed"),    get_coef(xs_retail$net, "mixed"),
                  get_coef(xs_wholesale$raw, "mixed"), get_coef(xs_wholesale$net, "mixed")),
  cross_state = c(NA_real_, get_coef(xs_retail$net, "cross_state"),
                  NA_real_, get_coef(xs_wholesale$net, "cross_state"))
) %>%
  mutate(across(c(mixed, cross_state), ~round(.x, 4)))

message("Mixed-status coefficient, raw vs net of cross-state geography:")
print(mixed_compare)
message("If `mixed` shrinks toward zero from raw to net, the raw mixed effect ",
        "was largely geography, not an APG-border break in uniform pricing.")

if (SAVE_CSV) {
  write.csv(mixed_compare, "tables_csv/17c_mixed_crossstate_compare.csv", row.names = FALSE)
}

# ==============================================================================
# RETAILER HETEROGENEITY REGRESSIONS  (mixed interactions added)
# ==============================================================================
# Only chains operating in >1 state can have mixed-status pairs. Per Table A.16,
# retailer 2 operates in all five states, retailer 3 in FL/GA, retailer 5 in FL
# only — so i(retailer, mixed) is estimable only for retailers 2 and 3, and
# fixest drops the empty retailer-5 interaction automatically (expected).
# ==============================================================================

run_heterog_regs <- function(df) {
  list(
    main_prod      = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) + i(retailer, mixed) | product,
                           data = df, cluster = ~ retailer_product, weights = unif_weights),
    rob_month      = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) + i(retailer, mixed) | month_fe,
                           data = df, cluster = ~ retailer_product, weights = unif_weights),
    rob_prod_month = feols(Diff_bar ~ 0 + i(retailer, during) + i(retailer, post) + i(retailer, mixed) | product + month_fe,
                           data = df, cluster = ~ retailer_product, weights = unif_weights)
  )
}

heterog_retail    <- run_heterog_regs(disp_retail)
heterog_wholesale <- run_heterog_regs(disp_wholesale)

HETEROG_NOTES <- c(
  "Each coefficient is a retailer-specific deviation from that retailer's pre-SOE pairs.",
  "``mixed'' = cross-state pairs with one store under active enforcement and one post-enforcement;",
  "only chains operating in more than one state can have mixed-status pairs.",
  "Standard errors clustered at the retailer-product level."
)

etable(
  list("(1) Product FE"         = heterog_retail$main_prod,
       "(2) Month FE"           = heterog_retail$rob_month,
       "(3) Product + Month FE" = heterog_retail$rob_prod_month),
  tex = TRUE, file = "tables_latex/19_tab_uniformity_heterog_retail.tex",
  title  = "Retailer heterogeneity in within-chain retail price uniformity (pair-status design)",
  label  = "tab:uniformity_heterog_retail",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes  = HETEROG_NOTES
)
message("Saved: tables_latex/19_tab_uniformity_heterog_retail.tex")

etable(
  list("(1) Product FE"         = heterog_wholesale$main_prod,
       "(2) Month FE"           = heterog_wholesale$rob_month,
       "(3) Product + Month FE" = heterog_wholesale$rob_prod_month),
  tex = TRUE, file = "tables_latex/20_tab_uniformity_heterog_wholesale.tex",
  title  = "Retailer heterogeneity in within-chain wholesale cost uniformity (pair-status design)",
  label  = "tab:uniformity_heterog_wholesale",
  digits = 3, se.below = TRUE, depvar = FALSE, fitstat = ~ n + r2,
  notes  = HETEROG_NOTES
)
message("Saved: tables_latex/20_tab_uniformity_heterog_wholesale.tex")

# ==============================================================================
# COEFFICIENT PLOTS FOR RETAILER HETEROGENEITY
# ==============================================================================

extract_heterog_coef <- function(heterog_list, outcome_label, specs) {
  purrr::map_dfr(specs, function(s) {
    broom::tidy(heterog_list[[s$key]], conf.int = TRUE) %>%
      mutate(spec = s$label)
  }) %>%
    filter(grepl(":during$|:post$|:mixed$", term)) %>%
    mutate(
      retailer = sub(".*retailer::([^:]+):.*", "\\1", term),
      period   = case_when(
        grepl(":during$", term) ~ "During SOE",
        grepl(":post$",   term) ~ "Post-SOE",
        TRUE                    ~ "Mixed status"
      ),
      outcome  = outcome_label
    )
}

all_specs <- list(
  list(key = "main_prod",      label = "Product FE"),
  list(key = "rob_month",      label = "Month FE"),
  list(key = "rob_prod_month", label = "Product + Month FE")
)

heterog_all_df <- bind_rows(
  extract_heterog_coef(heterog_retail,    "Retail",    all_specs),
  extract_heterog_coef(heterog_wholesale, "Wholesale", all_specs)
) %>%
  mutate(
    spec    = factor(spec,    levels = c("Product FE", "Month FE", "Product + Month FE")),
    period  = factor(period,  levels = c("During SOE", "Post-SOE", "Mixed status")),
    outcome = factor(outcome, levels = c("Retail", "Wholesale"))
  )

g_heterog <- ggplot(heterog_all_df,
                    aes(x = retailer, y = estimate, ymin = conf.low, ymax = conf.high,
                        color = period, shape = period)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  geom_text(aes(label = round(estimate, 3)),
            position = position_dodge(width = 0.6),
            vjust = -1.0, size = 2.8, show.legend = FALSE) +
  scale_shape_manual(values = c("During SOE" = 16, "Post-SOE" = 17, "Mixed status" = 15)) +
  facet_grid(outcome ~ spec) +
  labs(
    x     = "Retailer",
    y     = "Coefficient (mean absolute log price diff)",
    title = "Retailer heterogeneity in within-chain price uniformity (pair-status design)",
    color = NULL, shape = NULL
  ) +
  theme_bw() +
  theme(legend.position = "top", strip.text = element_text(size = 9))

ggsave("figures/18_fig_uniformity_heterog_coef.png", g_heterog,
       width = 11, height = 8, dpi = 300)
message("Saved: figures/18_fig_uniformity_heterog_coef.png")

# ==============================================================================
# OPTIONAL ALTERNATIVE DESIGN (not run): one row per retailer-product-week
# ==============================================================================
# If you prefer a strictly unique retailer-product-week panel, regress the
# cell-level Diff_bar (pooled over ALL pairs in the week) on the SHARES of pairs
# in each status (and, optionally, the share cross-state):
#
#   disp_alt <- pairs_retail %>%
#     group_by(retailer, product, week_seq) %>%
#     summarise(
#       Diff_bar     = mean(diff),
#       sh_during    = mean(pair_status == "During SOE"),
#       sh_post      = mean(pair_status == "Post-SOE"),
#       sh_mixed     = mean(pair_status == "Mixed status"),
#       sh_crossstate= mean(cross_state),
#       n_pairs      = n(), .groups = "drop"
#     )
#   feols(Diff_bar ~ sh_during + sh_post + sh_mixed + sh_crossstate |
#         retailer + product, data = disp_alt,
#         cluster = ~ interaction(retailer, product))
#
# Coefficients are then the effect of moving a cell from 0% to 100% of the given
# status/geography. Adopting one design as the headline is an author decision.
# ==============================================================================

message("Uniform pricing analysis complete (C1-fixed + cross-state control).")

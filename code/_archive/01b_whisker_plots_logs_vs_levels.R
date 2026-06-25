
## Step 0: build a single event-time variable k_soe
K <- 30  # window length

## ------------------------------------------------------------
## A) LOGS: build k_soe and restrict to [-K, K] around SoE episode
## ------------------------------------------------------------
df_log_es <- panel_est_trim %>%
  mutate(
    # event time around SoE episode
    k_soe = case_when(
      SoE == 1 ~ as.numeric(Dur_st),      # 0,1,2,... during SoE
      SoE == 0 & k_start < 0 ~ as.numeric(k_start),  # pre-activation: -1,-2,...
      SoE == 0 & k_end   > 0 ~ as.numeric(k_end),    # post-expiration: 1,2,...
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(k_soe), k_soe >= -K, k_soe <= K)

## ------------------------------------------------------------
## B) LEVELS: build k_soe and restrict to [-K, K]
## (Requires dp, dw, SoE, Dur_st, k_start, k_end in df_levels)
## If k_start/k_end/Dur_st are missing in panel_levels_trim, merge them in from panel_sc_week.
## ------------------------------------------------------------

# If panel_levels_trim already has SoE, Dur_st, k_start, k_end, you can skip this merge.
# Otherwise, uncomment and run:
# keys <- c("sst","store_id","category","week_seq")
# panel_levels_trim <- panel_levels_trim %>%
#   left_join(panel_sc_week %>% select(all_of(keys), SoE, Dur_st, k_start, k_end),
#             by = keys)

df_lvl_es <- panel_levels_trim %>%
  mutate(
    k_soe = case_when(
      SoE == 1 ~ as.numeric(Dur_st),
      SoE == 0 & k_start < 0 ~ as.numeric(k_start),
      SoE == 0 & k_end   > 0 ~ as.numeric(k_end),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(k_soe), k_soe >= -K, k_soe <= K)

ref_k <- -1

# Create factor for event time with a chosen reference
df_log_es <- df_log_es %>%
  mutate(k_soe_f = relevel(factor(k_soe), ref = as.character(ref_k)))

m_slope_log_es <- feols(
  dlnp ~ i(k_soe_f, dlnw, ref = as.character(ref_k)) | store_id^category + week_seq,
  cluster = ~ sst,
  data = df_log_es
)

etable(m_slope_log_es)

## Step 1: estimate slope-by-event-time models
## Logs (elasticity)
ref_k <- -1

# Create factor for event time with a chosen reference
df_log_es <- df_log_es %>%
  mutate(k_soe_f = relevel(factor(k_soe), ref = as.character(ref_k)))

m_slope_log_es <- feols(
  dlnp ~ i(k_soe_f, dlnw, ref = as.character(ref_k)) | store_id^category + week_seq,
  cluster = ~ sst,
  data = df_log_es
)

etable(m_slope_log_es)

## Levels ($ per $)

df_lvl_es <- df_lvl_es %>%
  mutate(k_soe_f = relevel(factor(k_soe), ref = as.character(ref_k)))

m_slope_lvl_es <- feols(
  dp ~ i(k_soe_f, dw, ref = as.character(ref_k)) | store_id^category + week_seq,
  cluster = ~ sst,
  data = df_lvl_es
)

etable(m_slope_lvl_es)

## Step 2: extract implied slopes + CI and plot (whisker) with a 1.0 line

extract_event_slopes <- function(model, k_vals, ref_k = -1, title_label) {
  b <- coef(model)
  V <- vcov(model)
  
  # Reference slope is the coefficient on the interacted regressor at ref (fixest i() uses naming)
  # We'll reconstruct slopes by reading the i() coefficients and adding them to the base slope.
  # In fixest, the base slope appears as the coefficient on dlnw (or dw) ONLY if included.
  # With i(k, x, ref), the "base" is the slope at ref embedded as the intercept in that interaction.
  # Easiest: recover slopes by using linear combinations via L vectors.
  
  cn <- names(b)
  
  # Identify the regressor name used inside i()
  # We will detect whether it's dlnw or dw from coef names
  xname <- if (any(grepl("dlnw", cn))) "dlnw" else "dw"
  
  # The i() terms look like: "k_soe_f::k#xname"
  # We'll build linear combos: slope at k = slope at ref + delta(k)
  # slope at ref is just the base slope at ref (which equals 0 delta by construction).
  # In fixest's i(), the delta terms are the coefficients themselves.
  # So slope at k = slope_ref + beta_k, and slope_ref is the omitted category's slope.
  #
  # To obtain slope_ref, we can compute it by fitting a model that includes x alone, but
  # simpler: compute slope_ref using predict-equivalent lincom:
  # slope at ref_k: set delta terms to 0 -> slope_ref is not directly in coef vector.
  #
  # Practical workaround: re-estimate with explicit x main effect + i(k, x, ref)
  # so the main effect is the slope at ref.
}

# Re-estimate with explicit main effect so slope at ref is identified directly:
m_slope_log_es2 <- feols(
  dlnp ~ dlnw + i(k_soe_f, dlnw, ref = as.character(ref_k)) | store_id^category + week_seq,
  cluster = ~ sst,
  data = df_log_es
)

m_slope_lvl_es2 <- feols(
  dp ~ dw + i(k_soe_f, dw, ref = as.character(ref_k)) | store_id^category + week_seq,
  cluster = ~ sst,
  data = df_lvl_es
)

lincombo_ci <- function(model, L, alpha = 0.05) {
  b <- coef(model); V <- vcov(model)
  est <- as.numeric(t(L) %*% b)
  se  <- sqrt(as.numeric(t(L) %*% V %*% L))
  z   <- qnorm(1 - alpha/2)
  tibble(est = est, se = se, lo = est - z*se, hi = est + z*se)
}

get_slopes_df <- function(model, x_main, i_prefix, k_vals, model_label) {
  cn <- names(coef(model))
  
  slope_rows <- lapply(k_vals, function(k) {
    # slope(k) = coef(x_main) + coef(i_term_for_k)  [i term = 0 at ref]
    L <- setNames(rep(0, length(cn)), cn)
    L[x_main] <- 1
    
    # i() term name pattern: "k_soe_f::k#x_main"
    # fixest uses something like: "k_soe_f::5#dlnw"
    term_k <- paste0(i_prefix, "::", k, "#", x_main)
    if (term_k %in% cn) L[term_k] <- 1
    
    ci <- lincombo_ci(model, L)
    tibble(k = k) %>% bind_cols(ci)
  })
  
  bind_rows(slope_rows) %>% mutate(model = model_label)
}

k_vals <- seq(-K, K)

# Determine i() prefix names
# In your model it's k_soe_f
i_prefix <- "k_soe_f"

df_plot_log <- get_slopes_df(m_slope_log_es2, "dlnw", i_prefix, k_vals, "Logs (elasticity)")
df_plot_lvl <- get_slopes_df(m_slope_lvl_es2, "dw",   i_prefix, k_vals, "Levels ($ per $)")

plot_df <- bind_rows(df_plot_log, df_plot_lvl)

# Whisker plot
g_event_slopes <- ggplot(plot_df, aes(x = k, y = est)) +
  geom_hline(yintercept = 1.0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) +  # k=0 = activation week (start of SoE)
  geom_point(size = 1.2) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3) +
  facet_wrap(~ model, ncol = 1) +
  labs(
    title = "Pass-through slopes around SoE episode (30 weeks pre / during / 30 weeks post)",
    subtitle = "k < 0: pre-activation; k >= 0 during SoE; k > 0: post-expiration. Dashed line = 1.0 (complete pass-through).",
    x = "Event time k (weeks)",
    y = "Implied pass-through slope"
  ) +
  theme_minimal()

print(g_event_slopes)
ggsave("images/whisker_pass_through_eventtime_logs_vs_levels.png",
       g_event_slopes, width = 8, height = 7, dpi = 300)


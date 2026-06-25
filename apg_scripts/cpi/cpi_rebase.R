# ==============================================================================
# cpi_rebase.R
#
# Purpose: Standalone diagnostic script for the CPI deflator used in the paper.
#   Reads the raw CPI-U Excel file, reshapes to long format, rebases to
#   Jan 2018 = 1 (deflator P_t) and Jan 2018 = 100 (index cpi_2018jan),
#   then plots YoY inflation and the deflator series.
#
#   Note: apg_analysis.R performs its own inline CPI construction. This script
#   is for visualization and verification only.
#
# Input:  cpi/cpi_20152025.xlsx  (BLS CPI-U, 1982-84 = 100, wide format)
# ==============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)

# ---- Inputs ----
# Run from the project root, or adjust the path below.
path  <- "cpi/cpi_20152025.xlsx"
sheet <- "Sheet1"

# ---- 1) Read the wide xlsx ----
cpi_wide <- read_excel(path, sheet = sheet)

# ---- 2) Reshape to long + build a proper date ----
cpi_long <- cpi_wide %>%
  rename_with(~ str_trim(.x)) %>%
  rename(year = Year) %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month_abbr",
    values_to = "cpi_8284"
  ) %>%
  mutate(
    month = match(month_abbr, month.abb),
    date  = make_date(year = as.integer(year), month = month, day = 1L)
  ) %>%
  arrange(date)

# ---- 3) Rebase to Jan 2018 ----
base_date <- as.Date("2018-01-01")

base_value <- cpi_long %>%
  filter(date == base_date) %>%
  summarise(base = first(cpi_8284)) %>%
  pull(base)

if (is.na(base_value) || length(base_value) == 0) {
  stop("No CPI value found for Jan 2018. Check that cpi_20152025.xlsx includes 2018.")
}

# P_t: deflator (ratio; Jan 2018 = 1) — used to convert nominal to real prices
# cpi_2018jan: index (Jan 2018 = 100) — used in diagnostic tables
cpi_rebased <- cpi_long %>%
  mutate(
    P_t         = cpi_8284 / base_value,
    cpi_2018jan = P_t * 100
  )

cpi_deflator <- cpi_rebased   # alias used in some downstream scripts

# ---- 4) Inflation series ----
cpi_infl <- cpi_rebased %>%
  arrange(date) %>%
  mutate(
    infl_mom = 100 * (cpi_2018jan / lag(cpi_2018jan) - 1),
    infl_yoy = 100 * (cpi_2018jan / lag(cpi_2018jan, 12) - 1)
  )

# ---- 5) Plot YoY inflation ----
df <- cpi_infl %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31")) %>%
  filter(!is.na(infl_yoy)) %>%
  arrange(date)

source_note  <- "Source: U.S. Bureau of Labor Statistics (BLS), CPI-U (1982-84 = 100), downloaded by author."
caption_note <- paste0(
  source_note,
  " CPI was rescaled so that Jan 2018 = 100. ",
  "\nYoY inflation is calculated as 100 x (CPI_t / CPI_{t-12} - 1), where t is month."
)

ggplot(df, aes(x = date, y = infl_yoy)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  scale_y_continuous(
    name   = "Year-over-year inflation (YoY, %)",
    labels = label_number(accuracy = 0.1)
  ) +
  labs(x = NULL, title = "CPI-U Inflation (YoY), 2018-2023", caption = caption_note) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, size = 9))

# ---- 6) Plot deflator ----
df_defl <- cpi_rebased %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31")) %>%
  arrange(date)

caption_defl <- paste0(
  source_note,
  " CPI-U is rebased so that Jan 2018 = 1. ",
  "\nThe plotted series is P_t = CPI_t / CPI_Jan2018, the deflator used to convert nominal prices to real Jan 2018 dollars."
)

ggplot(df_defl, aes(x = date, y = P_t)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  scale_y_continuous(
    name = expression(P[t]~"(Jan 2018 = 1)"),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x = NULL,
    title = "CPI-U Deflator, Jan 2018 = 1",
    caption = caption_defl
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.caption = element_text(hjust = 0, size = 9)
  )


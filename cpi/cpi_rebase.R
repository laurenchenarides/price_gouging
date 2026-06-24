library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)

# ---- Inputs ----
setwd("cpi")
path  <- "cpi_20152025.xlsx"
sheet <- "Sheet1"

# ---- 1) Read the wide xlsx ----
cpi_wide <- read_excel(path, sheet = sheet)

# ---- 2) Reshape to long + build a proper date ----
# Assumes the year column is literally named "Year" (trim if it has spaces)
cpi_long <- cpi_wide %>%
  rename_with(~ str_trim(.x)) %>%  # fixes "Year " or similar
  rename(year = Year) %>%          # change if your year col has a different name
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

# ---- 3) Rebase to Jan 2018 = 100 ----
base_date <- lubridate::ymd("2018-01-01")

base_value <- cpi_long %>%
  filter(date == base_date) %>%
  summarise(base = first(cpi_8284)) %>%
  pull(base)

if (is.na(base_value) || length(base_value) == 0) {
  stop("No CPI value found for Jan 2018. Check that your data includes 2018 and Jan.")
}

cpi_rebased <- cpi_long %>%
  mutate(cpi_2018jan = (cpi_8284 / base_value) * 100)

# cpi_rebased now has: year, month_abbr, month, date, cpi_8284, cpi_2018jan


cpi_infl <- cpi_rebased %>%
  arrange(date) %>%
  mutate(
    infl_mom = 100 * (cpi_2018jan / lag(cpi_2018jan) - 1)
  ) %>%
  mutate(
    infl_yoy = 100 * (cpi_2018jan / lag(cpi_2018jan, 12) - 1)
  )

# --- Plot inflation ---
df <- cpi_infl %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31")) %>%
  filter(!is.na(infl_yoy)) %>%
  arrange(date)

source_note <- "Source: U.S. Bureau of Labor Statistics (BLS), CPI-U (1982–84 = 100), downloaded by author."
base_date   <- as.Date("2018-01-01")
caption_note <- paste0(
  source_note,
  " CPI was rescaled so that Jan 2018 = 100. ",
  "\nYoY inflation is calculated as 100 × (CPI_t / CPI_{t−12} − 1), where t is month."
)

ggplot(df, aes(x = date, y = infl_yoy)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  scale_y_continuous(
    name = "Year-over-year inflation (YoY, %)",
    labels = label_number(accuracy = 0.1)
  ) +
  labs(
    x = NULL,
    title = "CPI-U Inflation (YoY), 2018–2023",
    caption = caption_note
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.caption = element_text(hjust = 0, size = 9)
  )


####################################
## Plot deflator
####################################


library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)

# ---- Inputs ----
setwd("cpi")
path  <- "cpi_20152025.xlsx"
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

# ---- 3) Rebase to Jan 2018 = 1 ----
base_date <- as.Date("2018-01-01")

base_value <- cpi_long %>%
  filter(date == base_date) %>%
  summarise(base = first(cpi_8284)) %>%
  pull(base)

if (is.na(base_value) || length(base_value) == 0) {
  stop("No CPI value found for Jan 2018. Check that your data includes 2018 and Jan.")
}

cpi_deflator <- cpi_long %>%
  mutate(
    P_t = cpi_8284 / base_value
  )

# ---- 4) Keep desired plotting window ----
df <- cpi_deflator %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31")) %>%
  arrange(date)

# ---- 5) Notes ----
source_note <- "Source: U.S. Bureau of Labor Statistics (BLS), CPI-U (1982–84 = 100), downloaded by author."
caption_note <- paste0(
  source_note,
  " CPI-U is rebased so that Jan 2018 = 1. ",
  "\nThe plotted series is P_t = CPI_t / CPI_Jan2018, which is the deflator used to convert nominal prices to real Jan 2018 dollars."
)

# ---- 6) Plot the deflator ----
ggplot(df, aes(x = date, y = P_t)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  scale_y_continuous(
    name = expression(P[t]~"(Jan 2018 = 1)"),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x = NULL,
    title = "CPI-U Deflator, Jan 2018 = 1",
    caption = caption_note
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.caption = element_text(hjust = 0, size = 9)
  )


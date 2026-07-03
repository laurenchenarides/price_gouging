# ==============================================================================
# run_all.R
#
# Food Retailer Pricing Behavior Under Anti-Price Gouging Laws: Evidence from
# Wholesale and Retail Scanner Data
# Chenarides, Richards, and Dong
#
# Master script. Source this file to reproduce all tables and figures.
# Open pg_project.Rproj in RStudio before running — it sets the working
# directory to the project root automatically.
#
# Prerequisites:
#   1. Server-side SQL build completed (code/BuildMarkupsNew_2026_04.sql,
#      then code/BuildMarkupsNew_PriceDiscrimination.sql).
#   2. cpi/cpi_20152025.xlsx present (BLS CPI-U, 1982-84 = 100).
#   3. Connection details set in code/config.R and global vars.
#
# Script execution order:
#   00_read_in_data.R                  SQL pull and panel assembly
#   01_price_sensitivity_diagnostic.R  Variable Construction (diagnostic)
#   02_build_panel.R                   Variable Construction
#   03_descriptive_tables.R            Results: Descriptive Evidence + Incidence of Price Gouging
#   04_residual_plots.R                Results: Descriptive Evidence
#   05_regressions.R                   Empirical Model + Results: Retail Prices + Markups
#   06_uniform_pricing.R               Mechanisms: Mechanism 1 (Constant Retail Prices)
#   07_passthrough.R                   Mechanisms: Mechanism 2 (Variation in Pass-Through)
#   08_demand_rotation.R               Mechanisms: Mechanism 3 (Countercyclical Pricing)
# ==============================================================================

rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  DBI, odbc, dbplyr,
  dplyr, tidyr, lubridate, stringr, purrr,
  ggplot2, scales,
  fixest, broom,
  knitr, kableExtra,
  readxl, rlang, ggpattern, remotes,
  fwildclusterboot, dqrng, tibble
)

options(dplyr.summarise.inform = FALSE)

# Create output directories
for (d in c("figures", "tables_csv", "tables_latex", "tables_latex/net_price", "images")) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---- Configuration and global flags (edit code/config.R, not this file) ------
source("code/config.R")

# ---- Run all scripts ---------------------------------------------------------

source("code/00_read_in_data.R")
source("code/01_price_sensitivity_diagnostic.R")
source("code/02_build_panel.R")
source("code/03_descriptive_tables.R")
source("code/04_residual_plots.R")
source("code/05_regressions.R")

source("code/06_uniform_pricing.R")
source("code/07_passthrough.R")
source("code/08_demand_rotation.R")

message("=== Analysis complete. ===")
message("Tables (LaTeX): tables_latex/")
message("Figures (PNG):  figures/")

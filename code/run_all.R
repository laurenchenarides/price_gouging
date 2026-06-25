# ==============================================================================
# run_all.R
#
# Food Retailer Pricing Behavior Under Anti-Price Gouging Laws: Evidence from Wholesale and Retail Scanner Data
# Chenarides, Richards, and Dong
#
# Master script. Source this file to reproduce all tables and figures.
# Open pg_project.Rproj in RStudio before running — it sets the working
# directory to the project root automatically.
#
# Script execution order:
#   00_read_in_data.R                  SQL pull and panel assembly
#   01_price_sensitivity_diagnostic.R  Variable Construction (diagnostic)
#   02_build_panel.R                   Variable Construction
#   03_descriptive_tables.R            Results: Descriptive Evidence + Incidence of Price Gouging
#   04_residual_plots.R                Results: Descriptive Evidence
#   05_regressions.R                   Empirical Model + Results: Retail Prices + Markups
#   06_passthrough.R                   Mechanisms: Mechanism 2 (Variation in Pass-Through)
#   07_uniform_pricing.R               Mechanisms: Mechanism 1 (Constant Retail Prices)
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
  readxl, rlang, ggpattern
)

options(dplyr.summarise.inform = FALSE)

# Create output directories
for (d in c("figures", "tables_csv", "tables_latex", "tables_latex/net_price", "images")) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---- Global flags ------------------------------------------------------------

# Set TRUE to also produce by-state and by-product residual trend plots
# (Section III.D and III.E). Useful for appendix or seminar materials.
SAVE_OPTIONAL_PLOTS <- TRUE

# Retailers to include (retailer 4 excluded: closed mid-sample)
RETAILERS_KEEP <- c(2, 3, 5)

# Set TRUE to run the pass-through duration extension (Section 11a)
RUN_DUR_EXTENSION <- TRUE

# Set TRUE to also write intermediate CSV files alongside LaTeX tables.
# LaTeX output is always produced. CSV files are optional.
SAVE_CSV <- FALSE

# ---- Run all scripts ---------------------------------------------------------

source("code/00_read_in_data.R")
source("code/01_price_sensitivity_diagnostic.R")
source("code/02_build_panel.R")
source("code/03_descriptive_tables.R")
source("code/04_residual_plots.R")
source("code/05_regressions.R")
source("code/06_passthrough.R")
source("code/07_uniform_pricing.R")
source("code/08_demand_rotation.R")

message("=== Analysis complete. ===")
message("Tables (LaTeX): tables_latex/")
message("Figures (PNG):  figures/")

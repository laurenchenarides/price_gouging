# ==============================================================================
# 00_read_in_data.R
#
# Purpose: Pull the five-product weekly panel from SQL Server (DecaData) and
#   bind into one long panel at the store-product-week level. Also pulls the
#   date index and store master. Saves panel_upc_week for use in 01_.
#
# Unit of observation: store_id x product x week_seq
#
# Products pulled: bananas, cabbage, cucumbers, lettuce, tomatoes
#
# This script is run once. The output panel is used by 01_deflate_and_construct.R.
# SQL connection details are omitted for privacy; supply odbc driver string below.
# ==============================================================================

library(DBI)
library(odbc)
library(dbplyr)
library(dplyr)
library(purrr)
library(lubridate)

# Open connection
message("Opening connection...")

con <- dbConnect(
  odbc::odbc(),
  Driver   = "SQL Server",         # or "ODBC Driver 17 for SQL Server" etc.
  Server   = "Orchard",            # e.g. "localhost" or "myserver\\SQLEXPRESS"
  Database = "DecaData",           # or whatever your DB is called
  Trusted_Connection = "Yes"       # or UID/PWD if you're using SQL auth
)

# Map product names to stg table names
tbl_map <- c(
  bananas   = "pos_bananas_4011",
  lettuce   = "pos_lettuce",
#  peppers   = "pos_peppers",
  cucumbers = "pos_cucumbers",
  tomatoes  = "pos_tomatoes",
  cabbage   = "pos_cabbage"
)


# Pull one product table and standardize column names
read_product_tbl <- function(product_name, table_name) {
  message("Pulling stg.", table_name, " ...")
  
  df <- dplyr::tbl(con, dbplyr::in_schema("stg", table_name)) %>%
    collect()
  
  df %>%
    mutate(
      product                    = product_name,
      upc                        = as.character(upc),
      week_seq                   = as.integer(week_seq),
      week_year                  = as.integer(week_year),
      week_of_year               = as.integer(week_of_year),
      SoE_apg_active             = as.integer(SoE_apg_active),
      apg_start_date             = as.Date(apg_start_date),
      apg_end_date               = as.Date(apg_end_date),
      # Both price versions carried through for diagnostic comparison
      p_ist_net                  = as.numeric(p_ijst_net),
      p_ist_gross                = as.numeric(p_ijst_gross),
      # Simple average prices for robustness comparison
      avg_unit_price             = as.numeric(avg_unit_price),
      avg_sale_price             = as.numeric(avg_sale_price),
      w_ist                      = as.numeric(w_ijst),
      avg_unit_cost_min          = as.numeric(avg_unit_cost_min),
      avg_unit_cost_max          = as.numeric(avg_unit_cost_max),
      upc_week_net_sales         = as.numeric(upc_week_net_sales),
      upc_week_gross_sales       = as.numeric(upc_week_gross_sales),
      upc_week_volume            = as.numeric(upc_week_volume),
      upc_week_total_cost        = as.numeric(upc_week_total_cost),
      share_on_sale              = as.numeric(share_on_sale),
      weekly_transactions_on_sale = as.integer(weekly_transactions_on_sale),
      weekly_transactions_total  = as.integer(weekly_transactions_total)
    )
}

# Pull all tables into a list
product_list <- purrr::imap(tbl_map, ~read_product_tbl(.y, .x))

# Bind into one long dataset
panel_upc_week <- dplyr::bind_rows(product_list) %>%
  select(
    product,
    store_id, retailer_id, sst,
    general_category, category,
    upc,
    week_seq, week_year, week_of_year,
    apg_start_date, apg_end_date, SoE_apg_active,
    upc_week_net_sales, upc_week_gross_sales,
    upc_week_volume, upc_week_total_cost,
    p_ist_net, p_ist_gross,
    avg_unit_price, avg_sale_price,
    w_ist, avg_unit_cost_min, avg_unit_cost_max,
    share_on_sale,
    weekly_transactions_on_sale,
    weekly_transactions_total
  )
# Pull date index (maps week_seq to calendar dates)
dwi <- dplyr::tbl(con, dbplyr::in_schema("stg", "date_week_index")) %>%
  collect() %>%
  mutate(
    week_start = as.Date(week_start),
    month = lubridate::month(week_start),
    month_name = lubridate::month(week_start, label = TRUE, abbr = FALSE)
  ) %>%
  select(yr, wk, week_start, week_seq, year_week, month, month_name)

# Pull store master (used in Section II descriptive table)
store_info <- dplyr::tbl(con, dbplyr::in_schema("stg", "pos_store_master")) %>%
  collect() %>% 
  filter(retailer_id != 1)


DBI::dbDisconnect(con)
message("Connection closed.")

# Join date index; rename SoE indicator; select final columns
panel_upc_week <- panel_upc_week %>%
  left_join(
    dwi %>% select(week_seq, week_start, month, month_name, year_week),
    by = "week_seq"
  ) %>%
  rename(SoE = SoE_apg_active) %>%
  select(
    product,
    store_id, retailer_id, sst,
    general_category, category,
    upc,
    week_seq, week_start, week_year, week_of_year, year_week,
    month, month_name,
    apg_start_date, apg_end_date, SoE,
    upc_week_net_sales, upc_week_gross_sales,
    upc_week_volume, upc_week_total_cost,
    p_ist_net, p_ist_gross,
    avg_unit_price, avg_sale_price,
    w_ist, avg_unit_cost_min, avg_unit_cost_max,
    share_on_sale,
    weekly_transactions_on_sale,
    weekly_transactions_total
  )


# Quick sanity checks
message("Row counts by product:")
print(dplyr::count(panel_upc_week, product))

message("SoE indicator distribution:")
print(table(panel_upc_week$SoE, useNA = "always"))

#saveRDS(panel_upc_week, "data/panel_upc_week_raw.rds")
#message("Saved: data/panel_upc_week_raw.rds")



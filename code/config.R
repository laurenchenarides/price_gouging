# ==============================================================================
# config.R
#
# Single home for user-specific connection details and global flags.
# Sourced by run_all.R before any pipeline script. Edit the DB_* values for
# your environment; nothing here contains credentials (trusted connection).
# ==============================================================================

# ---- SQL Server connection (user-specific) -----------------------------------
DB_DRIVER   <- Sys.getenv("PG_DB_DRIVER",  "SQL Server")  # e.g. "ODBC Driver 17 for SQL Server"
DB_SERVER   <- Sys.getenv("PG_DB_SERVER",  "Orchard")     # your server name
DB_DATABASE <- Sys.getenv("PG_DB_NAME",    "DecaData")    # your database name

open_decadata_connection <- function() {
  DBI::dbConnect(
    odbc::odbc(),
    Driver             = DB_DRIVER,
    Server             = DB_SERVER,
    Database           = DB_DATABASE,
    Trusted_Connection = "Yes"
  )
}

# ---- Global flags --------------------------------------------------------------

# Set TRUE to also produce by-state and by-product residual trend plots (04_residual_plots.R)
# Useful for appendix or seminar materials.
SAVE_OPTIONAL_PLOTS <- TRUE

# Retailers included in the analysis sample.
# Retailer 1 is excluded on the SQL side (see BuildMarkupsNew_2026_04.sql);
# Retailer 4 is excluded here because its data end 2021-04-12 (closed mid-sample).
# Keep this wording in sync with the paper's data section.
RETAILERS_KEEP <- c(2, 3, 5)

# Set to TRUE to run the pass-through duration extension (07_passthrough.R)
RUN_DUR_EXTENSION <- TRUE

# IV pass-through (07_passthrough.R, section 11a) — O(N^2)-ish distance build
RUN_IV <- TRUE

# Set TRUE to also write intermediate CSV files alongside LaTeX tables.
# LaTeX output is always produced. CSV files are optional.
SAVE_CSV <- FALSE

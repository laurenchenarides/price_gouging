/* ------------------------------------------------------------ 
* Decription: Pare down the DecaData to top products before doing analysis
* Origination: D:\Data\Lauren-Tim\02_Code\SQL\BuildMarkups_2025_11.sql
* Last Updated: April 24, 2026
* By: Lauren Chenarides
* ------------------------------------------------------------ */
USE [DecaData];
SELECT DB_NAME() AS current_database;

-- SELECT count(*) from dbo.DCD_Date;
-- n=10,957

/* ******************************************
 * Diagnostics:
 * Check to see what the differences are between these two tables: 
 * (1) DecaData.dbo.tempPOS_retailer_2_5
 * (2) DecaData.stg.pos
 ****************************************** */

-- Compare columns, data types, and nullability
SELECT 
    c.TABLE_SCHEMA,
    c.TABLE_NAME,
    c.COLUMN_NAME,
    c.DATA_TYPE,
    c.CHARACTER_MAXIMUM_LENGTH,
    c.NUMERIC_PRECISION,
    c.IS_NULLABLE
FROM INFORMATION_SCHEMA.COLUMNS c
WHERE --(c.TABLE_SCHEMA = 'dbo' AND c.TABLE_NAME = 'tempPOS_retailer_2_5')
   --OR 
   (c.TABLE_SCHEMA = 'stg'  AND c.TABLE_NAME = 'pos')
ORDER BY c.TABLE_SCHEMA, c.COLUMN_NAME;

-- Total rows
SELECT 'tempPOS_retailer_2_5' AS table_name, COUNT(*) AS row_count
FROM DecaData.dbo.tempPOS_retailer_2_5
UNION ALL
SELECT 'stg.pos' AS table_name, COUNT(*) AS row_count
FROM DecaData.stg.pos;
-- Both: n = 1,814,161,414

-- Earliest and latest transaction dates (if both tables have date fields)
SELECT 'tempPOS_retailer_2_5' AS table_name,
       MIN(CAST(CONVERT(char(8), TRANSACTION_DATE) AS date)) AS min_date,
       MAX(CAST(CONVERT(char(8), TRANSACTION_DATE) AS date)) AS max_date
FROM DecaData.dbo.tempPOS_retailer_2_5
UNION ALL
SELECT 'stg.pos',
       MIN(CAST(CONVERT(char(8), date) AS date)),
       MAX(CAST(CONVERT(char(8), date) AS date))
FROM DecaData.stg.pos;
-- Both: min_date = 2018-01-01
-- Both: max_date = 2023-07-22
   
-- Quick profile of coverage differences
SELECT 'tempPOS_retailer_2_5' AS table_name,
       COUNT(DISTINCT UPC) AS upc_count,
       COUNT(DISTINCT STORE_NUMBER) AS store_count
FROM DecaData.dbo.tempPOS_retailer_2_5
UNION ALL
SELECT 'stg.pos',
       COUNT(DISTINCT UPC),
       COUNT(DISTINCT store_id)
FROM DecaData.stg.pos;
-- Both: UPC count = 13,973
-- Both: store count = 893

SELECT COUNT(DISTINCT UPC) FROM DecaData.stg.pd;
-- UPC count = 25,513

SELECT DISTINCT ITEM_DEAL_QUANTITY FROM DecaData.dbo.tempPOS_retailer_2_5;

SELECT DISTINCT STORE_NUMBER
FROM DecaData.dbo.tempPOS_retailer_2_5;
-- NEXT TIME: CREATE A LEFT JOIN WITH DecaData.dbo.DCD_Store_New

-- For how many weeks is each retailer present?
-- Retailer 2: 2018-01-01 to 2023-07-17
-- Retailer 3: 2018-01-01 to 2023-07-17
-- Retailer 4: 2018-01-01 to 2021-04-12
-- Retailer 5: 2018-01-01 to 2023-07-17
SELECT
    dwi.yr  AS week_year,
    dwi.wk  AS week_of_year,
    dwi.week_seq,
    dwi.week_start,
    COUNT(*) AS n_rows,
    COUNT(DISTINCT p.STORE_NUMBER) AS n_stores
FROM DecaData.dbo.tempPOS_retailer_2_5 p
JOIN stg.store_dim sd
  ON sd.store_id = p.STORE_NUMBER
 AND sd.retailer_id = 5
JOIN stg.date_key dk
  ON dk.datekey = p.TRANSACTION_DATE
JOIN stg.date_week_index dwi
  ON dwi.yr = YEAR(dk.[date])
 AND dwi.wk = DATEPART(ISO_WEEK, dk.[date])
WHERE p.NUMBER_OF_UNITS_SCANNED > 0
GROUP BY
    dwi.yr, dwi.wk, dwi.week_seq, dwi.week_start
ORDER BY
    dwi.week_seq;

/* ******************************************
 * PRODUCT & CATEGORY SELECTION WORKFLOW
 *
 * Goal:
 *   Construct a clean set of perishable products and categories
 *   suitable for APG/pass-through analysis by:
 *     (i) defining APG periods by state,
 *     (ii) building a continuous week index,
 *     (iii) normalizing POS data and mapping UPCs to categories,
 *     (iv) computing store–UPC–week presence and coverage,
 *     (v) selecting products with sufficient temporal and cross-store support,
 *     (vi) collapsing to a small set of high-coverage, high-volume categories.
 ****************************************** */

/*==========================
  0) PARAMETERS
  - These set the empirical thresholds used in the paper:
    * temporal support around APG activation
    * pre-/post-APG weeks
    * cross-store coverage
    * target number of products per category
==========================*/
DECLARE @WINDOW_WEEKS        INT   = 26;    -- ±26 weeks around APG start (window for presence)
DECLARE @MIN_WINDOW_COVER    FLOAT = 0.80;  -- ≥80% of weeks present in that window
DECLARE @MIN_PRE_WEEKS       INT   = 5;     -- ≥5 weeks observed before APG start
DECLARE @MIN_POST_WEEKS      INT   = 5;     -- ≥5 weeks observed after APG end
DECLARE @MIN_STORE_SHARE     FLOAT = 0.75;  -- UPC passes coverage criteria in ≥X% of stores
DECLARE @PRODS_PER_CATEGORY  INT   = 5;     -- initial target of ~5 UPCs per category

/*==========================
  1) APG LAW PERIODS
  - Hard-code APG activation/end dates by state
  - This mirrors the statutory timeline described in the text.
  - Table: stg.state_apg_periods
==========================*/
IF OBJECT_ID('stg.state_apg_periods','U') IS NOT NULL DROP TABLE stg.state_apg_periods;

CREATE TABLE stg.state_apg_periods (
    state_name       VARCHAR(50),
    sst              CHAR(2),
    apg_start_date   DATE,
    apg_end_date     DATE
);

INSERT INTO stg.state_apg_periods (state_name, sst, apg_start_date, apg_end_date) VALUES
('Alabama','AL','2020-03-13','2021-07-06'),
('Florida','FL','2020-03-09','2021-06-26'),
('Georgia','GA','2020-03-14','2021-06-30'),
('Louisiana','LA','2020-03-11','2021-03-16'),
('Mississippi','MS','2020-03-14','2021-11-20');
--('North Carolina','NC','2020-03-10','2022-08-15'),
--('South Carolina','SC','2020-03-13','2021-06-06');
--('Tennessee','TN','2020-03-12','2021-11-19');

-- Sanity check
SELECT * FROM stg.state_apg_periods;

/*==========================
  2) CONTINUOUS WEEK INDEX
  - Construct a global week index (week_seq) used throughout:
      * to align POS dates to weeks
      * to map APG start/end to week indices
  - Based on stg.date_key (calendar dates).
  - Table: stg.date_week_index
==========================*/
IF OBJECT_ID('stg.date_week_index','U') IS NOT NULL DROP TABLE stg.date_week_index;

WITH w AS (
  SELECT
      YEAR(dk.DATE)               AS yr,
      DATEPART(ISO_WEEK, dk.DATE) AS wk,
      MIN(dk.DATE)                AS week_start
  FROM stg.date_key dk
  GROUP BY YEAR(dk.DATE), DATEPART(ISO_WEEK, dk.DATE)
)
SELECT
    yr,
    wk,
    week_start,
    ROW_NUMBER() OVER (ORDER BY week_start) AS week_seq,
    CAST(yr*100 + wk AS INT) AS year_week
INTO stg.date_week_index
FROM w;

CREATE UNIQUE INDEX UX_dwi_yr_wk    ON stg.date_week_index(yr, wk);
CREATE UNIQUE INDEX UX_dwi_week_seq ON stg.date_week_index(week_seq);

SELECT * FROM stg.date_week_index;

/*==========================
  3) ENRICH APG WITH WEEK FIELDS
  - Attach year/week and week_seq to APG start/end dates.
  - This allows us to work entirely in the week_seq domain.
  - Table: stg.state_apg_periods (updated in-place)
==========================*/
ALTER TABLE stg.state_apg_periods
ADD apg_start_year INT, apg_start_week INT, apg_start_seq INT,
    apg_end_year   INT, apg_end_week   INT, apg_end_seq   INT;

-- Derive year and ISO week for APG start/end
UPDATE sap
SET apg_start_year = YEAR(apg_start_date),
    apg_start_week = DATEPART(ISO_WEEK, apg_start_date),
    apg_end_year   = YEAR(apg_end_date),
    apg_end_week   = DATEPART(ISO_WEEK, apg_end_date)
FROM stg.state_apg_periods sap;

-- Map APG start to week_seq
UPDATE sap
SET apg_start_seq = dwi.week_seq
FROM stg.state_apg_periods sap
JOIN stg.date_week_index dwi
  ON dwi.yr = sap.apg_start_year AND dwi.wk = sap.apg_start_week;

-- Map APG end to week_seq
UPDATE sap
SET apg_end_seq = dwi.week_seq
FROM stg.state_apg_periods sap
JOIN stg.date_week_index dwi
  ON dwi.yr = sap.apg_end_year AND dwi.wk = sap.apg_end_week;

-- Quick check: APG calendar dates vs week_seq
SELECT sst, apg_start_date, apg_end_date, apg_start_seq, apg_end_seq
FROM stg.state_apg_periods
ORDER BY sst;

/*==========================
  4) POS CORE (normalize POS records)
  - Source: DecaData.stg.pos (store–UPC–date)
  - Tasks:
      * Convert integer date → DATE
      * Restrict to positive units and net sales
      * Keep core monetary/quantity fields
  - Output: stg.pos_core
==========================*/
IF OBJECT_ID('stg.pos_core','U') IS NOT NULL DROP TABLE stg.pos_core;

WITH typed AS (
    SELECT
        -- int YYYYMMDD -> varchar(8) -> date (style 112)
        CONVERT(date, CONVERT(varchar(8), p.TRANSACTION_DATE), 112) AS trx_date,
        p.STORE_NUMBER AS store_id,
        p.UPC          AS upc,
        p.ITEM_CODE	   AS item_code,
        TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE) AS unit_price,
        TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE)    AS sale_price,
        p.NUMBER_OF_UNITS_SCANNED AS units_sold,
        TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT)       AS item_weight,
        TRY_CONVERT(decimal(18,4), p.ITEM_NET_SALES)    AS net_sales,
        TRY_CONVERT(decimal(18,4), p.ITEM_GROSS_SALES)  AS gross_sales,
        TRY_CONVERT(decimal(18,4), p.ITEM_COST)         AS total_cost
    FROM DecaData.dbo.tempPOS_retailer_2_5 p
)
SELECT
    trx_date,
    store_id,
    upc,
    item_code,
    unit_price,
    sale_price,
    CASE 
        WHEN sale_price = 0 
             OR sale_price = unit_price 
        THEN 0
        ELSE 1
    END AS is_on_sale,
    units_sold,
    item_weight,
    net_sales,
    gross_sales,
    total_cost,
    CASE 
        WHEN item_weight IS NULL OR item_weight = 0 
            THEN total_cost / NULLIF(CAST(units_sold AS decimal(18,4)), 0)
        ELSE total_cost / NULLIF(item_weight, 0)
    END AS unit_cost,
    (gross_sales - total_cost) AS gross_margin,
    (gross_sales - total_cost) / NULLIF(gross_sales, 0) AS gross_margin_pct
INTO stg.pos_core
FROM typed
WHERE unit_price  > 0 
  AND total_cost  > 0 
  AND gross_sales > 0;	-- drop zero-sales rows

CREATE INDEX IX_pos_core_store_date ON stg.pos_core(store_id, trx_date);
CREATE INDEX IX_pos_core_upc        ON stg.pos_core(upc);

-- Spot check the normalized POS file
SELECT count(*) FROM stg.pos_core;
-- n = 1,800,009,397

/* ========================================================
 * Begin checks on unit, sale, and cost prices
 * ======================================================== */

-- Sale frequency by retailer for the five focal items
-- Scope: retailer_id <> 1, stores in NC and SC only
-- Items: four PLUs (bananas, tomatoes, cucumbers, peppers) and the lettuce UPC

SELECT
    sd.retailer_id,
    pc.upc,
    COUNT(*)                                      AS total_transactions,
    SUM(pc.is_on_sale)                            AS transactions_on_sale,
    CAST(SUM(pc.is_on_sale) AS float)
        / NULLIF(COUNT(*), 0)                     AS share_on_sale
FROM stg.pos_core pc
JOIN stg.store_dim sd
    ON sd.store_id = pc.store_id
WHERE sd.retailer_id <> 1
  AND sd.sst NOT IN ('NC', 'SC')
  AND pc.upc IN (
      '4011',   -- bananas
      '4087',   -- tomatoes
      '4062',   -- cucumbers
      '4065',   -- peppers (confirm this PLU)
      '7143001065'  -- replace with the actual lettuce UPC
  )
GROUP BY
    sd.retailer_id,
    pc.upc
ORDER BY
    sd.retailer_id,
    pc.upc;

-- Add SOE period breakdown to the sale frequency query
SELECT
    sd.retailer_id,
    pc.upc,
    CASE
        WHEN pc.trx_date < sap.apg_start_date THEN 'pre_SOE'
        WHEN pc.trx_date > sap.apg_end_date   THEN 'post_SOE'
        ELSE 'during_SOE'
    END AS soe_period,
    COUNT(*)                                      AS total_transactions,
    SUM(pc.is_on_sale)                            AS transactions_on_sale,
    CAST(SUM(pc.is_on_sale) AS float)
        / NULLIF(COUNT(*), 0)                     AS share_on_sale
FROM stg.pos_core pc
JOIN stg.store_dim sd
    ON sd.store_id = pc.store_id
JOIN stg.state_apg_periods sap
    ON sap.sst = sd.sst
WHERE sd.retailer_id <> 1
  AND sd.sst NOT IN ('NC', 'SC')
  AND pc.upc IN ('4011','4087','4062','4065','7143001065')
GROUP BY
    sd.retailer_id,
    pc.upc,
    CASE
        WHEN pc.trx_date < sap.apg_start_date THEN 'pre_SOE'
        WHEN pc.trx_date > sap.apg_end_date   THEN 'post_SOE'
        ELSE 'during_SOE'
    END
ORDER BY
    sd.retailer_id,
    pc.upc,
    soe_period;

-- Check whether unit_price is a reliable shelf price measure for that store-date. 
-- If distinct_unit_price comes back as 1, this means all transactions that day share the same posted price.
SELECT
    trx_date,
    store_id,
    upc,
    unit_price,
    COUNT(*)        AS transaction_count,
    MIN(unit_price) AS min_unit_price,
    MAX(unit_price) AS max_unit_price,
    COUNT(DISTINCT unit_price) AS distinct_unit_prices
FROM stg.pos_core
WHERE upc      = '4038'
  AND store_id = 19
  AND trx_date = '2019-06-04'
GROUP BY
    trx_date,
    store_id,
    upc,
    unit_price
ORDER BY
    transaction_count DESC;
    
SELECT
    trx_date,
    store_id,
    upc,
    sale_price,
    COUNT(*)        AS transaction_count,
    MIN(sale_price) AS min_sale_price,
    MAX(sale_price) AS max_sale_price,
    COUNT(DISTINCT unit_price) AS distinct_sale_prices
FROM stg.pos_core
WHERE upc      = '4038'
  AND store_id = 19
  AND trx_date = '2019-06-04'
GROUP BY
    trx_date,
    store_id,
    upc,
    sale_price
ORDER BY
    transaction_count DESC;


-- For each store-UPC-date, count distinct unit prices and flag any with more than one.
-- Scope: the five focal UPCs across all stores.
-- Use to assess whether unit_price is a reliable shelf price proxy before aggregating to p_ijst_gross.

SELECT
    trx_date,
    store_id,
    upc,
    COUNT(*)                   AS transaction_count,
    MIN(unit_price)            AS min_unit_price,
    MAX(unit_price)            AS max_unit_price,
    COUNT(DISTINCT unit_price) AS distinct_unit_prices,
    MIN(sale_price)            AS min_sale_price,
    MAX(sale_price)            AS max_sale_price,
    COUNT(DISTINCT sale_price) AS distinct_sale_prices
FROM stg.pos_core
WHERE upc IN ('4011', '4087', '4062', '4065', '7143001065')
GROUP BY
    trx_date,
    store_id,
    upc
HAVING COUNT(DISTINCT unit_price) > 1
    OR COUNT(DISTINCT sale_price) > 1
ORDER BY
    distinct_unit_prices DESC,
    distinct_sale_prices DESC,
    trx_date,
    store_id,
    upc;

-- Result: returns an empty table which means that posted unit and sales prices are the same on a given day.

-- Check variation in unit_cost within store-UPC-date for the five focal UPCs.
-- unit_cost is derived rather than directly observed, so unlike unit_price it is
-- expected to vary within a store-UPC-date as individual transactions differ in
-- weight or units. This check quantifies how much that variation is.
SELECT
    trx_date,
    store_id,
    upc,
    COUNT(*)                    AS transaction_count,
    MIN(unit_cost)              AS min_unit_cost,
    MAX(unit_cost)              AS max_unit_cost,
    COUNT(DISTINCT unit_cost)   AS distinct_unit_costs,
    AVG(unit_cost)              AS avg_unit_cost,
    MAX(unit_cost) - MIN(unit_cost) AS range_unit_cost
FROM stg.pos_core
WHERE upc IN ('4011', '4087', '4062', '4065', '7143001065')
GROUP BY
    trx_date,
    store_id,
    upc
HAVING COUNT(DISTINCT unit_cost) > 1
ORDER BY
    range_unit_cost DESC,
    distinct_unit_costs DESC,
    trx_date,
    store_id,
    upc;

-- Result: returns many rows.
-- Variation in unit_cost likely comes from one of a few sources:
-- 1) total_cost is recorded at the transaction level and may not scale 
-- cleanly with weight or units if it reflects a fixed invoice allocation 
-- rather than a per-unit charge
-- 2) item_weight may be zero or missing for some transactions, triggering 
-- the fallback to units_sold as the denominator, which could produce 
-- very different values if weight and unit counts are on different scales
-- 3) rounding in the source data

SELECT TOP 50
    trx_date,
    store_id,
    upc,
    units_sold,
    item_weight,
    total_cost,
    unit_cost
FROM stg.pos_core
WHERE upc      = '4065'         -- swap in whichever UPC shows the most variation
  AND store_id = 235     -- pick a store from the flagged results
  AND trx_date = '2020-07-22'     -- pick a date from the flagged results
ORDER BY unit_cost DESC;

-- Looking at a specific product shows that unit cost does not vary.
-- The results from the previous table are likely due to rounding.

SELECT
    trx_date,
    store_id,
    upc,
    COUNT(*)                            AS transaction_count,
    COUNT(DISTINCT unit_cost)           AS distinct_unit_costs_exact,
    COUNT(DISTINCT ROUND(unit_cost, 2)) AS distinct_unit_costs_rounded
FROM stg.pos_core
WHERE upc IN ('4011', '4087', '4062', '4065', '7143001065')
GROUP BY
    trx_date,
    store_id,
    upc
HAVING COUNT(DISTINCT ROUND(unit_cost, 2)) > 1
ORDER BY
    distinct_unit_costs_rounded DESC,
    trx_date,
    store_id,
    upc;

/*
-- Select a random sample of 200 observations
SELECT TOP 200 *
FROM DecaData.dbo.tempPOS_retailer_2_5
ORDER BY NEWID();

SELECT * FROM DecaData.dbo.tempPOS_retailer_2_5
WHERE REGISTER_NUMBER = 5 AND TRANSACTION_NUMBER = 50 AND TRANSACTION_DATE = 20180925 AND STORE_NUMBER = 5637;
*/

/*==========================
  4a) POS STORE MASTER (store universe)
  - Derive the universe of POS stores from stg.pos_core
  - Join to all available store metadata tables:
      * stg.store_info
      * dbo.DCD_STORE
      * dbo.DCD_Store_New_XYtoCBG
  - Construct a master store-level table with:
      * first/last transaction dates
      * state codes from multiple sources
      * retailer identifiers
  - Use this to verify which states (including TN) are actually
    represented in the POS data and to build stg.store_dim.
==========================*/

SELECT count(*) FROM DecaData.dbo.DCD_Store_New;
-- n = 1,177
SELECT count(*) FROM DecaData.dbo.DCD_Store_New_XYtoCBG;
-- n = 1,176
SELECT count(*) FROM DecaData.dbo.DCD_STORE;
-- n = 1,194
SELECT count(*) FROM DecaData.stg.store_info;
-- 1,066

-- Build a master list of POS stores
IF OBJECT_ID('stg.pos_store_list','U') IS NOT NULL DROP TABLE stg.pos_store_list;

SELECT
    store_id,
    MIN(trx_date) AS first_trx_date,
    MAX(trx_date) AS last_trx_date,
    COUNT(*)      AS n_rows
INTO stg.pos_store_list
FROM stg.pos_core
GROUP BY store_id;

-- Quick look
SELECT COUNT(*) AS n_pos_stores FROM stg.pos_store_list;
-- n = 887
SELECT TOP 50 * FROM stg.pos_store_list ORDER BY store_id;

-- Enrich with all store metadata sources
IF OBJECT_ID('stg.pos_store_temp','U') IS NOT NULL DROP TABLE stg.pos_store_temp;

SELECT
    psl.store_id,
    psl.first_trx_date,
    psl.last_trx_date,
    psl.n_rows,

    -- From stg.store_info
    si.retailer_id         AS retailer_id_info,
    si.sst                 AS sst_info,
    si.lon				   AS lon_info,
    si.lat 				   AS lat_info,

    -- From DCD_STORE 
    ds.retailer_id		   AS retailer_id_dcd,	-- these have NULLs
    ds.sst                 AS state_dcd,		-- these have NULLs
    ds.pstl_cd 		       AS postal_code_dcd,	-- these have NULLs
    ds.zipcode			   AS zipcode_dcd,

    -- From XY/CBG table
    xy.id         		   AS retailer_id_xy,
    xy.GEOFIPS             AS cbg_xy,
    xy.sst                 AS state_xy,
    xy.STATE_FIPS          AS state_fips_xy

INTO stg.pos_store_temp
FROM stg.pos_store_list psl
LEFT JOIN stg.store_info            si ON psl.store_id = si.store_id
LEFT JOIN dbo.DCD_STORE             ds ON psl.store_id = ds.store_id      
LEFT JOIN dbo.DCD_Store_New_XYtoCBG xy ON psl.store_id = xy.store_id;     

-- See what states we actually have in POS
SELECT 
    state_xy AS state,
    COUNT(DISTINCT store_id) AS n_stores
FROM stg.pos_store_temp
GROUP BY state_xy
ORDER BY state;

-- See what retail chains we have in POS
SELECT 
    retailer_id_xy AS retailer_id,
    COUNT(DISTINCT store_id) AS n_stores
FROM stg.pos_store_temp
GROUP BY retailer_id_xy
ORDER BY retailer_id;

-- Enrich with all store metadata sources
IF OBJECT_ID('stg.pos_store_master','U') IS NOT NULL DROP TABLE stg.pos_store_master;

SELECT 
	store_id,
	first_trx_date,
	last_trx_date,
	n_rows,
	retailer_id_xy	AS retailer_id,
	state_xy		AS sst,
	state_fips_xy	AS state_fips,
	cbg_xy			AS cbg,
	zipcode_dcd		AS zipcode,
	lon_info		AS lon,
	lat_info		AS lat
INTO stg.pos_store_master
FROM stg.pos_store_temp;

SELECT count(*) FROM stg.pos_store_master;
-- n = 943

SELECT DISTINCT retailer_id FROM stg.pos_store_master;

-- Drop temp table
IF OBJECT_ID('stg.pos_store_temp','U') IS NOT NULL DROP TABLE stg.pos_store_temp;

/*==========================
  5) STORE DIMENSION
  - Extract store-level identifiers and state codes.
  - Used to:
      * map stores to states (sst)
      * count distinct stores in the analytic sample
  - Output: stg.store_dim
==========================*/
IF OBJECT_ID('stg.store_dim','U') IS NOT NULL DROP TABLE stg.store_dim;

SELECT DISTINCT si.store_id, si.retailer_id, si.sst
INTO stg.store_dim
FROM stg.pos_store_master si
WHERE retailer_id <> 1;

SELECT COUNT(DISTINCT store_id) FROM stg.store_dim;
-- n = 868

SELECT sst, COUNT(DISTINCT store_id) FROM stg.store_dim GROUP BY sst;
-- n = 868

SELECT 
	retailer_id, COUNT(DISTINCT store_id) 
FROM stg.store_dim 
--WHERE retailer_id <> 1 
GROUP BY retailer_id;

/*==========================
  6) POS ENRICHED (attach weeks, store/state, and category)
  - Merge:
      * POS core (store–UPC–date)
      * week index (date → week_seq)
      * store_dim (store → retailer/state)
      * product dictionary + category_key (UPC → category, general_category)
  - Restrict to items for which we have category_key and mapping.
  - Exclude retailer_id = 1.
  - Output: stg.pos_enriched (store–UPC–day with week_seq and categories)
  - Time to execute: 17 min 3 sec
==========================*/
IF OBJECT_ID('stg.pos_enriched','U') IS NOT NULL DROP TABLE stg.pos_enriched;

SELECT
    pc.trx_date,
    dwi.week_seq,                  -- continuous week index
    dwi.yr         AS week_year,   -- optional for summaries
    dwi.wk         AS week_of_year,
    sd.store_id,
    sd.retailer_id,
    sd.sst,
    pd.UPC,
    ck.category,
    ck.general_category,
    pc.units_sold,
    pc.item_weight,
    pc.net_sales,
    pc.gross_sales,
    pc.unit_price,   -- posted shelf price; used to compute avg_unit_price in pos_weekly_presence
    pc.sale_price,    -- promotional price; used to compute avg_sale_price in pos_weekly_presence
    pc.total_cost,                  -- bring cost through
    ROUND(pc.unit_cost, 2) as unit_cost,
	pc.is_on_sale                   -- transaction-level sale flag; aggregated to week in store_upc_week
INTO stg.pos_enriched
FROM stg.pos_core pc
JOIN stg.date_week_index dwi
  ON dwi.yr = YEAR(pc.trx_date)
 AND dwi.wk = DATEPART(ISO_WEEK, pc.trx_date)
JOIN stg.store_dim sd    
  ON sd.store_id = pc.store_id
JOIN stg.pd pd           
  ON pd.UPC = pc.upc
JOIN stg.category_key ck 
  ON ck.category_key = pd.CATEGORY_KEY
WHERE sd.retailer_id <> 1
  AND sd.sst NOT IN ('NC', 'SC');   -- excluding retailer 1 and NC or SC

-- Time to execute: 25 min 49 sec
CREATE INDEX IX_pos_enriched_keys ON stg.pos_enriched(store_id, upc, week_seq);
CREATE INDEX IX_pos_enriched_cat  ON stg.pos_enriched(category);

-- POS enriched sample sizes
SELECT 
	retailer_id,
	COUNT(DISTINCT store_id) 
FROM stg.pos_enriched
GROUP BY retailer_id;
-- n=683

SELECT 
	retailer_id, 
	COUNT(DISTINCT store_id) 
FROM stg.pos_enriched 
GROUP BY retailer_id;

-- See what states we actually have in POS Enriched; we should not have TN (or NC or SC)
SELECT 
    sst,
    COUNT(DISTINCT store_id) AS n_stores
FROM stg.pos_enriched
GROUP BY sst
ORDER BY sst;
-- n = 683

SELECT count(*) FROM stg.pos_enriched;
-- n=1,355,190,542

/*==========================
  6a) DAILY PRICE COLLAPSE
  - Collapses pos_enriched to one row per store-UPC-date.
  - unit_price is confirmed identical within store-UPC-date, so MIN() is
    equivalent to MAX() here; either returns the single shelf price for that day.
  - This intermediate step ensures that when weekly averages are computed in
    pos_weekly_presence, each calendar day receives equal weight regardless
    of how many transactions occurred that day.
  - Output: stg.pos_daily
==========================*/
IF OBJECT_ID('stg.pos_daily', 'U') IS NOT NULL DROP TABLE stg.pos_daily;

SELECT
    store_id,
    retailer_id,
    upc,
    category,
    week_seq,
    trx_date,
    MIN(unit_price) AS daily_unit_price,   -- identical within store-UPC-date; MIN = MAX
    MIN(sale_price) AS daily_sale_price,   -- lowest sale price if multiple exist
    MIN(unit_cost)  AS daily_unit_cost_min, -- lower bound; should equal max if cost is stable within day
    MAX(unit_cost)  AS daily_unit_cost_max  -- upper bound; diverges from min only on genuine two-rate days
INTO stg.pos_daily
FROM stg.pos_enriched
GROUP BY
    store_id,
    retailer_id,
    upc,
    category,
    week_seq,
    trx_date;

CREATE INDEX IX_pos_daily ON stg.pos_daily(store_id, upc, week_seq);

/*==========================
  7) WEEKLY PRESENCE (store–UPC–week)
  - Collapse daily POS to weekly:
      * weekly units, weight, unified volume
      * weekly net sales, gross sales, total cost
      * binary presence flag (sold at least once)
  - Base for presence and coverage calculations.
  - Output: stg.pos_weekly_presence
==========================*/
IF OBJECT_ID('stg.pos_weekly_presence', 'U') IS NOT NULL DROP TABLE stg.pos_weekly_presence;

SELECT
    pe.store_id,
    pe.retailer_id,
    pe.upc,
    pe.category,
    pe.week_seq,
    SUM(pe.units_sold)    AS weekly_units,
    SUM(pe.item_weight)   AS weekly_weight,
    CASE
        WHEN SUM(pe.item_weight) > 0 THEN SUM(pe.item_weight)
        ELSE SUM(pe.units_sold)
    END AS weekly_volume,
    SUM(pe.net_sales)     AS weekly_net_sales,
    SUM(pe.gross_sales)   AS weekly_gross_sales,
    SUM(pe.total_cost)    AS weekly_total_cost,
    CASE
        WHEN SUM(pe.units_sold) > 0 OR SUM(pe.item_weight) > 0 THEN 1
        ELSE 0
    END AS present,
    SUM(pe.is_on_sale)    AS weekly_transactions_on_sale,
    COUNT(*)              AS weekly_transactions_total,
    -- Simple average of daily shelf prices: each day weighted equally regardless
    -- of transaction volume. Requires the pos_daily intermediary step to ensure
    -- one price observation per day before averaging across days in the week.
    AVG(pd.daily_unit_price) AS avg_unit_price,
    AVG(pd.daily_sale_price) AS avg_sale_price,
    AVG(pd.daily_unit_cost_min) AS avg_cost_min, 
    AVG(pd.daily_unit_cost_max) AS avg_cost_max
INTO stg.pos_weekly_presence
FROM stg.pos_enriched pe
JOIN stg.pos_daily pd
  ON pd.store_id  = pe.store_id
 AND pd.upc       = pe.upc
 AND pd.week_seq  = pe.week_seq
 AND pd.trx_date  = pe.trx_date
WHERE pe.retailer_id <> 1
GROUP BY
    pe.store_id,
    pe.retailer_id,
    pe.upc,
    pe.category,
    pe.week_seq;

CREATE INDEX IX_weekly_presence ON stg.pos_weekly_presence(store_id, upc, week_seq);

-- Quick check
SELECT TOP 50 * FROM stg.pos_weekly_presence;

SELECT 
	DISTINCT retailer_id,
	COUNT(store_id) AS n_stores
FROM stg.pos_weekly_presence 
GROUP BY retailer_id;


/*==========================
  8) APG WINDOWS BY STORE
  - Map APG start/end to each store based on its state.
  - Exclude Tennessee (TN) from APG timing because POS sample
    does not include TN stores used in the empirical analysis.
  - Define symmetric windows:
      * Start window: [apg_start_seq - WINDOW_WEEKS, apg_start_seq + WINDOW_WEEKS]
      * End window:   [apg_end_seq   - WINDOW_WEEKS, apg_end_seq   + WINDOW_WEEKS]
    These are used to measure presence around both activation
    and lifting of APG protections.
  - Output:
      * stg.store_apg_timing  (store-level APG start/end)
      * stg.store_apg_windows (store-level week_seq windows)
==========================*/
IF OBJECT_ID('stg.store_apg_timing','U') IS NOT NULL DROP TABLE stg.store_apg_timing;

SELECT
    sd.store_id,
    sd.retailer_id,
    sd.sst,
    sap.apg_start_seq,
    sap.apg_end_seq
INTO stg.store_apg_timing
FROM stg.store_dim sd
JOIN stg.state_apg_periods sap 
  ON sap.sst = sd.sst
WHERE sd.retailer_id <> 1
  AND sd.sst NOT IN ('TN', 'NC', 'SC');   -- exclude TN, NS, SC from APG timing

SELECT DISTINCT sst from stg.store_apg_timing;

IF OBJECT_ID('stg.store_apg_windows','U') IS NOT NULL DROP TABLE stg.store_apg_windows;

DECLARE @WINDOW_WEEKS INT = 26;

SELECT
    sat.store_id,
    sat.retailer_id,
    sat.sst,
    sat.apg_start_seq,
    sat.apg_end_seq,

    -- Window around APG start
    (sat.apg_start_seq - @WINDOW_WEEKS) AS start_win_start_seq,
    (sat.apg_start_seq + @WINDOW_WEEKS) AS start_win_end_seq,

    -- Window around APG end (if end known)
    CASE 
        WHEN sat.apg_end_seq IS NULL THEN NULL
        ELSE sat.apg_end_seq - @WINDOW_WEEKS
    END AS end_win_start_seq,
    CASE 
        WHEN sat.apg_end_seq IS NULL THEN NULL
        ELSE sat.apg_end_seq + @WINDOW_WEEKS
    END AS end_win_end_seq
INTO stg.store_apg_windows
FROM stg.store_apg_timing sat;

SELECT DISTINCT sst from stg.store_apg_windows;

SELECT DISTINCT retailer_id from stg.store_apg_windows;

CREATE INDEX IX_store_apg_win ON stg.store_apg_windows(store_id);


/*==========================
  9) COVERAGE METRICS (store–UPC)  [UNCONDITIONAL VERSION]
  - Goal: measure how regularly each UPC appears in a given store around APG
    activation and lifting windows (±26 weeks). Define coverage across
    the FULL STORE UNIVERSE (not only stores that ever carry the UPC).
      * We build a complete store × UPC grid first (for the store universe
        included in APG timing), and then compute window metrics by
        left-joining weekly presence. Stores with no sales for that UPC
        in a window get zero presence.

  - Restrict UPC universe to the categories listed below (case-insensitive).
  - Denominator for share_stores_pass is ALL stores in stg.store_apg_windows.

    - For each store–UPC:
      * start_weeks_in_window: # of weeks in the start window for that store
      * start_weeks_present:  # of those weeks with presence (sold at least once)
      * pct_present_start_window: start_weeks_present / start_weeks_in_window
      * end_weeks_in_window, end_weeks_present, pct_present_end_window
      * pre_weeks_present:  # present weeks before APG start within start window
      * post_weeks_present: # present weeks after APG end within end window

  - Output tables:
      * stg.coverage_metrics (store–UPC)
      * stg.store_upc_pass   (store–UPC pass flag)
      * stg.product_coverage (UPC-level unconditional share across stores)
      * stg.product_volume   (UPC totals + category)
      * stg.selected_products
==========================*/

DECLARE @MIN_WINDOW_COVER FLOAT = 0.80;
DECLARE @MIN_PRE_WEEKS    INT   = 5;
DECLARE @MIN_POST_WEEKS   INT   = 5;
DECLARE @MIN_STORE_SHARE  FLOAT = 0.75;
DECLARE @PRODS_PER_CATEGORY INT = 5;

-- Category restriction list
IF OBJECT_ID('tempdb..#cat_keep') IS NOT NULL DROP TABLE #cat_keep;
CREATE TABLE #cat_keep (category VARCHAR(50) PRIMARY KEY);

INSERT INTO #cat_keep(category) VALUES
('APPLES'),('AVOCADO'),('BANANAS'),('BEEF'),('BROCCOLI'),('CABBAGE'),('CARROT'),
('CAULIFLOWER'),('CELERY'),('CHERRY'),('CHICKEN'),('CORN'),('CRANBERRIES'),
('CUCUMBER'),('EGGPLANT'),('GRAPEFRUIT'),('GRAPES'),('KIWI'),('LEMON'),
('LETTUCE'),('MELONS'),('ONIONS'),('ORANGES'),('PEACH'),('PEARS'),('PEPPERS'),
('PINEAPPLES'),('PLUMS'),('PORK'),('POTATOES'),('SPINACH'),('SQUASH'),
('STRAWBERRIES'),('TOMATOES'),('TURKEY'),('WATERMELON');

IF OBJECT_ID('stg.coverage_metrics','U') IS NOT NULL DROP TABLE stg.coverage_metrics;

WITH store_universe AS (
    SELECT DISTINCT store_id
    FROM stg.store_apg_windows
),
upc_universe AS (
    -- Restrict UPC universe to the requested categories
    SELECT DISTINCT p.upc
    FROM stg.pos_weekly_presence p
    JOIN stg.pd pd
      ON pd.UPC = p.upc
    JOIN stg.category_key ck
      ON ck.category_key = pd.category_key
    JOIN #cat_keep k
      ON UPPER(ck.category) = k.category
),
store_upc_grid AS (
    SELECT su.store_id, uu.upc
    FROM store_universe su
    CROSS JOIN upc_universe uu
),

-- start window
start_window AS (
    SELECT
        g.store_id,
        g.upc,
        COUNT(*) AS start_weeks_in_window,
        SUM(COALESCE(p.present, 0)) AS start_weeks_present
    FROM store_upc_grid g
    JOIN stg.store_apg_windows w
      ON w.store_id = g.store_id
    JOIN (
        SELECT DISTINCT store_id, week_seq
        FROM stg.pos_weekly_presence
    ) wk
      ON wk.store_id = g.store_id
    LEFT JOIN stg.pos_weekly_presence p
      ON p.store_id = g.store_id
     AND p.upc      = g.upc
     AND p.week_seq = wk.week_seq
    WHERE wk.week_seq BETWEEN w.start_win_start_seq AND w.start_win_end_seq
    GROUP BY g.store_id, g.upc
),

-- end window
end_window AS (
    SELECT
        g.store_id,
        g.upc,
        COUNT(*) AS end_weeks_in_window,
        SUM(COALESCE(p.present, 0)) AS end_weeks_present
    FROM store_upc_grid g
    JOIN stg.store_apg_windows w
      ON w.store_id = g.store_id
     AND w.end_win_start_seq IS NOT NULL
    JOIN (
        SELECT DISTINCT store_id, week_seq
        FROM stg.pos_weekly_presence
    ) wk
      ON wk.store_id = g.store_id
    LEFT JOIN stg.pos_weekly_presence p
      ON p.store_id = g.store_id
     AND p.upc      = g.upc
     AND p.week_seq = wk.week_seq
    WHERE wk.week_seq BETWEEN w.end_win_start_seq AND w.end_win_end_seq
    GROUP BY g.store_id, g.upc
),

-- pre counts
pre_counts AS (
    SELECT
        g.store_id,
        g.upc,
        SUM(CASE WHEN COALESCE(p.present,0) = 1 THEN 1 ELSE 0 END) AS pre_weeks_present
    FROM store_upc_grid g
    JOIN stg.store_apg_windows w
      ON w.store_id = g.store_id
    JOIN (
        SELECT DISTINCT store_id, week_seq
        FROM stg.pos_weekly_presence
    ) wk
      ON wk.store_id = g.store_id
    LEFT JOIN stg.pos_weekly_presence p
      ON p.store_id = g.store_id
     AND p.upc      = g.upc
     AND p.week_seq = wk.week_seq
    WHERE wk.week_seq BETWEEN w.start_win_start_seq AND w.start_win_end_seq
      AND wk.week_seq < w.apg_start_seq
    GROUP BY g.store_id, g.upc
),

-- post counts
post_counts AS (
    SELECT
        g.store_id,
        g.upc,
        SUM(CASE WHEN COALESCE(p.present,0) = 1 THEN 1 ELSE 0 END) AS post_weeks_present
    FROM store_upc_grid g
    JOIN stg.store_apg_windows w
      ON w.store_id = g.store_id
     AND w.end_win_start_seq IS NOT NULL
    JOIN (
        SELECT DISTINCT store_id, week_seq
        FROM stg.pos_weekly_presence
    ) wk
      ON wk.store_id = g.store_id
    LEFT JOIN stg.pos_weekly_presence p
      ON p.store_id = g.store_id
     AND p.upc      = g.upc
     AND p.week_seq = wk.week_seq
    WHERE wk.week_seq BETWEEN w.end_win_start_seq AND w.end_win_end_seq
      AND wk.week_seq > w.apg_end_seq
    GROUP BY g.store_id, g.upc
)

SELECT
    sw.store_id,
    sw.upc,

    sw.start_weeks_in_window,
    sw.start_weeks_present,
    CASE 
        WHEN sw.start_weeks_in_window > 0
        THEN 1.0 * sw.start_weeks_present / sw.start_weeks_in_window
        ELSE 0
    END AS pct_present_start_window,

    ISNULL(ew.end_weeks_in_window, 0) AS end_weeks_in_window,
    ISNULL(ew.end_weeks_present, 0)   AS end_weeks_present,
    CASE 
        WHEN ISNULL(ew.end_weeks_in_window, 0) > 0
        THEN 1.0 * ISNULL(ew.end_weeks_present, 0) / ISNULL(ew.end_weeks_in_window, 0)
        ELSE 0
    END AS pct_present_end_window,

    ISNULL(pre.pre_weeks_present, 0)   AS pre_weeks_present,
    ISNULL(post.post_weeks_present, 0) AS post_weeks_present
INTO stg.coverage_metrics
FROM start_window sw
LEFT JOIN end_window ew
  ON ew.store_id = sw.store_id
 AND ew.upc      = sw.upc
LEFT JOIN pre_counts pre
  ON pre.store_id = sw.store_id
 AND pre.upc      = sw.upc
LEFT JOIN post_counts post
  ON post.store_id = sw.store_id
 AND post.upc      = sw.upc;

CREATE INDEX IX_cov_store_upc ON stg.coverage_metrics(store_id, upc);

-- I'm still confused as to why retailer 1 is showing up in this table...
-- We filter out retailer 1 from store_dim, just not TN, SC, or NC
SELECT 
	DISTINCT sd.retailer_id,
	COUNT(cm.store_id) AS n_stores
FROM stg.coverage_metrics cm
LEFT JOIN stg.store_dim sd
ON cm.store_id = sd.store_id
GROUP BY sd.retailer_id;

SELECT DISTINCT sd.retailer_id, count(cm.store_id) 
FROM stg.coverage_metrics cm
LEFT JOIN stg.store_dim sd
ON sd.store_id = cm.store_id
GROUP BY sd.retailer_id;

/*==========================
 10) PASS/FAIL & UPC-LEVEL COVERAGE ACROSS STORES (UNCONDITIONAL)
  - Step 10a: For each store–UPC, mark whether it passes:
      * ≥ @MIN_WINDOW_COVER presence in start window
      * ≥ @MIN_WINDOW_COVER presence in end window
      * ≥ @MIN_PRE_WEEKS pre-APG weeks present
      * ≥ @MIN_POST_WEEKS post-APG weeks present
    -> stg.store_upc_pass

  - Step 10b: Aggregate to UPC-level coverage across the FULL store universe:
      * stores_total = number of stores in store_universe
      * stores_pass  = sum(pass_flag) across that universe
      * share_stores_pass = stores_pass / stores_total
    -> stg.product_coverage
==========================*/

IF OBJECT_ID('stg.store_upc_pass','U') IS NOT NULL DROP TABLE stg.store_upc_pass;

DECLARE @MIN_WINDOW_COVER FLOAT = 0.80;
DECLARE @MIN_PRE_WEEKS    INT   = 5;
DECLARE @MIN_POST_WEEKS   INT   = 5;
DECLARE @MIN_STORE_SHARE  FLOAT = 0.75;
DECLARE @PRODS_PER_CATEGORY INT = 5;

SELECT
    c.store_id,
    sd.retailer_id,
    c.upc,
    CASE 
        WHEN c.pct_present_start_window >= @MIN_WINDOW_COVER
         AND c.pct_present_end_window   >= @MIN_WINDOW_COVER
         AND c.pre_weeks_present        >= @MIN_PRE_WEEKS
         AND c.post_weeks_present       >= @MIN_POST_WEEKS
        THEN 1 ELSE 0
    END AS pass_flag
INTO stg.store_upc_pass
FROM stg.coverage_metrics c
LEFT JOIN stg.store_dim sd
ON sd.store_id = c.store_id
WHERE sd.retailer_id <> 1;

CREATE INDEX IX_store_upc_pass ON stg.store_upc_pass(upc, store_id);

SELECT DISTINCT retailer_id FROM stg.store_upc_pass;

IF OBJECT_ID('stg.product_coverage','U') IS NOT NULL DROP TABLE stg.product_coverage;

WITH store_counts AS (
    SELECT
        upc,
        COUNT(*) AS stores_total,
        SUM(pass_flag) AS stores_pass
    FROM stg.store_upc_pass
    GROUP BY upc
)
SELECT
    upc,
    stores_total,
    stores_pass,
    CASE 
        WHEN stores_total > 0 THEN 1.0 * stores_pass / stores_total
        ELSE 0
    END AS share_stores_pass
INTO stg.product_coverage
FROM store_counts;

CREATE INDEX IX_product_cov ON stg.product_coverage(upc);

SELECT * FROM stg.product_coverage ORDER BY share_stores_pass DESC;


/*==========================
 11) PRODUCT VOLUME & INITIAL PRODUCT SELECTION
  - Now operating on restricted categories only + unconditional store share
==========================*/
IF OBJECT_ID('stg.product_volume','U') IS NOT NULL DROP TABLE stg.product_volume;

-- Category restriction list
IF OBJECT_ID('tempdb..#cat_keep') IS NOT NULL DROP TABLE #cat_keep;
CREATE TABLE #cat_keep (category VARCHAR(50) PRIMARY KEY);

INSERT INTO #cat_keep(category) VALUES
('APPLES'),('AVOCADO'),('BANANAS'),('BEEF'),('BROCCOLI'),('CABBAGE'),('CARROT'),
('CAULIFLOWER'),('CELERY'),('CHERRY'),('CHICKEN'),('CORN'),('CRANBERRIES'),
('CUCUMBER'),('EGGPLANT'),('GRAPEFRUIT'),('GRAPES'),('KIWI'),('LEMON'),
('LETTUCE'),('MELONS'),('ONIONS'),('ORANGES'),('PEACH'),('PEARS'),('PEPPERS'),
('PINEAPPLES'),('PLUMS'),('PORK'),('POTATOES'),('SPINACH'),('SQUASH'),
('STRAWBERRIES'),('TOMATOES'),('TURKEY'),('WATERMELON');

SELECT
    p.upc,
    ck.general_category,
    ck.category,
    SUM(p.weekly_net_sales) AS total_net_sales,
    SUM(p.weekly_volume)    AS total_volume
INTO stg.product_volume
FROM stg.pos_weekly_presence p
JOIN stg.pd pd
  ON p.upc = pd.UPC
JOIN stg.category_key ck
  ON ck.category_key = pd.category_key
JOIN #cat_keep k
  ON UPPER(ck.category) = k.category
GROUP BY
    p.upc,
    ck.general_category,
    ck.category;

SELECT * FROM stg.product_volume;

IF OBJECT_ID('stg.selected_products','U') IS NOT NULL DROP TABLE stg.selected_products;

DECLARE @MIN_WINDOW_COVER FLOAT = 0.80;
DECLARE @MIN_PRE_WEEKS    INT   = 5;
DECLARE @MIN_POST_WEEKS   INT   = 5;
DECLARE @MIN_STORE_SHARE  FLOAT = 0.75;
DECLARE @PRODS_PER_CATEGORY INT = 5;

WITH eligible AS (
    SELECT
        v.upc,
        v.general_category,
        v.category,
        v.total_net_sales,
        v.total_volume,
        pc.share_stores_pass
    FROM stg.product_volume v
    JOIN stg.product_coverage pc
      ON pc.upc = v.upc
    WHERE pc.share_stores_pass >= @MIN_STORE_SHARE
),
ranked AS (
    SELECT
        e.*,
        ROW_NUMBER() OVER (
            PARTITION BY e.category
            ORDER BY e.total_net_sales DESC, e.total_volume DESC
        ) AS rn
    FROM eligible e
)
SELECT
    general_category,
    category,
    upc,
    total_net_sales,
    total_volume,
    share_stores_pass,
    rn AS category_rank
INTO stg.selected_products
FROM ranked
WHERE rn <= @PRODS_PER_CATEGORY;

SELECT * FROM stg.selected_products
ORDER by general_category, category_rank, share_stores_pass DESC;

-- checks
SELECT COUNT(*) AS n_selected FROM stg.selected_products;
SELECT TOP 50 * FROM stg.selected_products ORDER BY share_stores_pass DESC, total_net_sales DESC;

-- For a given UPC, how many stores are in the denominator?
SELECT upc, stores_total, stores_pass, share_stores_pass
FROM stg.product_coverage
WHERE upc = '7143001065';

-- For that UPC, how many store-UPC pairs exist in store_upc_pass?
SELECT COUNT(*) AS n_store_upc_rows
FROM stg.store_upc_pass
WHERE upc = '7143001065';

-- Stores_total and stores_pass by retailer_id for a set of UPCs
-- (Denominator = store universe in stg.store_apg_windows; numerator = pass_flag=1 in stg.store_upc_pass)

WITH upc_list AS (
    SELECT upc
    FROM (VALUES
        ('4011'), --bananas
        ('64312604011'), --bananas
        ('4065'), --peppers
        ('4087'), --tomatoes
        ('4062'), --cucumber
        ('7143001065'), --lettuce
        ('4069') --cabbage
    ) v(upc)
),
universe AS (
    SELECT DISTINCT w.store_id
    FROM stg.store_apg_windows w
)
SELECT
    ul.upc,
    sd.retailer_id,
    COUNT(DISTINCT u.store_id) AS stores_total,
    COUNT(DISTINCT CASE WHEN sup.pass_flag = 1 THEN u.store_id END) AS stores_pass,
    1.0 * COUNT(DISTINCT CASE WHEN sup.pass_flag = 1 THEN u.store_id END)
        / NULLIF(COUNT(DISTINCT u.store_id), 0) AS share_stores_pass
FROM upc_list ul
CROSS JOIN universe u
JOIN stg.store_dim sd
  ON sd.store_id = u.store_id
LEFT JOIN stg.store_upc_pass sup
  ON sup.store_id = u.store_id
 AND sup.upc = ul.upc
GROUP BY
    ul.upc,
    sd.retailer_id
ORDER BY
    ul.upc,
    sd.retailer_id;

/*==========================
  13) STORE–UPC–WEEK PANEL (analysis dataset) + APG fields
  - Unit: store_id × upc × week_seq
  - Keeps weekly outcomes from stg.pos_weekly_presence
  - Adds:
      * week_year, week_of_year from stg.date_week_index
      * apg_start_date, apg_end_date from stg.state_apg_periods
      * SoE_apg_active (weekly): 1 if the week overlaps the APG interval
        overlap rule: week_start <= apg_end_date AND week_end >= apg_start_date
==========================*/

IF OBJECT_ID('stg.store_upc_week','U') IS NOT NULL
    DROP TABLE stg.store_upc_week;

-- eligible_upcs 
-- Filters the UPC universe to items that appear in at least @MIN_STORE_SHARE_UPC
-- share of stores during the relevant coverage window. Drops sparse or idiosyncratic
-- UPCs that would introduce noise into product-level price and cost averages.

-- select_weekly
-- Pulls weekly aggregates from pos_weekly_presence for eligible UPCs only.
-- pos_weekly_presence is the canonical weekly aggregation of pos_enriched and
-- already excludes retailer 1 and NC/SC stores.

-- sale_share
-- Computes the share of transactions flagged as on-sale at the store-UPC-week level.
-- share_on_sale is a transaction-count-based measure: transactions on sale divided by
-- total transactions. It is carried through as a diagnostic variable to assess whether
-- deal frequency shifts systematically during the SOE period, which would bias
-- p_ijst_net relative to p_ijst_gross.

-- agg
-- Joins store, category, calendar, and APG metadata onto the weekly UPC aggregates.
-- SUM() is applied to the three weekly revenue and cost totals because the join to
-- pd and category_key can produce multiple rows per store-UPC-week if a UPC maps to
-- more than one category key. Verify that this join is one-to-one; if so, the SUM
-- is harmless but the GROUP BY guards against double-counting.
--
-- The APG overlap condition flags a week as SOE-active if the seven-day window
-- [week_start, week_start + 6] overlaps the state-level APG enforcement interval
-- [apg_start_date, apg_end_date]. This is the correct interval-overlap logic.
-- Confirm that stg.state_apg_periods has exactly one row per state; if a state has
-- multiple APG periods the join will produce duplicate rows and the SUM will
-- double-count volume and revenue.

DECLARE @MIN_STORE_SHARE_UPC FLOAT = 0.75;

WITH eligible_upcs AS (
	SELECT pc.upc
    FROM stg.product_coverage pc
    WHERE pc.share_stores_pass >= @MIN_STORE_SHARE_UPC
),
selected_weekly AS (
    SELECT 
        p.store_id,
        p.upc,
        p.week_seq,
        p.weekly_volume,
        p.weekly_net_sales,
        p.weekly_gross_sales,
        p.weekly_total_cost,
        p.weekly_transactions_on_sale,
        p.weekly_transactions_total,
        p.avg_unit_price,   -- simple average of daily posted shelf prices
        p.avg_sale_price,    -- simple average of daily sale prices
        p.avg_cost_min,    -- simple average of daily unit cost (min)
        p.avg_cost_max    -- simple average of daily unit cost (max)
    FROM stg.pos_weekly_presence p
    JOIN eligible_upcs eu
      ON eu.upc = p.upc
),
sale_share AS (
    SELECT
        sw.store_id,
        sw.upc,
        sw.week_seq,
        CAST(sw.weekly_transactions_on_sale AS float)
            / NULLIF(sw.weekly_transactions_total, 0) AS share_on_sale,
        sw.weekly_transactions_on_sale,
        sw.weekly_transactions_total
    FROM selected_weekly sw
),
agg AS (
    SELECT
        sw.store_id,
        sd.retailer_id,
        sd.sst,
        ck.general_category,
        ck.category,
        sw.upc,
        sw.week_seq,
        dwi.yr AS week_year,
        dwi.wk AS week_of_year,
        -- UPC-level totals within store × week
        SUM(sw.weekly_net_sales)  AS upc_week_net_sales,
        SUM(sw.weekly_gross_sales) AS upc_week_gross_sales,
        SUM(sw.weekly_volume)     AS upc_week_volume,
        SUM(sw.weekly_total_cost) AS upc_week_total_cost,
        -- If the pd/category_key join is one-to-one these equal the input values;
        -- AVG() guards against inflating them if the join is not one-to-one
        AVG(sw.avg_unit_price)      AS avg_unit_price,
        AVG(sw.avg_sale_price)      AS avg_sale_price,
        AVG(sw.avg_cost_min)      AS avg_unit_cost_min,
        AVG(sw.avg_cost_max)      AS avg_unit_cost_max,
        sap.apg_start_date,
        sap.apg_end_date,
        -- Weekly APG active: week overlaps the APG interval
        CASE
            WHEN sap.apg_start_date IS NOT NULL
             AND sap.apg_end_date   IS NOT NULL
             AND dwi.week_start <= sap.apg_end_date
             AND DATEADD(day, 6, dwi.week_start) >= sap.apg_start_date
            THEN 1 ELSE 0
        END AS SoE_apg_active
    FROM selected_weekly sw
    JOIN stg.store_dim sd
      ON sd.store_id = sw.store_id
    JOIN stg.pd pd
      ON pd.UPC = sw.upc
    JOIN stg.category_key ck
      ON ck.category_key = pd.category_key
    JOIN stg.date_week_index dwi
      ON dwi.week_seq = sw.week_seq
    JOIN stg.state_apg_periods sap
      ON sap.sst = sd.sst
    WHERE sd.retailer_id <> 1
    GROUP BY
        sw.store_id,
        sd.retailer_id,
        sd.sst,
        ck.general_category,
        ck.category,
        sw.upc,
        sw.week_seq,
        dwi.yr,
        dwi.wk,
        dwi.week_start,
        sap.apg_start_date,
        sap.apg_end_date
)
SELECT
    a.store_id,
    a.retailer_id,
    a.sst,
    a.general_category,
    a.category,
    a.upc,
    a.week_seq,
    a.week_year,
    a.week_of_year,
    a.upc_week_net_sales,
    a.upc_week_gross_sales,
    a.upc_week_volume,
    a.upc_week_total_cost,
    -- Revenue-weighted prices
    -- Net price: total revenue after promotional discounts divided by volume.
    -- Reflects the average price actually paid. Will be pulled down in weeks
    -- with high deal activity relative to p_ijst_gross.
    CASE 
        WHEN a.upc_week_volume > 0 
        THEN a.upc_week_net_sales / a.upc_week_volume
        ELSE NULL 
    END AS p_ijst_net,
    -- Gross price: posted shelf revenue divided by volume.
    -- Strips out promotional discounts; closer to the shelf price a non-deal
    -- customer would face. Use as the primary price variable in main regressions;
    -- p_ijst_net serves as a robustness check.
    CASE 
        WHEN a.upc_week_volume > 0 
        THEN a.upc_week_gross_sales / a.upc_week_volume
        ELSE NULL 
    END AS p_ijst_gross,
    -- Simple average prices (robustness comparison against revenue-weighted)
    a.avg_unit_price,
    a.avg_sale_price,
    -- Unit wholesale cost: total acquisition cost divided by volume.
	-- Diagnostic checks confirm that unit_cost varies within store-UPC-date
	-- in two cases only: (1) floating point rounding artifacts from weight-based
	-- division, which collapse to a single value when rounded to 2 decimal places,
	-- and (2) rare genuine two-rate days reflecting mid-day invoice price changes
	-- or multiple shipments. The revenue-weighted w_ijst averages across both
	-- cases correctly and is preferred over transaction-level unit_cost.
	CASE 
        WHEN a.upc_week_volume > 0 
        THEN a.upc_week_total_cost / a.upc_week_volume
        ELSE NULL 
    END AS w_ijst,
    a.avg_unit_cost_min,
    a.avg_unit_cost_max,
    ss.share_on_sale,
    ss.weekly_transactions_on_sale,
    ss.weekly_transactions_total,
    a.apg_start_date,
    a.apg_end_date,
    a.SoE_apg_active
INTO stg.store_upc_week
FROM agg a
LEFT JOIN sale_share ss
  ON ss.store_id = a.store_id
 AND ss.upc      = a.upc
 AND ss.week_seq = a.week_seq;

CREATE INDEX IX_suw_store_upc_week
    ON stg.store_upc_week(store_id, upc, week_seq);

CREATE INDEX IX_suw_state_week
    ON stg.store_upc_week(sst, week_seq);

SELECT TOP 50 * FROM stg.store_upc_week;

-- Sanity checks
SELECT COUNT(*) AS n_rows FROM stg.store_upc_week;
--n=5,040,999

SELECT TOP 50 * FROM stg.store_upc_week ORDER BY store_id, upc, week_seq;

-- =======================
-- Choose specific items
-- oranges, bananas, apples, lettuce, cucumbers, squash, chicken, beef, turkey
-- =======================

-- Cabbage
IF OBJECT_ID('stg.pos_cabbage','U') IS NOT NULL
    DROP TABLE stg.pos_cabbage;

SELECT * 
INTO stg.pos_cabbage
FROM stg.store_upc_week
WHERE upc = '4069';


-- Bananas
IF OBJECT_ID('stg.pos_bananas_4011','U') IS NOT NULL
    DROP TABLE stg.pos_bananas_4011;

SELECT *
INTO stg.pos_bananas_4011
FROM stg.store_upc_week
WHERE upc = '4011';

SELECT TOP 10 *
FROM stg.pos_bananas_4011
ORDER BY store_id, week_seq;


-- Lettuce
IF OBJECT_ID('stg.pos_lettuce','U') IS NOT NULL
    DROP TABLE stg.pos_lettuce;

SELECT * 
INTO stg.pos_lettuce
FROM stg.store_upc_week
WHERE upc = '7143001065';

SELECT TOP 50 *
FROM stg.pos_lettuce
ORDER BY store_id, week_seq;


-- Peppers
IF OBJECT_ID('stg.pos_peppers','U') IS NOT NULL
    DROP TABLE stg.pos_peppers;

SELECT * 
INTO stg.pos_peppers
FROM stg.store_upc_week
WHERE upc = '4065';

SELECT TOP 50 *
FROM stg.pos_peppers
ORDER BY store_id, week_seq;


-- Cucumbers
IF OBJECT_ID('stg.pos_cucumbers','U') IS NOT NULL
    DROP TABLE stg.pos_cucumbers;

SELECT * 
INTO stg.pos_cucumbers
FROM stg.store_upc_week
WHERE upc = '4062';

SELECT TOP 50 *
FROM stg.pos_cucumbers
ORDER BY store_id, week_seq;


-- Tomatoes
IF OBJECT_ID('stg.pos_tomatoes','U') IS NOT NULL
    DROP TABLE stg.pos_tomatoes;

SELECT * 
INTO stg.pos_tomatoes
FROM stg.store_upc_week
WHERE upc = '4087';

SELECT TOP 50 *
FROM stg.pos_tomatoes
ORDER BY store_id, week_seq;


SELECT 
  TABLE_SCHEMA,
  TABLE_NAME,
  ORDINAL_POSITION,
  COLUMN_NAME,
  DATA_TYPE
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_SCHEMA = 'stg'
  AND TABLE_NAME IN (
    'pos_bananas_4011',
    'pos_lettuce',
    'pos_peppers',
    'pos_cucumbers',
    'pos_tomatoes'
  )
ORDER BY TABLE_NAME, ORDINAL_POSITION;

IF OBJECT_ID('stg.week_month_year','U') IS NOT NULL DROP TABLE stg.week_month_year;

SELECT
  week_seq,
  yr   AS year,
  wk   AS week_of_year,
  MONTH(week_start) AS month,
  DATENAME(MONTH, week_start) AS month_name,
  week_start
INTO stg.week_month_year
FROM stg.date_week_index;

CREATE UNIQUE INDEX UX_week_month_year_seq ON stg.week_month_year(week_seq);

SELECT * FROM stg.week_month_year;

-- How many UPCs pass the 0.85 threshold?
SELECT COUNT(*) AS n_upcs_pass
FROM stg.product_coverage
WHERE share_stores_pass >= 0.85;

-- How many rows in the analysis datasets?
SELECT COUNT(*) AS n_weekly_rows FROM stg.store_upc_week;



/*==========================
  Information to connect with R
==========================*/
-- Information needed to connect with R
SELECT @@SERVERNAME AS server_name;
-- Orchard

SELECT  
    @@SERVERNAME                              AS server_name,
    -- Orchard
    SERVERPROPERTY('MachineName')             AS machine_name,
    -- Orchard
    SERVERPROPERTY('InstanceName')            AS instance_name,
    -- [NULL]
    SERVERPROPERTY('Edition')                 AS edition,
    -- Developer Edition (64-bit)
    SERVERPROPERTY('ProductVersion')          AS product_version;
	-- 16.0.4222.2

SELECT DB_NAME() AS current_database;
-- DecaData

SELECT name AS database_name
FROM sys.databases
ORDER BY name;






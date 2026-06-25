/* ------------------------------------------------------------ 
* Decription: Pare down the DecaData to top products before doing analysis
* Origination: D:\Data\Lauren-Tim-Katya\02_Code\SQL\BuildMarkups_2025_11.sql
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
-- Distinct shoppers and shopping frequency by SOE period
-- Unit: store x week
-- Scope: five focal UPCs only, retailer 1 excluded, NC and SC excluded
-- Transaction ID: REGISTER_NUMBER + TRANSACTION_NUMBER + STORE_NUMBER
-- Shopper ID: REWARD_CARD_NUMBER, excluding 0 (non-loyalty transactions)
==========================*/

WITH focal_upcs AS (
    SELECT upc
    FROM (VALUES ('4011'), ('4087'), ('4062'), ('4065'), ('7143001065')) v(upc)
),
transactions AS (
    SELECT
        CONVERT(date, CONVERT(varchar(8), p.TRANSACTION_DATE), 112) AS trx_date,
        p.STORE_NUMBER                                               AS store_id,
        p.UPC                                                        AS upc,
        TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT)                   AS item_weight,
        p.NUMBER_OF_UNITS_SCANNED                                    AS units_sold,
        CONCAT(p.STORE_NUMBER, '-',
               p.REGISTER_NUMBER, '-',
               p.TRANSACTION_NUMBER)                                 AS transaction_id,
        CASE 
            WHEN TRY_CONVERT(bigint, p.REWARD_CARD_NUMBER) = 0 
              OR p.REWARD_CARD_NUMBER IS NULL 
            THEN NULL 
            ELSE p.REWARD_CARD_NUMBER 
        END                                                          AS card_number
    FROM DecaData.dbo.tempPOS_retailer_2_5 p
    JOIN focal_upcs fu
      ON fu.upc = p.UPC
    JOIN stg.store_dim sd
      ON sd.store_id = p.STORE_NUMBER
    WHERE sd.retailer_id <> 1
      AND sd.sst NOT IN ('NC', 'SC')
),
with_weeks AS (
    SELECT
        t.store_id,
        t.upc,
        t.trx_date,
        t.transaction_id,
        t.card_number,
        t.item_weight,
        t.units_sold,
        dwi.week_seq,
        sap.sst,
        CASE
            WHEN sap.apg_start_date IS NOT NULL
             AND sap.apg_end_date   IS NOT NULL
             AND t.trx_date >= sap.apg_start_date
             AND t.trx_date <= sap.apg_end_date  THEN 'During SOE'
            WHEN sap.apg_start_date IS NOT NULL
             AND t.trx_date < sap.apg_start_date THEN 'Pre-SOE'
            WHEN sap.apg_end_date IS NOT NULL
             AND t.trx_date > sap.apg_end_date   THEN 'Post-SOE'
        END AS soe_period
    FROM transactions t
    JOIN stg.store_dim sd
      ON sd.store_id = t.store_id
    JOIN stg.state_apg_periods sap
      ON sap.sst = sd.sst
    JOIN stg.date_week_index dwi
      ON dwi.yr = YEAR(t.trx_date)
     AND dwi.wk = DATEPART(ISO_WEEK, t.trx_date)
)
SELECT
    w.upc,
    w.soe_period,
    COUNT(DISTINCT w.week_seq)                                        AS n_weeks,
    COUNT(DISTINCT w.transaction_id)                                  AS n_transactions,
    COUNT(DISTINCT w.card_number)                                     AS n_distinct_shoppers,
    COUNT(DISTINCT CASE 
        WHEN w.card_number IS NULL THEN w.transaction_id 
    END)                                                              AS n_noloyalty_transactions,
    CAST(COUNT(DISTINCT w.transaction_id) AS float) 
        / NULLIF(COUNT(DISTINCT w.week_seq), 0)                       AS avg_transactions_per_week,
    CAST(COUNT(DISTINCT w.card_number) AS float)
        / NULLIF(COUNT(DISTINCT w.week_seq), 0)                       AS avg_shoppers_per_week_approx,
    SUM(w.item_weight)                                                AS total_weight,
    SUM(w.units_sold)                                                 AS total_units,
    -- Unified volume: weight if non-zero, else units
    SUM(CASE 
        WHEN w.item_weight > 0 
        THEN w.item_weight
        ELSE w.units_sold
    END)                                                              AS total_volume,
    -- Average volume per transaction
    SUM(CASE 
        WHEN w.item_weight > 0 
        THEN w.item_weight
        ELSE w.units_sold
    END) / NULLIF(COUNT(DISTINCT w.transaction_id), 0)               AS avg_volume_per_transaction,
    -- Average volume per shopper
    SUM(CASE 
        WHEN w.item_weight > 0 
        THEN w.item_weight
        ELSE w.units_sold
    END) / NULLIF(COUNT(DISTINCT w.card_number), 0)                  AS avg_volume_per_shopper
FROM with_weeks w
WHERE w.soe_period IS NOT NULL
GROUP BY
    w.upc,
    w.soe_period
ORDER BY
    w.upc,
    w.soe_period;

-- Check for coexistence of sale and non-sale transactions
-- within the same store-UPC-date for the five focal UPCs.
-- Uses effective price paid (net sales / volume) to classify
-- each transaction rather than relying on the sale price field alone.

WITH focal_upcs AS (
    SELECT upc
    FROM (VALUES ('4011'), ('4087'), ('4062'), ('4065'), ('7143001065')) v(upc)
),
transactions AS (
    SELECT
        CONVERT(date, CONVERT(varchar(8), p.TRANSACTION_DATE), 112) AS trx_date,
        p.STORE_NUMBER                                               AS store_id,
        p.UPC                                                        AS upc,
        TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE)       AS unit_price,
        TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE)          AS sale_price,
        -- Effective price paid: net sales divided by volume
        TRY_CONVERT(decimal(18,4), p.ITEM_NET_SALES) /
            NULLIF(CASE 
                WHEN TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT) > 0 
                THEN TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT)
                ELSE p.NUMBER_OF_UNITS_SCANNED
            END, 0)                                                  AS effective_price,
        TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE)       AS regular_price,
        CASE 
            WHEN TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE) = 0
              OR TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE) = 
                 TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE)
            THEN 0 ELSE 1
        END                                                          AS is_on_sale
    FROM DecaData.dbo.tempPOS_retailer_2_5 p
    JOIN focal_upcs fu
      ON fu.upc = p.UPC
    JOIN stg.store_dim sd
      ON sd.store_id = p.STORE_NUMBER
    WHERE sd.retailer_id <> 1
      AND sd.sst NOT IN ('NC', 'SC')
      AND p.ITEM_NET_SALES > 0
      AND p.NUMBER_OF_UNITS_SCANNED > 0
),
store_upc_date AS (
    -- For each store-UPC-date, count transactions at each price type
    SELECT
        trx_date,
        store_id,
        upc,
        COUNT(*)                                    AS total_transactions,
        SUM(is_on_sale)                             AS n_sale_transactions,
        COUNT(*) - SUM(is_on_sale)                  AS n_regular_transactions,
        -- Flag dates where both sale and regular transactions coexist
        CASE 
            WHEN SUM(is_on_sale) > 0 
             AND COUNT(*) - SUM(is_on_sale) > 0 
            THEN 1 ELSE 0 
        END                                         AS both_types_present,
        MIN(effective_price)                        AS min_effective_price,
        MAX(effective_price)                        AS max_effective_price,
        MIN(unit_price)                             AS regular_price,
        MIN(sale_price)                             AS sale_price
    FROM transactions
    GROUP BY trx_date, store_id, upc
)
-- Summary by UPC: how often do both transaction types coexist?
SELECT
    upc,
    COUNT(*)                                        AS total_store_upc_dates,
    SUM(both_types_present)                         AS n_dates_both_types,
    ROUND(100.0 * SUM(both_types_present) 
        / NULLIF(COUNT(*), 0), 2)                   AS pct_dates_both_types,
    SUM(n_sale_transactions)                        AS total_sale_transactions,
    SUM(n_regular_transactions)                     AS total_regular_transactions
FROM store_upc_date
GROUP BY upc
ORDER BY upc;

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

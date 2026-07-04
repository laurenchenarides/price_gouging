/* ----------------------------------------------------------------------------
 * BuildMarkupsNew_PriceDiscrimination.sql
 *
 * Purpose: Build a store-UPC-date summary table for the price discrimination
 *   mechanism (Mechanism 3). The key question is whether all shoppers pay the
 *   same price for the same product on the same day at the same store, or
 *   whether price dispersion exists within a store-day cell -- and whether
 *   that dispersion changed during the SOE.
 *
 * Countercyclical pricing angle (Butters 2025):
 *   Retailers use promotional discounts to price discriminate between
 *   price-elastic shoppers (who hunt for deals) and price-inelastic shoppers
 *   (who pay the regular price). If APG laws or pandemic-era demand shifts
 *   changed the composition of shoppers or the elasticity of demand, optimal
 *   retail strategy may have changed the frequency or depth of promotions.
 *
 * Transaction identification:
 *   The distinct transaction is REGISTER_NUMBER x TRANSACTION_NUMBER within
 *   a STORE_NUMBER x TRANSACTION_DATE. REWARD_CARD_NUMBER is retained as a
 *   descriptive variable (share_loyalty) but is not used as a shopper
 *   identifier: card numbers are not unique across retailers and may appear
 *   across hundreds of stores within a chain.
 *
 * Price dispersion measure:
 *   Within a store x UPC x date cell, price dispersion exists when both
 *   regular-price and sale-price transactions occur on the same day
 *   (both_types_present = 1). The discount depth is regular_price - sale_price,
 *   fixed within the cell. The share_on_sale measures what fraction of
 *   transactions received the promotional price.
 *
 * Focal UPCs: bananas (4011), cabbage (4069), cucumbers (4062),
 *   lettuce (7143001065), tomatoes (4087).
 *
 * Output tables:
 *   stg.pd_transactions    -- transaction grain (one row per scanned item)
 *   stg.pd_store_upc_day   -- store x UPC x date (price dispersion measures)
 *   stg.pd_store_upc_week  -- store x UPC x week (for joining to panel_est in R)
 *
 * R variables produced for Mechanism 3 regressions:
 *   share_on_sale      -- fraction of transactions at promotional price (weekly)
 *   avg_discount_depth -- mean regular - sale markdown when on sale (weekly)
 *   pct_days_dispersion-- share of days in week where sale/regular coexist
 *   p_net_weekly       -- net revenue-weighted price (= p_ijst_net analog)
 *   p_gross_weekly     -- gross revenue-weighted price (= p_ijst_gross analog)
 *   weekly_volume      -- quantity for demand equation ln Q
 *   share_loyalty      -- descriptive: fraction of transactions with loyalty card
 *
 * Last Updated: July 2, 2026
 * By: Lauren Chenarides
 * ---------------------------------------------------------------------------- */

USE [DecaData];

/* ============================================================================
 * 0. FOCAL UPCs
 * ========================================================================== */

DECLARE @focal_upcs TABLE (upc VARCHAR(20));
INSERT INTO @focal_upcs VALUES
    ('4011'),        -- bananas
    ('4069'),        -- cabbage
    ('4062'),        -- cucumbers
    ('7143001065'),  -- lettuce
    ('4087');        -- tomatoes

/* ============================================================================
 * 1. TRANSACTION-LEVEL EXTRACT
 *    Source: DecaData.dbo.tempPOS_retailer_2_5 (transaction grain)
 *    One row per scanned item line. The distinct transaction is
 *    STORE_NUMBER x TRANSACTION_DATE x REGISTER_NUMBER x TRANSACTION_NUMBER.
 *    card_number is retained for share_loyalty but not used as a shopper ID.
 * ========================================================================== */

IF OBJECT_ID('stg.pd_transactions','U') IS NOT NULL DROP TABLE stg.pd_transactions;

WITH raw AS (
    SELECT
        CONVERT(date, CONVERT(varchar(8), p.TRANSACTION_DATE), 112)  AS trx_date,
        p.STORE_NUMBER                                                 AS store_id,
        p.REGISTER_NUMBER                                              AS register_number,
        p.TRANSACTION_NUMBER                                           AS transaction_number,
        p.UPC                                                          AS upc,
        -- Loyalty card retained for share_loyalty descriptive only
        CASE
            WHEN TRY_CONVERT(bigint, p.REWARD_CARD_NUMBER) = 0
              OR p.REWARD_CARD_NUMBER IS NULL
            THEN NULL
            ELSE CAST(p.REWARD_CARD_NUMBER AS varchar(30))
        END                                                            AS card_number,
        -- Regular (shelf) price: the posted price any shopper faces
        TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE)         AS regular_price,
        -- Sale price: the promotional price a deal-seeking shopper faces
        -- NULL when not on sale (i.e., is_on_sale = 0)
        CASE
            WHEN TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE) > 0
             AND TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE) <>
                 TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE)
            THEN TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE)
            ELSE NULL
        END                                                            AS sale_price,
        -- On-sale flag: 1 if shopper paid promotional price, 0 otherwise
        CASE
            WHEN TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE) > 0
             AND TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE) <>
                 TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE)
            THEN 1 ELSE 0
        END                                                            AS is_on_sale,
        -- Volume: weight for produce sold by lb, units otherwise
        CASE
            WHEN TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT) > 0
            THEN TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT)
            ELSE CAST(p.NUMBER_OF_UNITS_SCANNED AS decimal(18,4))
        END                                                            AS volume,
        -- Revenue
        TRY_CONVERT(decimal(18,4), p.ITEM_NET_SALES)                  AS net_sales,
        TRY_CONVERT(decimal(18,4), p.ITEM_GROSS_SALES)                AS gross_sales
    FROM DecaData.dbo.tempPOS_retailer_2_5 p
    JOIN stg.store_dim sd
      ON sd.store_id = p.STORE_NUMBER
    WHERE p.UPC IN (SELECT upc FROM @focal_upcs)
      AND sd.retailer_id IN (2, 3, 5)
      AND sd.sst NOT IN ('NC', 'SC')
      AND p.NUMBER_OF_UNITS_SCANNED > 0
      AND TRY_CONVERT(decimal(18,4), p.ITEM_NET_SALES)          > 0
      AND TRY_CONVERT(decimal(18,4), p.ITEM_GROSS_SALES)        > 0
      AND TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE) > 0
)
SELECT
    r.trx_date,
    r.store_id,
    r.register_number,
    r.transaction_number,
    sd.retailer_id,
    sd.sst,
    r.upc,
    r.card_number,
    r.regular_price,
    r.sale_price,
    r.is_on_sale,
    r.volume,
    r.net_sales,
    r.gross_sales,
    -- SOE period label
    CASE
        WHEN sap.apg_start_date IS NOT NULL
         AND r.trx_date BETWEEN sap.apg_start_date AND sap.apg_end_date THEN 'During SOE'
        WHEN sap.apg_start_date IS NOT NULL
         AND r.trx_date < sap.apg_start_date                            THEN 'Pre-SOE'
        WHEN sap.apg_end_date IS NOT NULL
         AND r.trx_date > sap.apg_end_date                              THEN 'Post-SOE'
    END AS soe_period,
    dwi.week_seq
INTO stg.pd_transactions
FROM raw r
JOIN stg.store_dim sd
  ON sd.store_id = r.store_id
JOIN stg.state_apg_periods sap
  ON sap.sst = sd.sst
JOIN stg.date_week_index dwi
  ON dwi.yr = YEAR(r.trx_date)
 AND dwi.wk = DATEPART(ISO_WEEK, r.trx_date)
WHERE r.volume > 0;   -- exclude zero-volume records only

CREATE INDEX IX_pd_trx_store_upc_date ON stg.pd_transactions(store_id, upc, trx_date);

-- Sanity check: row counts by product
SELECT upc, COUNT(*) AS n_transactions FROM stg.pd_transactions
GROUP BY upc ORDER BY upc;

/* ============================================================================
 * 2. STORE x UPC x DATE SUMMARY (price dispersion grain)
 *
 *    Key dispersion measures:
 *      both_types_present  -- 1 if sale and regular transactions coexist
 *      share_on_sale       -- fraction of transactions at promotional price
 *      discount_depth      -- regular_price - sale_price (the markdown size)
 *      share_loyalty       -- fraction of transactions with a loyalty card
 *
 *    Revenue aggregates (net and gross) are carried forward so that
 *    revenue-weighted prices can be computed at the week level.
 * ========================================================================== */

IF OBJECT_ID('stg.pd_store_upc_day','U') IS NOT NULL DROP TABLE stg.pd_store_upc_day;

SELECT
    t.trx_date,
    t.store_id,
    t.retailer_id,
    t.sst,
    t.upc,
    t.week_seq,
    t.soe_period,
    -- Transaction counts
    COUNT(*)                                                        AS n_transactions,
    COUNT(t.card_number)                                            AS n_loyalty_transactions,
    SUM(t.is_on_sale)                                               AS n_sale_transactions,
    COUNT(*) - SUM(t.is_on_sale)                                    AS n_regular_transactions,
    -- Posted price points (fixed within store-UPC-day under uniform pricing)
    MIN(t.regular_price)                                            AS regular_price,
    MIN(CASE WHEN t.is_on_sale = 1 THEN t.sale_price END)          AS sale_price,
    -- Discount depth: size of markdown when a promotion is running
    -- NULL when no sale transactions exist on this day
    MIN(t.regular_price) -
        MIN(CASE WHEN t.is_on_sale = 1 THEN t.sale_price END)      AS discount_depth,
    -- Share measures
    CAST(SUM(t.is_on_sale) AS float)
        / NULLIF(COUNT(*), 0)                                       AS share_on_sale,
    CAST(COUNT(t.card_number) AS float)
        / NULLIF(COUNT(*), 0)                                       AS share_loyalty,
    -- Revenue and volume (carried forward for revenue-weighted price at week level)
    SUM(t.net_sales)                                                AS total_net_sales,
    SUM(t.gross_sales)                                              AS total_gross_sales,
    SUM(t.volume)                                                   AS total_volume,
    -- Price dispersion flag: both sale and regular transactions on same day
    CASE
        WHEN SUM(t.is_on_sale) > 0
         AND COUNT(*) - SUM(t.is_on_sale) > 0
        THEN 1 ELSE 0
    END                                                             AS both_types_present
INTO stg.pd_store_upc_day
FROM stg.pd_transactions t
WHERE t.soe_period IS NOT NULL
GROUP BY
    t.trx_date, t.store_id, t.retailer_id, t.sst,
    t.upc, t.week_seq, t.soe_period;

CREATE INDEX IX_pd_day_store_upc ON stg.pd_store_upc_day(store_id, upc, trx_date);

-- Summary: dispersion rates by product and SOE period
SELECT
    upc,
    soe_period,
    COUNT(*)                                                        AS n_store_upc_days,
    SUM(both_types_present)                                         AS n_days_with_dispersion,
    ROUND(100.0 * SUM(both_types_present) / COUNT(*), 2)           AS pct_days_with_dispersion,
    ROUND(AVG(share_on_sale) * 100, 2)                             AS avg_pct_on_sale,
    ROUND(AVG(CASE WHEN n_sale_transactions > 0
                   THEN discount_depth END), 4)                     AS avg_discount_depth,
    ROUND(AVG(share_loyalty) * 100, 2)                             AS avg_pct_loyalty
FROM stg.pd_store_upc_day
GROUP BY upc, soe_period
ORDER BY upc, soe_period;

-- Observations where shoppers paid different prices for the same product
-- on the same day at the same store
SELECT
    d.trx_date,
    d.store_id,
    d.retailer_id,
    d.sst,
    d.upc,
    d.soe_period,
    d.n_transactions,
    d.regular_price,
    d.sale_price,
    d.share_on_sale,
    d.share_loyalty,
    d.both_types_present
FROM stg.pd_store_upc_day d
WHERE d.both_types_present > 0
ORDER BY d.trx_date, d.store_id, d.upc;
-- RESULT: both_types_present = 0 everywhere means that on any given store-UPC-day, 
-- all shoppers paid the same price — either everyone got the sale price or everyone paid regular. 
-- There is no within-day price discrimination between individual shoppers.

-- This makes sense given share_loyalty = 96–99% across all products and periods. 
-- Sale prices in this data appear to be store-wide promotions (EDLP), not loyalty-card-exclusive discounts. 
-- When a sale runs, essentially the whole store gets it.

-- The price discrimination mechanism may be more inter-temporal (day-to-day than week-to-week). 
-- Retailers expanded how frequently they ran promotions during the SOE. So, not discriminating between
-- shoppers on the same day, but switching to more days/weeks with promotions.

/* ============================================================================
 * 3. STORE x UPC x WEEK SUMMARY (for R panel regressions)
 *
 *    This table joins to panel_est in R on (store_id, upc, week_seq).
 *    Variables map directly to the three Mechanism 3 tests:
 *
 *    Test 1 -- Gross vs net price (shelf price vs transaction price):
 *      p_gross_weekly, p_net_weekly
 *
 *    Test 2 -- Promotional expansion (share_on_sale regression):
 *      share_on_sale, avg_discount_depth
 *
 *    Test 3 -- Within-store price dispersion:
 *      pct_days_dispersion, both_types_ever (1 if any dispersion this week)
 *
 *    Demand equation (eq. demand_butters):
 *      weekly_volume (ln Q), p_net_weekly (ln p), soe_period
 * ========================================================================== */

IF OBJECT_ID('stg.pd_store_upc_week','U') IS NOT NULL DROP TABLE stg.pd_store_upc_week;

SELECT
    d.week_seq,
    d.store_id,
    d.retailer_id,
    d.sst,
    d.upc,
    d.soe_period,
    dwi.week_start,
    sap.apg_start_date,
    sap.apg_end_date,
    -- Transaction volume
    SUM(d.n_transactions)                                           AS weekly_transactions_total,
    SUM(d.n_loyalty_transactions)                                   AS weekly_loyalty_transactions,
    SUM(d.n_sale_transactions)                                      AS weekly_sale_transactions,
    -- Revenue-weighted prices (Test 1: gross vs net)
    -- p_net_weekly: average price actually paid after promotions
    SUM(d.total_net_sales)   / NULLIF(SUM(d.total_volume), 0)      AS p_net_weekly,
    -- p_gross_weekly: posted shelf revenue / volume; strips out promotions
    SUM(d.total_gross_sales) / NULLIF(SUM(d.total_volume), 0)      AS p_gross_weekly,
    -- Quantity for demand equation
    SUM(d.total_volume)                                             AS weekly_volume,
    SUM(d.total_net_sales)                                          AS weekly_net_sales,
    SUM(d.total_gross_sales)                                        AS weekly_gross_sales,
    -- Test 2: promotional expansion
    -- share_on_sale: fraction of transactions receiving promotional price
    CAST(SUM(d.n_sale_transactions) AS float)
        / NULLIF(SUM(d.n_transactions), 0)                         AS share_on_sale,
    -- avg_discount_depth: average markdown on days when a promotion ran
    AVG(CASE WHEN d.n_sale_transactions > 0
             THEN d.discount_depth END)                             AS avg_discount_depth,
    -- avg_regular_price and avg_sale_price for descriptive tables
    AVG(d.regular_price)                                            AS avg_regular_price,
    AVG(d.sale_price)                                               AS avg_sale_price,
    -- Test 3: within-store price dispersion
    -- pct_days_dispersion: share of days in this week where both price types coexist
    CAST(SUM(d.both_types_present) AS float)
        / NULLIF(COUNT(*), 0)                                       AS pct_days_dispersion,
    -- both_types_ever: 1 if any day this week had price dispersion
    MAX(d.both_types_present)                                       AS both_types_ever,
    -- Descriptive
    CAST(SUM(d.n_loyalty_transactions) AS float)
        / NULLIF(SUM(d.n_transactions), 0)                         AS share_loyalty,
    COUNT(*)                                                        AS n_days_observed
INTO stg.pd_store_upc_week
FROM stg.pd_store_upc_day d
JOIN stg.date_week_index dwi
  ON dwi.week_seq = d.week_seq
JOIN stg.state_apg_periods sap
  ON sap.sst = d.sst
GROUP BY
    d.week_seq, d.store_id, d.retailer_id, d.sst, d.upc, d.soe_period,
    dwi.week_start, sap.apg_start_date, sap.apg_end_date;

CREATE INDEX IX_pd_week_store_upc ON stg.pd_store_upc_week(store_id, upc, week_seq);

-- Final check: row counts by product
SELECT upc, COUNT(*) AS n_rows FROM stg.pd_store_upc_week GROUP BY upc ORDER BY upc;

-- Preview
SELECT TOP 100 * FROM stg.pd_store_upc_week ORDER BY store_id, upc, week_seq;

/* ============================================================================
 * 4. EXTENSIVE / INTENSIVE DECOMPOSITION INPUTS (Mechanism 3)
 *
 * Purpose: measure whether the SOE demand surge came from MORE purchase
 *   occasions buying the product (extensive margin) or LARGER quantity per
 *   occasion (intensive margin).
 *
 * Unit of a purchase occasion = a distinct basket, identified by
 *   STORE_NUMBER x TRANSACTION_DATE x REGISTER_NUMBER x TRANSACTION_NUMBER.
 *   (TRANSACTION_NUMBER is unique within a store-date-register.)
 *   REWARD_CARD_NUMBER is NOT used -- card numbers are not reliable shopper IDs.
 *
 * Identity (per store-UPC-week):
 *   weekly_volume = weekly_occasions x vol_per_occasion
 *   => ln Q = ln(occasions) + ln(volume per occasion)
 *
 * NOTE: stg.pd_store_upc_week already carries weekly_transactions_total (a count
 *   of transaction *item lines*) and weekly_volume, which approximate this
 *   decomposition. This table provides the exact distinct-basket occasion count.
 * ========================================================================== */

IF OBJECT_ID('stg.pd_ext_int_week','U') IS NOT NULL DROP TABLE stg.pd_ext_int_week;

SELECT
    t.week_seq,
    t.store_id,
    t.retailer_id,
    t.sst,
    t.upc,
    t.soe_period,
    dwi.week_start,
    -- Extensive margin: distinct purchase occasions (baskets) buying this
    -- product in this store-week. Date is included in the key because
    -- TRANSACTION_NUMBER resets by date.
    COUNT(DISTINCT CONCAT(
        CONVERT(varchar(8), t.trx_date, 112), '-',
        CAST(t.register_number    AS varchar(20)), '-',
        CAST(t.transaction_number AS varchar(20))
    ))                                                       AS weekly_occasions,
    -- Total quantity
    SUM(t.volume)                                            AS weekly_volume,
    -- Intensive margin: quantity per occasion
    SUM(t.volume) / NULLIF(COUNT(DISTINCT CONCAT(
        CONVERT(varchar(8), t.trx_date, 112), '-',
        CAST(t.register_number    AS varchar(20)), '-',
        CAST(t.transaction_number AS varchar(20))
    )), 0)                                                   AS vol_per_occasion,
    COUNT(*)                                                 AS weekly_item_lines
INTO stg.pd_ext_int_week
FROM stg.pd_transactions t
JOIN stg.date_week_index dwi
  ON dwi.week_seq = t.week_seq
WHERE t.soe_period IS NOT NULL
  AND t.volume > 0
GROUP BY
    t.week_seq, t.store_id, t.retailer_id, t.sst, t.upc, t.soe_period, dwi.week_start;

CREATE INDEX IX_pd_ext_int_week ON stg.pd_ext_int_week(store_id, upc, week_seq);

-- Descriptive: extensive vs intensive by product and SOE period
SELECT
    upc,
    soe_period,
    COUNT(*)                               AS n_store_weeks,
    AVG(CAST(weekly_occasions AS float))   AS avg_occasions,
    AVG(vol_per_occasion)                  AS avg_vol_per_occasion,
    AVG(weekly_volume)                     AS avg_volume
FROM stg.pd_ext_int_week
GROUP BY upc, soe_period
ORDER BY upc, soe_period;

-- Sanity: identity holds row-by-row (should return 0 rows)
SELECT TOP 20 *
FROM stg.pd_ext_int_week
WHERE ABS(weekly_volume - weekly_occasions * vol_per_occasion) > 0.01;

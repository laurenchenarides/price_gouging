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
 * Shopper identification: REWARD_CARD_NUMBER (loyalty card).
 *   - NULL or 0 = non-loyalty (anonymous) transaction.
 *   - Non-zero = identified loyalty shopper.
 *   Loyalty shoppers often receive promotional prices automatically at POS;
 *   non-loyalty shoppers typically pay the regular shelf price.
 *
 * Focal UPCs: bananas (4011), cabbage (4069), cucumbers (4062),
 *   lettuce (7143001065), tomatoes (4087).
 *
 * Output tables:
 *   stg.pd_store_upc_day   -- store x UPC x date level (price dispersion)
 *   stg.pd_store_upc_week  -- collapsed to week for joining to panel_est in R
 *
 * Last Updated: June 29, 2026
 * By: Lauren Chenarides
 * ---------------------------------------------------------------------------- */

USE [DecaData];

/* ============================================================================
 * 0. FOCAL UPCs
 * ========================================================================== */

-- Peppers (4065) replaced by cabbage (4069) due to very low purchase quantities.
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
 *    - One row per scanned item line
 *    - Restricted to focal UPCs, retailers 2/3/5, SE states only
 *    - card_number = NULL for non-loyalty transactions
 * ========================================================================== */

IF OBJECT_ID('stg.pd_transactions','U') IS NOT NULL DROP TABLE stg.pd_transactions;

WITH raw AS (
    SELECT
        CONVERT(date, CONVERT(varchar(8), p.TRANSACTION_DATE), 112)  AS trx_date,
        p.STORE_NUMBER                                                 AS store_id,
        p.UPC                                                          AS upc,
        -- Loyalty card: treat 0 and NULL as anonymous
        CASE
            WHEN TRY_CONVERT(bigint, p.REWARD_CARD_NUMBER) = 0
              OR p.REWARD_CARD_NUMBER IS NULL
            THEN NULL
            ELSE CAST(p.REWARD_CARD_NUMBER AS varchar(30))
        END                                                            AS card_number,
        -- Regular (shelf) price
        TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE)         AS regular_price,
        -- Sale price: NULL when not on sale
        TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE)            AS sale_price,
        -- Volume: prefer weight for produce sold by lb; fall back to units
        CASE
            WHEN TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT) > 0
            THEN TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT)
            ELSE CAST(p.NUMBER_OF_UNITS_SCANNED AS decimal(18,4))
        END                                                            AS volume,
        TRY_CONVERT(decimal(18,4), p.ITEM_NET_SALES)                  AS net_sales,
        TRY_CONVERT(decimal(18,4), p.ITEM_GROSS_SALES)                AS gross_sales,
        -- On-sale flag: sale price exists and differs from regular price
        CASE
            WHEN TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE) > 0
             AND TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_SALE_PRICE) <>
                 TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE)
            THEN 1 ELSE 0
        END                                                            AS is_on_sale,
        -- Effective price actually paid (net revenue / volume)
        TRY_CONVERT(decimal(18,4), p.ITEM_NET_SALES) /
            NULLIF(
                CASE
                    WHEN TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT) > 0
                    THEN TRY_CONVERT(decimal(18,4), p.ITEM_WEIGHT)
                    ELSE CAST(p.NUMBER_OF_UNITS_SCANNED AS decimal(18,4))
                END, 0)                                                AS effective_price
    FROM DecaData.dbo.tempPOS_retailer_2_5 p
    JOIN stg.store_dim sd
      ON sd.store_id = p.STORE_NUMBER
    WHERE p.UPC IN (SELECT upc FROM @focal_upcs)
      AND sd.retailer_id IN (2, 3, 5)
      AND sd.sst NOT IN ('NC', 'SC')
      AND p.NUMBER_OF_UNITS_SCANNED > 0
      AND TRY_CONVERT(decimal(18,4), p.ITEM_NET_SALES) > 0
      AND TRY_CONVERT(decimal(18,4), p.ITEM_UNIT_REGULAR_PRICE) > 0
)
SELECT
    r.trx_date,
    r.store_id,
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
    r.effective_price,
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
WHERE r.effective_price > 0
  AND r.effective_price < 50;   -- guard against residual volume artifacts

CREATE INDEX IX_pd_trx_store_upc_date ON stg.pd_transactions(store_id, upc, trx_date);

-- Sanity check
SELECT upc, COUNT(*) AS n FROM stg.pd_transactions GROUP BY upc ORDER BY upc;

/* ============================================================================
 * 2. STORE x UPC x DATE SUMMARY (price dispersion grain)
 *    Within each store-UPC-date:
 *      - How many transactions? How many were on sale?
 *      - What was the regular price? The sale price?
 *      - What was the effective price range (min/max)?
 *      - What share had a loyalty card?
 *    The key dispersion measure: max_effective_price - min_effective_price.
 *    When this is > 0 within a store-day, different shoppers paid different
 *    prices for the same product at the same store on the same day.
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
    COUNT(*)                                             AS n_transactions,
    COUNT(t.card_number)                                 AS n_loyalty_transactions,
    SUM(t.is_on_sale)                                    AS n_sale_transactions,
    COUNT(*) - SUM(t.is_on_sale)                         AS n_regular_transactions,
    -- Price points
    MIN(t.regular_price)                                 AS regular_price,
    MIN(CASE WHEN t.is_on_sale = 1 THEN t.sale_price END) AS sale_price,
    -- Effective price statistics (actual price paid)
    AVG(t.effective_price)                               AS avg_effective_price,
    MIN(t.effective_price)                               AS min_effective_price,
    MAX(t.effective_price)                               AS max_effective_price,
    MAX(t.effective_price) - MIN(t.effective_price)      AS price_spread,
    -- Discount depth: how deep is the sale relative to regular price?
    MIN(t.regular_price) -
        MIN(CASE WHEN t.is_on_sale = 1 THEN t.sale_price END) AS discount_depth,
    -- Share measures
    CAST(SUM(t.is_on_sale) AS float)
        / NULLIF(COUNT(*), 0)                            AS share_on_sale,
    CAST(COUNT(t.card_number) AS float)
        / NULLIF(COUNT(*), 0)                            AS share_loyalty,
    -- Volume
    SUM(t.volume)                                        AS total_volume,
    SUM(t.net_sales)                                     AS total_net_sales,
    -- Flag: do both sale and regular transactions coexist on this day?
    CASE
        WHEN SUM(t.is_on_sale) > 0
         AND COUNT(*) - SUM(t.is_on_sale) > 0
        THEN 1 ELSE 0
    END                                                  AS both_types_present
INTO stg.pd_store_upc_day
FROM stg.pd_transactions t
WHERE t.soe_period IS NOT NULL
GROUP BY
    t.trx_date, t.store_id, t.retailer_id, t.sst,
    t.upc, t.week_seq, t.soe_period;

CREATE INDEX IX_pd_day_store_upc ON stg.pd_store_upc_day(store_id, upc, trx_date);

-- Quick check: what fraction of store-UPC-days have price dispersion?
SELECT
    upc,
    soe_period,
    COUNT(*)                                                   AS n_store_upc_days,
    SUM(both_types_present)                                    AS n_days_both_types,
    ROUND(100.0 * SUM(both_types_present) / COUNT(*), 2)      AS pct_days_with_dispersion,
    ROUND(AVG(share_on_sale) * 100, 2)                        AS avg_pct_on_sale,
    ROUND(AVG(discount_depth), 4)                             AS avg_discount_depth,
    ROUND(AVG(share_loyalty) * 100, 2)                        AS avg_pct_loyalty
FROM stg.pd_store_upc_day
GROUP BY upc, soe_period
ORDER BY upc, soe_period;

/* ============================================================================
 * 3. STORE x UPC x WEEK SUMMARY (for joining to panel_est in R)
 *    Collapses store-day to store-week. This table is what gets read into R
 *    via 00_read_in_data.R and joined to panel_est for regressions.
 *
 *    Key variables for Mechanism 3:
 *      share_on_sale       -- fraction of transactions at sale price
 *      avg_discount_depth  -- mean (regular - sale) when on sale
 *      share_loyalty       -- fraction of transactions with loyalty card
 *      pct_days_dispersion -- share of days where sale/regular coexist
 *      avg_price_spread    -- mean (max - min) effective price within store-day
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
    SUM(d.n_transactions)                                      AS weekly_transactions_total,
    SUM(d.n_loyalty_transactions)                              AS weekly_loyalty_transactions,
    SUM(d.n_sale_transactions)                                 AS weekly_sale_transactions,
    -- Share on sale (weighted by transaction count)
    CAST(SUM(d.n_sale_transactions) AS float)
        / NULLIF(SUM(d.n_transactions), 0)                     AS share_on_sale,
    -- Share loyalty
    CAST(SUM(d.n_loyalty_transactions) AS float)
        / NULLIF(SUM(d.n_transactions), 0)                     AS share_loyalty,
    -- Price points (modal regular price within week)
    AVG(d.regular_price)                                       AS avg_regular_price,
    AVG(d.sale_price)                                          AS avg_sale_price,
    -- Effective price (revenue-weighted across days)
    SUM(d.total_net_sales) / NULLIF(SUM(d.total_volume), 0)   AS p_net_weekly,
    -- Discount depth: average markdown when on sale
    AVG(CASE WHEN d.n_sale_transactions > 0
             THEN d.discount_depth END)                        AS avg_discount_depth,
    -- Within-day price dispersion averaged across days
    AVG(d.price_spread)                                        AS avg_price_spread,
    -- Fraction of store-days with mixed sale/regular transactions
    CAST(SUM(d.both_types_present) AS float)
        / NULLIF(COUNT(*), 0)                                  AS pct_days_dispersion,
    -- Number of days observed this week
    COUNT(*)                                                   AS n_days_observed,
    -- Volume
    SUM(d.total_volume)                                        AS weekly_volume,
    SUM(d.total_net_sales)                                     AS weekly_net_sales
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

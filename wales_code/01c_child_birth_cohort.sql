
CALL fnc.drop_if_exists('sailw1151v.dcp02_child_birth');

-- =============================================================================
-- Create a main table of all births from 1 January 2000
-- =============================================================================

CREATE TABLE sailw1151v.dcp02_child_birth
(
/* child birth */
    c_alf_e                         bigint NOT NULL,     -- Child ALF
    c_alf_sts_cd                     varchar(2),          -- Child's matching status
    wob                              date,                -- Week of birth for child
    stillbirth_flg                   smallint,            -- 0/1 flag for whether baby was stillborn
    sex                              varchar(1),          -- Sex of child: m/f
    ethnicity_nm                     varchar(60),         -- NCCH and MIDS labels, supplemented by latest recorded in either GP or hospital records
    birth_weight                     integer,             -- Birth weight in grams
    gestational_age                  smallint,            -- Gestational age (weeks)
    preterm_birth_flg                smallint,            -- 0/1 flag for whether gestation age was under 37 weeks
    apgar_score                      smallint,            -- Total Apgar score at 5 minutes after birth, a measure of physical condition
    labour_onset_nm                  varchar(60),         -- How labour began or delivery by caesarean section
    delivery_nm                      varchar(60),         -- Mode of birth
    marriage_cd                      smallint,            -- ADBE code
    welsh_birth_flg                  smallint,            -- 0/1 flag for whether mother is a Welsh resident
/* child residence */
    c_residence_start_date           date,                -- Start date of living in Wales
    c_residence_end_date_1day        date,                -- End date of living in Wales in which they subsequently lived 1 day outside of Wales
    c_residence_end_date_28day       date,                -- End date of living in Wales in which they subsequently lived 28 consecutive days outside of Wales
/* child death */
    c_death_date                     date,                -- Week of death for the child, from ADDE only
    c_death_neonatal_flg             smallint,            -- 0/1 flag for if death is in first 27 days of life
    c_death_birth_asphyxia_flg       smallint,            -- 0/1 flag for if any diag code is P21*
    c_death_short_gestation_flg      smallint,            -- 0/1 flag for if any diag code is P07*
    c_death_sids_flg                 smallint,            -- 0/1 flag for if any diag code is R95*
    c_death_diag_1_cd                varchar(5),          -- Primary ICD-10 code for cause of death
/* mother */
    m_alf_e                         bigint,              -- Mother ALF
    m_alf_sts_cd                     varchar(2),          -- Mother's matching status
    m_prev_livebirths                smallint,            -- Number of previous live births for mother
    m_parity                         smallint,            -- Number of previous live births and stillbirths
    m_multiple_gestation_flg         smallint,            -- 0/1 flag for multiple gestation e.g. twins
    m_mids_breast_feeding_intent_flg smallint,            -- 0/1 flag for whether mother intends to breast feed (MIDS data source only)
    m_breast_feeding_flg             smallint,            -- 0/1 flag for whether mother was breast feeding between weeks 6-8
    m_mat_care_type                  varchar(60),         -- Type of maternity care received by the mother
    m_age                            smallint,            -- Age of mother at week of birth
    m_lsoa2011_cd                    varchar(10),         -- LSOA 2011 code of mother's area of residence at week of birth
    m_wimd2014_decile                smallint,            -- WIMD 2014 decile for the LSOA: 1 = most, 10 = least
    m_wimd2019_decile                smallint,            -- WIMD 2019 decile for the LSOA: 1 = most, 10 = least
    m_townsend2011_quintile          smallint,            -- Townsend 2011 quintile for the LSOA: 1 = most, 5 = least
    m_wob                            date,                -- Week of birth for mother
    m_age_wds                        smallint,            -- Age of mother at week of birth
    m_sex                            varchar(1),          -- Sex of mother: m/f
    m_smoking_cat                    varchar(3),          -- smoking status: Non, Ex, Smk
    m_birth_country_nm               varchar(26),         -- Name of mother's birth country
    m_birth_region_nm                varchar(13),         -- Name of mother's birth region: UK, EU, Not stated
    m_socioeconomic_class_cd         varchar(4),          -- ADBE code
    m_occ_class_cd                   varchar(4),          -- ADBE code
    m_ethnicity_nm                   varchar(60),         -- MIDS labels, supplemented by latest recorded in either GP or hospital records
    m_bmi                            decimal,
    m_bmi_dt                         date,
    -- m_diabetes
/* mother death */
    m_death_date                     date,                -- Week of death for the mother, from ADDE only
    m_death_diag_1_cd                varchar(5),          -- Primary ICD-10 code for cause of death
/* father */
    f_birth_country_nm               varchar(26),         -- Name of father's birth country
    f_birth_region_nm                varchar(13),         -- Name of father's birth region: UK, EU, Not stated
    f_socioeconomic_class_cd         varchar(4),          -- ADBE code
    f_occ_class_cd                   varchar(4),          -- ADBE code
/* data source */
    in_ncch                          smallint DEFAULT 0,  -- Is birth recorded in NCCH data source?
    in_mids                          smallint DEFAULT 0,  -- Is birth recorded in MIDS data source?
    in_adbe                          smallint DEFAULT 0,  -- Is birth recorded in ADBE data source?
    src                              varchar(4),          -- What data source was used? NCCH, MIDS, ADBE
/* pk */
    PRIMARY KEY (c_alf_e)
)
DISTRIBUTE BY hash (c_alf_e);


-- =============================================================================
-- Insert from NCCH birth table
-- =============================================================================

INSERT INTO sailw1151v.dcp02_child_birth
(
/* child birth */
    c_alf_e,
    c_alf_sts_cd,
    wob,
    stillbirth_flg,
    sex,
    ethnicity_nm,
    birth_weight,
    gestational_age,
    preterm_birth_flg,
    apgar_score,
    labour_onset_nm,
    delivery_nm,
    welsh_birth_flg,
/* mother */
    m_alf_e,
    m_alf_sts_cd,
    m_prev_livebirths,
    m_parity,
    m_multiple_gestation_flg,
    m_breast_feeding_flg,
    m_mat_care_type,
    m_age,
    m_smoking_cat,
/* data source */
    src
)
WITH
    ncch AS (
        SELECT
            ncch.alf_e                                          AS c_alf_e,
            ncch.alf_sts_cd                                     AS c_alf_sts_cd,
            ncch.wob                                            AS wob,
            ncch.stillbirth_flg                                 AS stillbirth_flg,
            ncch.gndr_cd                                        AS sex,
            ethn.ethn_ec_ons_date_latest_desc                     AS ethnicity_nm,
            cast(ncch.birth_weight * 1000 AS integer)           AS birth_weight, -- Convert kilograms to grams
            cast(ncch.gest_age AS smallint)                     AS gestational_age,
            CASE
                WHEN cast(ncch.gest_age AS smallint) < 37  THEN 1
                WHEN cast(ncch.gest_age AS smallint) >= 37 THEN 0
            END                                                 AS preterm_birth_flg,
            lkp_onset.main_description_60_chars                 AS labour_onset_nm,
            lkp_delivery.main_description_60_chars              AS delivery_nm,
            ncch.welsh_birth_flg                                AS welsh_birth_flg,
            ncch.apgar_2                                        AS apgar_score,
            ncch.mat_alf_e                                      AS m_alf_e,
            ncch.mat_alf_sts_cd                                 AS m_alf_sts_cd,
            ncch.prev_live_births                               AS m_prev_livebirths,
            ncch.prev_live_births + ncch.prev_stillbirth        AS m_parity,
            CASE
                WHEN ncch.tot_birth_num >= 2 THEN 1
                ELSE 0
            END                                                 AS m_multiple_gestation_flg,
            ncch.breastfeed_8_wks_flg                           AS m_breast_feeding_flg,
            lkp_mat_care.main_description_60_chars              AS m_mat_care_type,
            ncch.mat_age                                        AS m_age,
            CASE
                WHEN mat_smoking_cd = '0' THEN 'Non'
                WHEN mat_smoking_cd = '1' THEN 'Ex'
                WHEN mat_smoking_cd IN  ('2', '3', '4', '6') THEN 'Smk'
                ELSE ''
            END                                                 AS m_smoking_cat,
            row_number () OVER (PARTITION BY ncch.alf_e)       AS alf_row_num
        FROM
            sailwmc_v.c19_cohort_ncch_child_births              AS ncch
        LEFT JOIN
            sailw1151v.rrda_ethn                                AS ethn
            ON ncch.alf_e = ethn.alf_e
        LEFT JOIN
            sailukhdv.dd_wales_labour_delivery_onset_method_scd AS lkp_onset
            ON ncch.labour_onset_cd = lkp_onset.main_code_text
        LEFT JOIN
            sailukhdv.dd_wales_delivery_method_scd              AS lkp_delivery
            ON ncch.del_cd = lkp_delivery.main_code_text
        LEFT JOIN
            sailukhdv.dd_wales_maternal_care_scd                AS lkp_mat_care
            ON ncch.mat_care_cd = lkp_mat_care.main_code_text
        LEFT JOIN
             sailw1151v.dcp02_child_birth                       AS main
             ON ncch.alf_e = main.c_alf_e
        WHERE
            ncch.alf_e IS NOT NULL
            AND main.c_alf_e IS NULL
            AND ncch.wob >= '2000-01-01'
    )
SELECT
/* child birth */
    c_alf_e,
    c_alf_sts_cd,
    wob,
    stillbirth_flg,
    sex,
    ethnicity_nm,
    birth_weight,
    gestational_age,
    preterm_birth_flg,
    apgar_score,
    labour_onset_nm,
    delivery_nm,
    welsh_birth_flg,
/* mother */
    m_alf_e,
    m_alf_sts_cd,
    m_prev_livebirths,
    m_parity,
    m_multiple_gestation_flg,
    m_breast_feeding_flg,
    m_mat_care_type,
    m_age,
    m_smoking_cat,
/* data source */
    'NCCH' AS src
FROM
    ncch
WHERE
    alf_row_num = 1;


-- =============================================================================
-- Insert from NCCH child trust table
-- =============================================================================


INSERT INTO sailw1151v.dcp02_child_birth
(
/* child birth */
    c_alf_e,
    c_alf_sts_cd,
    wob,
    stillbirth_flg,
    sex,
    ethnicity_nm,
    birth_weight,
    gestational_age,
    preterm_birth_flg,
    apgar_score,
    labour_onset_nm,
    delivery_nm,
    welsh_birth_flg,
/* mother */
    m_alf_e,
    m_alf_sts_cd,
    m_prev_livebirths,
    m_parity,
    m_multiple_gestation_flg,
    m_breast_feeding_flg,
    m_mat_care_type,
    m_age,
    m_smoking_cat,
/* data source */
    src
)
WITH
    ncch AS (
        SELECT
            ncch.alf_e                                                       AS c_alf_e,
            ncch.alf_sts_cd                                                   AS c_alf_sts_cd,
            date(ncch.wob)                                                    AS wob,
            0                                                                 AS stillbirth_flg,
            ncch.gndr_cd                                                      AS sex,
            ethn.ethn_ec_ons_date_latest_desc                                 AS ethnicity_nm,
            cast(ncch.birth_weight * 1000 AS DOUBLE)                          AS birth_weight, -- Convert kilograms to grams
            cast(ncch.gestation_age AS smallint)                              AS gestational_age,
            CASE
                WHEN cast(ncch.gestation_age AS smallint) < 37  THEN 1
                WHEN cast(ncch.gestation_age AS smallint) >= 37 THEN 0
            END                                                               AS preterm_birth_flg,
            ncch.apgar_2                                                      AS apgar_score,
            lkp_onset.main_description_60_chars                               AS labour_onset_nm,
            lkp_delivery.main_description_60_chars                            AS delivery_nm,
            ncch.mat_alf_e                                                   AS m_alf_e,
            ncch.mat_alf_sts_cd                                               AS m_alf_sts_cd,
            ncch.prev_live_births                                             AS m_prev_livebirths,
            ncch.prev_live_births + ncch.prev_stillbirth                      AS m_parity,
            1                                                                 AS welsh_birth_flg,
            CASE
                WHEN ncch.tot_birth_num >= 2 THEN 1
                ELSE 0
            END                                                               AS m_multiple_gestation_flg,
            CASE
                WHEN ncch.age_breastfeed_ceased_wks >= 6 THEN 1
                WHEN ncch.age_breastfeed_ceased_wks < 6  THEN 0
            END                                                               AS m_breast_feeding_flg,
            lkp_mat_care.main_description_60_chars                            AS m_mat_care_type,
            floor((days(date(ncch.wob)) - days(ncch.mat_wob)) / 365.25)       AS m_age,
            CASE
                WHEN mat_smoking_cd = '0' THEN 'Non'
                WHEN mat_smoking_cd = '1' THEN 'Ex'
                WHEN mat_smoking_cd IN  ('2', '3', '4', '6') THEN 'Smk'
                ELSE ''
            END                                                               AS m_smoking_cat,
            row_number () OVER (PARTITION BY ncch.alf_e)                      AS alf_row_num
        FROM
            sailwmc_v.c19_cohort_ncch_child_trust                             AS ncch
        LEFT JOIN
            sailw1151v.rrda_ethn                                              AS ethn
            ON ncch.alf_e = ethn.alf_e
        LEFT JOIN
            sailukhdv.dd_wales_labour_delivery_onset_method_scd               AS lkp_onset
            ON ncch.labour_onset_cd = lkp_onset.main_code_text
        LEFT JOIN
            sailukhdv.dd_wales_delivery_method_scd                            AS lkp_delivery
            ON ncch.del_cd = lkp_delivery.main_code_text
        LEFT JOIN
            sailukhdv.dd_wales_maternal_care_scd                              AS lkp_mat_care
            ON ncch.mat_care_cd = lkp_mat_care.main_code_text
        LEFT JOIN
            sailw1151v.dcp02_child_birth                                      AS main
            ON ncch.alf_e = main.c_alf_e
        WHERE
            ncch.alf_e IS NOT NULL
            AND main.c_alf_e IS NULL
            AND date(ncch.wob) >= '2000-01-01'
)
SELECT
/* child birth */
    c_alf_e,
    c_alf_sts_cd,
    wob,
    stillbirth_flg,
    sex,
    ethnicity_nm,
    birth_weight,
    gestational_age,
    preterm_birth_flg,
    apgar_score,
    labour_onset_nm,
    delivery_nm,
    welsh_birth_flg,
/* mother */
    m_alf_e,
    m_alf_sts_cd,
    m_prev_livebirths,
    m_parity,
    m_multiple_gestation_flg,
    m_breast_feeding_flg,
    m_mat_care_type,
    m_age,
    m_smoking_cat,
/* data source */
    'NCCH' AS src
FROM
    ncch
WHERE
    alf_row_num = 1;

-- =============================================================================
-- Insert from MIDS tables
-- =============================================================================

INSERT INTO sailw1151v.dcp02_child_birth
(
/* child birth */
    c_alf_e,
    c_alf_sts_cd,
    wob,
    stillbirth_flg,
    sex,
    birth_weight,
    gestational_age,
    preterm_birth_flg,
    apgar_score,
    labour_onset_nm,
    delivery_nm,
/* mother */
    m_alf_e,
    m_alf_sts_cd,
    m_parity,
    m_multiple_gestation_flg,
    m_age,
    m_ethnicity_nm,
    m_bmi,
    m_bmi_dt,
/* data source */
    src
)
WITH
    /* only use initial assessment data if its at most 280 days prior to the birth */
    init_ass AS (
        SELECT init_ass.*
        FROM
            sailwmc_v.c19_cohort_mids_initial_assessment         AS init_ass
        INNER JOIN
            sailwmc_v.c19_cohort_mids_birth                      AS birth
            ON init_ass.mother_alf_e = birth.mother_alf_e
            AND init_ass.initial_ass_dt >= (birth.baby_birth_dt - 280 days)
            AND init_ass.initial_ass_dt <= (birth.baby_birth_dt)
    ),
    mids AS (
        SELECT
            birth.child_alf_e                                    AS c_alf_e,
            birth.child_alf_sts_cd                               AS c_alf_sts_cd,
            birth.baby_birth_dt                                  AS wob,
            CASE
                WHEN birth.birth_outcome_cd = 2 THEN 1
                WHEN birth.birth_outcome_cd = 1 THEN 0
            END                                                  AS stillbirth_flg,
            CASE
                WHEN birth.service_user_sex_cd = '1' THEN 'M'
                WHEN birth.service_user_sex_cd = '2' THEN 'F'
            END                                                  AS sex,
            birth.service_user_weight_grams                      AS birth_weight,
            birth.labour_onset_gest_weeks                        AS gestational_age,
            CASE
                WHEN birth.labour_onset_gest_weeks < 37  THEN 1
                WHEN birth.labour_onset_gest_weeks >= 37 THEN 0
            END                                                  AS preterm_birth_flg,
            birth.birth_apgar_score                              AS apgar_score,
            lkp_onset.main_description_60_chars                  AS labour_onset_nm,
            lkp_delivery.main_description_60_chars               AS delivery_nm,
            birth.mother_alf_e                                   AS m_alf_e,
            birth.mother_alf_sts_cd                              AS m_alf_sts_cd,
            init_ass.service_user_parity_cd                      AS m_parity,
            CASE
                WHEN birth.labour_onset_foetus_num >= 2 THEN 1
                ELSE 0
            END                                                  AS m_multiple_gestation_flg,
            birth.mat_age                                        AS m_age,
            LOWER(lkp_ethn.category)                             AS m_ethnicity_nm,
            case 
                when  init_ass.service_user_weight_kg = 0     or
                      init_ass.service_user_weight_kg is null or 
                      init_ass.service_user_height = 0        or 
                      init_ass.service_user_height is null
            then null
                when init_ass.service_user_height < 20 
            then init_ass.service_user_weight_kg / power(init_ass.service_user_height, 2)
                when init_ass.service_user_height < 75 and
                     init_ass.service_user_height >= 20
            then init_ass.service_user_weight_kg / power(init_ass.service_user_height*2.54, 2)
            else init_ass.service_user_weight_kg / power(init_ass.service_user_height/100, 2)
            end                                                  as m_bmi,
            init_ass.initial_ass_dt                              as m_bmi_dt,
            row_number () OVER (PARTITION BY birth.child_alf_e)  AS alf_row_num
        FROM
            sailwmc_v.c19_cohort_mids_birth                      AS birth
        LEFT JOIN
            init_ass
            ON init_ass.mother_alf_e = birth.mother_alf_e
        LEFT JOIN
            sailukhdv.dd_wales_mode_of_onset_of_labour_scd       AS lkp_onset
            ON birth.labour_onset_mode_cd = lkp_onset.main_code_text
        LEFT JOIN
            sailukhdv.dd_wales_mode_of_birth_scd                 AS lkp_delivery
            ON birth.birth_mode_cd = lkp_delivery.main_code_text
        LEFT JOIN
            sailukhdv.dd_wales_ethnic_group_mi_ds_scd            AS lkp_ethn
            ON UPPER(init_ass.service_user_ethnic_grp_cd) = lkp_ethn.main_code_text
        LEFT JOIN
            sailw1151v.dcp02_child_birth                         AS main
            ON birth.child_alf_e = main.c_alf_e
        WHERE
            birth.child_alf_e IS NOT NULL
            AND main.c_alf_e IS NULL
            AND birth.baby_birth_dt >= '2000-01-01'
    )
SELECT
/* child birth */
    c_alf_e,
    c_alf_sts_cd,
    wob,
    stillbirth_flg,
    sex,
    birth_weight,
    gestational_age,
    preterm_birth_flg,
    apgar_score,
    labour_onset_nm,
    delivery_nm,
/* mother */
    m_alf_e,
    m_alf_sts_cd,
    m_parity,
    m_multiple_gestation_flg,
    m_age,
    m_ethnicity_nm,
    m_bmi,
    m_bmi_dt,
/* data source */
    'MIDS' AS src
FROM
    mids
WHERE
    alf_row_num = 1;


-- =============================================================================
-- Insert from ADBE table
-- =============================================================================

INSERT INTO sailw1151v.dcp02_child_birth
(
/* child birth */
    c_alf_e,
    c_alf_sts_cd,
    wob,
    stillbirth_flg,
    sex,
    birth_weight,
/* mother */
    m_multiple_gestation_flg,
/* data source */
    src
)
SELECT DISTINCT
    adbe.alf_e                                      AS c_alf_e,
    adbe.alf_sts_cd                                 AS c_alf_sts_cd,
    date(adbe.wob)                                  AS wob,
    coalesce(adbe.stillbirth_ind, 0)                AS stillbirth_flg,
    CASE
        WHEN adbe.nenonate_sex_cd = 1 THEN 'M'
        WHEN adbe.nenonate_sex_cd = 2 THEN 'F'
    END                                             AS sex,
    adbe.birth_weight                               AS birth_weight,
    coalesce(adbe.multiplebirth_ind_cd, 0)          AS m_multiple_gestation_flg,
    'ADBE'                                          AS src
FROM
    sailwmc_v.c19_cohort_adbe_births                AS adbe
LEFT JOIN
    sailw1151v.dcp02_child_birth                       AS main
    ON adbe.alf_e = main.c_alf_e
WHERE
    adbe.alf_e IS NOT NULL
    AND main.c_alf_e IS NULL
    AND date(adbe.wob) >= date('2000-01-01');



-- =============================================================================
-- Replace missing parity info
-- Use rolling sum based on available birth records
-- =============================================================================

CALL fnc.drop_if_exists('sailw1151v.dcp02_mother_parity');

CREATE TABLE sailw1151v.dcp02_mother_parity (
    m_alf_e          BIGINT   NOT NULL,
    wob               DATE     NOT NULL,
    m_parity          SMALLINT NOT NULL,
    PRIMARY KEY (m_alf_e, wob)
) DISTRIBUTE BY hash (m_alf_e, wob);

/* NCCH and MIDS */

INSERT INTO sailw1151v.dcp02_mother_parity
WITH
    ncch_cb AS (
        SELECT
            alf_e AS c_alf_e,
            wob,
            mat_alf_e AS m_alf_e
        FROM
            sailwmc_v.c19_cohort_ncch_child_births AS ncch
        WHERE
            ncch.alf_e IS NOT NULL
            AND ncch.alf_sts_cd IN (1, 4, 39)
            AND ncch.wob IS NOT NULL
            AND ncch.mat_alf_e IS NOT NULL
            AND ncch.mat_alf_sts_cd IN (1, 4, 39)
    ),
    ncch_ct AS (
        SELECT
            alf_e AS c_alf_e,
            date(wob) AS wob,
            mat_alf_e AS m_alf_e
        FROM
            sailwmc_v.c19_cohort_ncch_child_trust AS ncch
        WHERE
            ncch.alf_e IS NOT NULL
            AND ncch.alf_sts_cd IN (1, 4, 39)
            AND ncch.wob IS NOT NULL
            AND ncch.mat_alf_e IS NOT NULL
            AND ncch.mat_alf_sts_cd IN (1, 4, 39)
    ),
    mids AS (
        SELECT
            child_alf_e AS alf_e,
            baby_birth_dt AS wob,
            mother_alf_e AS m_alf_e
        FROM
            sailwmc_v.c19_cohort_mids_birth AS mids
        WHERE
            mids.child_alf_e IS NOT NULL
            AND mids.child_alf_sts_cd IN (1, 4, 39)
            AND mids.baby_birth_dt IS NOT NULL
            AND mids.mother_alf_e IS NOT NULL
            AND mids.mother_alf_sts_cd IN (1, 4, 39)
    ),
    all_births AS (
        SELECT * FROM ncch_cb
        UNION
        SELECT * FROM ncch_ct
        UNION
        SELECT * FROM mids
    ),
    all_births_dedupe AS (
        SELECT *
        FROM all_births
        GROUP BY c_alf_e, wob, m_alf_e
    ),
    mother_births AS (
        SELECT m_alf_e, wob, count(*) AS births
        FROM all_births_dedupe
        GROUP BY m_alf_e, wob
    )
SELECT
    m_alf_e,
    wob,
    (sum(births) OVER (PARTITION BY m_alf_e ORDER BY wob)) - births AS m_parity
FROM
    mother_births
;

CALL sysproc.admin_cmd('runstats on table sailw1151v.dcp02_mother_parity with distribution and detailed indexes all');

MERGE INTO sailw1151v.dcp02_child_birth AS birth
USING sailw1151v.dcp02_mother_parity AS mp
ON birth.m_alf_e = mp.m_alf_e
AND birth.wob = mp.wob
AND birth.m_parity IS NULL
WHEN matched THEN UPDATE SET
    birth.m_parity = mp.m_parity;


-- =============================================================================
-- Replace missing number of previous live births
-- Just so we are consistent with how we replaced parity
-- =============================================================================

CALL fnc.drop_if_exists('sailw1151v.dcp02_mother_prev_livebirths');

CREATE TABLE sailw1151v.dcp02_mother_prev_livebirths (
    m_alf_e          BIGINT   NOT NULL,
    wob               DATE     NOT NULL,
    m_prev_livebirths SMALLINT NOT NULL,
    PRIMARY KEY (m_alf_e, wob)
) DISTRIBUTE BY hash (m_alf_e, wob);

/* NCCH and MIDS */

INSERT INTO sailw1151v.dcp02_mother_prev_livebirths
WITH
    ncch_cb AS (
        SELECT
            alf_e AS c_alf_e,
            wob,
            mat_alf_e AS m_alf_e
        FROM
            sailwmc_v.c19_cohort_ncch_child_births AS ncch
        WHERE
            ncch.alf_e IS NOT NULL
            AND ncch.alf_sts_cd IN (1, 4, 39)
            AND ncch.wob IS NOT NULL
            AND ncch.mat_alf_e IS NOT NULL
            AND ncch.mat_alf_sts_cd IN (1, 4, 39)
            AND ncch.stillbirth_flg = 0
    ),
    ncch_ct AS (
        SELECT
            alf_e AS c_alf_e,
            date(wob) AS wob,
            mat_alf_e AS m_alf_e
        FROM
            sailwmc_v.c19_cohort_ncch_child_trust AS ncch
        WHERE
            ncch.alf_e IS NOT NULL
            AND ncch.alf_sts_cd IN (1, 4, 39)
            AND ncch.wob IS NOT NULL
            AND ncch.mat_alf_e IS NOT NULL
            AND ncch.mat_alf_sts_cd IN (1, 4, 39)
    ),
    mids AS (
        SELECT
            child_alf_e AS alf_e,
            baby_birth_dt AS wob,
            mother_alf_e AS m_alf_e
        FROM
            sailwmc_v.c19_cohort_mids_birth AS mids
        WHERE
            mids.child_alf_e IS NOT NULL
            AND mids.child_alf_sts_cd IN (1, 4, 39)
            AND mids.baby_birth_dt IS NOT NULL
            AND mids.mother_alf_e IS NOT NULL
            AND mids.mother_alf_sts_cd IN (1, 4, 39)
            AND mids.birth_outcome_cd = 2
    ),
    ncch_mids AS (
        SELECT * FROM ncch_cb
        UNION
        SELECT * FROM ncch_ct
        UNION
        SELECT * FROM mids
    ),
    ncch_mids_dedupe AS (
        SELECT *
        FROM ncch_mids
        GROUP BY c_alf_e, wob, m_alf_e
    ),
    mother_livebiths AS (
        SELECT m_alf_e, wob, count(*) AS livebirths
        FROM ncch_mids_dedupe
        GROUP BY m_alf_e, wob
    )
SELECT
    m_alf_e,
    wob,
    (sum(livebirths) OVER (PARTITION BY m_alf_e ORDER BY wob)) - livebirths AS prev_livebirths
FROM
    mother_livebiths
;

CALL sysproc.admin_cmd('runstats on table sailw1151v.dcp02_mother_prev_livebirths with distribution and detailed indexes all');

MERGE INTO sailw1151v.dcp02_child_birth AS birth
USING sailw1151v.dcp02_mother_prev_livebirths AS mplb
ON birth.m_alf_e = mplb.m_alf_e
AND birth.wob = mplb.wob
AND birth.m_prev_livebirths IS NULL
WHEN matched THEN UPDATE SET
    birth.m_prev_livebirths = mplb.m_prev_livebirths;

/* top and tail previous live births and parity if one is available and the
 * other is null */

UPDATE sailw1151v.dcp02_child_birth
SET m_prev_livebirths = 0
WHERE
    m_prev_livebirths IS NULL
    AND m_parity IS NOT NULL;

UPDATE sailw1151v.dcp02_child_birth
SET m_parity = m_prev_livebirths
WHERE
    m_prev_livebirths IS NOT NULL
    AND m_parity IS NULL;


-- =============================================================================
-- Set data source flags
-- =============================================================================

/* NCCH */

MERGE INTO sailw1151v.dcp02_child_birth AS birth
USING (
    SELECT DISTINCT alf_e
    FROM sailwmc_v.c19_cohort_ncch_child_births
    WHERE wob >= '2000-01-01'
) AS src
ON birth.c_alf_e = src.alf_e
WHEN matched THEN UPDATE SET
    birth.in_ncch = 1;

MERGE INTO sailw1151v.dcp02_child_birth AS birth
USING (
    SELECT DISTINCT alf_e
    FROM sailwmc_v.c19_cohort_ncch_child_trust
    WHERE wob >= '2000-01-01'
) AS src
ON birth.c_alf_e = src.alf_e
WHEN matched THEN UPDATE SET
    birth.in_ncch = 1;

/* MIDS */

MERGE INTO sailw1151v.dcp02_child_birth AS birth
USING (
    SELECT DISTINCT child_alf_e
    FROM sailwmc_v.c19_cohort_mids_birth
    WHERE baby_birth_dt >= '2000-01-01'
) AS src
ON birth.c_alf_e = src.child_alf_e
WHEN matched THEN UPDATE SET
    birth.in_mids = 1;


-- =============================================================================
-- Tidy up time!
-- =============================================================================


CALL fnc.drop_if_exists('sailw1151v.dcp02_mother_parity');
CALL fnc.drop_if_exists('sailw1151v.dcp02_mother_prev_livebirths');
CALL fnc.drop_if_exists('sailw1151v.dcp02_mother_demographics');
CALL fnc.drop_if_exists('sailw1151v.dcp02_mother_address');
CALL fnc.drop_if_exists('sailw1151v.dcp02_mother_death');
CALL fnc.drop_if_exists('sailw1151v.dcp02_child_death');
CALL fnc.drop_if_exists('sailw1151v.dcp02_child_residence');
CALL fnc.drop_if_exists('sailw1151v.dcp02_tmp_alf');
CALL fnc.drop_if_exists('sailw1151v.dcp02_alf_ethn');
CALL fnc.drop_if_exists('sailw1151v.dcp02_alf_ethn_latest');


-- =============================================================================
-- Checks
-- =============================================================================

SELECT count(*) AS n
FROM sailw1151v.dcp02_child_birth;

SELECT year(wob), count(*) AS n
FROM sailw1151v.dcp02_child_birth
GROUP BY year(wob)
ORDER BY year(wob);

SELECT in_ncch, in_mids, in_adbe, count(*) AS n
FROM sailw1151v.dcp02_child_birth
GROUP BY in_ncch, in_mids, in_adbe;

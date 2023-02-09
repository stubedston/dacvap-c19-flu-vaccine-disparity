
-- =============================================================================
-- Step 1: Check how up to date is everything
-- =============================================================================

SELECT 'vacc' AS src, max(vacc_date) AS end_date
	FROM SAILWMC_V.C19_COHORT20_RRDA_CVVD 
UNION
SELECT 'cvvd' AS src, max(vacc_date_of_vaccine) AS end_date
	FROM SAILWMC_V.C19_COHORT_CVVD_DF_WIS_OUTCOMEDATAV2 
UNION
SELECT 'pcr' AS src, max(spcm_collected_dt) AS end_date
	FROM SAILWMC_V.C19_COHORT_PATD_DF_COVID_LIMS_TESTRESULTS 
	WHERE spcm_collected_dt <= CURRENT DATE
UNION
SELECT 'lft' AS src, date(max(teststart_dt)) AS end_date
	FROM SAILWMC_V.C19_COHORT_CVLF_DF_LATERAL_FLOW_TESTS 
	WHERE teststart_dt <= CURRENT DATE
UNION
SELECT 'mortality' AS src, max(dod) AS end_date
	FROM SAILWMC_V.C19_COHORT20 
UNION
SELECT 'pedw' AS src, max(admis_dt) AS end_date
	FROM SAILWMC_V.C19_COHORT_PEDW_SPELL 
	WHERE admis_dt < CURRENT DATE
UNION
SELECT 'wlgp' AS src, max(event_dt) AS end_date
	FROM SAILWMC_V.C19_COHORT_WLGP_GP_EVENT_CLEANSED ccwgec 
	WHERE event_dt < CURRENT DATE
	AND event_dt < avail_from_dt;


-- Check total number of PEDW episodes by week
-- and whether they have a diag code
WITH
	pedw_epi AS
	(
		SELECT
			spell.prov_unit_cd,
			spell.spell_num_e,
			CAST(date_trunc('week', spell.admis_dt) AS DATE) AS admis_dt,
			CAST(date_trunc('week', epi.epi_str_dt) AS DATE) AS epi_str_week,
			epi.diag_cd_1234 IS NOT NULL AS has_diag
		FROM
			SAILWMC_V.C19_COHORT_PEDW_SPELL AS spell 
		INNER JOIN
			SAILWMC_V.C19_COHORT_PEDW_EPISODE AS epi 
			ON spell.prov_unit_cd = epi.prov_unit_cd
			AND spell.spell_num_e = epi.spell_num_e
		WHERE
			spell.admis_dt BETWEEN '2021-01-01' AND CURRENT DATE -- need to update when agreed in SAP
			AND epi.epi_str_dt BETWEEN '2021-01-01' AND CURRENT DATE -- need to update when agreed in SAP
	)
SELECT epi_str_week, COUNT(*) AS epi_n, SUM(has_diag) AS epi_diag_n
FROM pedw_epi
GROUP BY epi_str_week
ORDER BY epi_str_week DESC;

-- =============================================================================
-- Step 2: Covid Vaccinations
-- =============================================================================

CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_c19_vacc');

CREATE TABLE sailw1151v.dcp02_c19_vacc
(
	alf_e                    BIGINT NOT NULL,
	c19_vacc_bad_record_flg  SMALLINT NOT NULL,
-- First primary dose
	c19_vacc_1_date          DATE,
	c19_vacc_1_name          VARCHAR(50),
-- Second primary dose
	c19_vacc_2_date          DATE,
	c19_vacc_2_name          VARCHAR(50),
-- Third primary dose or first booster dose
	c19_vacc_3_date          DATE,
	c19_vacc_3_name          VARCHAR(50),
-- Fourth primary dose or second booster dose
	c19_vacc_4_date          DATE,
	c19_vacc_4_name          VARCHAR(50),
-- Autumn 2022 dose
	c19_vacc_aut22_date      DATE,
	c19_vacc_aut22_name      VARCHAR(50),
	PRIMARY KEY (alf_e)
);

INSERT INTO sailw1151v.dcp02_c19_vacc
SELECT
	c20.alf_e,
	MAX(cvvd.alf_has_bad_vacc_record)                                                                           AS c19_vacc_bad_record_flg,
	MIN(CASE WHEN cvvd.vacc_date < '2022-08-29' AND cvvd.vacc_dose_seq = '1'           THEN cvvd.vacc_date END) AS c19_vacc_1_date,
	MIN(CASE WHEN cvvd.vacc_date < '2022-08-29' AND cvvd.vacc_dose_seq = '1'           THEN cvvd.vacc_name END) AS c19_vacc_1_name,
	MIN(CASE WHEN cvvd.vacc_date < '2022-08-29' AND cvvd.vacc_dose_seq = '2'           THEN cvvd.vacc_date END) AS c19_vacc_2_date,
	MIN(CASE WHEN cvvd.vacc_date < '2022-08-29' AND cvvd.vacc_dose_seq = '2'           THEN cvvd.vacc_name END) AS c19_vacc_2_name,
	MIN(CASE WHEN cvvd.vacc_date < '2022-08-29' AND cvvd.vacc_dose_seq IN ('3', 'B1')  THEN cvvd.vacc_date END) AS c19_vacc_3_date,
	MIN(CASE WHEN cvvd.vacc_date < '2022-08-29' AND cvvd.vacc_dose_seq IN ('3', 'B1')  THEN cvvd.vacc_name END) AS c19_vacc_3_name,
	MIN(CASE WHEN cvvd.vacc_date < '2022-08-29' AND cvvd.vacc_dose_seq IN ('4', 'B2')  THEN cvvd.vacc_date END) AS c19_vacc_4_date,
	MIN(CASE WHEN cvvd.vacc_date < '2022-08-29' AND cvvd.vacc_dose_seq IN ('4', 'B2')  THEN cvvd.vacc_name END) AS c19_vacc_4_name,
	MIN(CASE WHEN cvvd.vacc_date BETWEEN '2022-08-29' AND CURRENT DATE AND cvvd.vacc_date < '2023-08-29' THEN cvvd.vacc_date END)                 AS c19_vacc_aut22_date,
	MIN(CASE WHEN cvvd.vacc_date BETWEEN '2022-08-29' AND CURRENT DATE AND cvvd.vacc_date < '2023-08-29' THEN cvvd.vacc_name END)                 AS c19_vacc_aut22_name
FROM
	sailwmc_v.c19_cohort20 c20
INNER JOIN
	sailwmc_v.c19_cohort20_rrda_cvvd AS cvvd
	ON c20.alf_e = cvvd.alf_e
GROUP BY
	c20.alf_e;

GRANT ALL ON TABLE sailw1151v.dcp02_c19_vacc
TO ROLE NRDASAIL_SAIL_1151_ANALYST;

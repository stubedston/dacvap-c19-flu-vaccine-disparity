-- ==================================================================
-- 03_Create_Influenza_Outcomes
-- ==================================================================

-- =============================================================================
-- Step 1 : Influenza vaccinations
-- =============================================================================
--
-- step 1.1 - create lookup table from vaccination codes provided
--
-- create lookup table, script provided by Hoda
CALL FNC.DROP_IF_EXISTS('SAILW1151V.DCP02_PHEN_READ_INFLUENZA_VACC');

CREATE TABLE SAILW1151V.DCP02_PHEN_READ_INFLUENZA_VACC (
    name               char(20),
    code               char(5) not null,
    desc               char(255),
    desc_ukhd          char(198),
    desc_ukhd_30       char(30),
    desc_ukhd_60       char(60),
    desc_ukhd_198      char(198),
    category           char(35),
    is_latest          char(1),
    valid_from         date,
    valid_to           date,
    primary key (code)
    ) ;

--DROP TABLE schema_name.PHEN_READ_INFLUENZA_VACC;
--TRUNCATE TABLE schema_name.PHEN_READ_INFLUENZA_VACC IMMEDIATE;

INSERT INTO SAILW1151V.DCP02_PHEN_READ_INFLUENZA_VACC(code, desc)
VALUES
('65E..',	'Influenza vaccination'),
('65ED.',	'Seasonal influenza vaccination'),
('65ED0',	'Seasonal influenza vaccination given by pharmacist'),
('65ED1',	'First intranasal seasonal influenza vaccination'),
('65ED2',	'Seasonal influenza vaccination given while hospital inpatient'),
('65ED3',	'Second intranasal seasonal influenza vaccination'),
('65ED4',   'Administration of first inactivated seasonal influenza vacc'),
('65ED5',   'Administration of second inactivated seasonal influenza vacc'),
('65ED6',   'First intranasal seasonal influenza vacc given by pharmacist'),
('65ED7',   'Second intranasal seasonal influenza vacc givn by pharmacist'),
('65ED8',   'First inactivated seasonal influenza vacc gvn by pharmacist'),
('65ED9',   'Second inactivated seasonal influenza vacc gvn by pharmacist'),
('65EE.',   'Administration of intranasal influenza vaccination'),
('65EE0',   'Administration of first intranasal influenza vaccination'),
('65EE1',   'Administration of second intranasal influenza vaccination'),
('65E1.',	'Second pandemic influenza vaccination'),
('65E2.',	'Influenza vaccination given by other healthcare provider'),
('65E20',	'Seasonal influenza vaccination given by other healthcare provider'),
('65E21',	'First intranasal seasonal influenza vaccination given by other healthcare provider'),
('65E22',	'Second intranasal seasonal influenza vaccination given by other healthcare provider'),
('65E23',	'Second intramuscular seasonal influenza vaccination given by other healthcare provider'),
('65E24',	'First intramuscular seasonal influenza vaccination given by other healthcare provider'),
('9k7..',	'Influenza immunisation - enhanced services administration'),
('9OX..',	'Influenza vacc. administration.'),
--('9OX..11',	'Flu vaccination administration'),
('9OX1.',	'Has ''flu vaccination at home'),
('9OX2.',	'Has''flu vaccination at surgery'),
('9OX3.',	'Has ''flu vaccination at hosp.'),
('9OX8.',	'Has influenza vaccination at work'),
('9OXZ.',	'Influenza vacc.administrat.NOS'),
('n47E.',   'INFLUENZA VACCINE (LIVE ATTENUATED) nasal suspension 0.2mL'),  
('n47g.',   'INACT INFLUENZA VACC (SPLIT VIRION) prefilled syringe 0.5mL'), 
('n47h.',   'INACT INFLUENZA VAC(SURFACE ANTIGEN SUB-UNIT) syringe 0.5mL'),
('ZV048',	'[V]Influenza vaccination');
--('ZV04811',	'[V]Flu - influenza vaccination'),


UPDATE SAILW1151V.DCP02_PHEN_READ_INFLUENZA_VACC
SET name = 'INFLUENZA_VACC',
    category = 'Influenza vaccination',
    is_latest = '1',
    valid_from = '2021-12-13'
WHERE code IS NOT NULL;

UPDATE SAILW1151V.DCP02_PHEN_READ_INFLUENZA_VACC tgt
SET desc_ukhd = CASE WHEN pref_term_198 IS NOT NULL THEN pref_term_198
                     WHEN pref_term_60 IS NOT NULL THEN pref_term_60
                     ELSE pref_term_30
                END,
    desc_ukhd_30 = pref_term_30,
    desc_ukhd_60 = pref_term_60,
    desc_ukhd_198 = pref_term_198
FROM SAILUKHDV.READ_CD_CV2_SCD src
WHERE tgt.code = src.read_code;

GRANT ALL ON TABLE SAILW1151V.DCP02_PHEN_READ_INFLUENZA_VACC
to role nrdasail_sail_1151_analyst;
-- using updated code list provided by Hoda


-- step 1.2b create vaccination table for 2021 influenza season

--======
-- 2021
--======

CALL FNC.DROP_IF_EXISTS ('sailw1151v.dcp02_flu_vacc_2021');

CREATE TABLE sailw1151v.dcp02_flu_vacc_2021 (
    alf_e           bigint NOT null,
    event_dt    	date,
    event_cd		varchar(9),
    event_val		decimal(31, 8),
    read_desc		varchar(200),
    PRIMARY KEY (alf_e)
);

INSERT INTO sailw1151v.dcp02_flu_vacc_2021
WITH
    lkp_influenza_vaccine AS (
    	-- use provided csv as source for lookup values
	    -- chop read codes to just to 5 characters long
		-- and pad 4-length read codes to 5 using dot
        SELECT DISTINCT
            RPAD(SUBSTRING(code, 1, 5), 5, '.') AS read_cd,
            desc AS read_desc
        FROM
            SAILW1151V.DCP02_PHEN_READ_INFLUENZA_VACC -- SAILW1151V.DCP02_LKP_FLU_VACC 
    ),
    gp_flu_vacc AS
    (
SELECT
    gp.alf_e,
    gp.event_dt,
    gp.EVENT_CD,
    gp.EVENT_VAL, 
    lkp_influenza_vaccine.read_desc,
    ROW_NUMBER() OVER (PARTITION BY gp.ALF_E
					ORDER BY gp.event_DT) AS RK
FROM
    SAILWMC_V.C19_COHORT_WLGP_GP_EVENT_CLEANSED AS gp 
INNER JOIN
    lkp_influenza_vaccine
    ON gp.event_cd = lkp_influenza_vaccine.read_cd
WHERE
    gp.alf_e IS NOT NULL
	AND gp.alf_sts_cd IN ('1','4','39')
	AND	gp.event_cd IS NOT NULL
	AND gp.event_dt IS NOT NULL
	AND gp.event_dt BETWEEN '2021-08-01' AND '2022-07-31' -- Aug 21 to july 22, dates will need to be confirmed
GROUP BY
	alf_e, event_dt, event_cd, event_val, read_desc
	)
SELECT alf_e, event_dt, event_cd, event_val, read_desc
FROM gp_flu_vacc
WHERE RK = 1 -- taking first event, may need to review
ORDER BY alf_e;

-- SELECT * FROM sailw1151v.dcp02_flu_vacc ;



-- step 1.3 create vaccination table for 2020 influenze season

--======
-- 2020
--======

-- 2020-21 season
-- using updated code list provided by Hoda

CALL FNC.DROP_IF_EXISTS ('sailw1151v.dcp02_flu_vacc_2020'); -- dropped and recreate in 1151 new table name

CREATE TABLE sailw1151v.dcp02_flu_vacc_2020 (
    alf_e           bigint NOT null,
    event_dt    	date,
    event_cd		varchar(9),
    event_val		decimal(31, 8),
    read_desc		varchar(200),
    PRIMARY KEY (alf_e)
);

INSERT INTO sailw1151v.dcp02_flu_vacc_2020
WITH
    lkp_influenza_vaccine AS (
    	-- use provided csv as source for lookup values
	    -- chop read codes to just to 5 characters long
		-- and pad 4-length read codes to 5 using dot
        SELECT DISTINCT
            RPAD(SUBSTRING(code, 1, 5), 5, '.') AS read_cd,
            desc AS read_desc
        FROM
            SAILW1151V.DCP02_PHEN_READ_INFLUENZA_VACC -- SAILW1151V.DCP02_LKP_FLU_VACC 
    ),
    gp_flu_vacc AS
    (
SELECT
    gp.alf_e,
    gp.event_dt,
    gp.EVENT_CD,
    gp.EVENT_VAL, 
    lkp_influenza_vaccine.read_desc,
    ROW_NUMBER() OVER (PARTITION BY gp.ALF_E
					ORDER BY gp.event_DT) AS RK
FROM
    SAILWMC_V.C19_COHORT_WLGP_GP_EVENT_CLEANSED AS gp 
INNER JOIN
    lkp_influenza_vaccine
    ON gp.event_cd = lkp_influenza_vaccine.read_cd
WHERE
    gp.alf_e IS NOT NULL
	AND gp.alf_sts_cd IN ('1','4','39')
	AND	gp.event_cd IS NOT NULL
	AND gp.event_dt IS NOT NULL
	AND gp.event_dt BETWEEN '2020-08-01' AND '2021-07-31' -- initially look at full year
GROUP BY
	alf_e, event_dt, event_cd, event_val, read_desc
	)
SELECT alf_e, event_dt, event_cd, event_val, read_desc
FROM gp_flu_vacc
WHERE RK = 1 -- taking first event, may need to review
ORDER BY alf_e;

SELECT * FROM sailw1151v.dcp02_flu_vacc_2020 ;


-- note that multiple records may exist, even on the same day
-- important as description provides details of who may have administered it
-- need to consider how to manage this

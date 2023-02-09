
--==============================================================================
-- Step 1: Define cohort
--==============================================================================

CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_disparity_cohort');

create table SAILW1151V.dcp02_disparity_cohort
(
-- Selection criteria and demographics
	alf_e				bigint not null,
	wob					date,
	gndr_cd				smallint,
-- Living in Wales?
	wds_start_date		date,		-- from wdsd shows that they live in Wales
	wds_end_date		date,
-- Registered with the GP
	gp_flg				smallint,	-- has had at least one event in last 5 years
	gp_start_date		date,		-- shows they're registered with any Welsh GP
	gp_end_date			date,
	sgp_start_date		date,
	sgp_end_date		date,		-- specifically a SAIL GP
-- Age demographics
	age					int,		-- on 1st Sep 2020
	over18_flg			smallint,
	over50_flg			smallint,
	over65_flg			smallint,
	over80_flg			smallint,
-- Ethnicity demographics
	ethn_cat			varchar(8),
	ethn_date			date,
-- Household info
	ralf_e				bigint,
	ralf_sts_cd			varchar(1),
	lsoa2011_cd			varchar(10),
	wimd2019_quintile   smallint,
	lad2011_nm          varchar(48),
	health_board        varchar(48),
	urban_rural_class   varchar(56),
	hh_n            	smallint,	-- no. in house
	hh_age_min 			smallint,
	hh_age_avg 			decimal(5,1),
	hh_age_max 			smallint,
-- QCOVID flags
	qc_flg                        smallint,
	qc_b2_82                      smallint,
	qc_b2_leukolaba               smallint,
	qc_b2_prednisolone            smallint,
	qc_b_af                       smallint,
	qc_b_ccf                      smallint,
	qc_b_asthma                   smallint,
	qc_b_bloodcancer              smallint,
	qc_b_cerebralpalsy            smallint,
	qc_b_chd                      smallint,
	qc_b_cirrhosis                smallint,
	qc_b_congenheart              smallint,
	qc_b_copd                     smallint,
	qc_b_dementia                 smallint,
	qc_b_epilepsy                 smallint,
	qc_b_fracture4                smallint,
	qc_b_neurorare                smallint,
	qc_b_parkinsons               smallint,
	qc_b_pulmhyper                smallint,
	qc_b_pulmrare      		      smallint,
	qc_b_pvd                      smallint,
	qc_b_ra_sle                   smallint,
	qc_b_respcancer               smallint,
	qc_b_semi                     smallint,
	qc_b_sicklecelldisease        smallint,
	qc_b_stroke                   smallint,
	qc_diabetes_cat               smallint,
	diabetes_flg				  smallint,
	qc_b_vte                      smallint,
	qc_bmi                        decimal(31, 8),
	morbid_obese_flg			  smallint,
	qc_chemo_cat                  smallint,
	chemo_flg					  smallint,
	qc_home_cat                   smallint,
	qc_learn_cat                  smallint,
	qc_p_marrow6                  smallint,
	qc_p_radio6                   smallint,
	qc_p_solidtransplant          smallint,
	qc_renal_cat                  smallint,
	ckd_flg					  	  smallint,
	pregnant_flg				  smallint,
	concep_dt					  date,
	birth_dt					  date,
-- COVID vaccination
	c19_HAS_BAD_VACC_RECORD			smallint,
-- dose 1
	c19_vacc_dose1_flg				smallint,
	c19_VACC_DOSE1_DATE				date,
	c19_VACC_DOSE1_NAME				varchar(50),
-- dose 2
	c19_vacc_dose2_flg				smallint,
	c19_VACC_DOSE2_DATE				date,
	c19_VACC_DOSE2_NAME				varchar(50),
-- dose 3 or 1st Booster
	c19_vacc_dose3_flg				smallint,
	c19_VACC_DOSE3_DATE				date,
	c19_VACC_DOSE3_NAME				varchar(50),
-- flu vaccination
-- 2021
	fluvac21_flg					smallint,
	fluvac21_dt						date,
-- 2020
	fluvac20_flg					smallint,
	fluvac20_dt						date,
-- death
	dead_flg						smallint,		-- died before 31st Jan 2022
	death_date						date,
	PRIMARY KEY (alf_e)
)
;


-- =============================================================================
-- Step 2: Prepare SAIL GP
-- =============================================================================

CALL fnc.drop_if_exists('sailw1151v.dcp02_sgp');

CREATE TABLE sailw1151v.dcp02_sgp (
	alf_e                    bigint not null,
	start_date				 date,
	end_date                 date,
	PRIMARY KEY(alf_e)
);

INSERT INTO sailw1151v.dcp02_sgp
SELECT
	alf_e,
	max(start_date)			 AS start_date,
	max(END_DATE) 			 AS end_date
FROM sailwmc_v.C19_COHORT_WLGP_CLEAN_GP_REG_BY_PRAC_INCLNONSAIL_MEDIAN ccwcgrbpim
WHERE GP_DATA_FLAG = 1
GROUP BY alf_e
;

--==============================================================================
-- Step 3: GP flag
--==============================================================================

CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_gp');

create table sailw1151v.dcp02_gp
(
	alf_e 				bigint not null
)
;

insert into sailw1151v.dcp02_gp
select distinct
	alf_e  					
from sailwmc_v.c19_cohort_wlgp_GP_EVENT_CLEANSED
	where event_dt + 5 years > date('2020-09-01')
	and alf_sts_cd in (1,4,39)
;
--==============================================================================
-- Step 4: Age demographics
--==============================================================================

CALL FNC.DROP_IF_EXISTS('sailw1151v.DCP02_age');

create table SAILW1151V.dcp02_age
(
	alf_e				bigint not null,
	age    				int,			-- on 1st September 2020
	over18_flg			smallint,
	over50_flg			smallint,
	over65_flg			smallint,
	over80_flg			smallint,
	PRIMARY KEY (alf_e)
)
;

insert into SAILW1151V.DCP02_age
select
	c20.alf_e,
	floor(days_between(date('2020-09-01'), c20.wob)/365.25)	as age,
	cast(c20.wob + 18 years <= date('2020-09-01')
			as smallint)										as over18_flg,
	cast(c20.wob + 50 years <= date('2020-09-01')
			as smallint)										as over50_flg,
	cast(c20.wob + 65 years <= date('2020-09-01')
			as smallint)										as over65_flg,
	cast(c20.wob + 80 years <= date('2020-09-01')
			as smallint)										as over80_flg
from
	sailwmc_v.C19_COHORT20 c20
;


--==============================================================================
-- Step 5: Ethnicity info
--==============================================================================

CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_ethn');

CREATE TABLE sailw1151v.dcp02_ethn (
	alf_e              bigint not null,
	ethn_cat           varchar(8),
	ethn_date          date,
	ethn_src           varchar(4),
	PRIMARY KEY (alf_e)
);

INSERT INTO sailw1151v.dcp02_ethn
WITH
	ethn AS
	(
		-- one row per reporting of ethnicity ----------------------------- before 1st Sep 2022
		-- for those in c19 cohort 2020 with a row number to find
		-- the latest reporting
		SELECT
			ethn.alf_e,
			ethn.ethn_ec_ons_desc     AS ethn_cat,
			ethn.ethn_date            AS ethn_date,
			ethn.ethn_data_source     AS ethn_src,
			row_number () OVER (PARTITION BY ethn.alf_e ORDER BY ethn.ethn_date DESC) AS rank_latest
		FROM
			sailwmc_v.c19_cohort20 AS cohort
		INNER JOIN
			sailw1151v.rrda_ethn_prep_date AS ethn
				ON cohort.alf_e = ethn.alf_e
		WHERE
			ethn.ethn_date <= '2022-03-31'
	)
SELECT alf_e, ethn_cat, ethn_date, ethn_src
FROM ethn
WHERE rank_latest = 1;

--==============================================================================
-- Step 6: Household info
--==============================================================================

CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_hh');

CREATE TABLE sailw1151v.dcp02_hh (
	alf_e                  bigint   not null,
	res_date               date     not null,
	ralf_e                 bigint,
	ralf_sts_cd            varchar(1),
-- area
	lsoa2011_cd            varchar(10),
	wimd2019_quintile      smallint,
	lad2011_nm             varchar(48),
	health_board           varchar(48),
	urban_rural_class      varchar(56),
-- all household members
	household_n            smallint,
	household_age_min      smallint,
	household_age_avg      decimal(5,1),
	household_age_max      smallint,
-- children
	child_n                smallint,
	child_00_n             smallint,
	child_01_04_n          smallint,
	child_05_09_n          smallint,
	child_10_15_n          smallint,
	child_16_17_n          smallint,
	child_age_min          smallint,
	child_age_avg          decimal(5,1),
	child_age_max          smallint,
-- other adults
	adult_n                smallint,
	adult_male_18_59_n     smallint,
	adult_male_60_pl_n     smallint,
	adult_female_18_59_n   smallint,
	adult_female_60_pl_n   smallint,
	adult_age_min          smallint,
	adult_age_avg          decimal(5,1),
	adult_age_max          smallint,
	PRIMARY KEY (alf_e)
);


INSERT INTO sailw1151v.dcp02_hh
WITH
	cohort AS
	(
		SELECT
			alf_e,
			DATE '2020-09-01' AS res_date
		FROM
			sailwmc_v.C19_COHORT20
	),
	cohort_ralf AS
	(
		SELECT
			cohort.alf_e,
			cohort.res_date,
			wds_add.ralf_e,
			wds_add.ralf_sts_cd,
			wds_add.from_dt,
			wds_add.to_dt,
			wds_add.lsoa2011_cd,
			ROW_NUMBER() OVER (PARTITION BY cohort.alf_e, cohort.res_date
				ORDER BY wds_add.from_dt, wds_add.to_dt DESC) AS ralf_seq
		FROM
			cohort
		INNER JOIN
			sailwmc_v.c19_cohort_wdsd_ar_pers AS wds_prs
			ON wds_prs.alf_e = cohort.alf_e
		INNER JOIN
			sailwmc_v.c19_cohort_wdsd_ar_pers_add AS wds_add
			ON  wds_add.pers_id_e = wds_prs.pers_id_e
			AND wds_add.from_dt <= cohort.res_date
			AND wds_add.to_dt >= cohort.res_date
	),
	cohort_ralf_uniq AS
	(
		SELECT
			cohort_ralf.*,
			lkp_wimd.overall_quintile                               AS wimd2019_quintile,
			lkp_health_board.ua_desc                                AS lad2011_nm,
			lkp_health_board.lhb19nm                                AS health_board,
			lkp_urban_rural.ruc11cd || ' ' || lkp_urban_rural.ruc11 AS urban_rural_class
		FROM cohort_ralf
		LEFT JOIN sailrefrv.wimd2019_index_and_domain_ranks_by_small_area AS lkp_wimd
			ON cohort_ralf.lsoa2011_cd = lkp_wimd.lsoa2011_cd
		LEFT JOIN sailw1151v.dacvap_lkp_lsoa_hb AS lkp_health_board
			ON cohort_ralf.lsoa2011_cd = lkp_health_board.lsoa2011_cd
		LEFT JOIN sailrefrv.rural_urban_class_2011_of_llsoareas_in_eng_and_wal AS lkp_urban_rural
			ON cohort_ralf.lsoa2011_cd = lkp_urban_rural.lsoa11cd
		WHERE ralf_seq = 1
	),
	hh_member_all AS
	(
		SELECT
		-- person of interest
			cohort.alf_e,
			cohort.res_date,
			cohort.ralf_e,
			cohort.ralf_sts_cd,
		-- other household member
			wds_prs.alf_e   AS hh_alf_e,
			ROW_NUMBER() OVER (PARTITION BY cohort.alf_e, cohort.res_date, wds_prs.alf_e ORDER BY wds_add.from_dt, wds_add.to_dt DESC) AS ralf_seq,
			wds_prs.gndr_cd,
			floor((days(cohort.res_date) - days(wds_prs.wob)) / 365.25) AS age
		FROM
			cohort_ralf_uniq AS cohort
		INNER JOIN
			sailwmc_v.c19_cohort_wdsd_ar_pers_add AS wds_add
			ON cohort.ralf_e = wds_add.ralf_e
			AND cohort.res_date >= wds_add.from_dt
			AND cohort.res_date <= wds_add.to_dt
		INNER JOIN
			sailwmc_v.c19_cohort_wdsd_ar_pers AS wds_prs
			ON wds_prs.pers_id_e = wds_add.pers_id_e
		WHERE
			cohort.ralf_e IS NOT NULL
	),
	hh_member_ind AS
	(
		SELECT
			*,
			-- make indicators used to summarise household members
			CAST(age >= 0 AND age <= 17                  AS smallint) AS is_child,
			CAST(age = 0                                 AS smallint) AS is_child_00,
			CAST(age >= 01 AND age <= 04                 AS smallint) AS is_child_01_04,
			CAST(age >= 05 AND age <= 09                 AS smallint) AS is_child_05_09,
			CAST(age >= 10 AND age <= 15                 AS smallint) AS is_child_10_15,
			CAST(age >= 16 AND age <= 17                 AS smallint) AS is_child_16_17,
			CAST(age >= 18                               AS smallint) AS is_adult,
			CAST(gndr_cd = 1 AND age >= 18 AND age <= 59 AS smallint) AS is_adult_male_18_59,
			CAST(gndr_cd = 1 AND age >= 60               AS smallint) AS is_adult_male_60_pl,
			CAST(gndr_cd = 2 AND age >= 18 AND age <= 59 AS smallint) AS is_adult_female_18_59,
			CAST(gndr_cd = 2 AND age >= 60               AS smallint) AS is_adult_female_60_pl
		FROM
			hh_member_all
		WHERE
			-- remove any duplicated rows (there should on be a small number of duplicates)
			ralf_seq = 1
	),
	hh_summary AS
	(
		SELECT
			alf_e,
			res_date,
			ralf_e,
		-- all household members
			COUNT(*)                                           AS household_n,
			MIN(age)                                           AS household_age_min,
			AVG(age)                                           AS household_age_avg,
			MAX(age)                                           AS household_age_max,
		-- children
			SUM(is_child)                                      AS child_n,
			SUM(is_child_00)                                   AS child_00_n,
			SUM(is_child_01_04)                                AS child_01_04_n,
			SUM(is_child_05_09)                                AS child_05_09_n,
			SUM(is_child_10_15)                                AS child_10_15_n,
			SUM(is_child_16_17)                                AS child_16_17_n,
			MIN(CASE WHEN is_child = 1 THEN age ELSE NULL END) AS child_age_min,
			AVG(CASE WHEN is_child = 1 THEN age ELSE NULL END) AS child_age_avg,
			MAX(CASE WHEN is_child = 1 THEN age ELSE NULL END) AS child_age_max,
		-- adults
			SUM(is_adult)                                      AS adult_n,
			SUM(is_adult_male_18_59)                           AS adult_male_18_59_n,
			SUM(is_adult_male_60_pl)                           AS adult_male_60_pl_n,
			SUM(is_adult_female_18_59)                         AS adult_female_18_59_n,
			SUM(is_adult_female_60_pl)                         AS adult_female_60_pl_n,
			MIN(CASE WHEN is_adult = 1 THEN age ELSE NULL END) AS adult_age_min,
			AVG(CASE WHEN is_adult = 1 THEN age ELSE NULL END) AS adult_age_avg,
			MAX(CASE WHEN is_adult = 1 THEN age ELSE NULL END) AS adult_age_max
		FROM
			hh_member_ind
		GROUP BY
			alf_e, res_date, ralf_e
	)
SELECT
	cohort_ralf_uniq.alf_e,
	cohort_ralf_uniq.res_date,
	cohort_ralf_uniq.ralf_e,
	cohort_ralf_uniq.ralf_sts_cd,
-- area
	cohort_ralf_uniq.lsoa2011_cd,
	cohort_ralf_uniq.wimd2019_quintile,
	cohort_ralf_uniq.lad2011_nm,
	cohort_ralf_uniq.health_board,
	cohort_ralf_uniq.urban_rural_class,
-- all household members
	hh_summary.household_n,
	hh_summary.household_age_min,
	hh_summary.household_age_avg,
	hh_summary.household_age_max,
-- children
	hh_summary.child_n,
	hh_summary.child_00_n,
	hh_summary.child_01_04_n,
	hh_summary.child_05_09_n,
	hh_summary.child_10_15_n,
	hh_summary.child_16_17_n,
	hh_summary.child_age_min,
	hh_summary.child_age_avg,
	hh_summary.child_age_max,
-- other adults
	hh_summary.adult_n,
	hh_summary.adult_male_18_59_n,
	hh_summary.adult_male_60_pl_n,
	hh_summary.adult_female_18_59_n,
	hh_summary.adult_female_60_pl_n,
	hh_summary.adult_age_min,
	hh_summary.adult_age_avg,
	hh_summary.adult_age_max
FROM cohort_ralf_uniq
LEFT JOIN hh_summary
	ON 	hh_summary.alf_e    = cohort_ralf_uniq.alf_e
	AND hh_summary.res_date = cohort_ralf_uniq.res_date
	AND hh_summary.ralf_e   = cohort_ralf_uniq.ralf_e;

--==============================================================================
-- Step 7: Pregnant flag
--==============================================================================

CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_pregnancy');

create table sailw1151v.dcp02_pregnancy (
	alf_e  				bigint not null,
	concep_dt			date,
	birth_dt			date,
	pregnant_flg		smallint,
	m_bmi     			decimal,
	primary key (alf_e)
);

insert into sailw1151v.dcp02_pregnancy
with dates as
	(SELECT
		m_alf_e 			as alf_e,
		wob,
		case when gestational_age is null
		then (wob - (40*7) DAYS)
		else (wob - (gestational_age*7) DAYS)
		end as concep_dt
	from sailw1151v.dcp02_child_birth
	where m_alf_sts_cd in (1,4,39)
	),
flg as
	(select
		dates.alf_e,
		dates.wob,
		dates.concep_dt,
		cast(dates.wob > (date '2020-09-01' + 30 days) and dates.concep_dt < (date '2022-03-31' - 30 days)
			as smallint) 		as pregnant_flg
	from dates 
	where alf_e is not null
	and dates.wob > date '2020-09-01'
	and dates.concep_dt < date '2022-03-31'
	),
bmi_prep1 as 
	(select
		m_alf_e as alf_e,
		m_bmi,
		m_bmi_dt
	from sailw1151v.dcp02_child_birth
	where m_alf_sts_cd in (1,4,39)
	and m_alf_e is not null	
	and m_bmi_dt > (date '2020-09-01' - 5 years)
	and m_bmi_dt < date '2022-03-31'
	),
bmi_prep2 as
	(select
		bmi_prep1.alf_e,
		bmi_prep1.m_bmi,
		row_number() over (partition by bmi_prep1.alf_e order by bmi_prep1.m_bmi_dt) as row_num
	from bmi_prep1
	),
bmi as
	(select
		bmi_prep2.alf_e,
		bmi_prep2.m_bmi
	from bmi_prep2
	where bmi_prep2.row_num = 1
	)
select
	flg.alf_e,
	min(flg.concep_dt) as concep_dt,
	max(flg.wob) as birth_dt,
	max(flg.pregnant_flg) as pregnant_flg,
	max(bmi.m_bmi) as m_bmi
from flg
left join bmi
	on flg.alf_e = bmi.alf_e
	group by flg.alf_e
;
--==============================================================================
-- Step 8: Insert into table
--==============================================================================

insert into sailw1151v.dcp02_disparity_cohort
select
	c20.alf_e,
	c20.wob,
	c20.gndr_cd,
	c20.wds_start_date,
	c20.wds_end_date,
	cast(gp.alf_e is not null
		as smallint) as gp_flg,
	c20.gp_start_date,
	c20.gp_end_date,
	sgp.start_date 		as sgp_start_date,
	sgp.end_date 		as sgp_end_date,
	age.age,
	age.over18_flg,
	age.over50_flg,
	age.over65_flg,
	age.over80_flg,
	ethn.ethn_cat,
	ethn.ethn_date,
	hh.ralf_e,
	hh.ralf_sts_cd,
	hh.lsoa2011_cd,
	hh.wimd2019_quintile,
	hh.lad2011_nm,
	hh.health_board,
	hh.urban_rural_class,
	hh.household_n 		 as hh_n,
	hh.household_age_min as hh_age_min,
	hh.household_age_avg as hh_age_avg,
	hh.household_age_max as hh_age_max,
	cast(qc.alf_e is not null and c20.gp_end_date >= '2022-09-01'
		as smallint) as qc_flg,
	qc.b2_82 as qc_b2_82,
	qc.b2_leukolaba as qc_b2_leukolaba,
	qc.b2_prednisolone as qc_b2_prednisolone,
	qc.b_af as qc_b_af,
	qc.b_ccf as qc_b_ccf,
	qc.b_asthma as qc_b_asthma,
	qc.b_bloodcancer as qc_b_bloodcancer,
	qc.b_cerebralpalsy as qc_b_cerebralpalsy,
	qc.b_chd as qc_b_chd,
	qc.b_cirrhosis as qc_b_cirrhosis,
	qc.b_congenheart as qc_b_congenheart,
	qc.b_copd as qc_b_copd,
	qc.b_dementia as qc_b_dementia,
	qc.b_epilepsy as qc_b_epilepsy,
	qc.b_fracture4 as qc_b_fracture4,
	qc.b_neurorare as qc_b_neurorare,
	qc.b_parkinsons as qc_b_parkinsons,
	qc.b_pulmhyper as qc_b_pulmhyper,
	qc.b_pulmrare as qc_b_pulmrare,
	qc.b_pvd as qc_b_pvd,
	qc.b_ra_sle as qc_b_ra_sle,
	qc.b_respcancer as qc_b_respcancer,
	qc.b_semi as qc_b_semi,
	qc.b_sicklecelldisease as qc_b_sicklecelldisease,
	qc.b_stroke as qc_b_stroke,
	qc.diabetes_cat as qc_diabetes_cat,
	cast(qc.diabetes_cat != 0
		as smallint) as diabetes_flg,
	qc.b_vte as qc_b_vte,
	case when qc.bmi is null then preg.m_bmi
	else qc.bmi
	end as qc_bmi,
	cast(qc.bmi >= 40
		as smallint) as morbid_obese_flg,
	qc.chemocat as qc_chemo_cat,
	cast(qc.chemocat != 0
		as smallint) as chemo_flg,
	qc.homecat as qc_home_cat,
	qc.learncat as qc_learn_cat,
	qc.p_marrow6 as qc_p_marrow6,
	qc.p_radio6 as qc_p_radio6,
	qc.p_solidtransplant as qc_p_solidtransplant,
	qc.renalcat as qc_renal_cat,
	cast(qc.renalcat > 1
		as smallint) as ckd_flg,
	preg.pregnant_flg,
	preg.concep_dt,
	preg.birth_dt,
	c19_vacc.c19_vacc_bad_record_flg as c19_has_bad_vacc_record,
	case
		when c19_vacc.c19_vacc_1_date is not null then 1
		else 0
		end 					as c19_vacc_dose1_flg,
	c19_vacc.c19_VACC_1_DATE 	as c19_vacc_dose1_date,
	c19_vacc.c19_VACC_1_NAME	as c19_vacc_dose1_name,
	case
		when c19_vacc.c19_vacc_2_date is not null then 1
		else 0
		end 					as c19_vacc_dose2_flg,
	c19_vacc.c19_VACC_2_DATE 	as c19_vacc_dose2_date,
	c19_vacc.c19_VACC_2_NAME	as c19_vacc_dose2_name,
	case
		when c19_vacc.c19_vacc_3_date is not null then 1
		else 0
		end 					as c19_vacc_dose3_flg,
	c19_vacc.c19_VACC_3_DATE 	as c19_vacc_dose3_date,
	c19_vacc.c19_VACC_3_NAME	as c19_vacc_dose3_name,
	case
		when fluvac_21.event_dt is not null then 1
		else 0
		end 					as fluvac21_flg,
	fluvac_21.EVENT_DT 			as fluvac21_dt,
	case
		when fluvac_20.event_dt is not null then 1
		else 0
		end 					as fluvac20_flg,
	fluvac_20.EVENT_DT 			as fluvac20_dt,
	cast(mort.dod <= date('2022-03-31')
		as smallint) as dead_flg,
	mort.dod as death_date
from sailwmc_v.c19_cohort20 c20
left join sailw1151v.dcp02_sgp sgp
	on c20.alf_e = sgp.alf_e
left join sailwwmc_v.jl_dacvap_qcovid AS qc
	ON c20.alf_e = qc.alf_e
left join sailw1151v.dcp02_c19_vacc c19_vacc
	on c20.alf_e = c19_vacc.alf_e
left join sailw1151v.dcp02_flu_vacc_2021 fluvac_21
	on c20.alf_e = fluvac_21.alf_e
left join sailw1151v.dcp02_flu_vacc_2020 fluvac_20
	on c20.alf_e = fluvac_20.alf_e
left join sailw1151v.dcp02_gp gp
	on c20.alf_e = gp.alf_e
left join sailw1151v.dcp02_age age
	on c20.alf_e = age.alf_e
left join sailw1151v.dcp02_ethn ethn
	on c20.alf_e = ethn.alf_e
left join sailw1151v.dcp02_hh hh
	on c20.alf_e = hh.alf_e
left join sailw1151v.dcp02_pregnancy preg
	on c20.alf_e = preg.alf_e
left join sailwmc_v.c19_cohort20_mortality mort
	on c20.alf_e = mort.alf_e
;


--==============================================================================
-- Step 8: Tidying up
--==============================================================================

CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_gp');
CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_age');
CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_ethn');
CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_hh');
CALL FNC.DROP_IF_EXISTS('sailw1151v.dcp02_pregnancy');



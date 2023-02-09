
# ==========================================================================
# N.B. This script was not used in the final analysis
# ==========================================================================

source("r_clear_and_load.r")

cat("Main cohort\n")

# load cohort ==================================================================
cat("load cohort\n")

d_log_alfs <- qread(s_drive("d_log_alfs.qs"))
xvar_names <- qread(s_drive("xvar_names.qs"))
d_bmi_imp <- qread(s_drive("d_bmi_imp.qs"))
impute.bmi <- qread(s_drive("imputebmi_func.qs"))

cat("Separate by WIMD\n")

# Define cohorts to be analysed
levels(d_log_alfs$wimd2019_quintile) <- c(5,1,2,3,4)
d_log_alfs$wimd2019_quintile <- relevel(d_log_alfs$wimd2019_quintile, 1,2,3,4,5)

for (i in c(1:5)) {
# select based on wimd quintile
	#get alfs for describing data later
	assign(paste0("d_log_wimd_q",i,"_alfs"),
		   d_log_alfs[d_log_alfs$wimd2019_quintile == i,] %>% select(-wimd2019_quintile)
		  )
# impute bmi and remove alfs 
	assign(paste0("d_log_wimd_q",i),
		   get(paste0("d_log_wimd_q",i,"_alfs")) %>% impute.bmi() %>% select(-alf_e)
		  )

# remove the flu vacc data to not influence covid test
	assign(paste0("d_log_wimd_q",i,"_c19"),
		   get(paste0("d_log_wimd_q",i)) %>% select(-flu_vacc_complete_flg)
		  )
# remove the covid vacc data to not influence flu test
	assign(paste0("d_log_wimd_q",i,"_flu"),
		   get(paste0("d_log_wimd_q",i)) %>% select(-c19_vacc_complete_flg)
		  )
}

# ==========================================================================
# Modeling
# ==========================================================================
cat("Modeling...\n")

# Create adjusted log reg models
for (i in c(1:5)) {
# covid models
	assign(paste0("m_log_adj_q",i,"_c19"),
		   glm(c19_vacc_complete_flg ~ ., family = binomial, data = get(paste0("d_log_wimd_q",i,"_c19")))
		  )
# flu models
	assign(paste0("m_log_adj_q",i,"_flu"),
		   glm(flu_vacc_complete_flg ~ ., family = binomial, data = get(paste0("d_log_wimd_q",i,"_flu")))
		  )
}


# Create unadjusted log reg models

for (xvar in xvar_names[-1]) {
	# write formulae
	assign(paste0(xvar,"_formula_c19"),
		   as.formula(paste0("c19_vacc_complete_flg ~ ",xvar))
		  )
	assign(paste0(xvar,"_formula_flu"),
		   as.formula(paste0("flu_vacc_complete_flg ~ ",xvar))
		  )	
	# make models
	for (i in c(1:5)) {
		# covid models
		assign(paste0("m_log_unadj_q",i,"_c19_", xvar),
			   glm(get(paste0(xvar,"_formula_c19")), family = binomial, data = get(paste0("d_log_wimd_q",i,"_c19")))
			  )
		# flu models
		assign(paste0("m_log_unadj_q",i,"_flu_", xvar),
			   glm(get(paste0(xvar,"_formula_flu")), family = binomial, data = get(paste0("d_log_wimd_q",i,"_flu")))
			  )
	}
}
# ==========================================================================
# Save models
# ==========================================================================
cat("Saving...\n")

qsavem(
	m_log_adj_q1_c19,
	m_log_adj_q2_c19,
	m_log_adj_q3_c19,
	m_log_adj_q4_c19,
	m_log_adj_q5_c19,
	m_log_adj_q1_flu,
	m_log_adj_q2_flu,
	m_log_adj_q3_flu,
	m_log_adj_q4_flu,
	m_log_adj_q5_flu,
	m_log_unadj_q1_c19_age_cat,
	m_log_unadj_q1_c19_gndr_cd,
	m_log_unadj_q1_c19_ethn_cat,
	m_log_unadj_q1_c19_bmi_cat,
	m_log_unadj_q1_c19_urban_rural_class,
	m_log_unadj_q1_c19_hh_cat,
	m_log_unadj_q1_c19_num_clinical_conditions_cat,
	m_log_unadj_q1_c19_health_board,
	m_log_unadj_q2_c19_age_cat,
	m_log_unadj_q2_c19_gndr_cd,
	m_log_unadj_q2_c19_ethn_cat,
	m_log_unadj_q2_c19_bmi_cat,
	m_log_unadj_q2_c19_urban_rural_class,
	m_log_unadj_q2_c19_hh_cat,
	m_log_unadj_q2_c19_num_clinical_conditions_cat,
	m_log_unadj_q2_c19_health_board,
	m_log_unadj_q3_c19_age_cat,
	m_log_unadj_q3_c19_gndr_cd,
	m_log_unadj_q3_c19_ethn_cat,
	m_log_unadj_q3_c19_bmi_cat,
	m_log_unadj_q3_c19_urban_rural_class,
	m_log_unadj_q3_c19_hh_cat,
	m_log_unadj_q3_c19_num_clinical_conditions_cat,
	m_log_unadj_q3_c19_health_board,
	m_log_unadj_q4_c19_age_cat,
	m_log_unadj_q4_c19_gndr_cd,
	m_log_unadj_q4_c19_ethn_cat,
	m_log_unadj_q4_c19_bmi_cat,
	m_log_unadj_q4_c19_urban_rural_class,
	m_log_unadj_q4_c19_hh_cat,
	m_log_unadj_q4_c19_num_clinical_conditions_cat,
	m_log_unadj_q4_c19_health_board,
	m_log_unadj_q5_c19_age_cat,
	m_log_unadj_q5_c19_gndr_cd,
	m_log_unadj_q5_c19_ethn_cat,
	m_log_unadj_q5_c19_bmi_cat,
	m_log_unadj_q5_c19_urban_rural_class,
	m_log_unadj_q5_c19_hh_cat,
	m_log_unadj_q5_c19_num_clinical_conditions_cat,
	m_log_unadj_q5_c19_health_board,
	m_log_unadj_q1_flu_age_cat,
	m_log_unadj_q1_flu_gndr_cd,
	m_log_unadj_q1_flu_ethn_cat,
	m_log_unadj_q1_flu_bmi_cat,
	m_log_unadj_q1_flu_urban_rural_class,
	m_log_unadj_q1_flu_hh_cat,
	m_log_unadj_q1_flu_num_clinical_conditions_cat,
	m_log_unadj_q1_flu_health_board,
	m_log_unadj_q2_flu_age_cat,
	m_log_unadj_q2_flu_gndr_cd,
	m_log_unadj_q2_flu_ethn_cat,
	m_log_unadj_q2_flu_bmi_cat,
	m_log_unadj_q2_flu_urban_rural_class,
	m_log_unadj_q2_flu_hh_cat,
	m_log_unadj_q2_flu_num_clinical_conditions_cat,
	m_log_unadj_q2_flu_health_board,
	m_log_unadj_q3_flu_age_cat,
	m_log_unadj_q3_flu_gndr_cd,
	m_log_unadj_q3_flu_ethn_cat,
	m_log_unadj_q3_flu_bmi_cat,
	m_log_unadj_q3_flu_urban_rural_class,
	m_log_unadj_q3_flu_hh_cat,
	m_log_unadj_q3_flu_num_clinical_conditions_cat,
	m_log_unadj_q3_flu_health_board,
	m_log_unadj_q4_flu_age_cat,
	m_log_unadj_q4_flu_gndr_cd,
	m_log_unadj_q4_flu_ethn_cat,
	m_log_unadj_q4_flu_bmi_cat,
	m_log_unadj_q4_flu_urban_rural_class,
	m_log_unadj_q4_flu_hh_cat,
	m_log_unadj_q4_flu_num_clinical_conditions_cat,
	m_log_unadj_q4_flu_health_board,
	m_log_unadj_q5_flu_age_cat,
	m_log_unadj_q5_flu_gndr_cd,
	m_log_unadj_q5_flu_ethn_cat,
	m_log_unadj_q5_flu_bmi_cat,
	m_log_unadj_q5_flu_urban_rural_class,
	m_log_unadj_q5_flu_hh_cat,
	m_log_unadj_q5_flu_num_clinical_conditions_cat,
	m_log_unadj_q5_flu_health_board,
	file = s_drive("m_log_by_wimd.qsm")
)

qsavem(
	d_log_wimd_q1_alfs,
	d_log_wimd_q2_alfs,
	d_log_wimd_q3_alfs,
	d_log_wimd_q4_alfs,
	d_log_wimd_q5_alfs,
	file = s_drive("d_log_by_wimd_alfs.qsm")
	)

cat("Done!\n")
beep(0)
source("r_clear_and_load.r")

# load cohort ==================================================================
cat("load cohort\n")

d_log_alfs <- qread(s_drive("d_log_alfs.qs"))
d_log_alfs_preg <- qread(s_drive("d_log_alfs_pregnant.qs"))
d_bmi_imp <- qread(s_drive("d_bmi_imp.qs"))
impute.bmi <- qread(s_drive("imputebmi_func.qs"))

d_log <- impute.bmi(d_log_alfs) %>% select(-alf_e)
d_log_preg <- impute.bmi(d_log_alfs_preg) %>% select(-alf_e)

# fit interaction models =======================================================
cat("Fit interaction models\n")

# only take wimd 1 and 5

d_log_15 <- d_log %>%
	filter(wimd2019_quintile %in% c("1st (Most deprived)", "5th (Least deprived)"))

d_log_preg_15 <- d_log_preg %>%
	filter(wimd2019_quintile %in% c("1st (Most deprived)", "5th (Least deprived)"))

cat("Main cohort\n")

m_log_interaction_c19_adj <- glm(
	formula = c19_vacc_complete_flg ~ -1 +
									wimd2019_quintile +
									age_cat +
									gndr_cd +
									ethn_cat +
									bmi_cat +
									urban_rural_class +
									hh_cat +
									num_clinical_conditions_cat +
									health_board +
									wimd2019_quintile*age_cat +
									wimd2019_quintile*gndr_cd +
									wimd2019_quintile*ethn_cat +
									wimd2019_quintile*bmi_cat +
									wimd2019_quintile*urban_rural_class +
									wimd2019_quintile*hh_cat +
									wimd2019_quintile*num_clinical_conditions_cat +
									wimd2019_quintile*health_board,
	data = d_log_15,
	family = "binomial"
)


m_log_interaction_flu_adj <- glm(
	formula = flu_vacc_complete_flg ~ -1 +
									wimd2019_quintile +
									age_cat +
									gndr_cd +
									ethn_cat +
									bmi_cat +
									urban_rural_class +
									hh_cat +
									num_clinical_conditions_cat +
									health_board +
									wimd2019_quintile*age_cat +
									wimd2019_quintile*gndr_cd +
									wimd2019_quintile*ethn_cat +
									wimd2019_quintile*bmi_cat +
									wimd2019_quintile*urban_rural_class +
									wimd2019_quintile*hh_cat +
									wimd2019_quintile*num_clinical_conditions_cat +
									wimd2019_quintile*health_board,
	data = d_log_15,
	family = "binomial"
)

cat("Pregnant cohort\n")

m_log_interaction_c19_adj_preg <- glm(
	formula = c19_vacc_complete_flg ~ -1 +
									wimd2019_quintile +
									age_cat +
									ethn_cat +
									bmi_cat +
									urban_rural_class +
									hh_cat +
									num_clinical_conditions_cat +
									health_board +
									wimd2019_quintile*age_cat +
									wimd2019_quintile*ethn_cat +
									wimd2019_quintile*bmi_cat +
									wimd2019_quintile*urban_rural_class +
									wimd2019_quintile*hh_cat +
									wimd2019_quintile*num_clinical_conditions_cat +
									wimd2019_quintile*health_board,
	data = d_log_preg_15,
	family = "binomial"
)

m_log_interaction_flu_adj_preg <- glm(
	formula = flu_vacc_complete_flg ~ -1 +
									wimd2019_quintile +
									age_cat +
									ethn_cat +
									bmi_cat +
									urban_rural_class +
									hh_cat +
									num_clinical_conditions_cat +
									health_board +
									wimd2019_quintile*age_cat +
									wimd2019_quintile*ethn_cat +
									wimd2019_quintile*bmi_cat +
									wimd2019_quintile*urban_rural_class +
									wimd2019_quintile*hh_cat +
									wimd2019_quintile*num_clinical_conditions_cat +
									wimd2019_quintile*health_board,
	data = d_log_preg_15,
	family = "binomial"
)


# save =========================================================================
cat("Saving...\n")

qsavem(
	m_log_interaction_c19_adj,
	m_log_interaction_flu_adj,
	m_log_interaction_c19_adj_preg,
	m_log_interaction_flu_adj_preg,
	file = s_drive("m_log_interaction_adj.qsm")
)

cat("Done!\n")
beep(0)
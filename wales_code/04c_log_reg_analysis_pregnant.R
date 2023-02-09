source("r_clear_and_load.r")

cat("Pregnant cohort\n")

# load cohort ==================================================================
cat("load cohort\n")

d_cohort <- qread(s_drive("d_cohort_pregnant.qs"))
d_bmi_imp <- qread(s_drive("d_bmi_imp.qs"))
impute.bmi <- qread(s_drive("imputebmi_func.qs"))

# ==========================================================================
# Variable wrangling
# ==========================================================================
cat("pre-processing...\n")


# Set reference variables
set_refs <- function(df) {
    df %>%
    mutate(
        bmi_cat   = fct_relevel(bmi_cat, "18.5-24.9"),
        hh_cat    = fct_relevel(hh_cat, "2 members"),
        age_cat = fct_relevel(age_cat, "40-49"),
        ethn_cat  = fct_relevel(ethn_cat, "White"),
        urban_rural_class = fct_relevel(urban_rural_class, "Urban"),
        wimd2019_quintile = fct_relevel(wimd2019_quintile, "5th (Least deprived)")
    )
}


d_log_alfs <-
	d_cohort %>%
		mutate(
			num_clinical_conditions_cat = case_when(num_clinical_conditions == 0 ~ "No conditions",
																					num_clinical_conditions == 1 ~ "1 condition",
																					num_clinical_conditions == 2 ~ "2 conditions",
																					num_clinical_conditions == 3 ~ "3 conditions",
																					num_clinical_conditions >	 3 ~ "4+ conditions",
																				 ) %>% factor(levels = c("No conditions", "1 condition", "2 conditions", "3 conditions", "4+ conditions")),
			hh_cat = fct_recode(hh_cat, "Alone" = "1",
											 "2 members" = "2",
											 "3 members" = "3",
											 "4 members" = "4",
											 "5 members" = "5",
											 "6-10 members" = "6-10",
											 "11+ members" = "11+"
											),
			wimd2019_quintile = case_when(wimd2019_quintile == 1 ~ "1st (Most deprived)",
																		wimd2019_quintile == 2 ~ "2nd",
																		wimd2019_quintile == 3 ~ "3rd",
																		wimd2019_quintile == 4 ~ "4th",
																		wimd2019_quintile == 5 ~ "5th (Least deprived)"
																	 ) %>% factor()
		) %>%
		select(
			alf_e,
			wimd2019_quintile,
			age_cat,
			ethn_cat,
			bmi_cat,    
			urban_rural_class,
			hh_cat,
			num_clinical_conditions_cat,
			c19_vacc_complete_flg,
			flu_vacc_complete_flg,
			health_board
		) %>% set_refs()


# ==========================================================================
# Add imputed BMI and remove alf
# ==========================================================================

d_log <- impute.bmi(d_log_alfs) %>% select(-alf_e)


d_log_c19 <- d_log %>% select(-flu_vacc_complete_flg)
d_log_flu <- d_log %>% select(-c19_vacc_complete_flg)

# Unadjusted

xvar_names <- c(
	"wimd2019_quintile",
	"age_cat",
	"ethn_cat",
	"bmi_cat",
	"urban_rural_class",
	"hh_cat",
	"num_clinical_conditions_cat",
	"health_board"
)


# ==========================================================================
# Modeling
# ==========================================================================
cat("Modeling...\n")

#Unadjusted
for (xvar in xvar_names) {
# write formulae
	assign(paste0(xvar,"_formula_c19"),
		   as.formula(paste0("c19_vacc_complete_flg ~ ",xvar))
		  )
	assign(paste0(xvar,"_formula_flu"),
		   as.formula(paste0("flu_vacc_complete_flg ~ ",xvar))
		  )	
# make models
	# covid models
	assign(paste0("m_log_unadj_q0_c19_", xvar),
		   glm(get(paste0(xvar,"_formula_c19")), family = binomial, data = d_log_c19)
		  )
	# flu models
	assign(paste0("m_log_unadj_q0_flu_", xvar),
		   glm(get(paste0(xvar,"_formula_flu")), family = binomial, data = d_log_flu)
		  )

}

# Adjusted
m_log_adj_q0_c19 <- glm(c19_vacc_complete_flg ~ ., , family = binomial, data = d_log_c19)
m_log_adj_q0_flu <- glm(flu_vacc_complete_flg ~ ., , family = binomial, data = d_log_flu)

# ==========================================================================
# Save processed cohorts and models
# ==========================================================================
cat("Saving...\n")

# save log cohort
qsave(
	d_log_alfs,
	file = s_drive("d_log_alfs_pregnant.qs")
)

qsave(xvar_names, file = s_drive("xvar_names_pregnant.qs"))

# save models
qsavem(
  m_log_unadj_q0_c19_wimd2019_quintile,
  m_log_unadj_q0_flu_wimd2019_quintile,
  m_log_unadj_q0_c19_age_cat,
  m_log_unadj_q0_flu_age_cat,  
  m_log_unadj_q0_c19_ethn_cat,
  m_log_unadj_q0_flu_ethn_cat,  
  m_log_unadj_q0_c19_bmi_cat,
  m_log_unadj_q0_flu_bmi_cat,  
  m_log_unadj_q0_c19_urban_rural_class,
  m_log_unadj_q0_flu_urban_rural_class,   
  m_log_unadj_q0_c19_hh_cat,
  m_log_unadj_q0_flu_hh_cat,
  m_log_unadj_q0_c19_num_clinical_conditions_cat,
  m_log_unadj_q0_flu_num_clinical_conditions_cat,
  m_log_unadj_q0_c19_health_board,
  m_log_unadj_q0_flu_health_board,
  m_log_adj_q0_c19,
  m_log_adj_q0_flu,
  file = s_drive("m_log_q0_pregnant.qsm")
)

cat("Done!\n")
beep(0)
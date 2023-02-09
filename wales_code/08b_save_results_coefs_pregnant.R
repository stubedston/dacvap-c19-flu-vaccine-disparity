cat("Nearly there!\n\n")

source("r_clear_and_load.r")

cat("Pregnant cohort\n")

# load data ==============================================================
cat("load data\n")

qload(file = "results/coef_pregnant.qsm")
qload(file = "results/interactions.qsm")
xvar_names <- qread(file = s_drive("xvar_names_pregnant.qs"))
xlbl_lkp <- qread(s_drive("xlbl_lkp_preg.qs"))

# ==========================================================================
# Putting everything together
# ==========================================================================
cat("Put everything together\n")

for (x in xvar_names) {
	for(j in c("adj","unadj")) {
		for (v in c("c19","flu")) {
# overall
			assign(paste0("coefs_",x,"_",v,"_",j,"_overall"),
		d_n_coef %>%
			left_join(xlbl_lkp, by = c("xvar","xlbl")) %>%
			rename(xlbl_cd = cd) %>%
			relocate(xlbl_cd, .after = xlbl) %>%
			relocate(or_low, .after = or) %>%
			relocate(or_high, .after = or_low) %>%
			select(-n,-percent,-n_c19_complete,-n_flu_complete, -perc_c19_complete, -perc_flu_complete) %>%
			mutate(
				est = round(est,3),
				or = round(or,3),
				se = round(se,4),
				or_low = round(or_low,3),
				or_high = round(or_high,3),
				stat = round(stat,2),
				p = round(p,4)
				) %>%
			filter(
				xvar == x,
				model_type == j,
				vacc == v,
				wimd_q == 0)
		)
# by wimd
		if(x != "wimd2019_quintile"){
			for (q in 1:5) {
				assign(paste0("coefs_",x,"_",v,"_",j,"_q",q),
				d_n_coef %>%
					left_join(xlbl_lkp, by = c("xvar","xlbl")) %>%
					rename(xlbl_cd = cd) %>%
					relocate(xlbl_cd, .after = xlbl) %>%
					relocate(or_low, .after = or) %>%
					relocate(or_high, .after = or_low) %>%
					select(-n,-percent,-n_c19_complete,-n_flu_complete,-perc_c19_complete,-perc_flu_complete) %>%
					mutate(
					est = round(est,3),
					or = round(or,3),
					se = round(se,4),
					or_low = round(or_low,3),
					or_high = round(or_high,3),
					stat = round(stat,2),
					p = round(p,4)
					) %>%
					filter(
						xvar == x,
						model_type == j,
						vacc == v,
						wimd_q == q)
				)
			}
		}
		}
	}
}


# ==========================================================================
# Interaction coefficiants
# ==========================================================================

interaction_coefs_preg <- 
	rbind(
	d_interaction_coef_c19_preg,
	d_interaction_coef_flu_preg
	) %>%
	filter(model_type != "ref") %>%
	left_join(xlbl_lkp, by = c("xvar","xlbl")) %>%
	rename(xlbl_cd = cd) %>%
	mutate(wimd_q = "1on5", est = log(or)) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd) %>%
	relocate(vacc, .after = wimd_q) %>%
	relocate(model_type, .after = vacc) %>%
	relocate(est, .after = model_type)	


# ==========================================================================
# rbind tables
# ==========================================================================


overall_coefs <- rbind(
	coefs_wimd2019_quintile_c19_adj_overall,
	coefs_age_cat_c19_adj_overall,
	coefs_ethn_cat_c19_adj_overall,
	coefs_bmi_cat_c19_adj_overall,
	coefs_urban_rural_class_c19_adj_overall,
	coefs_hh_cat_c19_adj_overall,
	coefs_num_clinical_conditions_cat_c19_adj_overall,
	coefs_health_board_c19_adj_overall,
	coefs_wimd2019_quintile_flu_adj_overall,
	coefs_age_cat_flu_adj_overall,
	coefs_ethn_cat_flu_adj_overall,
	coefs_bmi_cat_flu_adj_overall,
	coefs_urban_rural_class_flu_adj_overall,
	coefs_hh_cat_flu_adj_overall,
	coefs_num_clinical_conditions_cat_flu_adj_overall,
	coefs_health_board_flu_adj_overall,
	coefs_wimd2019_quintile_c19_unadj_overall,
	coefs_age_cat_c19_unadj_overall,
	coefs_ethn_cat_c19_unadj_overall,
	coefs_bmi_cat_c19_unadj_overall,
	coefs_urban_rural_class_c19_unadj_overall,
	coefs_hh_cat_c19_unadj_overall,
	coefs_num_clinical_conditions_cat_c19_unadj_overall,
	coefs_health_board_c19_unadj_overall,
	coefs_wimd2019_quintile_flu_unadj_overall,
	coefs_age_cat_flu_unadj_overall,
	coefs_ethn_cat_flu_unadj_overall,
	coefs_bmi_cat_flu_unadj_overall,
	coefs_urban_rural_class_flu_unadj_overall,
	coefs_hh_cat_flu_unadj_overall,
	coefs_num_clinical_conditions_cat_flu_unadj_overall,
	coefs_health_board_flu_unadj_overall
) %>%
filter(xlbl != "3 conditions")


q1_coefs <- rbind(
	coefs_age_cat_c19_adj_q1,
	coefs_ethn_cat_c19_adj_q1,
	coefs_bmi_cat_c19_adj_q1,
	coefs_urban_rural_class_c19_adj_q1,
	coefs_hh_cat_c19_adj_q1,
	coefs_num_clinical_conditions_cat_c19_adj_q1,
	coefs_health_board_c19_adj_q1,
	coefs_age_cat_flu_adj_q1,
	coefs_ethn_cat_flu_adj_q1,
	coefs_bmi_cat_flu_adj_q1,
	coefs_urban_rural_class_flu_adj_q1,
	coefs_hh_cat_flu_adj_q1,
	coefs_num_clinical_conditions_cat_flu_adj_q1,
	coefs_health_board_flu_adj_q1,
	coefs_age_cat_c19_unadj_q1,
	coefs_ethn_cat_c19_unadj_q1,
	coefs_bmi_cat_c19_unadj_q1,
	coefs_urban_rural_class_c19_unadj_q1,
	coefs_hh_cat_c19_unadj_q1,
	coefs_num_clinical_conditions_cat_c19_unadj_q1,
	coefs_health_board_c19_unadj_q1,
	coefs_age_cat_flu_unadj_q1,
	coefs_ethn_cat_flu_unadj_q1,
	coefs_bmi_cat_flu_unadj_q1,
	coefs_urban_rural_class_flu_unadj_q1,
	coefs_hh_cat_flu_unadj_q1,
	coefs_num_clinical_conditions_cat_flu_unadj_q1,
	coefs_health_board_flu_unadj_q1
) %>%
filter(xlbl != "3 conditions")

q2_coefs <- rbind(
	coefs_age_cat_c19_adj_q2,
	coefs_ethn_cat_c19_adj_q2,
	coefs_bmi_cat_c19_adj_q2,
	coefs_urban_rural_class_c19_adj_q2,
	coefs_hh_cat_c19_adj_q2,
	coefs_num_clinical_conditions_cat_c19_adj_q2,
	coefs_health_board_c19_adj_q2,
	coefs_age_cat_flu_adj_q2,
	coefs_ethn_cat_flu_adj_q2,
	coefs_bmi_cat_flu_adj_q2,
	coefs_urban_rural_class_flu_adj_q2,
	coefs_hh_cat_flu_adj_q2,
	coefs_num_clinical_conditions_cat_flu_adj_q2,
	coefs_health_board_flu_adj_q2,
	coefs_age_cat_c19_unadj_q2,
	coefs_ethn_cat_c19_unadj_q2,
	coefs_bmi_cat_c19_unadj_q2,
	coefs_urban_rural_class_c19_unadj_q2,
	coefs_hh_cat_c19_unadj_q2,
	coefs_num_clinical_conditions_cat_c19_unadj_q2,
	coefs_health_board_c19_unadj_q2,
	coefs_age_cat_flu_unadj_q2,
	coefs_ethn_cat_flu_unadj_q2,
	coefs_bmi_cat_flu_unadj_q2,
	coefs_urban_rural_class_flu_unadj_q2,
	coefs_hh_cat_flu_unadj_q2,
	coefs_num_clinical_conditions_cat_flu_unadj_q2,
	coefs_health_board_flu_unadj_q2
) %>%
filter(xlbl != "3 conditions")

q3_coefs <- rbind(
	coefs_age_cat_c19_adj_q3,
	coefs_ethn_cat_c19_adj_q3,
	coefs_bmi_cat_c19_adj_q3,
	coefs_urban_rural_class_c19_adj_q3,
	coefs_hh_cat_c19_adj_q3,
	coefs_num_clinical_conditions_cat_c19_adj_q3,
	coefs_health_board_c19_adj_q3,
	coefs_age_cat_flu_adj_q3,
	coefs_ethn_cat_flu_adj_q3,
	coefs_bmi_cat_flu_adj_q3,
	coefs_urban_rural_class_flu_adj_q3,
	coefs_hh_cat_flu_adj_q3,
	coefs_num_clinical_conditions_cat_flu_adj_q3,
	coefs_health_board_flu_adj_q3,
	coefs_age_cat_c19_unadj_q3,
	coefs_ethn_cat_c19_unadj_q3,
	coefs_bmi_cat_c19_unadj_q3,
	coefs_urban_rural_class_c19_unadj_q3,
	coefs_hh_cat_c19_unadj_q3,
	coefs_num_clinical_conditions_cat_c19_unadj_q3,
	coefs_health_board_c19_unadj_q3,
	coefs_age_cat_flu_unadj_q3,
	coefs_ethn_cat_flu_unadj_q3,
	coefs_bmi_cat_flu_unadj_q3,
	coefs_urban_rural_class_flu_unadj_q3,
	coefs_hh_cat_flu_unadj_q3,
	coefs_num_clinical_conditions_cat_flu_unadj_q3,
	coefs_health_board_flu_unadj_q3
) %>%
filter(xlbl != "3 conditions")

q4_coefs <- rbind(
	coefs_age_cat_c19_adj_q4,
	coefs_ethn_cat_c19_adj_q4,
	coefs_bmi_cat_c19_adj_q4,
	coefs_urban_rural_class_c19_adj_q4,
	coefs_hh_cat_c19_adj_q4,
	coefs_num_clinical_conditions_cat_c19_adj_q4,
	coefs_health_board_c19_adj_q4,
	coefs_age_cat_flu_adj_q4,
	coefs_ethn_cat_flu_adj_q4,
	coefs_bmi_cat_flu_adj_q4,
	coefs_urban_rural_class_flu_adj_q4,
	coefs_hh_cat_flu_adj_q4,
	coefs_num_clinical_conditions_cat_flu_adj_q4,
	coefs_health_board_flu_adj_q4,
	coefs_age_cat_c19_unadj_q4,
	coefs_ethn_cat_c19_unadj_q4,
	coefs_bmi_cat_c19_unadj_q4,
	coefs_urban_rural_class_c19_unadj_q4,
	coefs_hh_cat_c19_unadj_q4,
	coefs_num_clinical_conditions_cat_c19_unadj_q4,
	coefs_health_board_c19_unadj_q4,
	coefs_age_cat_flu_unadj_q4,
	coefs_ethn_cat_flu_unadj_q4,
	coefs_bmi_cat_flu_unadj_q4,
	coefs_urban_rural_class_flu_unadj_q4,
	coefs_hh_cat_flu_unadj_q4,
	coefs_num_clinical_conditions_cat_flu_unadj_q4,
	coefs_health_board_flu_unadj_q4
) %>%
filter(xlbl != "3 conditions")

q5_coefs <- rbind(
	coefs_age_cat_c19_adj_q5,
	coefs_ethn_cat_c19_adj_q5,
	coefs_bmi_cat_c19_adj_q5,
	coefs_urban_rural_class_c19_adj_q5,
	coefs_hh_cat_c19_adj_q5,
	coefs_num_clinical_conditions_cat_c19_adj_q5,
	coefs_health_board_c19_adj_q5,
	coefs_age_cat_flu_adj_q5,
	coefs_ethn_cat_flu_adj_q5,
	coefs_bmi_cat_flu_adj_q5,
	coefs_urban_rural_class_flu_adj_q5,
	coefs_hh_cat_flu_adj_q5,
	coefs_num_clinical_conditions_cat_flu_adj_q5,
	coefs_health_board_flu_adj_q5,
	coefs_age_cat_c19_unadj_q5,
	coefs_ethn_cat_c19_unadj_q5,
	coefs_bmi_cat_c19_unadj_q5,
	coefs_urban_rural_class_c19_unadj_q5,
	coefs_hh_cat_c19_unadj_q5,
	coefs_num_clinical_conditions_cat_c19_unadj_q5,
	coefs_health_board_c19_unadj_q5,
	coefs_age_cat_flu_unadj_q5,
	coefs_ethn_cat_flu_unadj_q5,
	coefs_bmi_cat_flu_unadj_q5,
	coefs_urban_rural_class_flu_unadj_q5,
	coefs_hh_cat_flu_unadj_q5,
	coefs_num_clinical_conditions_cat_flu_unadj_q5,
	coefs_health_board_flu_unadj_q5
) %>%
filter(xlbl != "3 conditions")

# ==========================================================================
# Save as .csv
# ==========================================================================
cat("Write .csvs\n")

write.csv(overall_coefs, "results/pregnant/overall_coefs_preg.csv", row.names = FALSE)
write.csv(q1_coefs, "results/pregnant/by_wimd/q1_coefs_preg.csv", row.names = FALSE)
write.csv(q2_coefs, "results/pregnant/by_wimd/q2_coefs_preg.csv", row.names = FALSE)
write.csv(q3_coefs, "results/pregnant/by_wimd/q3_coefs_preg.csv", row.names = FALSE)
write.csv(q4_coefs, "results/pregnant/by_wimd/q4_coefs_preg.csv", row.names = FALSE)
write.csv(q5_coefs, "results/pregnant/by_wimd/q5_coefs_preg.csv", row.names = FALSE)
write.csv(interaction_coefs_preg, "results/pregnant/interaction_coefs_preg.csv", row.names = FALSE)

cat("Saving...\n")

qsavem(
	coefs_wimd2019_quintile_c19_adj_overall,
	coefs_age_cat_c19_adj_overall,
	coefs_ethn_cat_c19_adj_overall,
	coefs_bmi_cat_c19_adj_overall,
	coefs_urban_rural_class_c19_adj_overall,
	coefs_hh_cat_c19_adj_overall,
	coefs_num_clinical_conditions_cat_c19_adj_overall,
	coefs_health_board_c19_adj_overall,
	coefs_wimd2019_quintile_flu_adj_overall,
	coefs_age_cat_flu_adj_overall,
	coefs_ethn_cat_flu_adj_overall,
	coefs_bmi_cat_flu_adj_overall,
	coefs_urban_rural_class_flu_adj_overall,
	coefs_hh_cat_flu_adj_overall,
	coefs_num_clinical_conditions_cat_flu_adj_overall,
	coefs_health_board_flu_adj_overall,
	coefs_wimd2019_quintile_c19_unadj_overall,
	coefs_age_cat_c19_unadj_overall,
	coefs_ethn_cat_c19_unadj_overall,
	coefs_bmi_cat_c19_unadj_overall,
	coefs_urban_rural_class_c19_unadj_overall,
	coefs_hh_cat_c19_unadj_overall,
	coefs_num_clinical_conditions_cat_c19_unadj_overall,
	coefs_health_board_c19_unadj_overall,
	coefs_wimd2019_quintile_flu_unadj_overall,
	coefs_age_cat_flu_unadj_overall,
	coefs_ethn_cat_flu_unadj_overall,
	coefs_bmi_cat_flu_unadj_overall,
	coefs_urban_rural_class_flu_unadj_overall,
	coefs_hh_cat_flu_unadj_overall,
	coefs_num_clinical_conditions_cat_flu_unadj_overall,
	coefs_health_board_flu_unadj_overall,
	file = s_drive("coefs_final")
)
cat("Done!\n")
beep(0)
source("r_clear_and_load.r")

cat("Pregnant cohort imputed\n")

# load data ==============================================================
cat("load data\n")

d_log_alfs <- qread(s_drive("d_log_alfs_pregnant.qs"))
qload(s_drive("d_log_by_wimd_alfs_preg.qsm"))
qload(s_drive("d_describe_pregnant.qsm"))
d_describe <- d_describe_imputed_preg[,1:9]
xlbl_lkp <- qread(s_drive("xlbl_lkp_preg.qs"))
impute.bmi <- qread(s_drive("imputebmi_func.qs"))
d_bmi_imp <- qread(s_drive("d_bmi_imp.qs"))


d_log_alfs <- impute.bmi(d_log_alfs)

columns <- c("xvar","xlbl","xlbl_cd","wimd_q","n","percent","n_c19_complete","perc_c19_complete","n_flu_complete","perc_flu_complete")
total_descriptive <- data.frame(matrix(nrow = 6, ncol = length(columns)))
colnames(total_descriptive) <- columns

# ==========================================================================
# Putting everything together
# ==========================================================================
cat("Put everything together\n")


total_descriptive_overall <-
total_descriptive %>%
	mutate(
		wimd_q = 0:5,
		n = case_when(wimd_q == 0 ~ n_distinct(d_log_alfs$alf_e),
					  wimd_q == 1 ~ n_distinct(d_log_wimd_q1_alfs$alf_e),
					  wimd_q == 2 ~ n_distinct(d_log_wimd_q2_alfs$alf_e),
					  wimd_q == 3 ~ n_distinct(d_log_wimd_q3_alfs$alf_e),
					  wimd_q == 4 ~ n_distinct(d_log_wimd_q4_alfs$alf_e),
					  wimd_q == 5 ~ n_distinct(d_log_wimd_q5_alfs$alf_e)
					),
		percent = n/n_distinct(d_log_alfs$alf_e)*100,
		n_c19_complete = case_when(wimd_q == 0 ~ sum(d_log_alfs$c19_vacc_complete_flg),
					  wimd_q == 1 ~ sum(d_log_wimd_q1_alfs$c19_vacc_complete_flg),
					  wimd_q == 2 ~ sum(d_log_wimd_q2_alfs$c19_vacc_complete_flg),
					  wimd_q == 3 ~ sum(d_log_wimd_q3_alfs$c19_vacc_complete_flg),
					  wimd_q == 4 ~ sum(d_log_wimd_q4_alfs$c19_vacc_complete_flg),
					  wimd_q == 5 ~ sum(d_log_wimd_q5_alfs$c19_vacc_complete_flg)
					),
		perc_c19_complete = n_c19_complete/n*100,
		n_flu_complete = case_when(wimd_q == 0 ~ sum(d_log_alfs$flu_vacc_complete_flg),
					  wimd_q == 1 ~ sum(d_log_wimd_q1_alfs$flu_vacc_complete_flg),
					  wimd_q == 2 ~ sum(d_log_wimd_q2_alfs$flu_vacc_complete_flg),
					  wimd_q == 3 ~ sum(d_log_wimd_q3_alfs$flu_vacc_complete_flg),
					  wimd_q == 4 ~ sum(d_log_wimd_q4_alfs$flu_vacc_complete_flg),
					  wimd_q == 5 ~ sum(d_log_wimd_q5_alfs$flu_vacc_complete_flg)
					),
		perc_flu_complete = n_flu_complete/n*100
	)


wimd_descriptive_overall <-
d_describe %>%
	distinct() %>%
	filter(xvar == "wimd2019_quintile") %>%
	mutate(
		xlbl = fct_relevel(xlbl, "1st (Most deprived)", "2nd", "3rd", "4th", "5th (Least deprived)"),
		xlbl_cd = case_when(xlbl == "1st (Most deprived)"  ~ 1,
							xlbl == "2nd" 				   ~ 2,
							xlbl == "3rd" 				   ~ 3,
							xlbl == "4th" 				   ~ 4,
							xlbl == "5th (Least deprived)" ~ 5,
							)
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)

age_descriptive_overall <-
d_describe %>%
	distinct() %>%
	filter(
		xvar == "age_cat",
		wimd_q == 0
		) %>%
	mutate(
		xlbl = fct_relevel(xlbl, "<18", "18-24", "25-29", "30-34", "35-39", "40-49"),
		xlbl_cd = xlbl
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)


for (i in 1:5) {
	assign(paste0("age_descriptive_q", i),
	d_describe %>%
	distinct() %>%
	filter(
		xvar == "age_cat",
		wimd_q == i
		) %>%
	mutate(
		xlbl = fct_relevel(xlbl, "<18", "18-24", "25-29", "30-34", "35-39", "40-49"),
		xlbl_cd = xlbl
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)
	)
}

ethnicity_descriptive_overall <-
d_describe %>%
	distinct() %>%
	filter(
		xvar == "ethn_cat",
		wimd_q == 0
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "White" ~ "w",
							xlbl == "Asian" ~ "a",
							xlbl == "Black" ~ "b",
							xlbl == "Other" ~ "o",
							xlbl == "Mixed" ~ "m")
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)

for (i in 1:5) {
	assign(paste0("ethnicity_descriptive_q", i),
	d_describe %>%
	distinct() %>%
	filter(
		xvar == "ethn_cat",
		wimd_q == i
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "White" ~ "w",
							xlbl == "Asian" ~ "a",
							xlbl == "Black" ~ "b",
							xlbl == "Other" ~ "o",
							xlbl == "Mixed" ~ "m")
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)
	)
}

bmi_descriptive_overall <-
d_describe %>%
	distinct() %>%
	filter(
		xvar == "bmi_cat",
		wimd_q == 0
		) %>%
	mutate(
		xlbl_cd = xlbl
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)

for (i in 1:5) {
	assign(paste0("bmi_descriptive_q", i),
	d_describe %>%
	distinct() %>%
	filter(
		xvar == "bmi_cat",
		wimd_q == i
		) %>%
	mutate(
		xlbl_cd = xlbl
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)
	)
}

household_descriptive_overall <-
d_describe %>%
	distinct() %>%
	filter(
		xvar == "hh_cat",
		wimd_q == 0
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "Alone" ~ "1",
							xlbl == "2 members" ~ "2",
							xlbl == "3 members" ~ "3",
							xlbl == "4 members" ~ "4",
							xlbl == "5 members" ~ "5",
							xlbl == "6-10 members" ~ "6-10",
							xlbl == "11+ members" ~ "11+")
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)

for (i in 1:5) {
	assign(paste0("household_descriptive_q", i),
	d_describe %>%
	distinct() %>%
	filter(
		xvar == "hh_cat",
		wimd_q == i
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "Alone" ~ "1",
							xlbl == "2 members" ~ "2",
							xlbl == "3 members" ~ "3",
							xlbl == "4 members" ~ "4",
							xlbl == "5 members" ~ "5",
							xlbl == "6-10 members" ~ "6-10",
							xlbl == "11+ members" ~ "11+")
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)
	)
}


urban_rural_descriptive_overall <-
d_describe %>%
	distinct() %>%
	filter(
		xvar == "urban_rural_class",
		wimd_q == 0
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "Urban" ~ "u",
							xlbl == "Rural" ~ "r")		
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)

for (i in 1:5) {
	assign(paste0("urban_rural_descriptive_q", i),
	d_describe %>%
	distinct() %>%
	filter(
		xvar == "urban_rural_class",
		wimd_q == i
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "Urban" ~ "u",
							xlbl == "Rural" ~ "r")		
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)
	)
}


clinical_conditions_descriptive_overall <-
d_describe %>%
	distinct() %>%
	filter(
		xvar == "num_clinical_conditions_cat",
		wimd_q == 0
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "No conditions" ~ "0",
							xlbl == "1 condition" ~ "1",
							xlbl == "2 conditions" ~ "2",
							xlbl == "3 conditions" ~ "3",
							xlbl == "4+ conditions" ~ "4+")		
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)

for (i in 1:5) {
	assign(paste0("clinical_conditions_descriptive_q", i),
	d_describe %>%
	distinct() %>%
	filter(
		xvar == "num_clinical_conditions_cat",
		wimd_q == i
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "No conditions" ~ "0",
							xlbl == "1 condition" ~ "1",
							xlbl == "2 conditions" ~ "2",
							xlbl == "3 conditions" ~ "3",
							xlbl == "4+ conditions" ~ "4+")		
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)
	)
}

health_board_descriptive_overall <-
d_describe %>%
	distinct() %>%
	filter(
		xvar == "health_board",
		wimd_q == 0
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "Aneurin Bevan University Health Board" ~ "ab",
							xlbl == "Betsi Cadwaladr University Health Board" ~ "bc",
							xlbl == "Cardiff and Vale University Health Board" ~ "cv",
							xlbl == "Cwm Taf Morgannwg University Health Board" ~ "ct",
							xlbl == "Hywel Dda University Health Board" ~ "hd",
							xlbl == "Powys Teaching Health Board" ~ "pt",
							xlbl == "Swansea Bay University Health Board" ~ "sb")	
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)

for (i in 1:5) {
	assign(paste0("health_board_descriptive_q", i),
	d_describe %>%
	distinct() %>%
	filter(
		xvar == "health_board",
		wimd_q == i
		) %>%
	mutate(
		xlbl_cd = case_when(xlbl == "Aneurin Bevan University Health Board" ~ "ab",
							xlbl == "Betsi Cadwaladr University Health Board" ~ "bc",
							xlbl == "Cardiff and Vale University Health Board" ~ "cv",
							xlbl == "Cwm Taf Morgannwg University Health Board" ~ "ct",
							xlbl == "Hywel Dda University Health Board" ~ "hd",
							xlbl == "Powys Teaching Health Board" ~ "pt",
							xlbl == "Swansea Bay University Health Board" ~ "sb")		
	) %>%
	relocate(xlbl_cd, .after = xlbl) %>%
	relocate(wimd_q, .after = xlbl_cd)
	)
}

# ==========================================================================
# suppress small values
# ==========================================================================
cat("Suppress small values\n")

suppress <- function(data) {
data %>%
mutate(
	percent = case_when(n<=10 & n !=0 ~ NA_real_,
				n>=10 ~ percent),
	perc_c19_complete = case_when(n<=10 & n !=0 ~ NA_real_,
				n>=10 ~ perc_c19_complete),
	perc_flu_complete = case_when(n<=10 & n !=0 ~ NA_real_,
				n>=10 ~ perc_flu_complete),
	n = case_when(n<=10 & n !=0 ~ 10,
				n>=10 ~ round(n, -1)),
	n_c19_complete = case_when(n_c19_complete<=10 & n_c19_complete != 0 ~ 10,
				n_c19_complete>10 ~ round(n_c19_complete, -1)),
	n_flu_complete = case_when(n_flu_complete<=10 & n_flu_complete != 0 ~ 10,
				n_flu_complete>10 ~ round(n_flu_complete, -1)),
	percent = round(percent, 1),
	perc_c19_complete = round(perc_c19_complete, 1),
	perc_flu_complete = round(perc_flu_complete, 1)
)
}

descriptive_tables <- c(
	"age_descriptive",
	"ethnicity_descriptive",
	"bmi_descriptive",
	"household_descriptive",
	"urban_rural_descriptive",
	"clinical_conditions_descriptive",
	"health_board_descriptive")

total_descriptive_overall <- suppress(total_descriptive_overall)
wimd_descriptive_overall  <- suppress(wimd_descriptive_overall)


for (i in descriptive_tables) {
#overall
	assign(paste0(i, "_overall"),suppress(get(paste0(i, "_overall"))))
#wimd1-5
	for (j in 1:5) {
	assign((paste0(i, "_q",j)),suppress(get(paste0(i, "_q",j))))
	}
}

# ==========================================================================
# rbind all tables
# ==========================================================================

# overall 
total_descriptive_overall$xvar <- "total"

overall_descriptive_preg <- rbind(
	total_descriptive_overall,
	wimd_descriptive_overall,
	age_descriptive_overall,
	ethnicity_descriptive_overall,
	bmi_descriptive_overall,
	household_descriptive_overall,
	urban_rural_descriptive_overall,
	clinical_conditions_descriptive_overall,
	health_board_descriptive_overall
)

## splitting by wimd quintile 
#q1
q1_descriptive_preg <- rbind(
	total_descriptive_overall[2,],
	age_descriptive_q1,
	ethnicity_descriptive_q1,
	bmi_descriptive_q1,
	household_descriptive_q1,
	urban_rural_descriptive_q1,
	clinical_conditions_descriptive_q1,
	health_board_descriptive_q1
) %>% filter(xlbl != "3 conditions" | is.na(xlbl))
#q2
q2_descriptive_preg <- rbind(
	total_descriptive_overall[3,],
	age_descriptive_q2,
	ethnicity_descriptive_q2,
	bmi_descriptive_q2,
	household_descriptive_q2,
	urban_rural_descriptive_q2,
	clinical_conditions_descriptive_q2,
	health_board_descriptive_q2
) %>% filter(xlbl != "3 conditions")
#q3
q3_descriptive_preg <- rbind(
	total_descriptive_overall[4,],
	age_descriptive_q3,
	ethnicity_descriptive_q3,
	bmi_descriptive_q3,
	household_descriptive_q3,
	urban_rural_descriptive_q3,
	clinical_conditions_descriptive_q3,
	health_board_descriptive_q3
) %>% filter(xlbl != "3 conditions" | is.na(xlbl))
#q4
q4_descriptive_preg <- rbind(
	total_descriptive_overall[5,],
	age_descriptive_q4,
	ethnicity_descriptive_q4,
	bmi_descriptive_q4,
	household_descriptive_q4,
	urban_rural_descriptive_q4,
	clinical_conditions_descriptive_q4,
	health_board_descriptive_q4
) %>% filter(xlbl != "3 conditions" | is.na(xlbl))
#q5
q5_descriptive_preg <- rbind(
	total_descriptive_overall[6,],
	age_descriptive_q5,
	ethnicity_descriptive_q5,
	bmi_descriptive_q5,
	household_descriptive_q5,
	urban_rural_descriptive_q5,
	clinical_conditions_descriptive_q5,
	health_board_descriptive_q5
) %>% filter(xlbl != "3 conditions" | is.na(xlbl))

# ==========================================================================
# Save as .csv
# ==========================================================================
cat("Write .csvs\n")

write.csv(overall_descriptive_preg, "results/pregnant/overall_descriptive_preg_imputed.csv", row.names = FALSE)
write.csv(q1_descriptive_preg, "results/pregnant/by_wimd/imputed_q1_descriptive_preg.csv", row.names = FALSE)
write.csv(q2_descriptive_preg, "results/pregnant/by_wimd/imputed_q2_descriptive_preg.csv", row.names = FALSE)
write.csv(q3_descriptive_preg, "results/pregnant/by_wimd/imputed_q3_descriptive_preg.csv", row.names = FALSE)
write.csv(q4_descriptive_preg, "results/pregnant/by_wimd/imputed_q4_descriptive_preg.csv", row.names = FALSE)
write.csv(q5_descriptive_preg, "results/pregnant/by_wimd/imputed_q5_descriptive_preg.csv", row.names = FALSE)


# save =========================================================================
cat("Saving...\n")

qsavem(
	total_descriptive_overall,
	wimd_descriptive_overall,
	age_descriptive_overall,
	ethnicity_descriptive_overall,
	bmi_descriptive_overall,
	household_descriptive_overall,
	urban_rural_descriptive_overall,
	clinical_conditions_descriptive_overall,
	health_board_descriptive_overall,
    file = s_drive("descriptive_tables_final_pregnant.qsm")
)


cat("Done!\n")
beep(0)



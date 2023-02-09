source("r_clear_and_load.r")

cat("Pregnant cohort\n")

# load data ==============================================================
cat("load data\n")

d_log_alfs <- qread(s_drive("d_log_alfs_pregnant.qs"))
qload(s_drive("d_describe_pregnant.qsm"))
d_describe <- d_describe_preg[,1:9]
d_describe_imputed <- d_describe_imputed_preg


xlbl_lkp <-
	tribble(
		~xvar, ~xlbl, ~cd,
		"wimd2019_quintile","1st (Most deprived)","1",
		"wimd2019_quintile","2nd","2",
		"wimd2019_quintile","3rd","3",
		"wimd2019_quintile","4th","4",
		"wimd2019_quintile","5th (Least deprived)","5",
		"age_cat","<18","<18",
		"age_cat","18-24","18-24",
		"age_cat","25-29","25-29",
		"age_cat","30-34","30-34",
		"age_cat","35-39","35-39",
		"age_cat","40-49","40-49",
		"ethn_cat","White","w",
		"ethn_cat","Asian","a",
		"ethn_cat","Black","b",
		"ethn_cat","Other","o",
		"ethn_cat","Mixed","m",
		"ethn_cat","(Missing)","missing",
		"bmi_cat","<18.5","<18.5",
		"bmi_cat","18.5-24.9","18.5-24.9",
		"bmi_cat","25-29.9","25-29.9",
		"bmi_cat","30-39.9","30-39.9",
		"bmi_cat","40+","40+",
		"hh_cat","Alone","1",
		"hh_cat","2 members","2",
		"hh_cat","3 members","3",
		"hh_cat","4 members","4",
		"hh_cat","5 members","5",
		"hh_cat","6-10 members","6-10",
		"hh_cat","11+ members","11+",
		"urban_rural_class","Urban","u",
		"urban_rural_class","Rural","r",
		"num_clinical_conditions_cat","No conditions","0",
		"num_clinical_conditions_cat","1 condition","1",
		"num_clinical_conditions_cat","2 conditions","2",
		"num_clinical_conditions_cat","3 conditions","3",
		"num_clinical_conditions_cat","4+ conditions","4+",
		"health_board","Aneurin Bevan University Health Board","ab",
		"health_board","Betsi Cadwaladr University Health Board","bc",
		"health_board","Cardiff and Vale University Health Board","cv",
		"health_board","Cwm Taf Morgannwg University Health Board","ct",
		"health_board","Hywel Dda University Health Board","hd",
		"health_board","Powys Teaching Health Board","pt",
		"health_board","Swansea Bay University Health Board","sb")



columns <- c("xvar","xlbl","xlbl_cd","wimd_q","n","percent","n_c19_complete","perc_c19_complete","n_flu_complete","perc_flu_complete")
total_descriptive <- data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(total_descriptive) <- columns

# ==========================================================================
# Putting everything together
# ==========================================================================
cat("Put everything together\n")

total_descriptive_overall <-
total_descriptive %>%
	mutate(
		wimd_q = 0,
		n = n_distinct(d_log_alfs$alf_e),
		percent = n/n_distinct(d_log_alfs$alf_e)*100,
		n_c19_complete = sum(d_log_alfs$c19_vacc_complete_flg),
		perc_c19_complete = n_c19_complete/n*100,
		n_flu_complete = sum(d_log_alfs$flu_vacc_complete_flg),
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
	"total_descriptive",
	"wimd_descriptive",
	"age_descriptive",
	"ethnicity_descriptive",
	"bmi_descriptive",
	"household_descriptive",
	"urban_rural_descriptive",
	"clinical_conditions_descriptive",
	"health_board_descriptive")


for (i in descriptive_tables) {
#overall
	assign(paste0(i, "_overall"),suppress(get(paste0(i, "_overall"))))
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

# ==========================================================================
# Save as .csv
# ==========================================================================
cat("Write .csvs\n")

write.csv(overall_descriptive_preg, "results/pregnant/overall_descriptive_preg.csv", row.names = FALSE)



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


qsave(xlbl_lkp, file = s_drive("xlbl_lkp_preg.qs"))


cat("Done!\n")
beep(0)

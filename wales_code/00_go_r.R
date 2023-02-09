
# Load Data from DB2 ===========================================================

#source("01e_get_cohort.R")

# Select cohort ================================================================

source("02a_select_overall.r")
source("02b_select_eligible.r")

# Exploratory analysis =========================================================

source("03a_exploratory_analysis.R")
source("03b_exploratory_analysis_pregnant.R")

# Log reg ======================================================================

#source("04a_impute_bmi.R")
source("04b_log_reg_analysis.R")
source("04c_log_reg_analysis_pregnant.R")
source("04d_log_reg_by_wimd.R")
source("04e_log_reg_by_wimd_pregnant.R")
source("04f_interaction_models.R")


# Descriptive tables ===========================================================

source("05a_descriptive_table.R")
source("05b_descriptive_table_pregnant.R")

# Analyse models ===============================================================

source("06a_log_reg_plots.R")
source("06b_log_reg_plots_pregnant.R")
source("06c_interaction_plot.R")


# Save results =================================================================

source("07a_save_results_descriptive_tables.R")
source("07b_save_results_descriptive_tables_imputed.R")
source("07c_save_results_descriptive_tables_pregnant.R")
source("07d_save_results_descriptive_tables_pregnant_imputed.R")

source("08a_save_results_coefs.R")
source("08b_save_results_coefs_pregnant.R")

# Report =======================================================================


render(
    input = "99_report.rmd",
    output_format = html_document(toc = TRUE),
    quiet = TRUE
)

beep(0)
cat("Completely done!")
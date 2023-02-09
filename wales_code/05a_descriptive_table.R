source("r_clear_and_load.r")

cat("Main cohort\n")

# load samples =================================================================
cat("load cohort\n")

d_log_alfs <- qread(s_drive("d_log_alfs.qs"))
xvar_names <- qread(s_drive("xvar_names.qs"))
qload(s_drive("d_log_by_wimd_alfs.qsm"))
impute.bmi <- qread(s_drive("imputebmi_func.qs"))
d_bmi_imp <- qread(s_drive("d_bmi_imp.qs"))

# ==========================================================================
# Do Stats
# ==========================================================================
cat("Doing stats stuff...\n")

my_stats <- function(data, variable) {
    data %>%
    group_by_at(vars(one_of(variable))) %>%
    summarise(
        n = n_distinct(alf_e),
        n_c19_complete = sum(c19_vacc_complete_flg),
        n_flu_complete = sum(flu_vacc_complete_flg)
    ) %>%
    mutate(
        perc_c19_complete = n_c19_complete / n * 100,
        perc_flu_complete = n_flu_complete / n * 100
    ) %>%
    ungroup() %>%
    mutate(
        xvar = variable,
        percent = n / sum(n) * 100
    ) %>%
    select(
        xvar,
        xlbl = one_of(variable),
        n,
        percent,
        n_c19_complete,
        perc_c19_complete,
        n_flu_complete,
        perc_flu_complete
    )
}

d_overall_describe <-
    lapply(xvar_names, my_stats, data = d_log_alfs) %>%
    bind_rows() %>% mutate(wimd_q = 0)

d_q1_describe <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = d_log_wimd_q1_alfs) %>%
    bind_rows() %>% mutate(wimd_q = 1)

d_q2_describe <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = d_log_wimd_q2_alfs) %>%
    bind_rows() %>% mutate(wimd_q = 2)

d_q3_describe <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = d_log_wimd_q3_alfs) %>%
    bind_rows() %>% mutate(wimd_q = 3)

d_q4_describe <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = d_log_wimd_q4_alfs) %>%
    bind_rows() %>% mutate(wimd_q = 4)

d_q5_describe <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = d_log_wimd_q5_alfs) %>%
    bind_rows() %>% mutate(wimd_q = 5)

d_describe <-
rbind(
    d_overall_describe,
    d_q1_describe,
    d_q2_describe,
    d_q3_describe,
    d_q4_describe,
    d_q5_describe
)

d_describe <- 
    rbind(
    d_describe %>% mutate(vacc = "c19"),
    d_describe %>% mutate(vacc = "flu")
    )


# ==========================================================================
# impute BMI
# ==========================================================================

d_overall_describe_imputed <-
    lapply(xvar_names, my_stats, data = impute.bmi(d_log_alfs)) %>%
    bind_rows() %>% mutate(wimd_q = 0)

d_q1_describe_imputed <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = impute.bmi(d_log_wimd_q1_alfs)) %>%
    bind_rows() %>% mutate(wimd_q = 1)

d_q2_describe_imputed <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = impute.bmi(d_log_wimd_q2_alfs)) %>%
    bind_rows() %>% mutate(wimd_q = 2)

d_q3_describe_imputed <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = impute.bmi(d_log_wimd_q3_alfs)) %>%
    bind_rows() %>% mutate(wimd_q = 3)

d_q4_describe_imputed <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = impute.bmi(d_log_wimd_q4_alfs)) %>%
    bind_rows() %>% mutate(wimd_q = 4)

d_q5_describe_imputed <-
    lapply(xvar_names[2:length(xvar_names)], my_stats, data = impute.bmi(d_log_wimd_q5_alfs)) %>%
    bind_rows() %>% mutate(wimd_q = 5)

d_describe_imputed <-
rbind(
    d_overall_describe_imputed,
    d_q1_describe_imputed,
    d_q2_describe_imputed,
    d_q3_describe_imputed,
    d_q4_describe_imputed,
    d_q5_describe_imputed
)

d_describe_imputed <- 
    rbind(
    d_describe_imputed %>% mutate(vacc = "c19"),
    d_describe_imputed %>% mutate(vacc = "flu")
    )

# ==========================================================================
# Saving
# ==========================================================================
cat("Saving...\n")

qsavem(
    d_describe,
    d_describe_imputed,
    file = s_drive("d_describe.qsm")
)
cat("Done!\n")
beep(0)
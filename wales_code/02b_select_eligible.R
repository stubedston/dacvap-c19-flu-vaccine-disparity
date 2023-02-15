source("r_clear_and_load.r")


# load cohort ==================================================================
cat("load cohort\n")

d_cohort <- qread(s_drive("d_cohort_included_overall.qs"))

# Add eligibility criteria flags ===============================================
cat("Add eligibility criteria flags\n")

d_cohort <-
  d_cohort %>%
  mutate(
    is_over50  = over50_flg           == 1 & !is.na(over50_flg),
    is_over65  = over65_flg           == 1 & !is.na(over65_flg),
    resp_d     = (qc_b2_leukolaba     == 1 |
                 qc_b_asthma          == 1 |
                 qc_b_copd            == 1 |
                 qc_b_pulmrare        == 1
                 ) &
                 (!is.na(qc_b2_leukolaba)  |
                 !is.na(qc_b_asthma)       |
                 !is.na(qc_b_copd)         |
                 !is.na(qc_b_pulmrare)
                 ),
    heart_d    = (qc_b_af             == 1 |
                 qc_b_ccf             == 1 |
                 qc_b_congenheart     == 1 |
                 qc_b_pulmhyper       == 1
                 ) &
                 (!is.na(qc_b_af)          |
                  !is.na(qc_b_ccf)         |
                  !is.na(qc_b_congenheart) |
                  !is.na(qc_b_pulmhyper)
                 ),
    renal_d    = ckd_flg              == 1 & !is.na(ckd_flg),
    liver_d    = qc_b_cirrhosis       == 1 & !is.na(qc_b_cirrhosis),
    neuro_d    = qc_b_stroke          == 1 & !is.na(qc_b_stroke),
    diab       = diabetes_flg         == 1 & !is.na(diabetes_flg),
    imm_sup    = (qc_b2_82            == 1 |
                 qc_p_solidtransplant == 1 |
                 qc_p_marrow6         == 1 |
                 qc_p_radio6          == 1 |
                 chemo_flg            == 1
                 ) &
                 (!is.na(qc_b2_82)         |
                  !is.na(qc_p_solidtransplant)|
                  !is.na(qc_p_marrow6)     |
                  !is.na(qc_p_radio6)      |
                  !is.na(chemo_flg)
                 ),
    is_morbidly_obese = morbid_obese_flg == 1 & !is.na(morbid_obese_flg),
    #pregnancy flags
    pregnant_flg = replace_na(pregnant_flg, 0),
    is_pregnant  = pregnant_flg == 1,
    c19_date_eligible = case_when(
      resp_d | heart_d | renal_d | liver_d | neuro_d | diab | imm_sup | is_morbidly_obese ~ ymd("2020-12-08"),
      age >= 50 ~ ymd("2021-03-17"),
      age >= 45 ~ ymd("2021-04-13"),
      age >= 40 ~ ymd("2021-04-30"),
      age >= 30 ~ ymd("2021-05-26"),
      age >= 18 ~ ymd("2021-06-18")
      ),
    flu1_eligible = concep_dt + days(30) <= ymd("2021-03-31") & birth_dt - days(30) >= ymd("2020-09-01"),
    flu2_eligible = concep_dt + days(30) <= ymd("2022-03-31") & birth_dt - days(30) >= ymd("2021-09-01"),
    c19_eligible  =
      # unvaccinated at beginning of pregnancy and become eligible for vaccination before the end of pregnancy
      ((c19_vacc_dose1_date >= concep_dt + days(30) | is.na(c19_vacc_dose1_date)) & c19_date_eligible < birth_dt - days(30)) |
      # they've had their 1st dose before the beginning of pregnancy and they become eligible for their 2nd dose (8wks after 1st) and they haven't had their 2nd by the start of pregnancy
      ((c19_vacc_dose2_date >= concep_dt + days(30) | is.na(c19_vacc_dose2_date)) & c19_vacc_dose1_date + weeks(8) < birth_dt - days(30)) |
      # they've had their 2nd dose before the start of pregnancy and become eligible for their 3rd by the end of pregnancy
      c19_vacc_dose2_date + weeks(8) < birth_dt - days(30),
    pregnant_eligible = (flu1_eligible | flu2_eligible) & c19_eligible,
    num_clinical_conditions = int(resp_d) +
                              int(heart_d) +
                              int(renal_d) +
                              int(liver_d) +
                              int(neuro_d) +
                              int(diab) +
                              int(imm_sup)
  )

# Create eligibility criteria table ==============================================
cat("Create eligibility criteria table\n")


t_eligible_raw <- tribble(
  ~criteria, ~n,
  "**Cohort**",
    d_cohort %>% nrow(),
  "Over 50",
    d_cohort %>% filter(is_over50) %>% nrow(),
  "Under 50",
    d_cohort %>% filter(!is_over50) %>% nrow(),
  "Under 50 and has a chronic respiritory disease",
    d_cohort %>% filter(!is_over50, resp_d) %>% nrow(),
  "Under 50 and has a chronic heart disease",
    d_cohort %>% filter(!is_over50, heart_d) %>% nrow(),
  "Under 50 and has a chronic renal disease",
    d_cohort %>% filter(!is_over50, renal_d) %>% nrow(),
  "Under 50 and has a chronic liver disease",
    d_cohort %>% filter(!is_over50, liver_d) %>% nrow(),
  "Under 50 and has a chronic neurological disease",
    d_cohort %>% filter(!is_over50, neuro_d) %>% nrow(),
  "Under 50 and has type 1 or 2 diabetes",
    d_cohort %>% filter(!is_over50, diab) %>% nrow(),
  "Under 50 and has immunosuppression",
    d_cohort %>% filter(!is_over50, imm_sup) %>% nrow(),
  "Under 50 and is morbidly obese",
    d_cohort %>% filter(!is_over65, is_morbidly_obese) %>% nrow(),
  "Under 50 and is pregnant and clinically vulnerable",
    d_cohort %>% filter(!is_over50, is_pregnant, (resp_d|heart_d|renal_d|liver_d|neuro_d|diab|imm_sup|is_morbidly_obese)) %>% nrow(),
  "Under 50 and is pregnant during study window",
    d_cohort %>% filter(!is_over50, is_pregnant) %>% nrow(),
  "Under 50 and is pregnant and eligible for either flu vaccine",
    d_cohort %>% filter(!is_over50, (flu1_eligible|flu2_eligible)) %>% nrow(), 
  "Under 50 and is pregnant and eligible for the 2020/21 flu vaccine",
    d_cohort %>% filter(!is_over50, flu1_eligible) %>% nrow(), 
  "Under 50 and is pregnant and eligible for the 2021/22 flu vaccine",
    d_cohort %>% filter(!is_over50, flu2_eligible) %>% nrow(), 
  "Under 50 and is pregnant and eligible for any COVID-19 vaccine",
    d_cohort %>% filter(!is_over50, c19_eligible) %>% nrow(),
  "**Under 50 and is pregnant and eligible for both vaccines**",
    d_cohort %>% filter(!is_over50, pregnant_eligible) %>% nrow(),
  "Under 50 and is clinically vulnerable and isn't pregnant",
    d_cohort %>% filter(!is_over50, !is_pregnant, (resp_d|heart_d|renal_d|liver_d|neuro_d|diab|imm_sup|is_morbidly_obese))
    %>% nrow(),
  "**Over 50 or clinically vulnerable (and not pregnant)**",
    d_cohort %>% filter(is_over50|resp_d|heart_d|renal_d|liver_d|neuro_d|diab|imm_sup|is_morbidly_obese, !is_pregnant)
    %>% nrow()
  ) %>%
  mutate(
    p = n / nth(n, 1),
    p = if_else(row_number() < 1, NA_real_, p),
    p = percent(p)
  )


t_eligible <- t_eligible_raw %>%
  mutate(
    n = round(n, -1),
    p = n / nth(n, 1),
    p = if_else(row_number() < 1, NA_real_, p),
    p = percent(p, accuracy = 0.1)
  )

t_eligible_pretty <- t_eligible %>%
  kable(
    format.args = list(big.mark = ","),
    align = c("l", "r", "r")
  ) %>%
  kable_pretty()

# ==========================================================================
# Create cohorts
# ==========================================================================

cat("Create cohorts\n")

#
# Main cohort
#

d_cohort_eligible <- d_cohort %>%
  filter(is_over50 |
         resp_d |
         heart_d |
         renal_d |
         liver_d |
         neuro_d |
         diab |
         imm_sup |
         is_morbidly_obese,
         !is_pregnant
       )

#
# Pregnant cohort ==============================================================
#

d_cohort_pregnant <- d_cohort %>%
  filter(
    !is_over50,
    # Everything was run on one of the following two criteria for the covid cohort and flu cohort respectively
    # (pregnant_eligible is for both for the venn diagram)
    #c19_eligible,
    #(flu1_eligible|flu2_eligible)
    pregnant_eligible
    ) %>%
  mutate(
    c19_vacc_complete_flg = case_when(
                  c19_vacc_dose1_date >= concep_dt &
                  c19_vacc_dose1_date <= birth_dt ~ 1,
                  c19_vacc_dose2_date >= concep_dt &
                  c19_vacc_dose2_date <= birth_dt ~ 1,
                  c19_vacc_dose3_date >= concep_dt &
                  c19_vacc_dose3_date <= birth_dt ~ 1,
                  TRUE ~ 0
                  ),
    flu_vacc_complete_flg = case_when(
                  fluvac20_dt >= concep_dt &
                  fluvac20_dt <= birth_dt ~ 1,
                  fluvac21_dt >= concep_dt &
                  fluvac21_dt <= birth_dt ~ 1,
                  TRUE ~ 0
                  ),
    age_cat = case_when(age >= 40 & age <50 ~ "40-49",
                       age >= 35 & age <40 ~ "35-39",
                       age >= 30 & age <35 ~ "30-34",
                       age >= 25 & age <30 ~ "25-29",
                       age >= 18 & age <25 ~ "18-24",
                       TRUE ~ "<18"
                      ) %>% factor(levels = c("<18","18-24","25-29","30-34","35-39","40-49"))
  )

#
# Saving =======================================================================
#
cat("Saving...\n")

qsave(
  d_cohort_eligible,
  file = s_drive("d_cohort_eligible.qs")
)
qsave(
  d_cohort_pregnant,
  file = s_drive("d_cohort_pregnant.qs")
)

## Save selection table

qsave(
  t_eligible_raw,
  file = s_drive("t_eligible_raw.qs")
)


qsave(
  t_eligible,
  file = "results/t_eligible.qs"
)

cat("Done!\n")
beep(0)
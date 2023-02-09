source("r_clear_and_load.r")


# load cohort ==================================================================
cat("load cohort\n")

d_cohort <- qread(s_drive("d_cohort.qs"))


# Add inclusion criteria flags =================================================
cat("Add inclusion criteria flags\n")

d_cohort <-
  d_cohort %>%
  mutate(
    has_basic_info = !is.na(wds_start_date),
    is_alive =  is.na(death_date) | death_date >= ymd("2022-03-31"),
    is_living_in_wales = wds_start_date <= ymd("2020-09-01") & (is.na(wds_end_date) | wds_end_date >= ymd("2022-03-31")),
    has_sail_gp = sgp_start_date <= ymd("2020-09-01") & (is.na(sgp_end_date) | sgp_end_date >= ymd("2022-03-31")),
    attended_gp_5yrs = gp_flg == 1,
    has_lsoa = !is.na(lsoa2011_cd),
    has_ralf = !is.na(ralf_e),
    has_good_c19_vacc = c19_has_bad_vacc_record == 0 | is.na(c19_has_bad_vacc_record),
    is_over18 = over18_flg == 1
  )

# Create inclusion criteria table ==============================================
cat("Create inclusion criteria table\n")


t_sample_selection_overall <- tribble(
  ~step, ~criteria, ~n,
  0, "Cohort",
    d_cohort %>% nrow(),
  1, "Has basic info",
    d_cohort %>% filter(has_basic_info) %>% nrow(),
  2, "Alive on 31st Jan 2022",
    d_cohort %>% filter(has_basic_info, is_alive) %>% nrow(),
  3, "Living in Wales between 1st Sep 2020 and 31st Jan 2022",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales) %>% nrow(),
  4, "Has a SAIL GP",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp) %>% nrow(),
  5, "Has LSOA info",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp, attended_gp_5yrs, has_lsoa) %>% nrow(),
  6, "Has residency info",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp, attended_gp_5yrs, has_lsoa, has_ralf) %>% nrow(),
  7, "Has COVID vaccine records",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp, attended_gp_5yrs, has_lsoa, has_ralf, has_good_c19_vacc) %>% nrow(),
  8, "Over 18",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp, attended_gp_5yrs, has_lsoa, has_ralf, has_good_c19_vacc, is_over18) %>% nrow(),
) %>%
  mutate(
    n = round(n, -1),
    diff = n-lag(n),
    p = n / nth(n, 1),
    p = if_else(row_number() < 1, NA_real_, p),
    p = percent(p, accuracy = 0.1)
  )

t_sample_selection_overall_raw <- tribble(
  ~step, ~criteria, ~n,
  0, "Cohort",
    d_cohort %>% nrow(),
  1, "Has basic info",
    d_cohort %>% filter(has_basic_info) %>% nrow(),
  2, "Alive on 31st Jan 2022",
    d_cohort %>% filter(has_basic_info, is_alive) %>% nrow(),
  3, "Living in Wales between 1st Sep 2020 and 31st Jan 2022",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales) %>% nrow(),
  4, "Has a SAIL GP",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp) %>% nrow(),
  5, "Has LSOA info",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp, attended_gp_5yrs, has_lsoa) %>% nrow(),
  6, "Has residency info",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp, attended_gp_5yrs, has_lsoa, has_ralf) %>% nrow(),
  7, "Has COVID vaccine records",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp, attended_gp_5yrs, has_lsoa, has_ralf, has_good_c19_vacc) %>% nrow(),
  8, "Over 18",
    d_cohort %>% filter(has_basic_info, is_alive, is_living_in_wales, has_sail_gp, attended_gp_5yrs, has_lsoa, has_ralf, has_good_c19_vacc, is_over18) %>% nrow(),
) %>%
  mutate(
    diff = n-lag(n),
    p = n / nth(n, 1),
    p = if_else(row_number() < 1, NA_real_, p),
    p = percent(p)
  )


t_sample_selection_overall_pretty <- t_sample_selection_overall %>%
  kable(
    format.args = list(big.mark = ","),
    align = c("l", "r", "r")
  ) %>%
  kable_pretty()


# ==========================================================================
# Remove vacc flags out of study periods
# ==========================================================================
cat("Remove vacc flags out of study periods\n")

d_cohort$c19_vacc_dose1_flg[d_cohort$c19_vacc_dose1_date < ymd("2020-09-01")|d_cohort$c19_vacc_dose1_date > ymd("2022-03-31")] <- 0
d_cohort$c19_vacc_dose2_flg[d_cohort$c19_vacc_dose2_date < ymd("2020-09-01")|d_cohort$c19_vacc_dose2_date > ymd("2022-03-31")] <- 0
d_cohort$c19_vacc_dose3_flg[d_cohort$c19_vacc_dose3_date < ymd("2020-09-01")|d_cohort$c19_vacc_dose3_date > ymd("2022-03-31")] <- 0

d_cohort$fluvac20_flg[d_cohort$fluvac20_dt < ymd("2020-09-01")|d_cohort$fluvac20_dt > ymd("2021-03-31")] <- 0
d_cohort$fluvac21_flg[d_cohort$fluvac21_dt < ymd("2021-09-01")|d_cohort$fluvac21_dt > ymd("2022-03-31")] <- 0

# ==========================================================================
# Mutate cohort to include necessary flgs and relevel factors etc
# ==========================================================================
cat("Mutate cohort to include necessary flgs and relevel factors etc\n")

d_cohort <-
  d_cohort %>%
    mutate(
      over65_flg = as.factor(over65_flg),
      age_cat = case_when(over80_flg == 1 ~ "80+",
                             over65_flg == 1 ~ "65-80",
                             over50_flg == 1 ~ "50-65",
                             over18_flg == 1 ~ "18-50",
                             TRUE ~ "<18"
                            ) %>% factor(),
      gndr_cd = as.factor(gndr_cd),               
      ethn_cat = as.factor(ethn_cat),
      lsoa2011_cd = as.factor(lsoa2011_cd),
      wimd2019_quintile = as.factor(wimd2019_quintile),
      lad2011_nm = as.factor(lad2011_nm),
      health_board = as.factor(health_board),
      urban_rural_class = case_when(str_detect(d_cohort$urban_rural_class, "C.+") ~ "Urban",
                                    str_detect(d_cohort$urban_rural_class, "(D|E).+") ~ "Rural"
                                  ) %>% factor(levels=c("Urban", "Rural")),
      hh_cat = case_when(hh_n == 1 ~ "1",
                       hh_n == 2 ~ "2",
                       hh_n == 3 ~ "3",
                       hh_n == 4 ~ "4",
                       hh_n == 5 ~ "5",
                       hh_n >= 6  & hh_n <= 10 ~ "6-10",
                       hh_n >= 11 ~ "11+"
                      ) %>% factor(levels = c("1", "2", "3", "4", "5", "6-10", "11+")),
      c19_vacc_dose1_flg = as.factor(c19_vacc_dose1_flg),
      c19_vacc_dose1_name = as.factor(c19_vacc_dose1_name),
      c19_vacc_dose2_flg = as.factor(c19_vacc_dose2_flg),
      c19_vacc_dose2_name = as.factor(c19_vacc_dose2_name),
      c19_vacc_dose3_flg = as.factor(c19_vacc_dose3_flg),
      c19_vacc_dose3_name = as.factor(c19_vacc_dose3_name),
      c19_vacc_complete_flg = case_when(c19_vacc_dose1_flg == 1 &
                                     c19_vacc_dose2_flg == 1 &
                                     c19_vacc_dose3_flg == 1
                                     ~ 1,
                                     TRUE ~ 0
                                    ),
      flu_vacc_complete_flg = case_when(fluvac21_flg == 1 &
                                     fluvac20_flg == 1
                                     ~ 1,
                                     TRUE ~ 0
                                    ),
      dead_flg = case_when(dead_flg == 1 ~ 1,
                           TRUE ~ 0
                          ),
      bmi_cat = case_when(qc_bmi <  18.5              ~ "<18.5",
                          qc_bmi >= 18.5 & qc_bmi < 25 ~ "18.5-24.9",
                          qc_bmi >= 25  & qc_bmi < 30 ~ "25-29.9",
                          qc_bmi >= 30  & qc_bmi < 40 ~ "30-39.9",
                          qc_bmi >= 40                ~ "40+"
                         ) %>% factor(levels = c("<18.5", "18.5-24.9", "25-29.9", "30-39.9", "40+"))
    )

# Saving =======================================================================
cat("Saving...\n")

## Save included cohort

d_cohort_included_overall <- d_cohort %>% 
  filter(has_basic_info,
         is_alive,
         is_living_in_wales,
         has_sail_gp,
         attended_gp_5yrs,
         has_lsoa,
         has_ralf,
         has_good_c19_vacc,
         is_over18
       )

qsave(
  d_cohort_included_overall,
  file = s_drive("d_cohort_included_overall.qs")
)

## Save selection table

qsave(
  t_sample_selection_overall,
  file = "results/t_sample_selection_overall.qs"
)

qsave(
  t_sample_selection_overall_raw,
  file = s_drive("t_sample_selection_overall_raw.qs")
)

cat("Done!\n")
beep(0)


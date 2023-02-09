source("r_clear_and_load.r")

cat("Main cohort\n")

# load cohort ==============================================================
cat("load cohort\n")

qload(s_drive("m_log_q0.qsm"))
qload(s_drive("m_log_by_wimd.qsm"))
qload(s_drive("d_describe.qsm"))
d_describe <- d_describe_imputed
xvar_names <- qread(s_drive("xvar_names.qs"))

# extract coef =================================================================
cat("Extract coefficiants\n")

extract_coef <- function(m, xvar) {
    expr <- str_c("^", xvar)

    t_est <-
        m %>%
        tidy() %>%
        filter(str_detect(term, xvar)) %>%
        select(
            term,
            est = estimate,
            se = std.error,
            stat = statistic,
            p = p.value
        )

    t_or <-
        m %>%
        tidy(exponentiate = FALSE, conf.int = FALSE, conf.level = 0.95) %>%
        filter(str_detect(term, xvar)) %>%
        mutate(
            or = exp(estimate),
            or_low = exp(estimate - qnorm(0.975)*std.error),
            or_high = exp(estimate + qnorm(0.975)*std.error)
        ) %>%
        select(
            term,
            or,
            or_low,
            or_high
        )

    t_out <-
        t_est %>%
        left_join(t_or, by = "term") %>%
        mutate(xvar = xvar, .before = term) %>%
        mutate(term = str_replace(term, xvar, ""))

    return(t_out)
}

cat(".")

# ==========================================================================
# Including WIMD
# ==========================================================================

# unadjusted

d_c19_unadj_q0_coef <-
    bind_rows(
         m_log_unadj_q0_c19_wimd2019_quintile %>% extract_coef("wimd2019_quintile"),
         m_log_unadj_q0_c19_age_cat %>% extract_coef("age_cat"),
         m_log_unadj_q0_c19_gndr_cd %>% extract_coef("gndr_cd"),
         m_log_unadj_q0_c19_ethn_cat %>% extract_coef("ethn_cat"),
         m_log_unadj_q0_c19_bmi_cat %>% extract_coef("bmi_cat"),
         m_log_unadj_q0_c19_urban_rural_class %>% extract_coef("urban_rural_class"),
         m_log_unadj_q0_c19_hh_cat %>% extract_coef("hh_cat"),
         m_log_unadj_q0_c19_num_clinical_conditions_cat %>% extract_coef("num_clinical_conditions_cat"),
         m_log_unadj_q0_c19_health_board %>% extract_coef("health_board")
    ) %>%
    mutate(vacc = "c19", model_type = "unadj", wimd_q = 0, .before = term)

# flu
d_flu_unadj_q0_coef <-
    bind_rows(
         m_log_unadj_q0_flu_wimd2019_quintile %>% extract_coef("wimd2019_quintile"),
         m_log_unadj_q0_flu_age_cat %>% extract_coef("age_cat"),
         m_log_unadj_q0_flu_gndr_cd %>% extract_coef("gndr_cd"),
         m_log_unadj_q0_flu_ethn_cat %>% extract_coef("ethn_cat"),
         m_log_unadj_q0_flu_bmi_cat %>% extract_coef("bmi_cat"),
         m_log_unadj_q0_flu_urban_rural_class %>% extract_coef("urban_rural_class"),
         m_log_unadj_q0_flu_hh_cat %>% extract_coef("hh_cat"),
         m_log_unadj_q0_flu_num_clinical_conditions_cat %>% extract_coef("num_clinical_conditions_cat"),
         m_log_unadj_q0_flu_health_board %>% extract_coef("health_board")
    ) %>%
    mutate(vacc = "flu", model_type = "unadj", wimd_q = 0, .before = term)

cat(".")


# adjusted
d_c19_adj_q0_coef <- bind_rows(
        m_log_adj_q0_c19 %>% extract_coef("wimd2019_quintile"),
        m_log_adj_q0_c19 %>% extract_coef("age_cat"),
        m_log_adj_q0_c19 %>% extract_coef("gndr_cd"),
        m_log_adj_q0_c19 %>% extract_coef("ethn_cat"),
        m_log_adj_q0_c19 %>% extract_coef("bmi_cat"),
        m_log_adj_q0_c19 %>% extract_coef("urban_rural_class"),
        m_log_adj_q0_c19 %>% extract_coef("hh_cat"),
        m_log_adj_q0_c19 %>% extract_coef("num_clinical_conditions_cat"),
        m_log_adj_q0_c19 %>% extract_coef("health_board")
    ) %>%
    mutate(vacc = "c19", model_type = "adj", wimd_q = 0, .before = term)

d_flu_adj_q0_coef <- bind_rows(
        m_log_adj_q0_flu %>% extract_coef("wimd2019_quintile"),
        m_log_adj_q0_flu %>% extract_coef("age_cat"),
        m_log_adj_q0_flu %>% extract_coef("gndr_cd"),
        m_log_adj_q0_flu %>% extract_coef("ethn_cat"),
        m_log_adj_q0_flu %>% extract_coef("bmi_cat"),
        m_log_adj_q0_flu %>% extract_coef("urban_rural_class"),
        m_log_adj_q0_flu %>% extract_coef("hh_cat"),
        m_log_adj_q0_flu %>% extract_coef("num_clinical_conditions_cat"),
        m_log_adj_q0_flu %>% extract_coef("health_board")
    ) %>%
    mutate(vacc = "flu", model_type = "adj", wimd_q = 0, .before = term)

cat(".")

# ==========================================================================
# By WIMD
# ==========================================================================

#
# Adjusted
#


for(i in 1:5) {
# covid
    assign(paste0("d_c19_adj_q",i,"_coef"),
           bind_rows(
                get(paste0("m_log_adj_q",i,"_c19")) %>% extract_coef("age_cat"),
                get(paste0("m_log_adj_q",i,"_c19")) %>% extract_coef("gndr_cd"),
                get(paste0("m_log_adj_q",i,"_c19")) %>% extract_coef("ethn_cat"),
                get(paste0("m_log_adj_q",i,"_c19")) %>% extract_coef("bmi_cat"),
                get(paste0("m_log_adj_q",i,"_c19")) %>% extract_coef("urban_rural_class"),
                get(paste0("m_log_adj_q",i,"_c19")) %>% extract_coef("hh_cat"),
                get(paste0("m_log_adj_q",i,"_c19")) %>% extract_coef("num_clinical_conditions_cat"),
                get(paste0("m_log_adj_q",i,"_c19")) %>% extract_coef("health_board"),
            ) %>%
            mutate(vacc = "c19", model_type = "adj", wimd_q = i, .before = term)
           )
# flu
    assign(paste0("d_flu_adj_q",i,"_coef"),
           bind_rows(
                get(paste0("m_log_adj_q",i,"_flu")) %>% extract_coef("age_cat"),
                get(paste0("m_log_adj_q",i,"_flu")) %>% extract_coef("gndr_cd"),
                get(paste0("m_log_adj_q",i,"_flu")) %>% extract_coef("ethn_cat"),
                get(paste0("m_log_adj_q",i,"_flu")) %>% extract_coef("bmi_cat"),
                get(paste0("m_log_adj_q",i,"_flu")) %>% extract_coef("urban_rural_class"),
                get(paste0("m_log_adj_q",i,"_flu")) %>% extract_coef("hh_cat"),
                get(paste0("m_log_adj_q",i,"_flu")) %>% extract_coef("num_clinical_conditions_cat"),
                get(paste0("m_log_adj_q",i,"_flu")) %>% extract_coef("health_board"),
            ) %>%
            mutate(vacc = "flu", model_type = "adj", wimd_q = i, .before = term)
           )
}

cat(".")

#
# Unadjusted
#

for(i in 1:5) {
# covid
    assign(paste0("d_c19_unadj_q",i,"_coef"),
           bind_rows(
                get(paste0("m_log_unadj_q",i,"_c19_age_cat")) %>% extract_coef("age_cat"),
                get(paste0("m_log_unadj_q",i,"_c19_gndr_cd")) %>% extract_coef("gndr_cd"),
                get(paste0("m_log_unadj_q",i,"_c19_ethn_cat")) %>% extract_coef("ethn_cat"),
                get(paste0("m_log_unadj_q",i,"_c19_bmi_cat")) %>% extract_coef("bmi_cat"),
                get(paste0("m_log_unadj_q",i,"_c19_urban_rural_class")) %>% extract_coef("urban_rural_class"),
                get(paste0("m_log_unadj_q",i,"_c19_hh_cat")) %>% extract_coef("hh_cat"),
                get(paste0("m_log_unadj_q",i,"_c19_num_clinical_conditions_cat")) %>% extract_coef("num_clinical_conditions_cat"),
                get(paste0("m_log_unadj_q",i,"_c19_health_board")) %>% extract_coef("health_board")
            ) %>%
            mutate(vacc = "c19", model_type = "unadj", wimd_q = i, .before = term)
           )
# flu
    assign(paste0("d_flu_unadj_q",i,"_coef"),
           bind_rows(
                get(paste0("m_log_unadj_q",i,"_flu_age_cat")) %>% extract_coef("age_cat"),
                get(paste0("m_log_unadj_q",i,"_flu_gndr_cd")) %>% extract_coef("gndr_cd"),
                get(paste0("m_log_unadj_q",i,"_flu_ethn_cat")) %>% extract_coef("ethn_cat"),
                get(paste0("m_log_unadj_q",i,"_flu_bmi_cat")) %>% extract_coef("bmi_cat"),
                get(paste0("m_log_unadj_q",i,"_flu_urban_rural_class")) %>% extract_coef("urban_rural_class"),
                get(paste0("m_log_unadj_q",i,"_flu_hh_cat")) %>% extract_coef("hh_cat"),
                get(paste0("m_log_unadj_q",i,"_flu_num_clinical_conditions_cat")) %>% extract_coef("num_clinical_conditions_cat"),
                get(paste0("m_log_unadj_q",i,"_flu_health_board")) %>% extract_coef("health_board")
            ) %>%
            mutate(vacc = "flu", model_type = "unadj", wimd_q = i, .before = term)
           )
}


cat(".")

d_coef <-
    bind_rows(
        d_c19_adj_q0_coef,
        d_c19_adj_q1_coef,
        d_c19_adj_q2_coef,
        d_c19_adj_q3_coef,
        d_c19_adj_q4_coef,
        d_c19_adj_q5_coef,
        d_flu_adj_q0_coef,
        d_flu_adj_q1_coef,
        d_flu_adj_q2_coef,
        d_flu_adj_q3_coef,
        d_flu_adj_q4_coef,
        d_flu_adj_q5_coef,
        d_c19_unadj_q0_coef,
        d_c19_unadj_q1_coef,
        d_c19_unadj_q2_coef,
        d_c19_unadj_q3_coef,
        d_c19_unadj_q4_coef,
        d_c19_unadj_q5_coef,
        d_flu_unadj_q0_coef,
        d_flu_unadj_q1_coef,
        d_flu_unadj_q2_coef,
        d_flu_unadj_q3_coef,
        d_flu_unadj_q4_coef,
        d_flu_unadj_q5_coef,
    ) %>%
    select(
        vacc,
        xvar,
        xlbl = term,
        model_type,
        wimd_q,
        everything()
    )

cat(".")

# work out reference levels ----------------------------------------------------

coef_terms <-
    d_coef %>%
    select(vacc, wimd_q, xvar, xlbl) %>%
    distinct() %>%
    mutate(has_coef = 1)



d_coef_ref <-
    d_describe %>%
    select(xvar, xlbl, vacc, wimd_q) %>%
    distinct() %>%
    left_join(coef_terms, by = c("xvar", "xlbl", "vacc", "wimd_q")) %>%
    filter(is.na(has_coef)) %>%
    filter(xvar %in% coef_terms$xvar) %>%
    filter(!is.na(xlbl)) %>%
    mutate(
        model_type = "ref",
        est = 1,
        or = 1
    ) %>%
    select(-has_coef)

d_coef_bind <- bind_rows(d_coef_ref, d_coef)

cat(".\n")

# combine Ns and ors ===========================================================

d_n_coef <-
    d_describe %>%
    left_join(d_coef_bind, by = c("xvar", "xlbl", "vacc", "wimd_q"))
d_n_coef$wimd_q <- as.factor(d_n_coef$wimd_q)


# make pretty plot =============================================================
cat("Make a pretty plot\n")

lkp_xvar <- c(
    "Age"                            = "age_cat",
    "Sex"                            = "gndr_cd",
    "Ethnicity"                      = "ethn_cat",
    "BMI"                            = "bmi_cat",
    "Household\ncomposition"         = "hh_cat",
    "WIMD (2019)\nquintile"          = "wimd2019_quintile",
    "Urban/rural\nclass"             = "urban_rural_class",
    "No. of clinical\nconditions"    = "num_clinical_conditions_cat",
    "Health board"                   = "health_board"
)

lkp_model_type <- c(
    "Adjusted"   = "adj",
    "Unadjusted" = "unadj",
    "Reference"  = "ref"
)

# ==========================================================================
# Plot overall
# ==========================================================================

p_coef_overall <-
d_n_coef %>%
    filter(wimd_q == 0) %>%
    filter(xvar %in% lkp_xvar) %>%
    filter(!is.na(model_type)) %>%
    mutate(
        xvar = factor(xvar, lkp_xvar, names(lkp_xvar)),
        xlbl = xlbl %>% fct_relevel( # orders based numerically or by population
            # age
            "18-50",
            "50-65",
            "65-80",
            "80+",
            # ethnicity
            "White",
            "Asian",
            "Black",
            "Mixed",
            "Other",
            "(Missing)",
            # house hold
            "Alone",
            "2 members",
            "3 members",
            "4 members",
            "5 members",
            "6-10 members",
            "11+ members",
            # BMI
            "<18.5",
            "18.5-24.9",
            "25-29.9",
            "30-39.9",
            "40+",
            # wimd
            "1st (Most deprived)",
            "2nd",
            "3rd",
            "4th",
            "5th (Least deprived)",
            # clin conditions
            "No conditions",
            "1 condition",
            "2 conditions",
            "3 conditions",
            "4+ conditions"
        ),
        model_type = factor(model_type, lkp_model_type, names(lkp_model_type))
    ) %>%
ggplot(aes(
        x = or, xmin = or_low, xmax = or_high,
        y = xlbl,
        group = model_type, colour = model_type
    )) +
    facet_grid(xvar ~ vacc, scales = "free_y", space = "free_y", switch = "y") +
    geom_vline(xintercept = 1) +
    geom_pointrange(position = position_dodge(0.4)) +
    scale_colour_brewer(palette = "Set1") +
    coord_cartesian(xlim = c(0, 3)) +
    theme(
        axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)
    ) +
    ggtitle("ORs for vaccine uptake by type")

cat(".")

# ==========================================================================
# Plot for each wimd quintile
# ==========================================================================

p_coef_per_wimd_c19 <-
d_n_coef %>%
    filter(vacc == "c19") %>%
    filter(wimd_q != 0) %>%
    filter(xvar %in% lkp_xvar) %>%
    filter(!is.na(model_type)) %>%
    mutate(
        xvar = factor(xvar, lkp_xvar, names(lkp_xvar)),
        xlbl = xlbl %>% fct_relevel(
             # age
            "18-50",
            "50-65",
            "65-80",
            "80+",
            # ethnicity
            "White",
            "Asian",
            "Black",
            "Mixed",
            "Other",
            "(Missing)",
            # house hold
            "Alone",
            "2 members",
            "3 members",
            "4 members",
            "5 members",
            "6-10 members",
            "11+ members",
            # BMI
            "<18.5",
            "18.5-24.9",
            "25-29.9",
            "30-39.9",
            "40+",
            # clin conditions
            "No conditions",
            "1 condition",
            "2 conditions",
            "3 conditions",
            "4+ conditions"
        ),
        model_type = factor(model_type, lkp_model_type, names(lkp_model_type))
    ) %>%
ggplot(aes(
        x = or, xmin = or_low, xmax = or_high,
        y = xlbl,
        group = model_type, colour = model_type
    )) +
    facet_grid(xvar ~ wimd_q, scales = "free_y", space = "free_y", switch = "y") +
    geom_vline(xintercept = 1) +
    geom_pointrange(position = position_dodge(0.4)) +
    scale_colour_brewer(palette = "Set1") +
    coord_cartesian(xlim = c(0, 3)) +
    theme(
        axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)
    ) +
    ggtitle("ORs for COVID-19 vaccine uptake by WIMD quintile")

cat(".")

p_coef_per_wimd_flu <-
d_n_coef %>%
    filter(vacc == "flu") %>%
    filter(wimd_q != 0) %>%
    filter(xvar %in% lkp_xvar) %>%
    filter(!is.na(model_type)) %>%
    mutate(
        xvar = factor(xvar, lkp_xvar, names(lkp_xvar)),
        xlbl = xlbl %>% fct_relevel(
             # age
            "18-50",
            "50-65",
            "65-80",
            "80+",
            # ethnicity
            "White",
            "Asian",
            "Black",
            "Mixed",
            "Other",
            "(Missing)",
            # house hold
            "Alone",
            "2 members",
            "3 members",
            "4 members",
            "5 members",
            "6-10 members",
            "11+ members",
            # BMI
            "<18.5",
            "18.5-24.9",
            "25-29.9",
            "30-39.9",
            "40+",
            # clin conditions
            "No conditions",
            "1 condition",
            "2 conditions",
            "3 conditions",
            "4+ conditions"
        ),
        model_type = factor(model_type, lkp_model_type, names(lkp_model_type))
    ) %>%
ggplot(aes(
    x = or, xmin = or_low, xmax = or_high,
    y = xlbl,
    group = model_type, colour = model_type
    )) +
    facet_grid(xvar ~ wimd_q, scales = "free_y", space = "free_y", switch = "y") +
    geom_vline(xintercept = 1) +
    geom_pointrange(position = position_dodge(0.4)) +
    scale_colour_brewer(palette = "Set1") +
    coord_cartesian(xlim = c(0, 3)) +
    theme(
        axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)
    ) +
    ggtitle("ORs for Influenza vaccine uptake by WIMD quintile")
cat(".\n")

# ==========================================================================
# Trying a different way of representing it
# ==========================================================================

p_coef_compare_q1q5 <-
d_n_coef %>%
    filter(wimd_q %in% c(1,5)) %>%
    filter(model_type %in% c("adj", "ref")) %>%
    filter(xvar %in% lkp_xvar) %>%
    filter(!is.na(model_type)) %>%
    mutate(
        xvar = factor(xvar, lkp_xvar, names(lkp_xvar)),
        xlbl = xlbl %>% fct_relevel(
             # age
            "18-50",
            "50-65",
            "65-80",
            "80+",
            # ethnicity
            "White",
            "Asian",
            "Black",
            "Mixed",
            "Other",
            "(Missing)",
            # house hold
            "Alone",
            "2 members",
            "3 members",
            "4 members",
            "5 members",
            "6-10 members",
            "11+ members",
            # BMI
            "<18.5",
            "18.5-24.9",
            "25-29.9",
            "30-39.9",
            "40+",
            # clin conditions
            "No conditions",
            "1 condition",
            "2 conditions",
            "3 conditions",
            "4+ conditions"
        ),
        wimd_q = case_when(model_type == "ref" ~ "Reference",
                           wimd_q == 1 ~ "1",
                           wimd_q == 5 ~ "5"
                          )
    ) %>%
ggplot(aes(
        x = or, xmin = or_low, xmax = or_high,
        y = xlbl,
        group = wimd_q, colour = wimd_q
    )) +
    facet_grid(xvar ~ vacc, scales = "free_y", space = "free_y", switch = "y") +
    geom_vline(xintercept = 1) +
    geom_pointrange(position = position_dodge(0.4)) +
    scale_colour_brewer(palette = "Set1") +
    coord_cartesian(xlim = c(0, 3)) +
    theme(
        axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)
    ) +
    ggtitle("Adjusted ORs for vaccine uptake comparing most and least deprived")


# glance =======================================================================
cat("Check stats\n")

glance(m_log_adj_q5_c19)

tidy_glance <- function(model, col_name) {
    glance(model) %>%
    select(
        log_likelihood = logLik,
        AIC,
        BIC,
        nobs
    ) %>%
    pivot_longer(
        cols = everything(),
        names_to = "stat",
        values_to = col_name
    )
}

t_model_stats <-
    tidy_glance(m_log_adj_q0_c19, "c19") %>%
    left_join(tidy_glance(m_log_adj_q0_flu, "flu"), by = "stat") %>%
    left_join(tidy_glance(m_log_adj_q1_c19, "c19_wimd_q1"), by = "stat") %>%
    left_join(tidy_glance(m_log_adj_q1_flu, "flu_wimd_q1"), by = "stat") %>%
    pivot_longer(cols = -stat, names_to = "vacc") %>%
    pivot_wider(names_from = stat)



# save =========================================================================
cat("Saving...\n")

qsavem(
    d_coef,
    d_n_coef,
    p_coef_per_wimd_c19,
    p_coef_per_wimd_flu,
    p_coef_overall,
    p_coef_compare_q1q5,
    t_model_stats,
    file = "results/coef.qsm"
)
cat("Done!\n")
beep(0)
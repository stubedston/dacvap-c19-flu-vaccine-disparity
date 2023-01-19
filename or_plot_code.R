
# Where d_n_coef is a table of coefficiants for all wimd quintiles seperately and overall

# ==========================================================================
# lkps
# ==========================================================================

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



# ==========================================================================
# Overlapping q1 and q5
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


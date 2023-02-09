source("r_clear_and_load.r")

# load cohort ==============================================================
cat("load cohort\n")


qload(s_drive("m_log_interaction_adj.qsm"))
qload(s_drive("d_describe_pregnant.qsm"))
qload(s_drive("d_describe.qsm"))



# extract coefficiants =====================================================
cat("Extract coefficiants\n")
cat("Main cohort\n")

d_interaction_coef_c19 <- tidy(m_log_interaction_c19_adj) %>%
	filter(str_detect(term, "wimd2019_quintile1st \\(Most deprived\\):")) %>%
	mutate(
		term = str_replace(term, "wimd2019_quintile1st \\(Most deprived\\):", ""),
		xvar = str_extract(term, "(.*_cat|.*_class|.*board|.*_cd)"),
		xlbl = str_replace(term, "(.*_cat|.*_class|.*board|.*_cd)", ""),
		or = exp(estimate),
		or_low = exp(estimate - qnorm(0.975)*std.error),
        or_high = exp(estimate + qnorm(0.975)*std.error),
        se = std.error,
        stat = statistic,
        p = p.value,
        vacc = "c19",
        model_type = "adj"
		) %>% 
	select(
		xvar,
		xlbl,
		or,
		or_low,
		or_high,
		se,
		stat,
		p,
		vacc,
		model_type,
		)

d_interaction_coef_flu <- tidy(m_log_interaction_flu_adj) %>%
	filter(str_detect(term, "wimd2019_quintile1st \\(Most deprived\\):")) %>%
	mutate(
		term = str_replace(term, "wimd2019_quintile1st \\(Most deprived\\):", ""),
		xvar = str_extract(term, "(.*_cat|.*_class|.*board|.*_cd)"),
		xlbl = str_replace(term, "(.*_cat|.*_class|.*board|.*_cd)", ""),
		or = exp(estimate),
		or_low = exp(estimate - qnorm(0.975)*std.error),
        or_high = exp(estimate + qnorm(0.975)*std.error),
        se = std.error,
        stat = statistic,
        p = p.value,
        vacc = "flu",
        model_type = "adj"
		) %>% 
	select(
		xvar,
		xlbl,
		or,
		or_low,
		or_high,
		se,
		stat,
		p,
		vacc,
		model_type,
		)

# ==========================================================================
# pregnant cohort 
# ==========================================================================
cat("Pregnant cohort\n")


d_interaction_coef_c19_preg <- tidy(m_log_interaction_c19_adj_preg) %>%
    filter(str_detect(term, "wimd2019_quintile1st \\(Most deprived\\):")) %>%
    mutate(
        term = str_replace(term, "wimd2019_quintile1st \\(Most deprived\\):", ""),
        xvar = str_extract(term, "(.*_cat|.*_class|.*board|.*_cd)"),
        xlbl = str_replace(term, "(.*_cat|.*_class|.*board|.*_cd)", ""),
        or = exp(estimate),
        or_low = exp(estimate - qnorm(0.975)*std.error),
        or_high = exp(estimate + qnorm(0.975)*std.error),
        se = std.error,
        stat = statistic,
        p = p.value,
        vacc = "c19",
        model_type = "adj"
        ) %>% 
    select(
        xvar,
        xlbl,
        or,
        or_low,
        or_high,
        se,
        stat,
        p,
        vacc,
        model_type,
        )


d_interaction_coef_flu_preg <- tidy(m_log_interaction_flu_adj_preg) %>%
    filter(str_detect(term, "wimd2019_quintile1st \\(Most deprived\\):")) %>%
    mutate(
        term = str_replace(term, "wimd2019_quintile1st \\(Most deprived\\):", ""),
        xvar = str_extract(term, "(.*_cat|.*_class|.*board|.*_cd)"),
        xlbl = str_replace(term, "(.*_cat|.*_class|.*board|.*_cd)", ""),
        or = exp(estimate),
        or_low = exp(estimate - qnorm(0.975)*std.error),
        or_high = exp(estimate + qnorm(0.975)*std.error),
        se = std.error,
        stat = statistic,
        p = p.value,
        vacc = "flu",
        model_type = "adj"
        ) %>% 
    select(
        xvar,
        xlbl,
        or,
        or_low,
        or_high,
        se,
        stat,
        p,
        vacc,
        model_type,
        )

# ==========================================================================
# Add reference coefficiants
# ==========================================================================
cat("Add reference coefficiants\n")

xlbls <- d_describe_imputed %>% select(xvar, xlbl) %>% filter(xvar != "wimd2019_quintile") %>% distinct()

xlbls_preg <- d_describe_imputed_preg %>% select(xvar, xlbl) %>% filter(xvar != "wimd2019_quintile") %>% distinct()

# ==========================================================================
# Main cohort
# ==========================================================================

d_interaction_coef_c19 <-
	left_join(xlbls, d_interaction_coef_c19) %>% mutate(
	model_type = case_when(is.na(or) ~ "ref",
			TRUE ~ model_type),
	vacc = "c19",
	or = case_when(is.na(or) ~ 1,
			TRUE ~ or),
	or_low = case_when(is.na(or_low) ~ 1,
			TRUE ~ or_low),
	or_high = case_when(is.na(or_high) ~ 1,
			TRUE ~ or_high),
    vacc = "c19"
	)


d_interaction_coef_flu <-
	left_join(xlbls, d_interaction_coef_flu) %>% mutate(
	model_type = case_when(is.na(or) ~ "ref",
			TRUE ~ model_type),
	vacc = "c19",
	or = case_when(is.na(or) ~ 1,
			TRUE ~ or),
	or_low = case_when(is.na(or_low) ~ 1,
			TRUE ~ or_low),
	or_high = case_when(is.na(or_high) ~ 1,
			TRUE ~ or_high),
    vacc = "flu"
	)

# ==========================================================================
# pregnant cohort 
# ==========================================================================

d_interaction_coef_c19_preg <-
    left_join(xlbls_preg, d_interaction_coef_c19_preg) %>%
    filter(xlbl != "3 conditions") %>%
    mutate(
    model_type = case_when(is.na(or) ~ "ref",
            TRUE ~ model_type),
    vacc = "c19",
    or = case_when(is.na(or) ~ 1,
            TRUE ~ or),
    or_low = case_when(is.na(or_low) ~ 1,
            TRUE ~ or_low),
    or_high = case_when(is.na(or_high) ~ 1,
            TRUE ~ or_high),
    vacc = "c19"
    )


d_interaction_coef_flu_preg <-
    left_join(xlbls_preg, d_interaction_coef_flu_preg) %>% 
    filter(xlbl != "3 conditions") %>%
    mutate(
    model_type = case_when(is.na(or) ~ "ref",
            TRUE ~ model_type),
    vacc = "c19",
    or = case_when(is.na(or) ~ 1,
            TRUE ~ or),
    or_low = case_when(is.na(or_low) ~ 1,
            TRUE ~ or_low),
    or_high = case_when(is.na(or_high) ~ 1,
            TRUE ~ or_high),
    vacc = "flu"
    )

# ==========================================================================
# Let's get plotting
# ==========================================================================
cat("Let's get plotting\n")

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
    "WIMD quintile 1"   = "adj",
    "Unadjusted" = "unadj",
    "Reference"  = "ref"
)


p_interaction <-
rbind(
    d_interaction_coef_c19,
    d_interaction_coef_flu
    ) %>%
    filter(xvar %in% lkp_xvar) %>%
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
    xlab("<-- more influential on quintile 5                                                     OR                                      more influential on quintile 1 -->               ") +
    ggtitle("Adjusted ORs for vaccine uptake for WIMD 1 compared to WIMD 5")

print(p_interaction)

# ==========================================================================
# pregnant cohort 
# ==========================================================================


p_interaction_preg <-
rbind(
    d_interaction_coef_c19_preg,
    d_interaction_coef_flu_preg
    ) %>%
    filter(xvar %in% lkp_xvar) %>%
    mutate(
        xvar = factor(xvar, lkp_xvar, names(lkp_xvar)),
        xlbl = xlbl %>% fct_relevel( # orders based numerically or by population
            # age
            "18-24",
            "25-29",
            "30-34",
            "35-39",
            "40-49",
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
            "2 conditions"
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
    xlab("<-- more influential on quintile 5                                                     OR                                      more influential on quintile 1 -->               ") +
    ggtitle("Adjusted ORs for vaccine uptake for pregnant women with high deprivation compared to those with low deprivation")

print(p_interaction_preg)

# save =========================================================================
cat("Saving...\n")

qsavem(
    p_interaction,
    p_interaction_preg,
    d_interaction_coef_c19,
    d_interaction_coef_c19_preg,
    d_interaction_coef_flu,
    d_interaction_coef_flu_preg,
    file = "results/interactions.qsm"
)
cat("Done!\n")
beep(0)
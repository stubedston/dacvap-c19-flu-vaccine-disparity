source("r_clear_and_load.r")

cat("load cohort\n")

d_cohort <- qread(s_drive("d_cohort_included_overall.qs"))

cat("Prepare imputation variables\n")

# set BMI to use the following measures
bmi_xvar <- c(
    # outcome
    "fluvac18",
    "fluvac19",
    "fluvac20",
    "fluvac21",
    "c19_vacc_dose1_flg",
    "c19_vacc_dose2_flg",
    "c19_vacc_dose3_flg",
    # demographics
    "gndr_cd",
    "age_cat",
    "ethn_cat",
    # area
    "wimd2019_quintile",
    "health_board",
    "urban_rural_class",
    "hh_cat",
    # qcovid
    "qc_b2_leukolaba",
    "qc_b2_prednisolone",
    "qc_b_af",
    "qc_b_ccf",
    "qc_b_asthma",
    "qc_b_bloodcancer",
    "qc_b_cerebralpalsy",
    "qc_b_chd",
    "qc_b_cirrhosis",
    "qc_b_congenheart",
    "qc_b_copd",
    "qc_b_dementia",
    "qc_b_epilepsy",#
    "qc_b_fracture4",
    "qc_b_neurorare",
    "qc_b_parkinsons",
    "qc_b_pulmhyper",
    "qc_b_pulmrare",
    "qc_b_pvd",
    "qc_b_ra_sle",#
    "qc_b_respcancer",
    "qc_b_semi",
    "qc_b_sicklecelldisease",
    "qc_b_stroke",#
    "qc_diabetes_cat",
    "qc_b_vte",
    "qc_chemo_cat",
    "qc_home_cat",
    "qc_learn_cat",#
    "qc_marrow6",
    "qc_p_radio6",
    "qc_p_solidtransplant",
    "qc_renal_cat"
)

# ==========================================================================
# check for NAs
# ==========================================================================
nacols <- c() 
for(i in c(1:ncol(d_cohort))) {  
    if(d_cohort %>% filter(is.na(d_cohort[,i])) %>% nrow() != 0) {
        nacols[[i]] <- colnames(d_cohort[,i])
    }
}
nacols <- nacols[-which(sapply(nacols, is.null))]
nacols <- nacols[nacols %in% bmi_xvar]
print(nacols)

# ==========================================================================
# Impute BMI
# ==========================================================================
cat("Impute BMI\n")

d_cohort <- d_cohort %>% mutate(
                                qc_bmi_log = log(qc_bmi),
                                age_cat = case_when(
                                                age >=18 & age <25 ~ "18-25",
                                                age >=25 & age <30 ~ "25-30",
                                                age >=30 & age <35 ~ "30-35",
                                                age >=35 & age <40 ~ "35-40",
                                                age >=40 & age <45 ~ "40-45",
                                                age >=45 & age <50 ~ "45-50",
                                                age >=50 & age <55 ~ "50-55",
                                                age >=55 & age <60 ~ "55-60",
                                                age >=60 & age <65 ~ "30-65",
                                                age >=65 & age <70 ~ "65-70",
                                                age >=70 & age <75 ~ "70-75",
                                                age >=75 & age <80 ~ "75-80",
                                                age >=80 ~ "80+"
                                            ) %>% factor(levels = c("18-25","25-30","30-35","35-40","40-45","45-50","50-55","55-60","30-65","65-70","70-75","75-80","80+")),
                                ethn_cat = case_when(
                                                is.na(ethn_cat) ~ "(Missing)",
                                                TRUE ~ as.character(ethn_cat)
                                            ) %>% factor(levels = c("Asian","Black","Mixed","Other","White", "(Missing)")),
                                qc_b2_leukolaba = replace_na(qc_b2_leukolaba, 0),
                                qc_b2_prednisolone = replace_na(qc_b2_prednisolone, 0),
                                qc_b_af = replace_na(qc_b_af, 0),
                                qc_b_ccf = replace_na(qc_b_ccf, 0),
                                qc_b_asthma = replace_na(qc_b_asthma, 0),
                                qc_b_bloodcancer = replace_na(qc_b_bloodcancer, 0),
                                qc_b_cerebralpalsy = replace_na(qc_b_cerebralpalsy, 0),
                                qc_b_chd = replace_na(qc_b_chd, 0),
                                qc_b_cirrhosis = replace_na(qc_b_cirrhosis, 0),
                                qc_b_congenheart = replace_na(qc_b_congenheart, 0),
                                qc_b_copd = replace_na(qc_b_copd, 0),
                                qc_b_dementia = replace_na(qc_b_dementia, 0),
                                qc_b_epilepsy = replace_na(qc_b_epilepsy, 0),
                                qc_b_fracture4 = replace_na(qc_b_fracture4, 0),
                                qc_b_neurorare = replace_na(qc_b_neurorare, 0),
                                qc_b_parkinsons = replace_na(qc_b_parkinsons, 0),
                                qc_b_pulmhyper = replace_na(qc_b_pulmhyper, 0),
                                qc_b_pulmrare = replace_na(qc_b_pulmrare, 0),
                                qc_b_pvd = replace_na(qc_b_pvd, 0),
                                qc_b_ra_sle = replace_na(qc_b_ra_sle, 0),
                                qc_b_respcancer = replace_na(qc_b_respcancer, 0),
                                qc_b_semi = replace_na(qc_b_semi, 0),
                                qc_b_sicklecelldisease = replace_na(qc_b_sicklecelldisease, 0),
                                qc_b_stroke = replace_na(qc_b_stroke, 0),
                                qc_diabetes_cat = replace_na(qc_diabetes_cat, 0),
                                qc_b_vte = replace_na(qc_b_vte, 0),
                                qc_chemo_cat = replace_na(qc_chemo_cat, 0),
                                qc_home_cat = replace_na(qc_home_cat, 0),
                                qc_learn_cat = replace_na(qc_learn_cat, 0),
                                qc_p_radio6 = replace_na(qc_p_radio6, 0),
                                qc_p_solidtransplant = replace_na(qc_p_solidtransplant, 0),
                                qc_renal_cat = replace_na(qc_renal_cat, 1)
                            )

pred.mat <- matrix(
data = 0,
nrow = ncol(d_cohort),
ncol = ncol(d_cohort),
dimnames = list(
    names(d_cohort),
    names(d_cohort)
)
)

pred.mat[
    rownames(pred.mat) == "qc_bmi_log",
    colnames(pred.mat) %in% bmi_xvar
] <- 1



# method, choose from: sample, norm, norm.boot, rf
mice.method <- rep("", ncol(d_cohort))
mice.method[rownames(pred.mat) == "qc_bmi_log"] <- "norm"

md_imp_bmi <- mice(
    data            = d_cohort,
    m               = 1,
    predictorMatrix = pred.mat,
    method          = mice.method,
    printFlag       = TRUE,
    maxit           = 5
)

# summary plot of BMI imputations
d_bmi_imp <-
    md_imp_bmi %>%
    complete(
        action = "long",
        include = TRUE
    )

p_bmi_imp <-
    d_bmi_imp %>%
    select(imp = .imp, alf_e, qc_bmi_log, gndr_cd, age_cat) %>%
    mutate(imp = factor(imp, 0:1, c("observed", "cmp1"))) %>%
    ggplot(aes(x = exp(qc_bmi_log), group = imp, colour = imp)) +
    geom_density() +
    facet_grid(age_cat ~ gndr_cd) +
    labs(x = "qc_bmi_log") +
    xlim(10, 52)

ggsave(
    p_bmi_imp,
    file = "results/p_impute_bmi_density.png",
    width = p_width * 2,
    height = p_height
)

print(p_bmi_imp)

# Save =========================================================================
cat("Saving...\n")

d_bmi_imp <-
    md_imp_bmi %>%
    complete(
        action = "broad",
        include = TRUE
    ) %>%
    rename(
        alf_e   = alf_e.0,
        bmi_imp = qc_bmi_log.1,
    ) %>%
    select(
        alf_e,
        bmi_imp
    ) %>%
    mutate(
        bmi_imp = exp(bmi_imp)
    )

qsave(
    x = d_bmi_imp,
    file = s_drive("d_bmi_imp.qs")
)

cat("Done!\n")
beep(0)
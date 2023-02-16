# ==========================================================================
# Clear environment
# ==========================================================================
cat("Clear and load!\n")

if (Sys.info()["user"] == "william.midgley") {
  cat("Hi Will!\n")
  setwd("~/dcp02_covid_v_flu_coverage_disparities/dacvap-c19-flu-vaccine-disparity/main")
} else if (Sys.info()["user"] == "Stuart.Bedston") {
  cat("Hi Stu!\n")
  setwd("~/Projects/dacvap-c19-flu-vaccine-disparity/main")
} else {
  cat("Is that you, Utkarsh?! Add your name here\n") 
}

rm(list = ls())

if (!is.null(sessionInfo()$otherPkgs)) {
  suppressWarnings(
    invisible(
      lapply(
        paste0('package:', names(sessionInfo()$otherPkgs)
          ),
        detach,
        character.only=TRUE,
        unload=TRUE
        )
      )
    )
}

# ==========================================================================
# Load
# ==========================================================================

pkgs <- c(
  "tidyverse",
  "beepr",
  "eulerr",
  "grid",
  "janitor",
  "patchwork",
  "readr",
  "scales",
  "stringr"
  )

for (pkg in pkgs) {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
      )
    )
}

# ==========================================================================
# Load data
# ==========================================================================

# AWAITING META-ANALYSIS RE-RUN

d_meta_or <- read.csv("data_odds_ratios/wales/wales_main_coefs_interaction.csv")

d_meta_or <-
d_meta_or %>% mutate(
  xlbl = case_when(
    xvar == "age_cat" & xlbl == "18-50"   ~ "18-49",
    xvar == "age_cat" & xlbl == "50-65"   ~ "50-64",
    xvar == "age_cat" & xlbl == "65-80"   ~ "65-79",
    xvar == "bmi_cat" & xlbl == "25-29.9" ~ "25.0-29.9",
    xvar == "bmi_cat" & xlbl == "30-39.9" ~ "30.0-39.9",
    xvar == "bmi_cat" & xlbl == "40+"     ~ "40.0+",
    xlbl == "(Missing)"                   ~ "(Ethnicity missing)",
    xvar == "total"                       ~ "Total",
    TRUE                                  ~ xlbl
    )
  )

# ==========================================================================
# lkps
# ==========================================================================

lkp_xvar_table <- c(
    "IMD quintile"                   = "imd",
    "Age"                            = "age",
    "Sex"                            = "sex",
    "Ethnicity"                      = "ethnicity",
    "BMI"                            = "bmi",
    "Household composition"          = "household_status",
    "Urban/rural class"              = "urban_rural",
    "No. of clinical conditions"     = "num_clinical_conditions_cat"
)

lkp_xvar <- c(
    "IMD (2019)\nquintile"           = "imd",
    "Age"                            = "age",
    "Sex"                            = "sex",
    "Ethnicity"                      = "ethnicity",
    "BMI"                            = "bmi",
    "Household\ncomposition"         = "household_status",
    "Urban/rural\nclass"             = "urban_rural",
    "No. of clinical\nconditions"    = "num_clinical_conditions_cat"
)

lkp_xlbls <- c(
    # wimd
    "1st (Most deprived)",
    "2nd",
    "3rd",
    "4th",
    "5th (Least deprived)",
    # sex
    "Female",
    "Male",
    # age
    "18-49",
    "50-64",
    "65-79",
    "80+",
    # BMI
    "<18.5",
    "18.5-24.9",
    "25.0-29.9",
    "30.0-39.9",
    "40.0+",
    "(BMI missing)",
    # ethnicity
    "White",
    "Asian",
    "Black",
    "Mixed",
    "Other",
    "(Ethnicity missing)",
    # house hold
    "Alone",
    "2 members",
    "3 members",
    "4 members",
    "5 members",
    "6-10 members",
    "11+ members",
    # clin conditions
    "No conditions",
    "1 condition",
    "2 conditions",
    "3 conditions",
    "4+ conditions",
    # Rurality
    "Urban",
    "Rural")

lkp_model_type <- c(
    "Adjusted"   = "adj",
    "Unadjusted" = "unadj",
    "Reference"  = "ref"
)

lkp_vacc <- c(
    "COVID-19"  = "c19",
    "Influenza" = "flu"
)


# ==========================================================================
# Make pretty table
# ==========================================================================

colnames(d_pool_or) <- c("or", "or_low", "or_high", "country", "xvar", "xlbl", "vacc")

d_pool_or <-
d_pool_or %>% mutate(
    xlbl = case_when(
        xvar == "ethnicity" & xlbl == "Male" ~ "Mixed",
        xlbl == "2"                          ~ "2 members",
        xlbl == "hh 1"                       ~ "Alone",
        xlbl == "hh 3"                       ~ "3 members",
        xlbl == "hh 4"                       ~ "4 members",
        xlbl == "hh 5"                       ~ "5 members",
        xlbl == "hh 6-10"                    ~ "6-10 members",
        xlbl == "hh 7+"                      ~ "11+ members",
        xlbl == "imd 5 - Least deprived"     ~ "5th (Least deprived)",
        xlbl == "imd 1 - Most deprived"      ~ "1st (Most deprived)",
        xlbl == "imd 2"                      ~ "2nd",
        xlbl == "imd 3"                      ~ "3rd",
        xlbl == "imd 4"                      ~ "4th",
        xlbl == "rsk 0"                      ~ "No conditions",
        xlbl == "rsk 1"                      ~ "1 condition",
        xlbl == "rsk 2"                      ~ "2 conditions",
        xlbl == "rsk 3"                      ~ "3 conditions",
        xlbl == "rsk 4+"                     ~ "4+ conditions",
        TRUE                                 ~ xlbl
        )
    )

d_england_or <- d_pool_or %>% filter(country == "england")
d_wales_or   <- d_pool_or %>% filter(country == "wales")
d_meta_or    <- d_pool_or %>% filter(country == "meta")



d_meta_or_pretty <-
d_meta_or %>% 
    mutate(
    xlbl = xlbl %>% fct_relevel(lkp_xlbls)
    ) %>%
    select(
        xvar,
        xlbl,
        vacc,
        or,
        or_low,
        or_high
    ) %>%
    arrange(xlbl)

d_meta_or_pretty <-
    d_meta_or_pretty %>% mutate(
        Variable = factor(xvar, lkp_xvar_table, names(lkp_xvar_table)),
        Category = xlbl,
        `OR (95% CI)` = case_when(
            or == 1 & or_low == 1 & or_high == 1 ~ "1",
            TRUE ~ paste0(format(round(or, 2), nsmall = 2), " (", format(round(or_low, 2), nsmall = 2), "-", format(round(or_high, 2), nsmall = 2), ")")
            )
        ) %>%
    select(
        Variable,
        Category,
        vacc,
        `OR (95% CI)`
        )

d_meta_or_pretty_c19 <- d_meta_or_pretty %>% filter(vacc == "c19") %>% select(-vacc) %>% distinct()
d_meta_or_pretty_flu <- d_meta_or_pretty %>% filter(vacc == "flu") %>% select(-vacc) %>% distinct()

colnames(d_meta_or_pretty_c19) <- c("Variable", "Category", "COVID-19 uptake OR (95% CI)")
colnames(d_meta_or_pretty_flu) <- c("Variable", "Category", "Influenza uptake OR (95% CI)")

d_meta_or_pretty <- full_join(d_meta_or_pretty_c19, d_meta_or_pretty_flu)

# ==========================================================================
# Plot
# ==========================================================================

d_meta_or <- d_meta_or %>% mutate(
    model_type = case_when(
        or == 1 & or_low == 1 & or_high == 1 ~ "ref",
        TRUE ~ "adj"
        )
    )

p_meta_or <-
d_meta_or %>%
    filter(xvar %in% lkp_xvar) %>%
    filter(!is.na(model_type)) %>%
    mutate(
        xvar = factor(xvar, lkp_xvar, names(lkp_xvar)),
        xlbl = xlbl %>% fct_relevel( # orders based numerically or by population
            # wimd
            "1st (Most deprived)",
            "2nd",
            "3rd",
            "4th",
            "5th (Least deprived)",
            # sex
            "Female",
            "Male",         
            # age
            "18-49",
            "50-64",
            "65-79",
            "80+",
            # BMI
            "<18.5",
            "18.5-24.9",
            "25.0-29.9",
            "30.0-39.9",
            "40.0+",
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
            # clin conditions
            "No conditions",
            "1 condition",
            "2 conditions",
            "3 conditions",
            "4+ conditions",
            # Rurality
            "Urban",
            "Rural"
        ),
        model_type = factor(model_type, lkp_model_type, names(lkp_model_type)),
        vacc = factor(vacc, lkp_vacc, names(lkp_vacc))
    ) %>%
ggplot(aes(
        x = or, xmin = or_low, xmax = or_high,
        y = xlbl,
        group = model_type, colour = model_type
    )) +
    facet_grid(xvar ~ vacc, scales = "free_y", space = "free_y", switch = "y") +
    geom_vline(xintercept = 1) +
    geom_pointrange(position = position_dodge(0.4)) +
    scale_colour_manual(values = cbPalette) +
    coord_cartesian(xlim = c(0, 3.4)) +
    theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)
    ) +
    ggtitle("ORs for vaccine uptake by type")

p_meta_or

# ==========================================================================
# Save plot
# ==========================================================================
cat("Saving...\n")

write_csv(
  d_meta_or_pretty,
  file = "data_odds_ratios/meta_main_coefs_overall_pretty.csv"
  )

ggsave(
  plot     = p_meta_or,
  filename = "meta_main_coefs_overall.png",
  path     = "plots",
  width    = 10,
  height   = 10
)
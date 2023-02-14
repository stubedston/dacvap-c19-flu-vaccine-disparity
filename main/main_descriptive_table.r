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

# TO BE UPDATED WITH POOLED DATA

d_pool_desc <- read.csv("data_descriptive_tables/wales/wales_main_descriptive.csv")


# ==========================================================================
# lkps
# ==========================================================================

lkp_xvar_table <- c(
    "Total"                          = "total",
    "IMD (2019) quintile"            = "wimd2019_quintile",
    "Age"                            = "age_cat",
    "Sex"                            = "gndr_cd",
    "Ethnicity"                      = "ethn_cat",
    "BMI"                            = "bmi_cat",
    "Household composition"          = "hh_cat",
    "Urban/rural class"              = "urban_rural_class",
    "No. of clinical conditions"     = "num_clinical_conditions_cat",
    "Health board"                   = "health_board"
)


lkp_model_type <- c(
    "Adjusted"   = "adj",
    "Unadjusted" = "unadj",
    "Reference"  = "ref"
)

lkp_xlbls <- c("Total",
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
            "18-50",
            "50-65",
            "65-80",
            "80+",
            # BMI
            "<18.5",
            "18.5-24.9",
            "25-29.9",
            "30-39.9",
            "40+",
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
            # clin conditions
            "No conditions",
            "1 condition",
            "2 conditions",
            "3 conditions",
            "4+ conditions",
            # Rurality
            "Urban",
            "Rural")
# ==========================================================================
# Make pretty table
# ==========================================================================

d_pool_desc <-
d_pool_desc %>% 
    mutate(
        xlbl = fct_explicit_na(xlbl, "Total"),
        xlbl = xlbl %>%
                fct_relevel( # orders based numerically or by population
                lkp_xlbls
            )
    ) %>%
    select(
        xvar,
        xlbl,
        n,
        n_c19_complete,
        perc_c19_complete,
        n_flu_complete,
        perc_flu_complete
    ) %>%
    arrange(xlbl)

d_pool_desc_pretty <-
    d_pool_desc %>% mutate(
        Variable = factor(xvar, lkp_xvar_table, names(lkp_xvar_table)),
        Category = xlbl,
        `Total n` = n,
        `Complete COVID-19 vaccination n (%)` = paste0(n_c19_complete, " (", perc_c19_complete, "%)"),
        `Complete Influenza vaccination n (%)` = paste0(n_flu_complete, " (", perc_flu_complete, "%)")
        ) %>%
    select(
        Variable,
        Category,
        `Total n`,
        `Complete COVID-19 vaccination n (%)`,
        `Complete Influenza vaccination n (%)`
        )

# ==========================================================================
# Save plot
# ==========================================================================
cat("Saving...\n")

write_csv(
  d_pool_desc_pretty,
  file = "data_descriptive_tables/pool_main_descriptive_pretty.csv"
  )

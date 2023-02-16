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

d_wales_desc <- read.csv("data_descriptive_tables/wales/wales_main_descriptive.csv")
d_wales_desc <- d_wales_desc[1:37, c(1, 2, 5, 7, 9)]
colnames(d_wales_desc) <- c("xvar", "xlbl", "n_wales", "n_c19_complete_wales", "n_flu_complete_wales")

d_england_desc <- read.csv("data_descriptive_tables/england/england_main_descriptive.csv")
d_england_desc <- d_england_desc[2:nrow(d_england_desc),1:5]
colnames(d_england_desc) <- c("xvar", "xlbl", "n_c19_complete_england", "n_flu_complete_england", "n_england")

# ==========================================================================
# Process england data
# ==========================================================================

total_england <- tribble(~xvar, ~xlbl, ~n_c19_complete_england, ~n_flu_complete_england, ~n_england,
  "total",
  "Total",
  d_england_desc %>%
    filter(xvar == "IMDQuintile") %>%
    mutate(n = as.numeric(n_c19_complete_england)) %>%
    select(n) %>%
    sum(),
    d_england_desc %>%
    filter(xvar == "IMDQuintile") %>%
    mutate(n = as.numeric(n_flu_complete_england)) %>%
    select(n) %>%
    sum(),
  d_england_desc %>%
    filter(xvar == "IMDQuintile") %>%
    mutate(n = as.numeric(n_england)) %>%
    select(n) %>%
    sum()
    )

d_england_desc <-
d_england_desc %>% mutate(
  xlbl = case_when(
    xvar == "bmi_cat"         & xlbl == "missing" ~ "(BMI missing)",
    xvar == "bmi_cat"         & xlbl == "40+"     ~ "40.0+",
    xvar == "Ethnicity"       & xlbl == "missing" ~ "(Ethnicity missing)",
    xvar == "Ethnicity"       & xlbl == "M"       ~ "Mixed",
    xvar == "Ethnicity"       & xlbl == "W"       ~ "White",
    xvar == "Ethnicity"       & xlbl == "A"       ~ "Asian",
    xvar == "Ethnicity"       & xlbl == "B"       ~ "Black",
    xvar == "Ethnicity"       & xlbl == "O"       ~ "Other",
    xvar == "household_n_cat" & xlbl == "1"       ~ "Alone",
    xvar == "household_n_cat" & xlbl == "2"       ~ "2 members",
    xvar == "household_n_cat" & xlbl == "3"       ~ "3 members",
    xvar == "household_n_cat" & xlbl == "4"       ~ "4 members",
    xvar == "household_n_cat" & xlbl == "5"       ~ "5 members",
    xvar == "household_n_cat" & xlbl == "06-Oct"  ~ "6-10 members",
    xvar == "household_n_cat" & xlbl == "11+"     ~ "11+ members",
    xvar == "IMDQuintile"     & xlbl == "1"       ~ "1st (Most deprived)",
    xvar == "IMDQuintile"     & xlbl == "2"       ~ "2nd",
    xvar == "IMDQuintile"     & xlbl == "3"       ~ "3rd",
    xvar == "IMDQuintile"     & xlbl == "4"       ~ "4th",
    xvar == "IMDQuintile"     & xlbl == "5"       ~ "5th (Least deprived)",
    xvar == "nrg_cat"         & xlbl == "0"       ~ "No conditions",
    xvar == "nrg_cat"         & xlbl == "1"       ~ "1 condition",
    xvar == "nrg_cat"         & xlbl == "2"       ~ "2 conditions",
    xvar == "nrg_cat"         & xlbl == "3"       ~ "3 conditions",
    xvar == "nrg_cat"         & xlbl == "4+"       ~ "4+ conditions",
    xvar == "Sex"             & xlbl == "F"       ~ "Female",
    xvar == "Sex"             & xlbl == "M"       ~ "Male",
    TRUE                                          ~ xlbl
    ),
  xvar = case_when(
    xvar == "Ethnicity"       ~ "ethn_cat",
    xvar == "household_n_cat" ~ "hh_cat",
    xvar == "IMDQuintile"     ~ "wimd2019_quintile",
    xvar == "nrg_cat"         ~ "num_clinical_conditions_cat",
    xvar == "Sex"             ~ "gndr_cd",
    xvar == "UrbanRural"      ~ "urban_rural_class",
    TRUE                      ~ xvar
    )
  ) %>% rbind(total_england)

# ==========================================================================
# Process wales data
# ==========================================================================

d_wales_desc <-
d_wales_desc %>% mutate(
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
# Pool data
# ==========================================================================

d_pool_desc <- full_join(d_england_desc, d_wales_desc) %>% mutate(
  n_c19_complete = replace_na(as.numeric(n_c19_complete_england), 0) + replace_na(as.numeric(n_c19_complete_wales), 0),
  n_flu_complete = replace_na(as.numeric(n_flu_complete_england), 0) + replace_na(as.numeric(n_flu_complete_wales), 0),
  n = replace_na(as.numeric(n_england), 0) + replace_na(as.numeric(n_wales), 0),
  perc_c19_complete = format(round(n_c19_complete*100/n, 1), nsmall = 1),
  perc_flu_complete = format(round(n_flu_complete*100/n, 1), nsmall = 1)
  )

# ==========================================================================
# lkps
# ==========================================================================

lkp_xvar_table <- c(
    "Total"                          = "total",
    "IMD quintile"                   = "wimd2019_quintile",
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

lkp_xlbls <- c(
    "Total",
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

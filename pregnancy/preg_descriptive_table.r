# ==========================================================================
# Clear environment
# ==========================================================================
cat("Clear and load!\n")

if (Sys.info()["user"] == "william.midgley") {
  cat("Hi Will!\n")
  setwd("~/dcp02_covid_v_flu_coverage_disparities/dacvap-c19-flu-vaccine-disparity/pregnancy")
} else if (Sys.info()["user"] == "Stuart.Bedston") {
  cat("Hi Stu!\n")
  setwd("~/Projects/dacvap-c19-flu-vaccine-disparity/pregnancy")
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

d_pool_desc_c19 <- read.csv("data_descriptive_tables/pool_preg_descriptive_c19.csv")
d_pool_desc_flu <- read.csv("data_descriptive_tables/pool_preg_descriptive_flu.csv")

d_pool_desc_c19 <- d_pool_desc_c19[-1, c(1, 6, 11:13)]
d_pool_desc_flu <- d_pool_desc_flu[-1, c(1, 6, 11:13)]

colnames(d_pool_desc_c19) <- c("xvar", "xlbl", "n_c19_complete", "n", "perc_c19_complete")
colnames(d_pool_desc_flu) <- c("xvar", "xlbl", "n_flu_complete", "n", "perc_flu_complete")


total_c19 <- tribble(~xvar, ~xlbl, ~n_c19_complete, ~n,
  "total",
  "Total",
  d_pool_desc_c19 %>%
    filter(xvar == "IMDQuintile") %>%
    mutate(n = as.numeric(n_c19_complete)) %>%
    select(n) %>%
    sum(),
  d_pool_desc_c19 %>%
    filter(xvar == "IMDQuintile") %>%
    mutate(n = as.numeric(n)) %>%
    select(n) %>%
    sum()
    ) %>% mutate(
      perc_c19_complete = round(n_c19_complete*100/n, 1)
    )

total_flu <- tribble(~xvar, ~xlbl, ~n_flu_complete, ~n,
  "total",
  "Total",
  d_pool_desc_flu %>%
    filter(xvar == "IMDQuintile") %>%
    mutate(n = as.numeric(n_flu_complete)) %>%
    select(n) %>%
    sum(),
  d_pool_desc_flu %>%
    filter(xvar == "IMDQuintile") %>%
    mutate(n = as.numeric(n)) %>%
    select(n) %>%
    sum()
    ) %>% mutate(
      perc_flu_complete = round(n_flu_complete*100/n, 1)
    )

d_pool_desc_c19 <-
  d_pool_desc_c19 %>% rbind(
    total_c19
    )

d_pool_desc_flu <-
  d_pool_desc_flu %>% rbind(
    total_flu
    )

# ==========================================================================
# lkps
# ==========================================================================

lkp_xvar_table <- c(
    "Total"                          = "total",
    "IMD quintile"                   = "IMDQuintile",
    "Age"                            = "age_cat",
    "Sex"                            = "gndr_cd",
    "Ethnicity"                      = "Ethnicity",
    "BMI"                            = "bmi_cat",
    "Household composition"          = "household_n_cat",
    "Urban/rural class"              = "UrbanRural",
    "No. of clinical conditions"     = "nrg_cat"
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
            # age
            "18-24",
            "25-29",
            "30-34",
            "35-39",
            "40-49",
            # BMI
            "<18.5",
            "18.5-24.9",
            "25-29.9",
            "30-39.9",
            "40+",
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

d_pool_desc_c19 <-
d_pool_desc_c19 %>% 
    mutate(
      xlbl = paste0(xlbl),
      xlbl = case_when(
        xlbl == "(Missing)" ~ "(Ethnicity missing)",
        xlbl == "missing"   ~ "(BMI missing)",
        TRUE                ~ xlbl
        ) %>% factor(levels = lkp_xlbls),
      perc_c19_complete = format(as.numeric(perc_c19_complete), nsmall = 1)
    ) %>%
    select(
      xvar,
      xlbl,
      n,
      n_c19_complete,
      perc_c19_complete,
    ) %>%
    arrange(xlbl)

d_pool_desc_c19_pretty <-
    d_pool_desc_c19 %>% mutate(
        Variable = factor(xvar, lkp_xvar_table, names(lkp_xvar_table)),
        Category = xlbl,
        `Total eligible for COVID-19 vaccination n` = n,
        `Taken COVID-19 vaccination n (%)` = paste0(n_c19_complete, " (", perc_c19_complete, "%)")
        ) %>%
    select(
        Variable,
        Category,
        `Total eligible for COVID-19 vaccination n`,
        `Taken COVID-19 vaccination n (%)`
        )


d_pool_desc_flu <-
d_pool_desc_flu %>% 
    mutate(
      xlbl = paste0(xlbl),
      xlbl = case_when(
        xlbl == "(Missing)" ~ "(Ethnicity missing)",
        xlbl == "missing"   ~ "(BMI missing)",
        TRUE                ~ xlbl
        ) %>% factor(levels = lkp_xlbls),
      perc_flu_complete = format(as.numeric(perc_flu_complete), nsmall = 1)
    ) %>%
    select(
      xvar,
      xlbl,
      n,
      n_flu_complete,
      perc_flu_complete
    ) %>%
    arrange(xlbl)

d_pool_desc_flu_pretty <-
    d_pool_desc_flu %>% mutate(
        Variable = factor(xvar, lkp_xvar_table, names(lkp_xvar_table)),
        Category = xlbl,
        `Total eligible for Influenza vaccination n` = n,
        `Taken Influenza vaccination n (%)` = paste0(n_flu_complete, " (", perc_flu_complete, "%)")
        ) %>%
    select(
        Variable,
        Category,
        `Total eligible for Influenza vaccination n`,
        `Taken Influenza vaccination n (%)`
        )

d_pool_desc_pretty <- full_join(d_pool_desc_c19_pretty, d_pool_desc_flu_pretty)

# ==========================================================================
# Save plot
# ==========================================================================
cat("Saving...\n")

write_csv(
  d_pool_desc_pretty,
  file = "data_descriptive_tables/pool_preg_descriptive_pretty.csv"
  )

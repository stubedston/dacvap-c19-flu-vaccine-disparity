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

cbPalette <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#56B4E9", "#D55E00", "#CC79A7")

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

d_pool_or <- read.csv("data_odds_ratios/wales/wales_preg_coefs_overall.csv")
d_pool_desc <- read.csv("data_descriptive_tables/wales/wales_preg_descriptive.csv")
d_pool_desc <- d_pool_desc[2:nrow(d_pool_desc),1:3]

d_refs <- d_pool_desc[!(d_pool_desc[,"xlbl"] %in% d_pool_or[,"xlbl"]),] %>%
    mutate(
        or = 1,
        or_low = 1,
        or_high = 1,
        model_type = "ref",
        vacc = "c19"
    )

d_refs <- d_refs %>% rbind(
    d_refs %>% mutate(
        vacc = "flu"
        )
    )

d_pool_or <- bind_rows(d_pool_or, d_refs)

# ==========================================================================
# lkps
# ==========================================================================

lkp_xvar_table <- c(
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

lkp_xvar <- c(
    "IMD (2019)\nquintile"          = "wimd2019_quintile",
    "Age"                            = "age_cat",
    "Sex"                            = "gndr_cd",
    "Ethnicity"                      = "ethn_cat",
    "BMI"                            = "bmi_cat",
    "Household\ncomposition"         = "hh_cat",
    "Urban/rural\nclass"             = "urban_rural_class",
    "No. of clinical\nconditions"    = "num_clinical_conditions_cat",
    "Health board"                   = "health_board"
)


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

d_pool_or_pretty <-
d_pool_or %>% 
    mutate(
    xlbl = xlbl %>% fct_relevel( # orders based numerically or by population
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
        "Rural"
        )
    ) %>%
    select(
        xvar,
        xlbl,
        vacc,
        model_type,
        or,
        or_low,
        or_high,
        p
    ) %>%
    arrange(xlbl) %>%
    filter(xvar != "total flu eligable")

d_pool_or_pretty <-
    d_pool_or_pretty %>% mutate(
        xvar = factor(xvar, lkp_xvar_table, names(lkp_xvar_table)),
        `OR (95% CI)` = case_when(
            model_type == "ref" ~ "1",
            TRUE ~ paste0(or, " (", or_low, "-", or_high, ")")
            ),
        p = case_when(
            model_type == "ref" ~ "0",
            p < 0.0001 ~ "<0.0001",
            TRUE ~ paste0(p))
        ) %>%
    select(
        xvar,
        xlbl,
        vacc,
        model_type,
        `OR (95% CI)`,
        p
        )

d_pool_or_pretty_adj <-
    d_pool_or_pretty %>%
        filter(model_type != "unadj") %>%
        select(-model_type)
colnames(d_pool_or_pretty_adj) <- c("Variable", "Category", "vacc", "Adjusted OR (95% CI)", "Adjusted p-value")

d_pool_or_pretty_unadj <-
    d_pool_or_pretty %>%
        filter(model_type != "adj") %>%
        select(-model_type)
colnames(d_pool_or_pretty_unadj) <- c("Variable", "Category", "vacc", "Unadjusted OR (95% CI)", "Undjusted p-value")

d_pool_or_pretty <-
    full_join(
        d_pool_or_pretty_adj,
        d_pool_or_pretty_unadj,
        by = c("Variable", "Category", "vacc")
    )

d_pool_or_pretty_c19 <- d_pool_or_pretty %>% filter(vacc == "c19") %>% select(-vacc) %>% distinct()
d_pool_or_pretty_flu <- d_pool_or_pretty %>% filter(vacc == "flu") %>% select(-vacc) %>% distinct()


# ==========================================================================
# Plot
# ==========================================================================

p_pool_or <-
d_pool_or %>%
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
    scale_colour_manual("Model Type", values = cbPalette) +
    coord_cartesian(xlim = c(0, 3)) +
    theme(
        axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)
    ) +
    ggtitle("ORs for vaccine uptake by type")

p_pool_or

# ==========================================================================
# Save plot
# ==========================================================================
cat("Saving...\n")

write_csv(
  d_pool_or_pretty_c19,
  file = "data_odds_ratios/pool_preg_coefs_overall_c19_pretty.csv"
  )

write_csv(
  d_pool_or_pretty_flu,
  file = "data_odds_ratios/pool_preg_coefs_overall_flu_pretty.csv"
  )


ggsave(
  plot     = p_pool_or,
  filename = "pool_preg_coefs.png",
  path     = "plots",
  width    = 10,
  height   = 10
)

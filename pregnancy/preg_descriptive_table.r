# ==========================================================================
# Clear environment
# ==========================================================================
cat("clear\n")

# set working directory
if (Sys.info()["user"] == "william.midgley") {
  cat("Hi Will!\n")
  setwd("~/dcp02_covid_v_flu_coverage_disparities/dacvap-c19-flu-vaccine-disparity/pregnancy")
} else if (Sys.info()["user"] == "Stuart.Bedston") {
  cat("Hi Stu!\n")
  setwd("~/Projects/dacvap-c19-flu-vaccine-disparity/pregnancy")
} else {
  cat("Is that you, Utkarsh?! Add your name here\n") 
}

# delete everything in R's environment
rm(list = ls())

# unload any loaded packages
if (!is.null(sessionInfo()$otherPkgs)) {
  suppressWarnings(invisible(
    lapply(
      paste0('package:', names(sessionInfo()$otherPkgs)),
      detach,
      character.only = TRUE,
      unload = TRUE
    )
  ))
}

# load packages
pkgs <- c(
  "tidyverse",
  "beepr",
  "eulerr",
  "grid",
  "janitor",
  "patchwork",
  "readr",
  "readxl",
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

rm(pkg, pkgs)


# ==========================================================================
# Lookup tables
# ==========================================================================
cat("lookups\n")

lkp_xvar_xlbl <-
  read_xlsx("lookup_xvar_xlbl.xlsx") %>% 
  mutate(
    new_name = fct_inorder(new_name)
  )

lkp_model <- c(
  "Adjusted"   = "adj",
  "Unadjusted" = "unadj",
  "Reference"  = "ref"
)


# ==========================================================================
# Process Covid-19 
# ==========================================================================
cat("c19\n")

# load pooled counts
d_c19_desc <-
  read_csv(
    file = "data_descriptive_tables/pool_preg_descriptive_c19.csv",
    skip = 1,
    name_repair = make_clean_names,
    show_col_types = FALSE
  ) %>% 
  select(
    name,
    value,
    c19_total_n = total_n_2,
    c19_event_n = covid_event_2
  )

# calc total
d_c19_total <-
  d_c19_desc %>% 
  filter(name == "age_cat") %>% 
  summarise(
    c19_total_n = sum(c19_total_n),
    c19_event_n = sum(c19_event_n)
  ) %>% 
  mutate(
    name = "total",
    value = "total"
  ) %>% 
  select(
    name,
    value,
    c19_total_n,
    c19_event_n
  )

# add total and format
d_c19_desc <-
  bind_rows(
    d_c19_total,
    d_c19_desc
  ) %>% 
  # clean variable names and category labels
  left_join(
    lkp_xvar_xlbl,
    join_by(name, value)
  ) %>% 
  select(
    xvar = new_name,
    xlbl = new_value,
    c19_total_n,
    c19_event_n
  ) %>% 
  # round numbers to nearest 10
  mutate(
    c19_total_n = round_half_up(c19_total_n, -1),
    c19_event_n = round_half_up(c19_event_n, -1)
  ) %>% 
  # add percentage
  group_by(xvar) %>% 
  mutate(
    c19_total_p = c19_total_n / sum(c19_total_n),
    c19_event_p = c19_event_n / sum(c19_event_n)
  ) %>% 
  ungroup() %>% 
  # combine columns
  mutate(
    c19_total_np = str_glue(
      "{n} ({p})",
      n = format(c19_total_n, big.mark = ",", trim = TRUE),
      p = percent(c19_total_p, accuracy = 0.1)
    ),
    c19_event_np = str_glue(
      "{n} ({p})",
      n = format(c19_event_n, big.mark = ",", trim = TRUE),
      p = percent(c19_event_p, accuracy = 0.1)
    )
  ) %>% 
  # final select
  select(
    xvar,
    xlbl,
    c19_total_np,
    c19_event_np
  )


# ==========================================================================
# Process flu
# ==========================================================================
cat("flu\n")

# load pooled counts
d_flu_desc <-
  read_csv(
    file = "data_descriptive_tables/pool_preg_descriptive_flu.csv",
    skip = 1,
    name_repair = make_clean_names,
    show_col_types = FALSE
  ) %>% 
  select(
    name,
    value,
    flu_total_n = total_n_2,
    flu_event_n = flu_event_2
  )

# calc total
d_flu_total <-
  d_flu_desc %>% 
  filter(name == "age_cat") %>% 
  summarise(
    flu_total_n = sum(flu_total_n),
    flu_event_n = sum(flu_event_n)
  ) %>% 
  mutate(
    name = "total",
    value = "total"
  ) %>% 
  select(
    name,
    value,
    flu_total_n,
    flu_event_n
  )

# add total and format
d_flu_desc <-
  bind_rows(
    d_flu_total,
    d_flu_desc
  ) %>% 
  # clean variable names and category labels
  left_join(
    lkp_xvar_xlbl,
    join_by(name, value)
  ) %>% 
  select(
    xvar = new_name,
    xlbl = new_value,
    flu_total_n,
    flu_event_n
  ) %>% 
  # round numbers to nearest 10
  mutate(
    flu_total_n = round_half_up(flu_total_n, -1),
    flu_event_n = round_half_up(flu_event_n, -1)
  ) %>% 
  # add percentage
  group_by(xvar) %>% 
  mutate(
    flu_total_p = flu_total_n / sum(flu_total_n),
    flu_event_p = flu_event_n / sum(flu_event_n)
  ) %>% 
  ungroup() %>% 
  # combine columns
  mutate(
    flu_total_np = str_glue(
      "{n} ({p})",
      n = format(flu_total_n, big.mark = ",", trim = TRUE),
      p = percent(flu_total_p, accuracy = 0.1)
    ),
    flu_event_np = str_glue(
      "{n} ({p})",
      n = format(flu_event_n, big.mark = ",", trim = TRUE),
      p = percent(flu_event_p, accuracy = 0.1)
    )
  ) %>% 
  # final select
  select(
    xvar,
    xlbl,
    flu_total_np,
    flu_event_np
  )


# ==========================================================================
# Combine
# ==========================================================================
cat("combine\n")

d_c19_flu_desc <- full_join(d_c19_desc, d_flu_desc, join_by(xvar, xlbl))


# ============================================================================
# Save
# ============================================================================
cat("save\n")

write_csv(
  d_c19_flu_desc,
  file = "data_descriptive_tables/pool_preg_descriptive_pretty.csv"
)

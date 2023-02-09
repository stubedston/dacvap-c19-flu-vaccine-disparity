cat("clearing workspace and loading stuff\n")

# try to close open connection =================================================

tryCatch(
    expr = {
        sail_close(con)
    },
    error = function(e) {invisible()}
)

tryCatch(
    expr = {
        sail_close(conn)
    },
    error = function(e) {invisible()}
)

# clear workspace ==============================================================

rm(list = ls())
gc()

# set working directory ========================================================

usr <- Sys.info()["user"]

if (usr == "*******")
    setwd("*******")

if (usr == "*******")
    setwd("*******")

rm(usr)

# options ======================================================================

options(
    dplyr.summarise.inform = FALSE,
    readr.show_col_types = FALSE,
    lubridate.week.start = 1 # Monday
)

# portable library =============================================================

# if we are using a portable version of R, use only the packages within its
# own library, i.e. ignore user library
if (grepl(x = R.home(), pattern = "R-Portable")) {
    .libPaths(paste0(R.home(), "/library"))
}

# load packages ================================================================

pkgs <- c(
    # load first so they dont take over other packages
    "purrr",
    # alphabetical
    "assertr",
    "beepr",
    "broom",
    "dplyr",
    "eulerr",
    "forcats",
    "ggplot2",
    "ggstance",
    "grid",
    "gtsummary",
    "knitr",
    "kableExtra",
    "mice",
    "janitor",
    "lubridate",
    "patchwork",
    "qs",
    "rlang",
    "rmarkdown",
    "sailr",
    "scales",
    "stringr",
    "survminer",
    "readr",
    "survival",
    "tibble",
    "tidyr",
    "tidyverse"
)

for (pkg in pkgs) {
    suppressWarnings(
        suppressPackageStartupMessages(
            library(pkg, character.only = TRUE)
        )
    )
}

# custom functions =============================================================

quotemeta <- function(string) {
    str_replace_all(string, "(\\W)", "\\\\\\1")
}

s_drive <- function(...) {
    str_c("*******", ...)
}

kable_pretty <- function(
    kable_input,
    bootstrap_options = "striped",
    full_width = FALSE,
    position = "center",
    ...
) {
    kable_styling(
        kable_input = kable_input,
        bootstrap_options = bootstrap_options,
        full_width = full_width,
        position = position,
        ...
    )
}

# plot dimensions ==============================================================

p_width  <- 5.20
p_height <- 8.75

# useful dates =================================================================

useful_dates <- list(
    study_start             = ymd("2020-09-01"),
    adult_vacc_start        = ymd("2020-12-08"),
    adult_booster_start     = ymd("2021-09-20"),
    cohort_end              = ymd("2022-04-30"),
    study_end               = ymd("2022-01-31")
)

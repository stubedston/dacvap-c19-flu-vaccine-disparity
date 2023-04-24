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
  "beepr",
  "dplyr",
  "eulerr",
  "forcats",
  "ggplot2",
  "grid",
  "janitor",
  "lubridate",
  "patchwork",
  "readr",
  "readxl",
  "scales",
  "stringr",
  "tidyr"
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
# Lookups 
# ==========================================================================

cbPalette <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)

cbPalette2 <- c(
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442"
)


# ==========================================================================
# Process England data
# ==========================================================================
cat("Load England\n")

clean_england <- function(data) { # nolint
  data %>%
    mutate(
      vacc_seq =
        case_when(
          vacc_seq == "FirstVaccinationDoseDate" ~ "dose1",
          vacc_seq == "SecondVaccinationDoseDate" ~ "dose2",
          vacc_seq == "ThirdVaccinationDoseDate" ~ "dose3",
          vacc_seq == "FluVaccinationDate202021" ~ "2020",
          vacc_seq == "FluVaccinationDate202122" ~ "2021"
        ),
      vacc_type = 
        case_when(
          vacc_type == "COVID-19" ~ "c19",
          vacc_type == "Influenza" ~ "flu"
          ),
      n_england = n,
      vacc_date = ymd(vacc_date) + days(1)
      ) %>%
      select(
        vacc_seq,
        vacc_date,
        n_england,
        vacc_type
    )
}

d_c19_eng <-
  read.csv("data_weekly_vacc/england/england_preg_weekly_vacc.csv") %>% 
  clean_england() %>%
  filter(vacc_type == "c19")

d_flu_eng <-
  read.csv("data_weekly_vacc/england/england_preg_weekly_vacc.csv") %>%
  clean_england() %>%
  filter(vacc_type == "flu")


# ==========================================================================
# Process Wales data
# ==========================================================================
cat("Load Wales\n")

clean_wales <- function(data) {
  data %>% mutate(
    vacc_seq = case_when(
      vacc_type == "flu" ~ paste0("20",vacc_seq),
      vacc_type == "c19" ~ paste(vacc_seq)
      ),
    vacc_date = ymd(vacc_date),
    n_wales = n
    ) %>%
    select(-n)
}

d_c19_wales <-
  read.csv("data_weekly_vacc/wales/wales_preg_weekly_vacc_c19.csv") %>% 
  clean_wales()

d_flu_wales <-
  read.csv("data_weekly_vacc/wales/wales_preg_weekly_vacc_flu.csv") %>% 
  clean_wales()


# ==========================================================================
# Pool England and Wales
# ==========================================================================
cat("Pooling data...\n")

pool <- function(england, wales) {
  full_join(england, wales, join_by(vacc_seq, vacc_date, vacc_type)) %>%
  as_tibble() %>% 
  mutate(
    vacc_seq  = factor(vacc_seq),
    vacc_seq  = fct_rev(vacc_seq),
    n_wales   = replace_na(n_wales, 0),
    n_england = replace_na(n_england, 0),
    n         = n_wales + n_england
  )
}

d_c19_pool <- pool(d_c19_eng, d_c19_wales)
d_flu_pool <- pool(d_flu_eng, d_flu_wales)


# ==========================================================================
# Plotting graphs
# ==========================================================================
cat("Time to start plotting!\n")


x_limits <- c(
  ymd("2020-09-01"),
  ymd("2022-03-31")
)

y_limits <- c(0, 1100)

my_theme <- function() {
  theme_bw(base_size = 10) +
  theme(
    axis.title.x         = element_blank(),
    legend.title         = element_blank(),
    legend.position      = c(0.99, 0.97),
    legend.justification = c(1, 1),
    legend.key.height    = unit(1, "lines"),
    legend.key.width     = unit(1, "lines"),
    legend.margin        = margin(t=-0.25,l=0.05,b=0.0,r=0.05, unit='cm'),
    panel.grid.minor.x   = element_blank(),
    panel.grid.minor.y   = element_blank(),
    plot.title           = element_text(size = 10, face = "bold"),
    plot.title.position  = "plot"
  )
}


p_c19 <-
  d_c19_pool %>%
  ggplot(aes(
    x    = vacc_date,
    y    = n,
    fill = vacc_seq
  )) +
  geom_col() +
  scale_x_date(
    limits = x_limits,
    date_breaks = "2 months",
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    name = "Number per week",
    limits = y_limits,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  scale_fill_manual(
    values = cbPalette[3:1],
    labels = c("Dose 3", "Dose 2", "Dose 1"),
    guide = guide_legend(reverse = TRUE)
  ) +
  ggtitle("(a) COVID-19 vaccination") +
  my_theme()

p_flu <-
  d_flu_pool %>%
  ggplot(aes(
    x = vacc_date,
    y = n,
    fill = vacc_seq
  )) +
  geom_col() +
  scale_x_date(
    limits = x_limits,
    date_breaks = "2 months",
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    name =  "Number per week",
    limits = y_limits,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  scale_fill_manual(
    values = cbPalette2,
    labels = c("Winter 2020/21", "Winter 2021/22"),
    guide = guide_legend(reverse = TRUE)
  ) +
  ggtitle("(b) Influenza vaccination") +
  my_theme()

p_c19_flu <-
  p_c19 +
  p_flu +
  plot_layout(
    ncol = 1
  )

print(p_c19_flu)

# ==========================================================================
# Save plots
# ==========================================================================
cat("saving...\n")

ggsave(
  plot     = p_c19_flu,
  filename = "plots/pool_main_weekly_vacc.png",
  width    = 5.2,
  height   = 5
)

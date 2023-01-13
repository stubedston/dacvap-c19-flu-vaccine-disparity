
# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

rm(list = ls())

if (Sys.info()["user"] == "william.midgley") {
  setwd("C:/Users/william.midgley/Documents/dcp02_covid_v_flu_coverage_disparities/dacvap-c19-flu-vaccine-disparity")
} else if (Sys.info()["user"] == "Stuart.Bedston") {
  cat("Put your wd in, Stu!\n")
} else {
  cat("Is that you, Utkarsh? Add your name here\n") 
}

library(tidyverse)
library(patchwork)
library(grid)
library(eulerr)
library(lubridate)
library(scales)

# England
d_weekly_vacc_england_main <- read.csv("d-weekly-vacc/vacc_uptake_full_cohort.csv")
d_weekly_vacc_england_preg <- read.csv("d-weekly-vacc/vacc_uptake_preg_cohort.csv")
# Wales
d_weekly_vacc_wales_main_c19 <- read.csv("d-weekly-vacc/d_c19_vacc_week.csv")
d_weekly_vacc_wales_main_flu <- read.csv("d-weekly-vacc/d_flu_vacc_week.csv")
d_weekly_vacc_wales_preg_c19 <- read.csv("d-weekly-vacc/d_c19_vacc_week_preg.csv")
d_weekly_vacc_wales_preg_flu <- read.csv("d-weekly-vacc/d_flu_vacc_week_preg.csv")

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette2 <- c("#0072B2", "#D55E00", "#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#F0E442")


# ==========================================================================
# Process England data
# ==========================================================================
cat("processing...\n")

process.england <- function(data) { # nolint
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

d_weekly_vacc_england_main_c19 <- process.england(d_weekly_vacc_england_main) %>% filter(vacc_type == "c19")
d_weekly_vacc_england_main_flu <- process.england(d_weekly_vacc_england_main) %>% filter(vacc_type == "flu")
d_weekly_vacc_england_preg_c19 <- process.england(d_weekly_vacc_england_preg) %>% filter(vacc_type == "c19")
d_weekly_vacc_england_preg_flu <- process.england(d_weekly_vacc_england_preg) %>% filter(vacc_type == "flu")

# ==========================================================================
# Process Wales data
# ==========================================================================

process.wales <- function(data) {
data %>% mutate(
	vacc_seq = case_when(
		vacc_type == "flu" ~ paste0("20",vacc_seq),
		TRUE ~ paste(vacc_seq)
		),
	vacc_date = ymd(vacc_date)
	)
}

d_weekly_vacc_wales_main_c19 <- process.wales(d_weekly_vacc_wales_main_c19)
d_weekly_vacc_wales_main_flu <- process.wales(d_weekly_vacc_wales_main_flu)
d_weekly_vacc_wales_preg_c19 <- process.wales(d_weekly_vacc_wales_preg_c19)
d_weekly_vacc_wales_preg_flu <- process.wales(d_weekly_vacc_wales_preg_flu)

# ==========================================================================
# Pool England and Wales
# ==========================================================================
cat("pooling data...\n")
pool.data <- function(england, wales) {
	full_join(england, wales) %>%
	mutate(
		n = replace_na(n, 0),
		n_england = replace_na(n_england, 0),
		n = n + n_england
	) %>%
	select(-n_england)
}

d_weekly_vacc_pooled_main_c19 <- pool.data(d_weekly_vacc_england_main_c19, d_weekly_vacc_wales_main_c19)
d_weekly_vacc_pooled_main_flu <- pool.data(d_weekly_vacc_england_main_flu, d_weekly_vacc_wales_main_flu)
d_weekly_vacc_pooled_preg_c19 <- pool.data(d_weekly_vacc_england_preg_c19, d_weekly_vacc_wales_preg_c19)
d_weekly_vacc_pooled_preg_flu <- pool.data(d_weekly_vacc_england_preg_flu, d_weekly_vacc_wales_preg_flu)

# ==========================================================================
# Plotting graphs
# ==========================================================================
cat("Time to start plotting!\n")


x_limits <- c(
  ymd("2020-09-01"),
  ymd("2022-03-31")
)

x_breaks <- seq(
  from = ymd('2020-09-01'),
  to   = ymd('2022-03-31'),
  by   = "6 months"
)

y_limits <- c(0, 500000)
y_limits_preg <- c(0, 23000) 


# ==========================================================================
# Main cohort
# ==========================================================================

p_c19_main <-
  d_weekly_vacc_pooled_main_c19 %>%
  ggplot(aes(
    x = vacc_date,
    y = n,
    fill = vacc_seq
  )) +
  geom_col() +
  scale_x_date(
    name = "",
    limits = x_limits,
    breaks = x_breaks,
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    name = "Number per week",
    limits = y_limits,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  labs(
    fill = "Vaccine Dose"
  ) +
  scale_fill_manual(
    values = cbPalette,
    labels = c("Dose 1", "Dose 2", "Dose 3")
  ) +
  ggtitle(
    "COVID-19 vaccinations"
  )

p_flu_main <-
  d_weekly_vacc_pooled_main_flu %>%
  ggplot(aes(
    x = vacc_date,
    y = n,
    fill = vacc_seq
  )) +
  geom_col() +
  scale_x_date(
    name = "",
    limits = x_limits,
    breaks = x_breaks,
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    name = "Number per week",
    limits = y_limits,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  labs(
    fill = "Vaccine Dose"
  ) +
  scale_fill_manual(
    values = cbPalette2,
    labels = c("Winter 2020", "Winter 2021")
  ) +
  ggtitle(
    "Influenza vaccinations"
  )

p_main <- p_c19_main / p_flu_main


# ==========================================================================
# Pregnant cohort
# ==========================================================================


p_c19_preg <-
  d_weekly_vacc_pooled_preg_c19 %>%
  ggplot(aes(
    x = vacc_date,
    y = n,
    fill = vacc_seq
  )) +
  geom_col() +
  scale_x_date(
    name = "",
    limits = x_limits,
    breaks = x_breaks,
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    name = "Number per week",
    limits = y_limits_preg,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  labs(
    fill = "Vaccine Dose"
  ) +
  scale_fill_manual(
    values = cbPalette,
    labels = c("Dose 1", "Dose 2", "Dose 3")
  ) +
  ggtitle(
    "COVID-19 vaccinations"
  )

p_flu_preg <-
  d_weekly_vacc_pooled_preg_flu %>%
  ggplot(aes(
    x = vacc_date,
    y = n,
    fill = vacc_seq
  )) +
  geom_col() +
  scale_x_date(
    name = "",
    limits = x_limits,
    breaks = x_breaks,
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    name = "Number per week",
    limits = y_limits_preg,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  labs(
    fill = "Vaccine Dose"
  ) +
  scale_fill_manual(
    values = cbPalette2,
    labels = c("Winter 2020", "Winter 2021")
  ) +
  ggtitle(
    "Influenza vaccinations"
  )

p_preg <- p_c19_preg / p_flu_preg

# ==========================================================================
# Save plots
# ==========================================================================
cat("saving...\n")

ggsave("p_weekly_vacc_main.png", p_main, "png", "C:/Users/william.midgley/Documents/dcp02_covid_v_flu_coverage_disparities/Results/Plots", width = 10, height = 10)
ggsave("p_weekly_vacc_preg.png", p_preg, "png", "C:/Users/william.midgley/Documents/dcp02_covid_v_flu_coverage_disparities/Results/Plots", width = 10, height = 10)
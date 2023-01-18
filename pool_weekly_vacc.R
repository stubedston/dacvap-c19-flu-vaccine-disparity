
# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

rm(list = ls())

if (Sys.info()["user"] == "william.midgley") {
  cat("Hi Will!\n")
  setwd("C:/Users/william.midgley/Documents/dcp02_covid_v_flu_coverage_disparities/dacvap-c19-flu-vaccine-disparity")
} else if (Sys.info()["user"] == "Stuart.Bedston") {
  cat("Hi Stu!\n")
  setwd("~/Projects/dacvap-c19-flu-vaccine-disparity")
} else {
  cat("Is that you, Utkarsh?! Add your name here\n") 
}

library(tidyverse)
library(patchwork)
library(grid)
library(eulerr)
library(lubridate)
library(forcats)
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
  by   = "3 months"
)

y_limits <- c(0, 500000)
y_limits_preg <- c(0, 23000) 


# ==========================================================================
# Main cohort
# ==========================================================================

my_theme <- function() {
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(1, "lines")
  )
}

p_c19_main <-
  d_weekly_vacc_pooled_main_c19 %>%
  as_tibble() %>% 
  mutate(
    vacc_seq = factor(vacc_seq),
    vacc_seq = fct_rev(vacc_seq)
  ) %>% 
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
    name = "COVID-19 vaccinations",
    limits = y_limits,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  scale_fill_manual(
    values = cbPalette[3:1],
    labels = c("Dose 3", "Dose 2", "Dose 1"),
    guide = guide_legend(reverse = TRUE)
  ) +
  my_theme() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
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
    name = "Influenza vaccinations",
    limits = y_limits,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  scale_fill_manual(
    values = cbPalette2,
    labels = c("Winter 2020/21", "Winter 2021/22")
  ) +
  my_theme()

p_main <-
  p_c19_main +
  plot_spacer() +
  p_flu_main +
  plot_layout(
    ncol = 1,
    heights = c(10, -1.5, 10)
  ) +
  plot_annotation(
    title = "(a) Main cohort",
    theme = theme(title = element_text(size = 10))
  )

p_main

# ==========================================================================
# Pregnant cohort
# ==========================================================================

p_c19_preg <-
  d_weekly_vacc_pooled_preg_c19 %>%
  as_tibble() %>% 
  mutate(
    vacc_seq = factor(vacc_seq),
    vacc_seq = fct_rev(vacc_seq)
  ) %>% 
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
    name = "COVID-19 vaccinations",
    limits = y_limits_preg,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  scale_fill_manual(
    values = cbPalette[3:1],
    labels = c("Dose 3", "Dose 2", "Dose 1"),
    guide = guide_legend(reverse = TRUE)
  ) +
  my_theme() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
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
    name =  "Influenza vaccinations",
    limits = y_limits_preg,
    breaks = breaks_pretty(),
    labels = comma
  ) +
  scale_fill_manual(
    values = cbPalette2,
    labels = c("Winter 2020/21", "Winter 2021/22")
  ) +
  my_theme() +
  theme(legend.position = "none")

p_preg <-
  p_c19_preg +
  plot_spacer() +
  p_flu_preg + 
  plot_layout(
    ncol = 1,
    heights = c(10, -1.5, 10)
  ) +
  plot_annotation(
    title = "(b) Pregnancy cohort",
    theme = theme(title = element_text(size = 10))
  )

p_preg

# ==========================================================================
# Ultimate Figure 1
# ==========================================================================

p_weekly <- wrap_elements(p_main) | wrap_elements(p_preg)

p_weekly

# ==========================================================================
# Save plots
# ==========================================================================
cat("saving...\n")

ggsave(
  plot     = p_main,
  filename = "p_weekly_vacc_main.png",
  path     = "plots",
  width    = 10,
  height   = 10
)

ggsave(
  plot     = p_preg,
  filename = "p_weekly_vacc_preg.png",
  path     = "plots",
  width    = 10,
  height   = 10
)

ggsave(
  plot     = p_weekly,
  filename = "p_weekly_vacc_both.png",
  path     = "plots",
  width    = 7.5,
  height   = 5.9
)
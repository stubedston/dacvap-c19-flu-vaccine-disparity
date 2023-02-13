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
  "janitor",
  "lubridate",
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

# England
d_weekly_england <- read.csv("data_weekly_vacc/england/england_main_weekly_vacc.csv")
# Wales
d_weekly_wales_c19 <- read.csv("data_weekly_vacc/wales/wales_main_weekly_vacc_c19.csv")
d_weekly_wales_flu <- read.csv("data_weekly_vacc/wales/wales_main_weekly_vacc_flu.csv")

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

d_weekly_england_c19 <- process.england(d_weekly_england) %>% filter(vacc_type == "c19")
d_weekly_england_flu <- process.england(d_weekly_england) %>% filter(vacc_type == "flu")

# ==========================================================================
# Process Wales data
# ==========================================================================

process.wales <- function(data) {
  data %>% mutate(
  	vacc_seq = case_when(
  		vacc_type == "flu" ~ paste0("20",vacc_seq),
  		vacc_type == "c19" ~ paste(vacc_seq)
  		),
  	vacc_date = as.Date(vacc_date),
    n_wales = n
  	) %>%
    select(-n)
}

d_weekly_wales_c19 <- process.wales(d_weekly_wales_c19)
d_weekly_wales_flu <- process.wales(d_weekly_wales_flu)

# ==========================================================================
# Pool England and Wales
# ==========================================================================
cat("Pooling data...\n")

pool.data <- function(england, wales) {
	full_join(england, wales) %>%
	mutate(
		n_wales = replace_na(n_wales, 0),
		n_england = replace_na(n_england, 0),
		n = n_wales + n_england
	)
}

d_weekly_pooled_c19 <- pool.data(d_weekly_england_c19, d_weekly_wales_c19)
d_weekly_pooled_flu <- pool.data(d_weekly_england_flu, d_weekly_wales_flu)

# ==========================================================================
# Make a pretty table
# ==========================================================================

d_weekly_pooled_pretty_c19 <- d_weekly_pooled_c19 %>%
  mutate(
    `Vaccination date` = vacc_date,
    `COVID-19 Dose` = str_replace(vacc_seq, "dose", ""),
    `Pooled n (COVID-19)` = n,
    `England n (COVID-19)` = n_england,
    `Wales n (COVID-19)` = n_wales
    ) %>%
  select(
    `Vaccination date`,
    `COVID-19 Dose`,
    `Pooled n (COVID-19)`,
    `England n (COVID-19)`,
    `Wales n (COVID-19)`
    )

d_weekly_pooled_pretty_flu <- d_weekly_pooled_flu %>%
  mutate(
    `Vaccination date` = vacc_date,
    `Influenza season` = vacc_seq,
    `Pooled n (flu)` = n,
    `England n (flu)` = n_england,
    `Wales n (flu)` = n_wales
    ) %>%
  select(
    `Vaccination date`,
    `Influenza season`,
    `Pooled n (flu)`,
    `England n (flu)`,
    `Wales n (flu)`
    )

d_weekly_pooled_pretty <- full_join(d_weekly_pooled_pretty_c19, d_weekly_pooled_pretty_flu, by = "Vaccination date")

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

my_theme <- function() {
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor.x   = element_blank(),
    panel.grid.minor.y   = element_blank(),
    legend.title         = element_blank(),
    legend.position      = c(0.99, 0.97),
    legend.justification = c(1, 1),
    legend.key.height    = unit(1, "lines"),
    legend.key.width     = unit(1, "lines"),
    legend.margin        = margin(t=-0.25,l=0.05,b=0.0,r=0.05, unit='cm'),
    axis.title.x         = element_blank()
  )
}

p_weekly_pooled_c19 <-
  d_weekly_pooled_c19 %>%
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
    guide = guide_legend(reverse = TRUE, label.position = "left")
  ) +
  my_theme() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p_weekly_pooled_flu <-
  d_weekly_pooled_flu %>%
  ggplot(aes(
    x = vacc_date,
    y = n,
    fill = vacc_seq
  )) +
  geom_col() +
  scale_x_date(
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
    labels = c("Winter 2020/21", "Winter 2021/22"),
    guide = guide_legend(label.position = "left")
  ) +
  my_theme() +
  theme(
  )

p_weekly_pooled <-
  p_weekly_pooled_c19 +
  plot_spacer() +
  p_weekly_pooled_flu +
  plot_layout(
    ncol = 1,
    heights = c(15, -1.5, 15)
  ) +
  plot_annotation(
    title = "Weekly vaccine uptake"
  )

p_weekly_pooled

# ==========================================================================
# Save plots
# ==========================================================================
cat("saving...\n")

write_csv(
  d_weekly_pooled_pretty,
  file = "data_weekly_vacc/pool_main_weekly_vacc.csv"
  )

ggsave(
  plot     = p_weekly_pooled,
  filename = "pool_main_weekly_vacc.png",
  path     = "plots",
  width    = 7.5,
  height   = 5
)
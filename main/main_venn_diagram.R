# ==========================================================================
# Clear environment
# ==========================================================================
cat("Clear environment!\n")

if (Sys.info()["user"] == "william.midgley") {
  cat("Hi Will!\n")
  setwd("C:/Users/william.midgley/Documents/dcp02_covid_v_flu_coverage_disparities/dacvap-c19-flu-vaccine-disparity")
} else if (Sys.info()["user"] == "Stuart.Bedston") {
  cat("Hi Stu!\n")
  setwd("~/Projects/dacvap-c19-flu-vaccine-disparity")
} else {
  cat("Is that you, Utkarsh?! Add your name here\n") 
}

rm(list = ls())

invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))

# ==========================================================================
# Load
# ==========================================================================

pkgs <- c(
  "tidyverse",
  "beepr",
  "patchwork",
  "grid",
  "eulerr",
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

# England
t_venn_england <- read.csv("data_venn_diagram/england/___.csv")
# Wales
t_venn_wales_main <- read.csv("data_venn_diagram/wales/t_ea_flu_c19.csv") %>% select(-p)
t_venn_wales_preg <- read.csv("d-venn/t_ea_flu_c19_preg.csv") %>% select(-p)

# I exported the wrong data, so I'm getting it from the report that was approved also :P

t_venn_wales_preg <- tribble(
  ~c19_vacc_complete_flg, ~flu_vacc_complete_flg, ~n,
  TRUE,      TRUE,     4690,
  FALSE,     TRUE,     9120,
  TRUE,      FALSE,    3780,
  FALSE,     FALSE,    11840
)

# ==========================================================================
# Process England data
# ==========================================================================
cat("processing...\n")

colnames(t_venn_england) <- c("c19_vacc_complete_flg", "flu_vacc_complete_flg", "n_england")

t_venn_england <-
t_venn_england %>% mutate(
  c19_vacc_complete_flg = as.logical(c19_vacc_complete_flg),
  flu_vacc_complete_flg = as.logical(flu_vacc_complete_flg),
  n_england = gsub(",","", t_venn_england$n_england) %>% as.integer()
)

t_venn_england_main <- t_venn_england[2:5,]

t_venn_england_preg <- t_venn_england[10:13,]

# ==========================================================================
# Pool England and Wales
# ==========================================================================
cat("pooling data...\n")

t_venn_pooled_main <-
full_join(t_venn_england_main, t_venn_wales_main) %>% mutate(
	n = n+n_england
  ) %>% select(-n_england)
t_venn_pooled_main$p <- round(t_venn_pooled_main$n/sum(t_venn_pooled_main$n) * 100,1)

t_venn_pooled_preg <-
full_join(t_venn_england_preg, t_venn_wales_preg) %>% mutate(
  n = n+n_england
  ) %>% select(-n_england)
t_venn_pooled_preg$p <- round(t_venn_pooled_preg$n/sum(t_venn_pooled_preg$n) * 100,1)

# ==========================================================================
# Turn tables into a df 
# ==========================================================================

# Main cohort

l_venn_pooled_main <- list()

for (i in 1:nrow(t_venn_pooled_main)) {
    d_venn_pooled_main <- data.frame(
        c19_flg = as.logical(t_venn_pooled_main$c19_vacc_complete_flg[i]),
        flu_flg = as.logical(t_venn_pooled_main$flu_vacc_complete_flg[i])
    )
    l_venn_pooled_main[[i]] <- sample_n(d_venn_pooled_main, size = t_venn_pooled_main$n[i], replace = TRUE)
}

d_venn_pooled_main <-
  bind_rows(l_venn_pooled_main) %>%
    mutate(
      neither = !c19_flg & !flu_flg
    )

# Pregnant cohort

l_venn_pooled_preg <- list()

for (i in 1:nrow(t_venn_pooled_preg)) {
    d_venn_pooled_preg <- data.frame(
        c19_flg = as.logical(t_venn_pooled_preg$c19_vacc_complete_flg[i]),
        flu_flg = as.logical(t_venn_pooled_preg$flu_vacc_complete_flg[i])
    )
    l_venn_pooled_preg[[i]] <- sample_n(d_venn_pooled_preg, size = t_venn_pooled_preg$n[i], replace = TRUE)
}

d_venn_pooled_preg <-
  bind_rows(l_venn_pooled_preg) %>%
    mutate(
      neither = !c19_flg & !flu_flg
    )

# ==========================================================================
# Plot a good ol' fashion bar chart
# ==========================================================================

p_bar <-
  bind_rows(
    t_venn_pooled_main %>% mutate(cohort = "main"),
    t_venn_pooled_preg %>% mutate(cohort = "preg")
  ) %>% 
  mutate(
    vacc_cat = case_when(
      c19_vacc_complete_flg == 1 & flu_vacc_complete_flg == 1 ~ "Both",
      c19_vacc_complete_flg == 1 & flu_vacc_complete_flg == 0 ~ "C19 only",
      c19_vacc_complete_flg == 0 & flu_vacc_complete_flg == 1 ~ "Flu only",
      c19_vacc_complete_flg == 0 & flu_vacc_complete_flg == 0 ~ "Neither"
    ),
    vacc_cat = factor(vacc_cat, c("Neither", "C19 only", "Flu only", "Both"))
  ) %>% 
  ggplot(aes(
    x = vacc_cat,
    y = p / 100,
  )) +
  facet_wrap(~cohort, nrow = 1) +
  geom_col() +
  scale_y_continuous(
    name = "Uptake",
    label = percent
  )

p_bar

# ==========================================================================
# Make a crosstab
# ==========================================================================

t_vacc_status <-
  bind_rows(
    t_venn_pooled_main %>% mutate(cohort = "main"),
    t_venn_pooled_preg %>% mutate(cohort = "preg")
  ) %>% 
  mutate(
    vacc_cat = case_when(
      c19_vacc_complete_flg == 1 & flu_vacc_complete_flg == 1 ~ "Both",
      c19_vacc_complete_flg == 1 & flu_vacc_complete_flg == 0 ~ "C19_only",
      c19_vacc_complete_flg == 0 & flu_vacc_complete_flg == 1 ~ "Flu_only",
      c19_vacc_complete_flg == 0 & flu_vacc_complete_flg == 0 ~ "Neither"
    ),
    vacc_cat = factor(vacc_cat, c("Neither", "C19_only", "Flu_only", "Both"))
  ) %>% 
  mutate(
    n = round_half_up(n, -1),
    n = comma(n),
    p = percent(p, accuracy = 0.1, scale = 1),
    np = str_c(n, " (", p, ")")
  ) %>% 
  select(
    cohort,
    vacc_cat,
    np
  ) %>% 
  pivot_wider(
    names_from = vacc_cat,
    values_from = np,
    names_sort = TRUE
  )

write_csv(
  t_vacc_status,
  file = "plots/t_vacc_status.csv"
)

# ==========================================================================
# Stu's attempt at an euler diagram
# ==========================================================================

?euler
?eulerr:::plot.euler
?grid::grid.text

# main chort
e_main <- euler(d_venn_pooled_main)

p_euler_main <- plot(
  e_main,
  quantities = list(type = c("counts", "percent")),
  fill = "transparent",
  lty = c(1, 1, 2),
  labels = c("COVID-19", "Influenza", "Neither")
)

p_euler_main_title <-
  wrap_elements(p_euler_main) +
  plot_annotation(
    title = "(a) Main cohort uptake",
    theme = theme(title = element_text(size = 10))
  )

# pregnancy cohort
e_preg <- euler(d_venn_pooled_preg)

p_euler_preg <- plot(
  e_preg,
  quantities = list(type = c("counts", "percent")),
  fill = "transparent",
  lty = c(1, 1, 2),
  labels = c("COVID-19", "Influenza", "Neither")
)

p_euler_preg_title <-
  wrap_elements(p_euler_preg) +
  plot_annotation(
    title = "(b) Pregnancy cohort uptake",
    theme = theme(title = element_text(size = 10))
  )

# combine
p_euler <-
  wrap_elements(p_euler_main_title) /
  wrap_elements(p_euler_preg_title)

print(p_euler)

# ==========================================================================
# Plot the venn diagram
# ==========================================================================
cat("Time to start plotting!\n")

# Main cohort

euler(d_venn_pooled_main) %>%
plot(counts = TRUE, labels = c("COVID", "", "Neither"), main = "Main cohort vaccine uptake") # same process for pregnant cohort
grid.text(expression(bold("Flu")), x=0.51, y=0.47) # flu label
grid.text(paste0(t_venn_pooled_main$p[2], "%"), x=0.096, y=0.427) # covid only %         # These are changed to fit in the right place
grid.text(paste0(t_venn_pooled_main$p[1], "%"), x=0.515, y=0.448) # flu & covid %
grid.text(paste0(t_venn_pooled_main$p[3], "%"), x=0.738, y=0.46) # flu only %
grid.text(paste0(t_venn_pooled_main$p[4], "%"), x=0.855, y=0.23) # neither %
p_venn_pooled_main <- recordPlot()

print(p_venn_pooled_main)

# Pregnant cohort

euler(d_venn_pooled_preg) %>% plot(counts = TRUE, labels = c("COVID", "Flu", "Neither"), main = "Pregnant cohort vaccine uptake")
grid.text(paste0(t_venn_pooled_preg$p[2], "%"), x=0.085, y=0.46) # covid only %
grid.text(paste0(t_venn_pooled_preg$p[1], "%"), x=0.26, y=0.465) # flu & covid %
grid.text(paste0(t_venn_pooled_preg$p[3], "%"), x=0.47, y=0.46) # flu only %
grid.text(paste0(t_venn_pooled_preg$p[4], "%"), x=0.785, y=0.453) # neither %
p_venn_pooled_preg <- recordPlot()

print(p_venn_pooled_preg)

# ==========================================================================
# Save plots
# ==========================================================================
cat("saving...\n")

png("C:/Users/william.midgley/Documents/dcp02_covid_v_flu_coverage_disparities/Results/Plots/p_venn_pooled_main.png", width = 600, height = 600)
p_venn_pooled_main
dev.off()

png("C:/Users/william.midgley/Documents/dcp02_covid_v_flu_coverage_disparities/Results/Plots/p_venn_pooled_preg.png", width = 600, height = 600)
p_venn_pooled_preg
dev.off()

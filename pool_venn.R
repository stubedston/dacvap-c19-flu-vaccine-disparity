# ==========================================================================
# Clear and load
# ==========================================================================
cat("Clear and load!\n")

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

library(tidyverse)
library(patchwork)
library(grid)
library(eulerr)
library(lubridate)
library(scales)

# England
t_venn_england <- read.csv("d-venn/vacc_venn_diag_cohort.csv")
# Wales
t_venn_wales_main <- read.csv("d-venn/t_ea_flu_c19.csv") %>% select(-p)
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
# Plot the venn diagram
# ==========================================================================
cat("Time to start plotting!\n")

# Main cohort

euler(d_venn_pooled_main) %>% plot(counts = TRUE, labels = c("COVID", "", "Neither"), main = "Main cohort vaccine uptake") # same process for pregnant cohort
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

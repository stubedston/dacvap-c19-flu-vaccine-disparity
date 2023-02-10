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

# England
t_venn_england <- read.csv("data_venn_diagram/england/england_main_crosstabs.csv") %>% na.omit()
# Wales
t_venn_wales <- read.csv("data_venn_diagram/wales/wales_main_crosstabs.csv") %>% select(-p)

# ==========================================================================
# Process data
# ==========================================================================
cat("processing...\n")

colnames(t_venn_england) <- c("c19_complete", "flu_complete", "n_england")
colnames(t_venn_wales) <- c("flu_complete", "c19_complete", "n_wales")

t_venn_wales <- t_venn_wales[,c("c19_complete", "flu_complete", "n_wales")]

t_venn_wales[,c(1,2)] <- sapply(t_venn_wales[,c(1,2)], as.logical)

t_venn_england <-
  t_venn_england %>% mutate(
    c19_complete = as.logical(c19_complete),
    flu_complete = as.logical(flu_complete),
    n_england = gsub(",","", t_venn_england$n_england) %>% as.integer()
    )

# ==========================================================================
# Pool England and Wales
# ==========================================================================
cat("pooling data...\n")

t_venn_pooled <-
full_join(t_venn_england, t_venn_wales) %>% mutate(
	n = n_wales+n_england
  ) %>% select(-n_england, -n_wales)

t_venn_pooled$p <- round(t_venn_pooled$n/sum(t_venn_pooled$n) * 100,1)

# ==========================================================================
# Turn tables into a df 
# ==========================================================================

# Main cohort

l_venn_pooled <- list()

for (i in 1:nrow(t_venn_pooled)) {
    d_venn_pooled <- data.frame(
        c19_flg = as.logical(t_venn_pooled$c19_complete[i]),
        flu_flg = as.logical(t_venn_pooled$flu_complete[i])
    )
    l_venn_pooled[[i]] <- sample_n(d_venn_pooled, size = t_venn_pooled$n[i], replace = TRUE)
}

d_venn_pooled <-
  bind_rows(l_venn_pooled) %>%
    mutate(
      neither = !c19_flg & !flu_flg
    )

# ==========================================================================
# Plot a good ol' fashion bar chart
# ==========================================================================

p_bar <-
    t_venn_pooled %>%
  mutate(
    vacc_cat = case_when(
      c19_complete == 1 & flu_complete == 1 ~ "Both",
      c19_complete == 1 & flu_complete == 0 ~ "C19 only",
      c19_complete == 0 & flu_complete == 1 ~ "Flu only",
      c19_complete == 0 & flu_complete == 0 ~ "Neither"
    ),
    vacc_cat = factor(vacc_cat, c("Neither", "C19 only", "Flu only", "Both"))
  ) %>% 
  ggplot(aes(
    x = vacc_cat,
    y = p / 100,
  )) +
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
  t_venn_pooled %>% 
  mutate(
    vacc_cat = case_when(
      c19_complete == 1 & flu_complete == 1 ~ "Both",
      c19_complete == 1 & flu_complete == 0 ~ "C19_only",
      c19_complete == 0 & flu_complete == 1 ~ "Flu_only",
      c19_complete == 0 & flu_complete == 0 ~ "Neither"
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
  file = "data_venn_diagram/pool_main_crosstabs.csv"
)

# ==========================================================================
# Plot the venn diagram
# ==========================================================================
cat("Time to start plotting!\n")

# Main cohort

euler(d_venn_pooled) %>%
plot(counts = TRUE, labels = c("COVID", "", "Neither"), main = "Relative vaccine uptake")
grid.text(expression(bold("Flu")), x=0.51, y=0.47) # flu label
grid.text(paste0(t_venn_pooled$p[2], "%"), x=0.096, y=0.427) # covid only %         # These are changed to fit in the right place
grid.text(paste0(t_venn_pooled$p[1], "%"), x=0.515, y=0.448) # flu & covid %
grid.text(paste0(t_venn_pooled$p[3], "%"), x=0.738, y=0.46) # flu only %
grid.text(paste0(t_venn_pooled$p[4], "%"), x=0.855, y=0.23) # neither %
p_venn_pooled <- recordPlot()

print(p_venn_pooled)

# ==========================================================================
# Save plots
# ==========================================================================
cat("Saving...\n")

png("plots/pool_main_venn.png", width = 600, height = 600)
p_venn_pooled
dev.off()
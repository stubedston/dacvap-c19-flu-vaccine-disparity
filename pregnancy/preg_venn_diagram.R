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
t_venn_england <- read.csv("data_venn_diagram/england/england_preg_crosstabs.csv") %>% na.omit()

#t_venn_england <- t_venn_england[2:5,]
t_venn_england <- t_venn_england[10:13,]

# Wales
t_venn_wales <- read.csv("data_venn_diagram/wales/wales_preg_crosstabs.csv") %>% select(-p)

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
# Make a pretty crosstab
# ==========================================================================

crosstab <- function(data) {
  ct <- data

  if (is.null(ct$p)) {
    colnames(ct) <- c("c19_complete", "flu_complete", "n")
    ct$p <- round(ct$n/sum(ct$n) * 100,1)
  }

  ct <- ct %>% 
  mutate(
    vacc_cat = case_when(
      c19_complete == 1 & flu_complete == 1 ~ "Both",
      c19_complete == 1 & flu_complete == 0 ~ "COVID-19 only",
      c19_complete == 0 & flu_complete == 1 ~ "Influenza only",
      c19_complete == 0 & flu_complete == 0 ~ "Neither"
    ),
    vacc_cat = factor(vacc_cat, c("Neither", "COVID-19 only", "Influenza only", "Both"))
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
  )
  colnames(ct) <- c("Vaccine", str_replace(paste0(deparse(substitute(data))), "t_venn_(.+)", "\\1") %>% str_to_title())
  return(ct)
}

pool_crosstabs <- cbind(
  crosstab(t_venn_pooled),
  crosstab(t_venn_england)["England"],
  crosstab(t_venn_wales)["Wales"]
  )
# ==========================================================================
# Plot the venn diagram
# ==========================================================================
cat("Time to start plotting!\n")


euler(d_venn_pooled) %>% plot(counts = TRUE, labels = c("COVID", "Flu", "Neither"), main = "Relative Vaccine uptake")
grid.text(paste0(t_venn_pooled$p[2], "%"), x=0.093, y=0.46) # covid only %
grid.text(paste0(t_venn_pooled$p[1], "%"), x=0.285, y=0.465) # flu & covid %
grid.text(paste0(t_venn_pooled$p[3], "%"), x=0.485, y=0.465) # flu only %
grid.text(paste0(t_venn_pooled$p[4], "%"), x=0.78, y=0.45) # neither %
p_venn_pooled <- recordPlot()

print(p_venn_pooled)

# ==========================================================================
# Save plots
# ==========================================================================
cat("Saving...\n")

png("plots/pool_preg_venn.png", width = 600, height = 600)
p_venn_pooled
dev.off()

write_csv(
  pool_crosstabs,
  file = "data_venn_diagram/pool_preg_crosstabs.csv"
)
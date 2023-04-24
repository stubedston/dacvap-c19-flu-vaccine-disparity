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
  "gridGraphics",
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
# Load data
# ==========================================================================
cat("load data\n")

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
cat("pool england and wales\n")

t_venn_pooled <-
  t_venn_england %>% 
  full_join(t_venn_wales, join_by(c19_complete, flu_complete)) %>%
  mutate(
    n = n_wales + n_england
  ) %>%
  select(
    -n_england,
    -n_wales
  ) %>% 
  mutate(
    p = n / sum(n),
    p_pretty = percent(p, accuracy = 0.1)
  )


# Turn aggregate tables into individual-level data

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
  mutate(neither = !c19_flg & !flu_flg)

# ==========================================================================
# Plot the diagram
# ==========================================================================
cat("Plot\n")


d_venn_pooled %>% 
euler() %>%
plot(
  fills = list(fill = c("#fb8072", "#80b1d3", "#d9d9d9")),
  edges = list(lwd = 1),
  labels = c("", "", "")
) %>% 
print()

# labels
grid.text("COVID-19", gp=gpar(fontsize = 10, fontface="bold"), x=0.093, y=0.485)
grid.text("Both",     gp=gpar(fontsize = 10, fontface="bold"), x=0.285, y=0.485)
grid.text("Flu",      gp=gpar(fontsize = 10, fontface="bold"), x=0.485, y=0.485)
grid.text("Neither",  gp=gpar(fontsize = 10, fontface="bold"), x=0.780, y=0.485)

# percentages
grid.text(t_venn_pooled$p_pretty[2], gp=gpar(fontsize = 10), x=0.093, y=0.425)
grid.text(t_venn_pooled$p_pretty[1], gp=gpar(fontsize = 10), x=0.285, y=0.425)
grid.text(t_venn_pooled$p_pretty[3], gp=gpar(fontsize = 10), x=0.485, y=0.425)
grid.text(t_venn_pooled$p_pretty[4], gp=gpar(fontsize = 10), x=0.780, y=0.425)

# save the plot
p_c19_flu_euler <- grid.grab()

grid.draw(p_c19_flu_euler)


# ==========================================================================
# Save plots
# ==========================================================================
cat("Save\n")

ggsave(
  plot     = p_c19_flu_euler,
  filename = "plots/pool_preg_venn.png",
  width    = 5.2,
  height   = 2.3
)

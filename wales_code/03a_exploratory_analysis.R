source("r_clear_and_load.r")

cat("Main cohort\n")

# load cohort ==================================================================
cat("load cohort\n")

d_cohort <- qread(s_drive("d_cohort_eligible.qs"))

# ==========================================================================
# Weekly vacc plots
# ==========================================================================

cat("count weekly vaccination\n")

# count weekly covid vaccination ===============================================

x_limits <- c(
  ymd("2020-09-01"),
  ymd("2022-03-31")
)

x_breaks <- seq(
  from = ymd('2020-09-01'),
  to   = ymd('2022-03-31'),
  by   = "6 months"
)

y_limits <- c(0, 150000)

d_c19_vacc_week <-
  d_cohort %>%
  select(
    matches("c19_vacc_.*_date")
  ) %>%
  pivot_longer(
    cols           = everything(),
    names_to       = "vacc_seq",
    values_to      = "vacc_date",
    values_drop_na = TRUE
  ) %>%
  mutate(
    vacc_seq = str_replace(vacc_seq, "c19_vacc_(.+)_date", "\\1"),
    vacc_date = floor_date(vacc_date, "week")
  ) %>%
  count(vacc_seq, vacc_date) %>%
  filter(n >= 10) %>%
  mutate(n = round(n, -1)) %>%
  mutate(vacc_type = "c19")

p_c19_vacc <-
  d_c19_vacc_week %>%
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
    breaks = pretty_breaks(),
    labels = comma
  ) +
  labs(
    fill = "Vaccine Dose"
  ) +
  scale_fill_brewer(
    palette = "Set2"
  ) +
  scale_fill_discrete(
    labels = c("Dose 1", "Dose 2", "Dose 3")
  ) +
  ggtitle(
    "Main cohort COVID-19 vaccinations"
  )
print(p_c19_vacc)

# count weekly flu vaccination =================================================

d_flu_vacc_week <-
  d_cohort %>%
  select(
    matches("fluvac.*_dt")
  ) %>%
  pivot_longer(
    cols           = everything(),
    names_to       = "vacc_seq",
    values_to      = "vacc_date",
    values_drop_na = TRUE
  ) %>%
  mutate(
    vacc_seq = str_replace(vacc_seq, "fluvac(.+)_dt", "\\1"),
    vacc_date = floor_date(vacc_date, "week")
  ) %>%
  count(vacc_seq, vacc_date) %>%
  filter(n >= 10) %>%
  mutate(n = round(n, -1)) %>%
  mutate(vacc_type = "flu")

p_flu_vacc <-
  d_flu_vacc_week %>%
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
    breaks = pretty_breaks(),
    labels = comma
  ) +
  labs(
    fill = "Vaccine Dose"
  ) +
  scale_fill_brewer(
    palette = "Set1"
  ) +
  scale_fill_discrete(
    labels = c("Winter 2020", "Winter 2021")
  ) +
  ggtitle(
    "Main cohort Flu vaccinations"
  )
print(p_flu_vacc)

# combine plots ================================================================

p_ea_vacc_week <- p_flu_vacc / p_c19_vacc

print(p_ea_vacc_week)

# summarise flu and vacc uptake ================================================

flg_summary <- function(x) {
  xvar <- enquo(x)
  nm <- deparse(substitute(x))

  d_cohort %>%
    count(!!xvar) %>%
    pivot_wider(names_from = !!xvar, values_from = "n") %>%
    mutate(xvar = nm) %>%
    select(xvar, everything())
}

t_ea_vacc_univar <-
  bind_rows(
    flg_summary(fluvac20_flg),
    flg_summary(fluvac21_flg),
    flg_summary(c19_vacc_dose1_flg),
    flg_summary(c19_vacc_dose2_flg),
    flg_summary(c19_vacc_dose3_flg)
  )

# suppression and formatting

t_ea_vacc_univar <-
  t_ea_vacc_univar %>%
  mutate(
    "1" = replace_na(`1`, 0)
  ) %>%
  rename(
    Yes_n = "1",
    No_n = "0"
  ) %>%
  mutate(
    No_p   = No_n  / (Yes_n + No_n) * 100,
    Yes_p  = Yes_n / (Yes_n + No_n) * 100,
    No_np  = str_glue("{n} ({p}%)",
      n = format(round(No_n, -1), nsmall = 0, big.mark = ","),
      p = format(round(No_p,  1), nsmall = 1, trim = TRUE)
    ),
    Yes_np = str_glue("{n} ({p}%)",
      n = format(round(Yes_n, -1), nsmall = 0, big.mark = ","),
      p = format(round(Yes_p,  1), nsmall = 1, trim = TRUE)
    )
  ) %>%
  mutate(
    xvar = str_replace(xvar, "(.*)_flg", "\\1"),
    xvar = str_replace_all(xvar, "_", " ")
  ) %>%
  select(
    xvar,
    No = No_np,
    Yes = Yes_np
  )

print(t_ea_vacc_univar)

# Covid vaccinations by brand

d_c19_vacc_names <- 
  rbind(
  d_cohort %>%
    mutate(
      vacc_date = c19_vacc_dose1_date,
      vacc_name = c19_vacc_dose1_name
    ) %>%
    select(
      vacc_date,
      vacc_name
    ),
  d_cohort %>%
    mutate(
      vacc_date = c19_vacc_dose2_date,
      vacc_name = c19_vacc_dose2_name
    ) %>%
    select(
      vacc_date,
      vacc_name
    ),
  d_cohort %>%
    mutate(
      vacc_date = c19_vacc_dose3_date,
      vacc_name = c19_vacc_dose3_name
    ) %>%
    select(
      vacc_date,
      vacc_name
    ) %>%
    select(
      vacc_date,
      vacc_name
    )
) %>%
  mutate(
    vacc_date = floor_date(vacc_date, "week")
  ) %>%
  mutate(
    Brand = case_when(vacc_name == "Astrazeneca" ~ "Astrazeneca",
                          vacc_name == "Pfizer Biontech" | vacc_name == "Comirnaty" ~ "Pfizer Biontech",
                          vacc_name == "Moderna" | vacc_name == "Spikevax" ~ "Moderna"
                          )
  ) %>%
  count(Brand, vacc_date) %>%
  filter(n >= 10) %>%
  mutate(n = round(n, -1))


p_c19_vacc_names <- 
  d_c19_vacc_names %>%
  ggplot(aes(
    x = vacc_date,
    y = n,
    fill = Brand
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
    breaks = pretty_breaks(),
    labels = comma
  ) +
  scale_fill_brewer(
    palette = "Set1"
  ) +
  ggtitle(
    "COVID-19 vaccinations by brand"
  ) 

print(p_c19_vacc_names)



# Overlap ======================================================================

cat("Crosstabs and Venn diagram\n")

t_ea_flu_c19 <-
  d_cohort %>%
  count(
    flu_vacc_complete_flg,
    c19_vacc_dose3_flg,
  ) %>%
  mutate(
    p = n / sum(n) * 100
  ) %>%
  arrange(desc(n)
  ) %>%
  mutate(
    vaccines = case_when(
                flu_vacc_complete_flg == 1 & c19_vacc_dose3_flg == 1 ~ "Both",
                flu_vacc_complete_flg == 0 & c19_vacc_dose3_flg == 1 ~ "COVID only",
                flu_vacc_complete_flg == 1 & c19_vacc_dose3_flg == 0 ~ "Flu only",
                flu_vacc_complete_flg == 0 & c19_vacc_dose3_flg == 0 ~ "Neither"
                ),
    np = str_glue("{n} ({p}%)",
                n = format(round(n, -1), nsmall = 0, big.mark = ","),
                p = format(round(p,  1), nsmall = 1, trim = TRUE)
                )
  ) %>%
  select(
    vaccines,
    n = np
  )
print(t_ea_flu_c19)


# Raw data

t_ea_flu_c19_raw <-
  d_cohort %>%
  count(
    flu_vacc_complete_flg,
    c19_vacc_complete_flg,
  ) %>%
  mutate(
    p = n / sum(n) * 100
  ) %>%
  arrange(desc(n)
  )

# Make an Euler plot

t_ea_flu_c19 <- 
t_ea_flu_c19_raw %>%
    mutate(
        n = round(n, -1),
        p = round(p, 1)
    )



l_ea_flu_c19 <- list()

for (i in 1:nrow(t_ea_flu_c19)) {
    d_ea_flu_c19 <- data.frame(
        c19_flg = as.logical(t_ea_flu_c19$c19_vacc_complete_flg[i]),
        flu_flg = as.logical(t_ea_flu_c19$flu_vacc_complete_flg[i])
    )
    l_ea_flu_c19[[i]] <- sample_n(d_ea_flu_c19, size = t_ea_flu_c19$n[i], replace = TRUE)
}

d_ea_flu_c19 <-
  bind_rows(l_ea_flu_c19) %>%
    mutate(
      neither = !c19_flg & !flu_flg
    )


euler(d_ea_flu_c19) %>% plot(counts = TRUE, labels = c("COVID", "", "Neither"), main = "Main cohort vaccine uptake") %>% print()
grid.text(expression(bold("Flu")), x=0.51,y=0.47) # flu label
grid.text(paste0(t_ea_flu_c19$p[2],"%"), x=0.109,y=0.45) # covid only %
grid.text(paste0(t_ea_flu_c19$p[1],"%"), x=0.515,y=0.448) # flu & covid %
grid.text(paste0(t_ea_flu_c19$p[4],"%"), x=0.738,y=0.46) # flu only %
grid.text(paste0(t_ea_flu_c19$p[3],"%"), x=0.862,y=0.235) # neither %
p_ea_flu_c19_euler <- recordPlot()

print(p_ea_flu_c19_euler)



# save =========================================================================
cat("Saving...\n")

qsavem(
  d_c19_vacc_week,
  p_c19_vacc,
  d_flu_vacc_week,
  p_flu_vacc,
  p_ea_vacc_week,
  p_c19_vacc_names,
  t_ea_vacc_univar,
  t_ea_flu_c19,
  t_ea_flu_c19_raw,
  p_ea_flu_c19_euler,
  file = "results/exploratory_vacc_analysis.qsm"
)

cat("Done!\n")
beep(0)
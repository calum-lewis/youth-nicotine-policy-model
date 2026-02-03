################################################################################
# 01.data_cleaning: UKHLS Wave L (l_indresp) clean extract
################################################################################

library(haven)
library(dplyr)
library(janitor)
library(labelled)
library(forcats)

l_path <- "U:/Modelling/R Project/data/data_raw/us_indresp/l_indresp.dta"

l_raw <- read_dta(l_path)

usoc_l_clean <- l_raw |>
  clean_names() |>
  transmute(
    pidp,
    wave = "L",
    age = as.numeric(haven::zap_labels(l_age_dv)),
    age = if_else(age < 0, NA_real_, age),
    gender = haven::as_factor(l_sex, levels = "labels") |> forcats::fct_drop(),
    jbstat = haven::as_factor(l_jbstat, levels = "labels") |> forcats::fct_drop(),
    country = haven::as_factor(l_country, levels = "labels") |> forcats::fct_drop()
    
  ) |>
  relocate(pidp, wave, age, gender, jbstat, country)

saveRDS(usoc_l_clean, "U:/Modelling/R Project/data/data_clean/usoc_wave_l_indresp_clean.rds")

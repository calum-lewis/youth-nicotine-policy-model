################################################################################
# 01.data_cleaning: UKHLS Wave N (n_indresp) clean extract
################################################################################

library(haven)
library(dplyr)
library(janitor)
library(labelled)
library(forcats)

n_path <- "U:/Modelling/R Project/data/data_raw/us_indresp/n_indresp.dta"

n_raw <- read_dta(n_path)

usoc_n_clean <- n_raw |>
  clean_names() |>
  transmute(
    pidp,
    wave = "N",
    age = as.numeric(haven::zap_labels(n_age_dv)),
    age = if_else(age < 0, NA_real_, age),
    gender = haven::as_factor(n_sex, levels = "labels") |> forcats::fct_drop(),
    jbstat = haven::as_factor(n_jbstat, levels = "labels") |> forcats::fct_drop(),
    country = haven::as_factor(n_country, levels = "labels") |> forcats::fct_drop()
    
  )  |>
  relocate(pidp, wave, age, gender, jbstat, country)

saveRDS(usoc_n_clean, "U:/Modelling/R Project/data/data_clean/usoc_wave_n_indresp_clean.rds")
cat("\nSaved: U:/Modelling/R Project/data_clean/usoc_wave_n_indresp_clean.rds\n")

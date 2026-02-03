################################################################################
# 01.data_cleaning: UKHLS Wave M (m_indresp) clean extract
################################################################################

library(haven)
library(dplyr)
library(janitor)
library(labelled)
library(forcats)

m_path <- "U:/Modelling/R Project/data/data_raw/us_indresp/m_indresp.dta"

m_raw <- read_dta(m_path)

usoc_m_clean <- m_raw |>
  clean_names() |>
  transmute(
    pidp,
    wave = "M",
    age = as.numeric(haven::zap_labels(m_age_dv)),
    age = if_else(age < 0, NA_real_, age),
    gender = haven::as_factor(m_sex, levels = "labels") |> forcats::fct_drop(),
    jbstat = haven::as_factor(m_jbstat, levels = "labels") |> forcats::fct_drop(),
    country = haven::as_factor(m_country, levels = "labels") |> forcats::fct_drop()
    
  ) |>
  relocate(pidp, wave, age, gender, jbstat, country)

saveRDS(usoc_m_clean, "U:/Modelling/R Project/data/data_clean/usoc_wave_m_indresp_clean.rds")

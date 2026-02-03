################################################################################
# 01.data_cleaning: UKHLS / Understanding Society (Wave O) clean extract
# Purpose: create a CLEAN (not model-derived) dataset for downstream modelling.
#
# End product:
# - Keep core variables only: pidp, age, gender, jbstat, IMD quintiles (if present)
# - Standardise missingness: negative special codes -> NA
# - Convert labelled variables to factors (labels preserved)
# - DO NOT derive model constructs (e.g., post-16 education indicator) here
################################################################################

library(haven)
library(dplyr)
library(janitor)
library(labelled)
library(forcats)

# ---- Inputs -------------------------------------------------------------------
# Adjust as needed:
o_path <- "U:/Modelling/R Project/data/data_raw/us_indresp/o_indresp.dta"

# ---- 1) Read and basic cleaning ------------------------------------------------
o_raw <- read_dta(o_path)


usoc_o_clean <- o_raw |>
  clean_names() |>
  transmute(
    pidp,
    wave = "O",
    age = as.numeric(haven::zap_labels(o_age_dv)),
    age = if_else(age < 0, NA_real_, age),
    gender = haven::as_factor(o_sex, levels = "labels") |> forcats::fct_drop(),
    jbstat = haven::as_factor(o_jbstat, levels = "labels") |> forcats::fct_drop(),
    country = haven::as_factor(o_country, levels = "labels") |> forcats::fct_drop()
    
  ) |>
  relocate(pidp, wave, age, gender, jbstat, country)


# ----Save -------------------------------------------------------------------

saveRDS(usoc_o_clean, "U:/Modelling/R Project/data/data_clean/usoc_wave_o_indresp_clean.rds")

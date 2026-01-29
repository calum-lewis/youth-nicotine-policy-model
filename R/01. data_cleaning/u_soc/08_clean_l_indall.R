################################################################################
# 01.data_cleaning: UKHLS Wave N indall – IMD quintiles clean extract
# Purpose: create a CLEAN (not model-derived) dataset for downstream modelling.
#
# End product:
# - Keep core variables only:
#     pidp
#     imd2019qe_dv  (England IMD quintile)
# - Standardise missingness: negative special codes -> NA
# - Convert labelled variables to factors (labels preserved)
################################################################################

library(haven)
library(dplyr)
library(janitor)
library(labelled)


# ---- Input --------------------------------------------------------------------
indall_l_path <- "U:/Modelling/R Project/data/data_raw/us_indall/l_indall.dta"
# adjust path if needed

# ---- 1) Read + basic cleaning --------------------------------------------------
indall_raw <- read_dta(indall_l_path)


usoc_l_indall_clean <- indall_raw |>
  clean_names() |>
  mutate(across(where(is.labelled), ~ as_factor(.x, levels = "labels"))) |>
  select(pidp, l_imd2019qe_dv) |>
  mutate(
    # Standardise names (drop the wave prefix in the clean output)
    imd2019qe_dv = l_imd2019qe_dv
  ) |>
  select(pidp, imd2019qe_dv)

# ---- 2) Save -------------------------------------------------------------------
saveRDS(
  usoc_n_indall_clean,
  "U:/Modelling/R Project/data/data_clean/usoc_wave_l_indall_imd_clean.rds"
)

cat("\nSaved: U:/Modelling/R Project/data_clean/usoc_wave_l_indall_imd_clean.rds\n")
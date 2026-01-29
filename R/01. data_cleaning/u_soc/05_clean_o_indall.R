################################################################################
# 01.data_cleaning: UKHLS Wave O indall – IMD quintiles clean extract
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
indall_o_path <- "U:/Modelling/R Project/data/data_raw/us_indall/o_indall.dta"
# adjust path if needed

# ---- 1) Read + basic cleaning --------------------------------------------------
indall_raw <- read_dta(indall_o_path)

usoc_o_indall_clean <- indall_raw |>
  clean_names() |>
  mutate(across(where(is.labelled), ~ as_factor(.x, levels = "labels"))) |>
  select(pidp, o_imd2019qe_dv) |>
  mutate(
    # Standardise names (drop the wave prefix in the clean output)
    imd2019qe_dv = o_imd2019qe_dv
  ) |>
  select(pidp, imd2019qe_dv)

# ---- 2) Save -------------------------------------------------------------------
saveRDS(
  usoc_o_indall_clean,
  "U:/Modelling/R Project/data/data_clean/usoc_wave_o_indall_imd_clean.rds"
)

cat("\nSaved: U:/Modelling/R Project/data/data_clean/usoc_wave_o_indall_imd_clean.rds\n")
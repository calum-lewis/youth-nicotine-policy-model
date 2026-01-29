################################################################################
# 05_params_edu_transitions_lmno_16_25.R
#
# Purpose
# Estimate education-state transition probabilities using pooled adjacent UKHLS
# waves (L→M, M→N, N→O) for respondents in England aged 16–25 at the start of
# each transition.
#
# Education state definition (from main activity: jbstat)
#   post16 = Full-time student / apprenticeship / government training scheme
#   notEdu = all other categories
#
# Outputs
# - Pooled transition matrix (annual probability + monthly equivalent)
# - Age-specific transition table (annual + monthly)
# - Age-banded transition table (annual + monthly) for: 16–17, 18–20, 21–25
# - Saved RDS: edu_transitions_by_band_eng_16_25.rds
################################################################################

library(dplyr)
library(tidyr)

# ---- 0) Load ------------------------------------------------------------------
usoc <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_lmno_panel_with_imd.rds")

# ---- 1) Subset + define education state --------------------------------------
# Assumes columns exist: pidp, wave, age, jbstat, country

usoc_eng <- usoc %>%
  filter(country == "England") %>%
  filter(age >= 16, age <= 25) %>%  # <16 handled separately in the model (assumed FT education)
  mutate(
    # Ensure waves are ordered so lead() produces correct adjacent transitions
    wave = factor(wave, levels = c("L", "M", "N", "O"), ordered = TRUE),
    
    # Binary education state (adjust labels here if jbstat labels differ)
    edu_state = case_when(
      jbstat %in% c("Full-time student", "On apprenticeship", "Govt training scheme") ~ "post16",
      TRUE ~ "notEdu"
    ),
    edu_state = factor(edu_state, levels = c("post16", "notEdu"))
  ) %>%
  select(pidp, wave, age, edu_state)

# ---- 2) Create adjacent-wave transitions (L→M, M→N, N→O) ----------------------
transitions <- usoc_eng %>%
  arrange(pidp, wave) %>%
  group_by(pidp) %>%
  mutate(
    wave_to   = lead(wave),
    age_to    = lead(age),
    edu_to    = lead(edu_state),
    wave_pair = paste0(as.character(wave), "→", as.character(wave_to))
  ) %>%
  ungroup() %>%
  filter(wave_pair %in% c("L→M", "M→N", "N→O")) %>%
  filter(!is.na(edu_state), !is.na(edu_to))

# ---- Helper: annual probability -> monthly probability ------------------------
annual_to_monthly <- function(p_yr) {
  1 - (1 - p_yr)^(1 / 12)
}

# ---- 3) Pooled annual transition matrix (16–25) ------------------------------
edu_trans_pooled <- transitions %>%
  count(edu_from = edu_state, edu_to, name = "n") %>%
  group_by(edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr   = n / n_from,
    p_mo   = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(edu_from, edu_to)

print(edu_trans_pooled)

# ---- 4) Age-specific transitions (diagnostic) --------------------------------
edu_trans_by_age <- transitions %>%
  count(age_from = age, edu_from = edu_state, edu_to, name = "n") %>%
  group_by(age_from, edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr   = n / n_from,
    p_mo   = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(age_from, edu_from, edu_to)

print(edu_trans_by_age)

# ---- 5) Age-banded transitions (model-ready) ---------------------------------
edu_trans_by_band <- transitions %>%
  mutate(
    age_band = case_when(
      age >= 16 & age <= 17 ~ "16–17",
      age >= 18 & age <= 20 ~ "18–20",
      age >= 21 & age <= 25 ~ "21–25",
      TRUE ~ NA_character_
    ),
    age_band = factor(age_band, levels = c("16–17", "18–20", "21–25"))
  ) %>%
  filter(!is.na(age_band)) %>%
  count(age_band, edu_from = edu_state, edu_to, name = "n") %>%
  group_by(age_band, edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr   = n / n_from,
    p_mo   = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(age_band, edu_from, edu_to)

print(edu_trans_by_band)

# Optional check: probabilities sum to 1 within each (age_band, edu_from)
# edu_trans_by_band %>%
#  group_by(age_band, edu_from) %>%
#   summarise(sum_p = sum(p_yr), .groups = "drop") %>%
#   print()

# ---- 6) Save ------------------------------------------------------------------
dir.create("U:/Modelling/R Project/data/data_params", showWarnings = FALSE, recursive = TRUE)

saveRDS(
  edu_trans_by_band,
  "U:/Modelling/R Project/data/data_params/edu_transitions_by_band_eng_16_25.rds"
)

  
  
################################################################################
# 05_params_edu_transitions_lmno_16_25.R
# Estimate annual education state transitions using pooled UKHLS adjacent waves:
#   l→m, m→n, n→o
# Define education state from *_jbstat (main activity):
#   post16 = FT student/apprenticeship/govt training scheme
#   notEdu = otherwise (work/unemployed/inactive/etc.)
#
# Outputs:
# - pooled transition matrix (annual + monthly hazard equivalents)
# - age-specific transition table (annual + monthly)
# - wave-pair stability diagnostics
# - age-banded hazards (16–17, 18–20, 21–25), incl. counts
################################################################################

library(haven)
library(dplyr)
library(labelled)
library(tidyr)
library(purrr)

# ---- Inputs -------------------------------------------------------------------
indresp_path <- "U:/Modelling/R Project/data_raw/us_indresp"

# Adjacent wave pairs to pool
waves <- tibble(
  wave_from = c("l", "m", "n"),
  wave_to   = c("m", "n", "o")
)

# ---- Helpers ------------------------------------------------------------------
annual_to_monthly <- function(p_yr) {
  1 - (1 - p_yr)^(1/12)
}

make_edu_state <- function(df, w) {
  age_var    <- paste0(w, "_age_dv")
  jbstat_var <- paste0(w, "_jbstat")
  
  df %>%
    mutate(
      age = if_else(.data[[age_var]] < 0, NA_real_, as.numeric(.data[[age_var]])),
      jbstat_f = as_factor(.data[[jbstat_var]], levels = "labels"),
      edu_state = case_when(
        jbstat_f %in% c("Full-time student", "On apprenticeship", "Govt training scheme") ~ "post16",
        jbstat_f %in% c(
          "Paid employment(ft/pt)", "Self employed", "Unemployed",
          "Family care or home", "Doing something else",
          "Unpaid, family business", "LT sick or disabled",
          "Retired", "On maternity leave", "On furlough",
          "Temporarily laid off/short term working",
          "on shared parental leave", "on adoption leave"
        ) ~ "notEdu",
        TRUE ~ NA_character_
      )
    ) %>%
    select(pidp, age, edu_state)
}

build_transition <- function(w_from, w_to) {
  
  file_from <- file.path(indresp_path, paste0(w_from, "_indresp.dta"))
  file_to   <- file.path(indresp_path, paste0(w_to, "_indresp.dta"))
  
  df_from <- read_dta(file_from)
  df_to   <- read_dta(file_to)
  
  from <- make_edu_state(df_from, w_from) %>%
    rename(age_from = age, edu_from = edu_state)
  
  to <- make_edu_state(df_to, w_to) %>%
    rename(age_to = age, edu_to = edu_state)
  
  from %>%
    inner_join(to, by = "pidp") %>%
    filter(age_from >= 16, age_from <= 25) %>%
    filter(!is.na(edu_from), !is.na(edu_to)) %>%
    mutate(wave_pair = paste0(w_from, "→", w_to))
}

# ---- 1) Build pooled lmno transitions -----------------------------------------
transitions <- waves %>%
  pmap_dfr(~ build_transition(..1, ..2))

cat("Total transitions (baseline age 16–25, non-missing states): ", nrow(transitions), "\n")

# ---- 2) Pooled annual transition matrix ---------------------------------------
trans_mat <- transitions %>%
  count(edu_from, edu_to) %>%
  group_by(edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr = n / n_from,
    p_mo = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(edu_from, edu_to)

print(trans_mat)

# ---- 3) Age-specific transitions ----------------------------------------------
trans_by_age <- transitions %>%
  count(age_from, edu_from, edu_to) %>%
  group_by(age_from, edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr = n / n_from,
    p_mo = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(age_from, edu_from, edu_to)

print(trans_by_age)

# ---- 4) Diagnostics: by wave pair (stability) ---------------------------------
trans_by_wave <- transitions %>%
  count(wave_pair, edu_from, edu_to) %>%
  group_by(wave_pair, edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr = n / n_from
  ) %>%
  ungroup() %>%
  arrange(wave_pair, edu_from, edu_to)

print(trans_by_wave)

# ---- 5) Age-banded hazards (your chosen bands) --------------------------------
transitions_banded <- transitions %>%
  mutate(
    age_band = case_when(
      age_from >= 16 & age_from <= 17 ~ "16–17",
      age_from >= 18 & age_from <= 20 ~ "18–20",
      age_from >= 21 & age_from <= 25 ~ "21–25",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_band))

band_trans <- transitions_banded %>%
  count(age_band, edu_from, edu_to) %>%
  group_by(age_band, edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr = n / n_from,
    p_mo = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(age_band, edu_from, edu_to)

# Just the hazards you actually code (state changes)
band_hazards <- band_trans %>%
  filter(edu_from != edu_to) %>%
  arrange(age_band, edu_from, edu_to)

print(band_hazards)

# Wide version (easy to read / paste)
band_hazards_wide <- band_hazards %>%
  mutate(trans = paste0(edu_from, "→", edu_to)) %>%
  select(age_band, trans, n, n_from, p_yr, p_mo) %>%
  pivot_wider(
    names_from = trans,
    values_from = c(n, n_from, p_yr, p_mo)
  )

print(band_hazards_wide)

# ---- 6) Save outputs -----------------------------------------------------------
dir.create("data/params", showWarnings = FALSE)

saveRDS(
  list(
    pooled_transitions = trans_mat,
    age_specific = trans_by_age,
    by_wave_pair = trans_by_wave,
    band_transitions_all = band_trans,        # includes stay rows too
    band_hazards = band_hazards,              # only change hazards
    band_hazards_wide = band_hazards_wide,
    n_transitions = nrow(transitions),
    source = "UKHLS pooled adjacent wave transitions: l→m, m→n, n→o; education state from *_jbstat main activity"
  ),
  "data/params/usoc_edu_transitions_lmno_16_25.rds"
)

cat("Saved: data/params/usoc_edu_transitions_lmno_16_25.rds\n")
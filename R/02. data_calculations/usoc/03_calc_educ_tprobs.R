################################################################################
# 03
# use different waves of usoc
# to calculate the trans probs of moving between education/not in education
# for people in the model 16+
################################################################################
library(dplyr)

# ---- Helper: annual probability -> monthly probability ----
annual_to_monthly <- function(p_yr) 1 - (1 - p_yr)^(1/12)

# ---- 0) Load ----
usoc <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_lmno_panel_with_imd.rds")

# ---- 1) Subset + define education state ----
usoc_eng <- usoc %>%
  filter(country == "England", between(age, 16, 25)) %>% #england to match hse
  mutate(
    wave = factor(wave, levels = c("L", "M", "N", "O"), ordered = TRUE),
    edu_state = case_when(
      jbstat %in% c("Full-time student", "On apprenticeship", "Govt training scheme") ~ "post16",
      TRUE ~ "notEdu"
    ),
    edu_state = factor(edu_state, levels = c("post16", "notEdu"))
  ) %>%
  select(pidp, wave, age, edu_state)

# ---- 2) Adjacent-wave transitions (L->M, M->N, N->O) ----
transitions <- usoc_eng %>%
  arrange(pidp, wave) %>%
  group_by(pidp) %>%
  mutate(
    edu_to = lead(edu_state),
    age_to = lead(age),
    wave_to = lead(wave)
  ) %>%
  ungroup() %>%
  filter(!is.na(wave_to)) %>%                 # keeps only rows with a next wave
  filter(!is.na(edu_state), !is.na(edu_to)) %>%
  transmute(
    age_from = age,
    edu_from = edu_state,
    edu_to   = edu_to
  )

# ---- 3) Pooled annual transition matrix (16–25) ----
edu_trans_pooled <- transitions %>%
  count(edu_from, edu_to, name = "n") %>%
  group_by(edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr   = n / n_from,
    p_mo   = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(edu_from, edu_to)

print(edu_trans_pooled)

# ---- 4) Age-specific transitions (diagnostic) ----
edu_trans_by_age <- transitions %>%
  count(age_from, edu_from, edu_to, name = "n") %>%
  group_by(age_from, edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr   = n / n_from,
    p_mo   = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(age_from, edu_from, edu_to)

print(edu_trans_by_age)

# ---- 5) Age-banded transitions (model-ready) ----
edu_trans_by_band <- transitions %>%
  mutate(
    age_band = case_when(
      age_from %in% 16:17 ~ "16–17",
      age_from %in% 18:20 ~ "18–20",
      age_from %in% 21:25 ~ "21–25",
      TRUE ~ NA_character_
    ),
    age_band = factor(age_band, levels = c("16–17", "18–20", "21–25"))
  ) %>%
  filter(!is.na(age_band)) %>%
  count(age_band, edu_from, edu_to, name = "n") %>%
  group_by(age_band, edu_from) %>%
  mutate(
    n_from = sum(n),
    p_yr   = n / n_from,
    p_mo   = annual_to_monthly(p_yr)
  ) %>%
  ungroup() %>%
  arrange(age_band, edu_from, edu_to)

print(edu_trans_by_band)

# Optional check: probs sum to 1 in each (age_band, edu_from)
edu_trans_by_band %>%
  group_by(age_band, edu_from) %>%
  summarise(sum_p = sum(p_yr), .groups = "drop") %>%
  print()

# ---- 6) Save ----
dir.create("U:/Modelling/R Project/data/data_params", showWarnings = FALSE, recursive = TRUE)

saveRDS(
  edu_trans_by_band,
  "U:/Modelling/R Project/data/data_params/edu_transitions_by_band_eng_16_25.rds"
)

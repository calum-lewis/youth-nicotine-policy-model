################################################################################
# HSE 2022: Baseline population (ages 11–17) + key variables for modelling
################################################################################

# install.packages(c("haven", "dplyr", "janitor", "labelled", "forcats"))

library(haven)
library(dplyr)
library(janitor)
library(labelled)
library(forcats)

# ---- 1) Import and basic cleaning ----------------------------------------------

# Read HSE 2022 Stata file
hse_raw <- read_dta("U:/Modelling/R Project/Data/hse_2022_eul_v1.dta")

# Clean variable names + convert labelled variables to factors (readable categories)
hse <- hse_raw |>
  clean_names() |>
  mutate(across(where(is.labelled), ~ as_factor(.x, levels = "labels")))

# ---- 2) Define analytic population (ages 11–17 using AGE35G bands) --------------

# NOTE: Exact age in years has been removed from the HSE 2022 dataset
# We therefore define eligibility using AGE35G age bands.
# Bands "11-12", "13-15", and "16-19" cover ages 11–17 (but 16–19 also includes 18–19).
# In the microsimulation initialisation step below, we restrict the 16–19 band to 16–17 only.

hse <- hse |>
  mutate(
    in_11_17_band = if_else(age35g %in% c("11-12", "13-15", "16-19"), 1L, 0L)
  )

hse_11_17 <- hse |>
  filter(in_11_17_band == 1L)

# Quick check: confirm included age bands
count(hse_11_17, age35g, sort = TRUE)

# ---- 3) Select variables needed for baseline modelling --------------------------

# Create a baseline dataset containing only variables needed for:
# - defining baseline e-cigarette status
# - defining baseline cigarette smoking status
# - key demographics/SES
# - survey design variables

hse_baseline_11_17 <- hse_11_17 |>
  select(
    # Demographics / structure
    sex,
    age35g,
    in_11_17_band,
    
    # E-cigarette status (HSE-derived: never / ex / current)
    eciguse_19,
    
    # Smoking items used to derive cigarette status
    cigevr_19,
    cignow_19,
    
    # Socioeconomic status (IMD quintile)
    qimd19,
    
    # Survey design
    wt_int,
    psu_scr
  )

# Inspect categories before cleaning (useful for documenting survey routing)
count(hse_baseline_11_17, eciguse_19, sort = TRUE)
count(hse_baseline_11_17, cigevr_19, sort = TRUE)
count(hse_baseline_11_17, cignow_19, sort = TRUE)
count(hse_baseline_11_17, qimd19, sort = TRUE)
count(hse_baseline_11_17, sex, sort = TRUE)

# ---- 4) Clean non-substantive / routed responses --------------------------------

# E-cigarettes:
# - "Don't know" -> NA
# - "Not applicable" likely reflects module routing in youth -> NA (conservative)
hse_baseline_11_17 <- hse_baseline_11_17 |>
  mutate(
    eciguse_19 = na_if(eciguse_19, "Don't know"),
    eciguse_19 = na_if(eciguse_19, "Not applicable")
  )

# Cigarettes (youth routing):
# For young people in HSE, "Not applicable" commonly reflects routing and can be treated as "No".
# - Ever smoked: "Not applicable" -> "No"
# - Smoke nowadays: "Not applicable" -> "No"
hse_baseline_11_17 <- hse_baseline_11_17 |>
  mutate(
    cigevr_19 = if_else(cigevr_19 == "Not applicable", "No", as.character(cigevr_19)),
    cignow_19 = if_else(cignow_19 == "Not applicable", "No", as.character(cignow_19))
  )

# Derive cigarette smoking status to mirror e-cigarette status structure
hse_baseline_11_17 <- hse_baseline_11_17 |>
  mutate(
    cig_status = case_when(
      cignow_19 == "Yes" ~ "Current smoker",
      cigevr_19 == "Yes" & cignow_19 == "No" ~ "Ex-smoker",
      cigevr_19 == "No" ~ "Never smoker",
      TRUE ~ NA_character_
    )
  )

# Final tidy-up:
# - Drop unused factor levels
# - Make derived smoking status a factor with sensible ordering
hse_baseline_11_17_clean <- hse_baseline_11_17 |>
  mutate(
    # Make IMD explicit: retain missingness as a real "Unknown" category
    imd_group = fct_explicit_na(qimd19, na_level = "Unknown"),
    
    # Drop unused factor levels elsewhere
    eciguse_19 = fct_drop(eciguse_19),
    sex = fct_drop(sex),
    
    # Ensure cigarette smoking status has a clear ordering
    cig_status = factor(
      cig_status,
      levels = c("Never smoker", "Ex-smoker", "Current smoker")
    )
  )


# Final checks
count(hse_baseline_11_17_clean, eciguse_19, sort = TRUE)
count(hse_baseline_11_17_clean, cig_status, sort = TRUE)
count(hse_baseline_11_17_clean, qimd19, sort = TRUE)

# Save clean baseline dataset (recommended: build everything downstream from this file)
dir.create("data", showWarnings = FALSE)
saveRDS(hse_baseline_11_17_clean, "data/hse_baseline_11_17_clean.rds")

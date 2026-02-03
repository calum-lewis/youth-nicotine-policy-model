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
hse_raw <- read_dta("U:/Modelling/R Project/data/data_raw/hse_2022_eul_v1.dta")

# Clean variable names + convert labelled variables to factors
hse <- hse_raw |>
  clean_names() |>
  mutate(across(where(is.labelled), ~ as_factor(.x, levels = "labels")))

# ---- 2) Define  population (ages 11–17 using AGE35G bands) --------------

# NOTE: Exact age in years has been removed from HSE 2022.
# We therefore define eligibility using AGE35G age bands.
# Bands "11-12", "13-15", and "16-19" cover ages 11–17, but "16-19" also includes 18–19.
# We keep the full "16-19" band here and handle any refinement downstream (02.data_calculations).

hse_tgt <- hse |>
  filter(age35g %in% c("11-12", "13-15", "16-19")) |>
  mutate(
    ageband_hse = fct_drop(age35g) # keep explicit label "16-19"
  )

# ---- 3) Select variables needed for baseline modelling --------------------------


hse_2022_11_19_clean <- hse_tgt |>
  select(
    # Demographics / structure
    sex,
    ageband_hse,   # explicit "11-12", "13-15", "16-19"
    age35g,        # keep original too (optional, but helpful for traceability)
    
    # Socioeconomic status (IMD quintile)
    qimd19,
    
    # E-cigarette / vaping items (routing differs by age)
    eciguse_19,     # adult module (primarily 16–19)
    kecigevd_19,    # youth module (primarily 11–15)
    ndpnow_19,      # adult nicotine products now (diagnostic)
    
    # Cigarette smoking items (routing differs by age)
    cigevr_19,      # adult module (primarily 16–19)
    cignow_19,      # adult module (primarily 16–19)
    k_cig_reg,      # youth detailed frequency (diagnostic/granular)
    kcigregg,       # youth current smoking wording (diagnostic)
    kcigregd,       # youth ever/never summary (useful)
    
    # Survey design
    wt_int,
    psu_scr
  )


# ---- 4) Standardise non-substantive/routing responses to NA ---------------------
# Important: we keep "Not applicable" as NA everywhere
# Any interpretation of routing happens downstream in 02.data_calculations.

to_na_levels <- c("Refused", "Don't know", "Not applicable")

hse_2022_11_19_clean <- hse_2022_11_19_clean |>
  mutate(
    across(
      .cols = c(
        eciguse_19, ndpnow_19,
        cigevr_19, cignow_19,
        kecigevd_19,
        kcigregd
      ),
      .fns = ~ {
        x <- as.character(.x)
        x[x %in% to_na_levels] <- NA_character_
        x
      }
    ),
    # keep sex and ageband factors tidy
    sex = fct_drop(sex),
    ageband_hse = fct_drop(ageband_hse)
  )
# ---- 5) Save tidy output --------------------------------------------------------

saveRDS(hse_2022_11_19_clean, "U:/Modelling/R Project/data/data_clean/hse_2022_11_19_ageband_clean.rds")



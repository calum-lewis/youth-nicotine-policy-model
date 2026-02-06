################################################################################
# 01.data_cleaning: Smoking Toolkit Study (STS) – youth extract (16–17)
#
# Purpose:
# Create a CLEAN (not model-derived) dataset for downstream modelling.
#
# End product:
# - Keep core demographic + smoking/vaping variables only
# - Restrict to ages 16–17
# - Preserve variable and value labels
# - Convert labelled variables to factors (labels preserved)
# - DO NOT derive model constructs (e.g. use states, SEP groupings)
#
################################################################################

# ---- Packages -----------------------------------------------------------------
library(haven)
library(dplyr)
library(labelled)
library(forcats)
library(janitor)

# ---- Inputs -------------------------------------------------------------------
sts_path <- "C:/Users/ecq17cdl/modelling_extra_storage/STS and ATS files Dec 25/STS and ATS files Dec 25/Latest omnibus SPSS data file/omni230_39.1_65.2cot_31.3a_25.4s_113.5s_recodes_a.sav"
# didn't have enough storage for this in usual location
# ---- 1) Read raw STS data -----------------------------------------------------
sts_raw <- read_sav(sts_path)

# ---- 2) Select core variables & restrict age ----------------------------------
vars_to_keep <- c(
  "xwave",
  "xyear",
  "actage",
  "sexz",
  "sgz",
  "qual",
  "LAcode",
  "cigsmok",
  "smokstat",
  "allecig",
  "@weight0"
)

sts_16_24_clean <- sts_raw %>%
  select(any_of(vars_to_keep)) %>%
  filter(actage %in% 16:25) %>%
  mutate(
    sexz     = haven::as_factor(sexz)     |> forcats::fct_drop(),
    sgz      = haven::as_factor(sgz)      |> forcats::fct_drop(),
    qual     = haven::as_factor(qual)     |> forcats::fct_drop()
  #  smokstat = haven::as_factor(smokstat) |> forcats::fct_drop(), #this is what is used in STAPM and other UoS modelling
  #  cigsmok  = haven::as_factor(cigsmok)  |> forcats::fct_drop()
  ) %>%
  rename(
    wave   = xwave,
    year   = xyear,
    age    = actage,
    sex    = sexz,
    soc    = sgz,
    educ   = qual,
    weight = `@weight0`
  ) %>%
  relocate(wave, year, age, sex, soc, educ, LAcode, cigsmok, smokstat, allecig, weight)


# ---- Save clean dataset ----------------------------------------------------
saveRDS(
  sts_16_24_clean,
  "U:/Modelling/R Project/data/data_clean/sts_16_24_clean.rds"
)

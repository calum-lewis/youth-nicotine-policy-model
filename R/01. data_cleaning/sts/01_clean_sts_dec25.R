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
  "allecig",
  "@weight0"
)

sts_16_17_clean <- sts_raw %>%
  select(any_of(vars_to_keep)) %>%
  filter(actage %in% c(16, 17)) %>%
  
  # ---- 3) Convert labelled vars to factors (labels preserved) -----------------
mutate(
  sexz  = as_factor(sexz, levels = "labels") |> fct_drop(),
  sgz   = as_factor(sgz,  levels = "labels") |> fct_drop(),
  qual  = as_factor(qual, levels = "labels") |> fct_drop()
) %>%
  
  # ---- 4) Relabel columns (optional but recommended) --------------------------
rename(
  wave   = xwave,
  year   = xyear,
  age    = actage,
  sex    = sexz,
  soc    = sgz,
  educ   = qual,
  weight = `@weight0`
) %>%
  
  relocate(wave, year, age, sex, soc, educ, LAcode, cigsmok, allecig, weight)

# ---- 5) Quick integrity checks ------------------------------------------------
# table(sts_16_17_clean$sex, useNA = "ifany")
# table(sts_16_17_clean$soc, useNA = "ifany")
# table(sts_16_17_clean$educ, useNA = "ifany")

# ---- 6) Save clean dataset ----------------------------------------------------
saveRDS(
  sts_16_17_clean,
  "U:/Modelling/R Project/data/data_clean/sts_16_17_clean.rds"
)


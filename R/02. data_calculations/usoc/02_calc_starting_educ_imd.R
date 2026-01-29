################################################################################
# 04_params_post16_init_wave_o_16_17.R
# Estimate post-16 education/training as main activity among ages 16–17
# from UKHLS Wave o (o_indresp), using o_jbstat.
################################################################################

library(haven)
library(dplyr)
library(labelled)

# ---- Inputs -------------------------------------------------------------------
# 
usoc_path <- "U:/Modelling/R Project/data/data_clean/usoc_lmno_panel_with_imd.rds"

usoc <- readRDS(usoc_path)

# ---- Restrict to ages 16–17 and england (to match HSE) ---------------------------------------------------
usoc_16_17_eng <- usoc %>%
  filter(age %in% c(16, 17)) %>%
  filter(country %in% c('England'))

# ---- Define education/training as main activity -------------------------------
usoc_16_17_eng <- usoc_16_17_eng %>%
  mutate(
    in_ft_education_or_training_main = case_when(
      jbstat %in% c("Full-time student", "On apprenticeship", "Govt training scheme") ~ 1L,
      jbstat %in% c(
        "Paid employment(ft/pt)",
        "Self employed",
        "Unemployed",
        "Family care or home",
        "Doing something else",
        "Unpaid, family business",
        "LT sick or disabled",
        "Retired",
        "On maternity leave",
        "On furlough",
        "Temporarily laid off/short term working",
        "on shared parental leave",
        "on adoption leave"
      ) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# ---- Estimate proportions --------------------------------
# count educ status across waves
# ---- Education: starting distribution (England, ages 16–17) ----

edu_start <- usoc_16_17_eng %>%
  filter(age %in% c(16, 17),
         !is.na(in_ft_education_or_training_main)) %>%
  count(age, in_ft_education_or_training_main) %>%
  group_by(age) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()

print(edu_start)

-----------------------------------------------------
  
  # ---- IMD: starting distribution (England, pooled across waves) ----

imd_start <- usoc_16_17_eng %>%
  filter(imd2019qe_dv %in% 1:5) %>%
  count(imd2019qe_dv) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  )

print(imd_start)



# ---- Save parameter ------------------------------------------------------------
saveRDS(edu_start, "U:/Modelling/R Project/data/data_params/edu_start_eng_16_17.rds")

saveRDS(imd_start, "U:/Modelling/R Project/data/data_params/imd_start_eng_16_17.rds")
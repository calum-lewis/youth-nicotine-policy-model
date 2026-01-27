################################################################################
# 04_params_post16_init_wave_o_16_17.R
# Estimate post-16 education/training as main activity among ages 16–17
# from UKHLS Wave o (o_indresp), using o_jbstat.
################################################################################

library(haven)
library(dplyr)
library(labelled)

# ---- Inputs -------------------------------------------------------------------
# Adjust if your o_indresp file is elsewhere:
o_path <- "U:/Modelling/R Project/data_raw/us_indresp/o_indresp.dta"

usoc_o_ind <- read_dta(o_path)

# ---- Restrict to ages 16–17 ---------------------------------------------------
usoc_16_17 <- usoc_o_ind %>%
  mutate(
    age = if_else(o_age_dv < 0, NA_real_, as.numeric(o_age_dv)),
    jbstat_f = as_factor(o_jbstat, levels = "labels")
  ) %>%
  filter(age %in% c(16, 17))

# ---- Define education/training as main activity -------------------------------
usoc_16_17 <- usoc_16_17 %>%
  mutate(
    in_ft_education_or_training_main = case_when(
      jbstat_f %in% c("Full-time student", "On apprenticeship", "Govt training scheme") ~ 1L,
      jbstat_f %in% c(
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

# ---- Estimate (renormalised among non-missing) --------------------------------
edu_overall <- usoc_16_17 %>%
  count(in_ft_education_or_training_main) %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(edu_overall)

p_post16_edu <- usoc_16_17 %>%
  filter(!is.na(in_ft_education_or_training_main)) %>%
  summarise(p = mean(in_ft_education_or_training_main)) %>%
  pull(p)

# ---- Save parameter ------------------------------------------------------------
dir.create("data/params", showWarnings = FALSE)

saveRDS(
  list(
    source = "UKHLS Wave o (o_indresp), ages 16–17, o_jbstat main activity; post16 includes FT student/apprenticeship/govt training",
    p_post16_edu = p_post16_edu
  ),
  "data/params/usoc_post16_init_16_17.rds"
)

cat("Saved: data/params/usoc_post16_init_16_17.rds\n")
cat("p_post16_edu =", round(p_post16_edu, 3), "\n")

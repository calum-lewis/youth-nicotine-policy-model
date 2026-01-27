################################################################################
# 03_init_age_and_edu.R
# Initialise exact ages (months) for the synthetic population and initialise
# education status at model entry using USoc-derived post-16 proportions.
################################################################################

library(dplyr)

# Reproducibility: this seed controls BOTH age draws and education draws
set.seed(1996)

# ---- Inputs -------------------------------------------------------------------
syn_path <- "data/syn_11_17_n100k.rds"
params_path <- "data/params/usoc_post16_init_16_17.rds"

syn_11_17_n100k <- readRDS(syn_path)

edu_init_params <- readRDS(params_path)
p_post16_edu <- edu_init_params$p_post16_edu

# ---- Age initialisation --------------------------------------------------------
syn_11_17_n100k_init <- syn_11_17_n100k |>
  mutate(
    # Draw an exact age (years) uniformly within the HSE band.
    # NOTE: We intentionally restrict the 16–19 band to 16–17 at model entry.
    age_years_init = case_when(
      age35g == "11-12" ~ runif(n(), 11, 13),
      age35g == "13-15" ~ runif(n(), 13, 16),
      age35g == "16-19" ~ runif(n(), 16, 18),
      TRUE ~ NA_real_
    ),
    
    # Convert to integer months
    age_months_init = floor(age_years_init * 12),
    
    # Convenience check (years, in 1/12ths)
    age_years_check = age_months_init / 12,
    
    # ---- Education initialisation based on ACTUAL age --------------------------
    # Under 16: compulsory education (deterministic)
    # Ages 16–17: probabilistic split using USoc (Wave o) estimate:
    #   p_post16_edu = proportion whose main activity is FT education/training
    education_status = case_when(
      age_years_init < 16 ~ "in_compulsory_education",
      
      age_years_init >= 16 & age_years_init < 18 ~ if_else(
        runif(n()) < p_post16_edu,
        "post_16_education",
        "not_in_education"
      ),
      
      TRUE ~ NA_character_
    ),
    
    education_status = factor(
      education_status,
      levels = c("in_compulsory_education", "post_16_education", "not_in_education")
    )
  ) |>
  filter(!is.na(age_months_init))

# ---- Save ---------------------------------------------------------------------
saveRDS(syn_11_17_n100k_init, "data/syn_11_17_n100k_init.rds")

# ---- Quick checks --------------------------------------------------------------
range(syn_11_17_n100k_init$age_years_check)
summary(syn_11_17_n100k_init$age_years_check)

syn_11_17_n100k_init |>
  count(education_status) |>
  mutate(pct = round(100 * n / sum(n), 1))

syn_11_17_n100k_init |>
  mutate(age_group = case_when(
    age_years_init < 16 ~ "Under 16",
    age_years_init >= 16 & age_years_init < 18 ~ "16–17",
    TRUE ~ "Other"
  )) |>
  count(age_group, education_status) |>
  group_by(age_group) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup()


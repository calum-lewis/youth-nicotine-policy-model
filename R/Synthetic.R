################################################################################
# SYNTHETIC POPULATION (restart): Age-band constrained weighted resampling
################################################################################

set.seed(1996)
N_syn <- 100000

# 1) Base sample for synthetic population (clean baseline, valid weights)
base_for_syn <- hse_baseline_11_17_clean |>
  filter(!is.na(wt_int) & wt_int > 0) |> # only keep ppl with +ve weights
  mutate(age35g = fct_drop(age35g))   # remove unused factor levels (19+) cos it was tripping up code

# 2) Decide how many synthetic people to draw in each age band (based on baseline sample)
targets_age <- base_for_syn |>
  count(age35g, name = "n_band") |> # number of respondents in each band
  mutate(
    p_band = n_band / sum(n_band), # proportion of respondents in each band
    N_band = round(N_syn * p_band) # prop * syn size for band target
  )

targets_age  # <-- print this; should be 3 rows only

# 3) Sample within each age band, using wt_int as selection weights
syn_11_17_n100k <- bind_rows(
  # 11–12
  base_for_syn |>
    filter(age35g == "11-12") |>
    slice_sample( # draw rows at random
      n = targets_age$N_band[targets_age$age35g == "11-12"], # we want n to = the target band for 11-12
      replace = TRUE, # same respondent can appear multiple times
      weight_by = wt_int #use HSE weight
    ),
  
  # 13–15
  base_for_syn |>
    filter(age35g == "13-15") |>
    slice_sample(
      n = targets_age$N_band[targets_age$age35g == "13-15"],
      replace = TRUE,
      weight_by = wt_int
    ),
  
  # 16–19
  base_for_syn |>
    filter(age35g == "16-19") |>
    slice_sample(
      n = targets_age$N_band[targets_age$age35g == "16-19"],
      replace = TRUE,
      weight_by = wt_int
    )
) |>
  mutate(id = row_number())

# 4) Checks: size + age band distribution should match baseline closely
nrow(syn_11_17_n100k)

prop.table(table(base_for_syn$age35g))
prop.table(table(syn_11_17_n100k$age35g))

# Sex
prop.table(table(base_for_syn$sex))
prop.table(table(syn_11_17_n100k$sex))

# IMD (using your explicit imd_group)
prop.table(table(base_for_syn$imd_group, useNA = "ifany"))
prop.table(table(syn_11_17_n100k$imd_group, useNA = "ifany"))

# Cigarette smoking status
prop.table(table(base_for_syn$cig_status))
prop.table(table(syn_11_17_n100k$cig_status))

# E-cigarette status (note the NA share should be similar)
prop.table(table(base_for_syn$eciguse_19, useNA = "ifany"))
prop.table(table(syn_11_17_n100k$eciguse_19, useNA = "ifany"))

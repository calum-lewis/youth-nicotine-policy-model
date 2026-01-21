################################################################################
# Initialise age for microsimulation (model time)
################################################################################

set.seed(1996)

syn_11_17_n100k <- syn_11_17_n100k |>
  mutate(
    age_years_init = case_when(
      age35g == "11-12" ~ runif(n(), 11, 13),
      age35g == "13-15" ~ runif(n(), 13, 16),
      age35g == "16-19" ~ runif(n(), 16, 18),
      TRUE ~ NA_real_
    ),
    age_months_init = floor(age_years_init * 12),
    age_years_check = age_months_init / 12
  )

range(syn_11_17_n100k$age_years_check)
summary(syn_11_17_n100k$age_years_check)

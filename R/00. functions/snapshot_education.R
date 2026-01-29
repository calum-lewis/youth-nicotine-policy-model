# functions/snapshot_education.R

suppressPackageStartupMessages({
  library(dplyr)
})

make_edu_snapshot <- function(pop_df, month_t) {
  
  pop_df %>%
    mutate(
      age_years_t = (age_months_init + month_t) / 12,
      age_years_floor = floor(age_years_t),
      age_band = case_when(
        age_years_t < 16 ~ "<16",
        age_years_floor >= 16 & age_years_floor <= 17 ~ "16–17",
        age_years_floor >= 18 & age_years_floor <= 20 ~ "18–20",
        age_years_floor >= 21 & age_years_floor <= 25 ~ "21–25",
        TRUE ~ "25+"
      )
    ) %>%
    count(age_band, education_status, name = "n") %>%
    group_by(age_band) %>%
    mutate(
      prop = n / sum(n),
      month = month_t
    ) %>%
    ungroup() %>%
    select(month, age_band, education_status, n, prop) %>%
    arrange(month, age_band, education_status)
}

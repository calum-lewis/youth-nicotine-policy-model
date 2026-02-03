#Given a person’s age (numeric), 
# it returns a label for which band they fall into:
age_to_band <- function(age) {
  dplyr::case_when(
    age >= 11 & age <= 15 ~ "11–15",
    age >= 16 & age <= 17 ~ "16–17",
    age >= 18 & age <= 25 ~ "18–25",
    TRUE ~ NA_character_
  )
}
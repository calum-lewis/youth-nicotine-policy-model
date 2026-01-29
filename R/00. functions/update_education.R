# functions/update_education.R

suppressPackageStartupMessages({
  library(dplyr)
})
# put age into bands that resemble educational milestones
age_band_16_25 <- function(age_years_floor) {
  case_when(
    age_years_floor >= 16 & age_years_floor <= 17 ~ "16–17",
    age_years_floor >= 18 & age_years_floor <= 20 ~ "18–20",
    age_years_floor >= 21 & age_years_floor <= 25 ~ "21–25",
    TRUE ~ NA_character_
  )
}

# one month education update function
update_education <- function(pop_df, month_t, hazards, p_post16_edu = 0.714) {
 
  # column check 
  required_cols <- c("age_band", "p_mo_notEdu_to_post16", "p_mo_post16_to_notEdu")
  if (!all(required_cols %in% names(hazards))) {
    stop("hazards must include columns: ", paste(required_cols, collapse = ", "))
  }
  
  # get each persons age at month_t
  pop_df %>%
    mutate(
      age_months_t = age_months_init + month_t,
      age_years_t  = age_months_t / 12,
      age_years_floor = floor(age_years_t),
      age_band = age_band_16_25(age_years_floor),
      
      # if under 16 then force to be in compulsory educ
      education_status = if_else(
        age_years_t < 16,
        "in_compulsory_education",
        as.character(education_status)
      )
    )  %>% # join hazards to age band
    left_join(hazards, by = "age_band") %>%
    mutate(
      crossed_16 = (education_status == "in_compulsory_education") & (age_years_t >= 16),
      
      # generate probablity draws
      u_init  = runif(n()),
      u_trans = runif(n()),
      
      # turning-16 split when you can be in other cat
      education_status = case_when(
        crossed_16 & (u_init < p_post16_edu) ~ "post_16_education",
        crossed_16                           ~ "not_in_education",
        TRUE                                 ~ education_status
      ),
      
      # monthly transitions (16–25)
      education_status = case_when(
        age_years_t >= 16 & age_years_t <= 25 &
          education_status == "not_in_education" &
          !is.na(p_mo_notEdu_to_post16) &
          (u_trans < p_mo_notEdu_to_post16) ~ "post_16_education",
        
        age_years_t >= 16 & age_years_t <= 25 &
          education_status == "post_16_education" &
          !is.na(p_mo_post16_to_notEdu) &
          (u_trans < p_mo_post16_to_notEdu) ~ "not_in_education",
        
        TRUE ~ education_status
      ),
      
      education_status = factor(
        education_status,
        levels = c("in_compulsory_education", "post_16_education", "not_in_education")
      )
    ) %>%
    select(-p_mo_notEdu_to_post16, -p_mo_post16_to_notEdu,
           -u_init, -u_trans, -crossed_16)
}

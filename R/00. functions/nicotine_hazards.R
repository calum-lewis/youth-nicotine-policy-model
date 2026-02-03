################################################################################
# nicotine_hazards.R
#
# Purpose:
#   Return individual-level monthly nicotine hazards (cigarettes + vaping),
#   given age, IMD, and education status.
#
################################################################################

get_nicotine_hazards <- function(age,
                                 imd,
                                 edu_state,
                                 nic_haz_base,
                                 nic_mult_or) {
  
  # ---- 1) Determine age band -----------------------------------------------
  age_band <- age_to_band(age)
  
  if (is.na(age_band)) {
    stop("Age outside supported range (11–25)")
  }
  
  # ---- 2) Baseline hazards --------------------------------------------------
  base <- nic_haz_base %>%
    dplyr::filter(age_band == !!age_band)
  
  if (nrow(base) != 1) {
    stop("Baseline hazards not found or not unique for age band")
  }
  
  # ---- 3) IMD multipliers ---------------------------------------------------
  imd_row <- nic_mult_or$imd %>%
    dplyr::filter(imd == !!imd)
  
  if (nrow(imd_row) != 1) {
    imd_row <- tibble::tibble(
      OR_cig_init  = 1,
      OR_cig_cess  = 1,
      OR_vape_init = 1,
      OR_vape_cess = 1
    )
  }
  
  # ---- 4) Education multipliers --------------------------------------------
  if (!is.na(edu_state)) {
    edu_row <- nic_mult_or$edu %>%
      dplyr::filter(edu_state == !!edu_state)
    
    if (nrow(edu_row) != 1) {
      edu_row <- tibble::tibble(
        OR_cig_init  = 1,
        OR_cig_cess  = 1,
        OR_vape_init = 1,
        OR_vape_cess = 1
      )
    }
  } else {
    edu_row <- tibble::tibble(
      OR_cig_init  = 1,
      OR_cig_cess  = 1,
      OR_vape_init = 1,
      OR_vape_cess = 1
    )
  }
  
  # ---- 5) Apply odds-ratio multipliers -------------------------------------
  p_cig_init <- apply_or(
    p_base = base$p_cig_init,
    ORs = c(imd_row$OR_cig_init, edu_row$OR_cig_init)
  )
  
  p_cig_cess <- apply_or(
    p_base = base$p_cig_cess,
    ORs = c(imd_row$OR_cig_cess, edu_row$OR_cig_cess)
  )
  
  p_vape_init <- apply_or(
    p_base = base$p_vape_init,
    ORs = c(imd_row$OR_vape_init, edu_row$OR_vape_init)
  )
  
  p_vape_cess <- apply_or(
    p_base = base$p_vape_cess,
    ORs = c(imd_row$OR_vape_cess, edu_row$OR_vape_cess)
  )
  
  # ---- 6) Return ------------------------------------------------------------
  list(
    age_band     = age_band,
    p_cig_init   = p_cig_init,
    p_cig_cess   = p_cig_cess,
    p_vape_init  = p_vape_init,
    p_vape_cess  = p_vape_cess
  )
}

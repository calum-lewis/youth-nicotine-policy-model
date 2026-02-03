################################################################################
# update_nicotine.R
#
# Purpose:
#   Update an individual's cigarette and vaping status by one monthly cycle.
#
################################################################################

update_nicotine_state <- function(cig_current,
                                  vape_current,
                                  age,
                                  imd,
                                  edu_state,
                                  nic_haz_base,
                                  nic_mult_or) {
  
  # ---- 1) Get monthly hazards ----------------------------------------------
  haz <- get_nicotine_hazards(
    age = age,
    imd = imd,
    edu_state = edu_state,
    nic_haz_base = nic_haz_base,
    nic_mult_or = nic_mult_or
  )
  
  # ---- 2) Update cigarette use ---------------------------------------------
  if (isFALSE(cig_current)) {
    cig_current <- (stats::runif(1) < haz$p_cig_init)
  } else {
    cig_current <- !(stats::runif(1) < haz$p_cig_cess)
  }
  
  # ---- 3) Update vaping ----------------------------------------------------
  if (isFALSE(vape_current)) {
    vape_current <- (stats::runif(1) < haz$p_vape_init)
  } else {
    vape_current <- !(stats::runif(1) < haz$p_vape_cess)
  }
  
  # ---- 4) Derive 4-state nicotine variable ---------------------------------
  use_state <- dplyr::case_when(
    !cig_current & !vape_current ~ "no_use",
    !cig_current &  vape_current ~ "vape_only",
    cig_current & !vape_current ~ "cig_only",
    cig_current &  vape_current ~ "dual_use"
  )
  
  list(
    cig_current  = cig_current,
    vape_current = vape_current,
    use_state    = factor(
      use_state,
      levels = c("no_use", "vape_only", "cig_only", "dual_use")
    ),
    hazards = haz
  )
}



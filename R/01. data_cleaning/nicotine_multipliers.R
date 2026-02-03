mult_or <- list(
  imd = tibble::tibble(
    imd = 1:5,
    OR_cig_init  = rep(1, 5),
    OR_cig_cess  = rep(1, 5),
    OR_vape_init = rep(1, 5),
    OR_vape_cess = rep(1, 5)
  ),
  edu = tibble::tibble(
    edu_state = c("post16", "notEdu"),
    OR_cig_init  = c(1, 1),
    OR_cig_cess  = c(1, 1),
    OR_vape_init = c(1, 1),
    OR_vape_cess = c(1, 1)
  )
)

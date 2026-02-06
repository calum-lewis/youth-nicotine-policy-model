source("U:/Modelling/R Project/R/00. functions/run_microsim.R")

syn <- readRDS("U:/Modelling/R Project/data/data_clean/syn_pop_eng_11_24_N100k_with_behaviours.rds")


state_levels <- c("smoke", "vape", "dual", "nonuse")


P <- rbind(
  smoke  = c(
    smoke  = 0.85,  # continue smoking
    vape   = 0.02,  # switch to vaping
    dual   = 0.10,  # become dual
    nonuse = 0.03   # quit entirely
  ),
  
  vape   = c(
    smoke  = 0.02,  # switch to smoking
    vape   = 0.85,  # continue vaping
    dual   = 0.10,  # become dual
    nonuse = 0.03   # quit entirely
  ),
  
  dual   = c(
    smoke  = 0.10,  # drop vaping
    vape   = 0.10,  # drop smoking
    dual   = 0.75,  # continue dual use
    nonuse = 0.05   # quit both
  ),
  
  nonuse = c(
    smoke  = 0.03,  # initiate smoking
    vape   = 0.03,  # initiate vaping
    dual   = 0.01,  # initiate both
    nonuse = 0.93   # remain non-user
  )
)


model_run <- run_microsim(
                syn = syn,
                P = P,
                state_levels = state_levels,
                n_months = 120,
                seed = 1996
)
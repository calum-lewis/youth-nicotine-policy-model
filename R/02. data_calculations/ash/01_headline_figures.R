####ash headline figures as placeholder starting pop figures

start_use_11_17 <- tibble::tribble(
  ~use_state,   ~pct,
  "no_use",     90.4,
  "vape_only",   4.2,
  "cig_only",    2.6,
  "dual_use",    2.8
)

saveRDS(
  start_use_11_17,
  "U:/Modelling/R Project/data/data_params/start_use_11_17_ash_base.rds"
)

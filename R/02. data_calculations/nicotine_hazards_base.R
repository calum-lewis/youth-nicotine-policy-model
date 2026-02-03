library(tibble)
# baseline parameter table: one row per age band, four monthly probabilities:
# p_cig_init : P(start smoking this month | not currently smoking)
# p_cig_cess : P(stop smoking this month | currently smoking)
# p_vape_init : P(start vaping this month | not currently vaping)
# p_vape_cess : P(stop vaping this month | currently vaping)

nic_haz_base <- tribble(
  ~age_band, ~p_cig_init, ~p_cig_cess, ~p_vape_init, ~p_vape_cess,
  "11–15",   0.0003,      0.15,        0.0008,      0.20,
  "16–17",   0.0006,      0.12,        0.0015,      0.18,
  "18–25",   0.0003,      0.10,        0.0006,      0.15
)
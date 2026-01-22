library(dplyr)
library(tidyr)

vars <- c(
  "sex",
  "age35g",
  "in_11_17_band",
  "eciguse_19",
  "cigevr_19",
  "cignow_19",
  "qimd19"
)

dict_table <- hse_baseline_11_17 %>%
  select(all_of(vars)) %>%
  mutate(across(everything(), as.character)) %>%   # <- KEY FIX
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "level"
  ) %>%
  mutate(
    level = ifelse(is.na(level), "NA", level),
    level_type = case_when(
      level == "NA" ~ "Missing (NA)",
      level %in% c("Don't know", "Refused") ~ "Missing (DK/Refused)",
      level == "Not applicable" ~ "Routed / Not applicable",
      TRUE ~ "Observed"
    )
  ) %>%
  count(variable, level, level_type, name = "n") %>%
  group_by(variable) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(variable, desc(n))

write.csv(dict_table, "data_dictionary_hse_2022_v1.csv", row.names = FALSE)




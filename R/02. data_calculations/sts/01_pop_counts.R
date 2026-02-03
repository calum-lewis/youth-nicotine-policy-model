################################################################################
# 02.baseline_descriptives: STS – youth baseline descriptives (16–17)
#
# Purpose:
# - Load clean STS extract (already label-converted where appropriate)
# - Restrict to analysis years (2022–2025)
# - Create transparent derived variables for modelling (collapses + use_state)
# - Produce counts (and optional proportions) by year
# - Save baseline dataset for synthesis
################################################################################

library(dplyr)
library(forcats)
library(readr)

# ---- Inputs -------------------------------------------------------------------
clean_path <- "U:/Modelling/R Project/data/data_clean/sts_16_17_clean.rds"

# Outputs (adjust if you like)
out_counts_csv <- "U:/Modelling/R Project/outputs/sts_counts_by_year_16_17_2022_2025.csv"
out_baseline_rds <- "U:/Modelling/R Project/data/data_clean/sts_16_17_baseline_2022_2025.rds"

# ---- 1) Load clean data -------------------------------------------------------
sts <- readRDS(clean_path)

# ---- 2) Restrict to analysis years -------------------------------------------
sts_22_25 <- sts %>%
  filter(year >= 2022 & year <= 2025)

# ---- 3) Transparent recodes for stable cells ---------------------------------

sts_22_25 <- sts_22_25 %>%
  mutate(
    # Sex: keep Men/Women only for stable stratification
    sex2 = fct_collapse(as.factor(sex),
                        Men = "Men",
                        Women = "Women",
                        other_level = NA_character_),
    
    # Social grade: 3 groups to avoid sparse cells
    sg3 = case_when(
      as.character(soc) == "AB" ~ "AB",
      as.character(soc) %in% c("C1", "C2") ~ "C1C2",
      as.character(soc) %in% c("D", "E") ~ "DE",
      TRUE ~ NA_character_
    ),
    
    # Education: binary (optional but safe)
    educ2 = if_else(as.character(educ) == "STILL STUDYING", "Still studying", "Other"),
    
    # Use state: 4 mutually exclusive categories
    use_state = case_when(
      cigsmok == 1 & allecig == 1 ~ "Dual",
      cigsmok == 1 & allecig == 0 ~ "Smoker only",
      cigsmok == 0 & allecig == 1 ~ "Vaper only",
      cigsmok == 0 & allecig == 0 ~ "No use",
      TRUE ~ NA_character_
    )
  )

# ---- 4) Counts by year --------------------------------------------------------
counts_by_year <- sts_22_25 %>%
  filter(!is.na(use_state)) %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    smokers_only_n = sum(use_state == "Smoker only"),
    vapers_only_n  = sum(use_state == "Vaper only"),
    dual_users_n   = sum(use_state == "Dual"),
    no_use_n       = sum(use_state == "No use"),
    .groups = "drop"
  ) %>%
  arrange(year)

counts_by_year

# Optional: unweighted proportions for quick sense-checks
props_by_year <- counts_by_year %>%
  mutate(
    smokers_only_pct = round(100 * smokers_only_n / n_total, 1),
    vapers_only_pct  = round(100 * vapers_only_n  / n_total, 1),
    dual_users_pct   = round(100 * dual_users_n   / n_total, 1),
    no_use_pct       = round(100 * no_use_n       / n_total, 1)
  )

props_by_year

# ---- 5) Save outputs ----------------------------------------------------------
write_csv(counts_by_year, out_counts_csv)
saveRDS(sts_22_25, out_baseline_rds)

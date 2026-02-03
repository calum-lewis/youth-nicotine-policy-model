library(dplyr)

N <- 100000
set.seed(1)

N <- 100000
set.seed(1)

# HSE df already in memory:
hse <- hse_2022_11_19_clean

# Parameter tables (from your saved RDS files)
imd_start_eng_16_17 <- readRDS("U:/Modelling/R Project/data/data_params/imd_start_eng_16_17.rds")
edu_start_eng_16_17 <- readRDS("U:/Modelling/R Project/data/data_params/edu_start_eng_16_17.rds")

# Output path
out_spine_rds <- "U:/Modelling/R Project/data/data_clean/syn_spine_eng_11_17_N100k.rds"

# ---- 1) Create single-year age from age bands (abridging 16–19 to 16–17) -----
hse_11_17 <- hse %>%
  mutate(
    age_single = case_when(
      age35g %in% c("11-12", "11–12") ~ sample(11:12, size = n(), replace = TRUE),
      age35g %in% c("13-15", "13–15") ~ sample(13:15, size = n(), replace = TRUE),
      age35g %in% c("16-19", "16–19") ~ sample(16:17, size = n(), replace = TRUE),
      TRUE ~ NA_integer_
    ),
    sex = as.character(sex)
  ) %>%
  filter(age_single >= 11, age_single <= 17) %>%
  filter(!is.na(sex), !is.na(wt_int))

# ---- 2) Weighted joint distribution: age × sex --------------------------------
age_sex_dist <- hse_11_17 %>%
  group_by(age_single, sex) %>%
  summarise(w = sum(wt_int, na.rm = TRUE), .groups = "drop") %>%
  mutate(p = w / sum(w))

# Groups respondents by age_single and sex.
# Adds up weights within each group → w.
# Converts weights to probabilities → p (so all p’s sum to 1).

# ---- 3) Sample synthetic spine (age, sex) -------------------------------------
syn_spine <- age_sex_dist %>%
  slice_sample(n = N, weight_by = p, replace = TRUE) %>%
  transmute(
    age = age_single,
    sex = sex
  )

# slice_sample(..., weight_by = p, replace = TRUE) samples rows from age_sex_dist with probability p.
# Because it samples rows, you get exactly the joint distribution in expectation.
# transmute keeps only age and sex columns (renaming age).

# ---- 4) Assign IMD quintile using USoc parameter table ------------------------
# Expecting columns: imd2019qe_dv, pct
imd_probs_tbl <- imd_start_eng_16_17 %>%
  mutate(
    imd_q = as.character(imd2019qe_dv), #rename
    prob  = pct / sum(pct) #turn perc into prob
  ) %>%
  select(imd_q, prob) %>%
  arrange(as.numeric(imd_q)) #order quintiles

syn_spine$imd_q <- sample(
  imd_probs_tbl$imd_q,
  size = N,
  replace = TRUE,
  prob = imd_probs_tbl$prob
) # sample based on quintile probs


# ---- 5) Add FT education/training (USoc England; placeholder rules) -----------
ft_probs <- edu_start_eng_16_17 %>%
  mutate(
    age = as.integer(age),
    ft  = as.integer(in_ft_education_or_training_main), #rename
    prob = pct / 100
  ) %>%
  filter(ft %in% c(0, 1)) %>%
  select(age, ft, prob)

p16 <- ft_probs %>% filter(age == 16, ft == 1) %>% pull(prob) #perc FT 16
p17 <- ft_probs %>% filter(age == 17, ft == 1) %>% pull(prob) #perc ft 17

# Safety checks
stopifnot(length(p16) == 1, length(p17) == 1)

syn_spine <- syn_spine %>%
  mutate(
    ft_edu = case_when(
      age <= 15 ~ 1L, # assume FT under 16
      age == 16 ~ rbinom(n(), 1, p16), #flip weighted coin
      age == 17 ~ rbinom(n(), 1, p17), #flip weighted coin
      TRUE ~ NA_integer_
    )
  )

# ---- 6) Quick validation prints ----------------------------------------------
cat("\n--- Synthetic spine checks ---\n")

cat("\nAge distribution:\n")
print(table(syn_spine$age))

cat("\nSex distribution:\n")
print(table(syn_spine$sex))

cat("\nIMD distribution (synthetic):\n")
print(round(100 * prop.table(table(syn_spine$imd_q)), 1))

cat("\nIMD distribution (target from USoc):\n")
print(round(100 * imd_probs_tbl$prob, 1))
print(imd_probs_tbl$imd_q)

cat("\nFT education mean by age (synthetic):\n")
print(with(syn_spine, tapply(ft_edu, age, mean)))

cat("\nFT education targets:\n")
cat("Age 16 P(FT=1): ", p16, "\n")
cat("Age 17 P(FT=1): ", p17, "\n")

# ---- 7) Save ------------------------------------------------------------------
saveRDS(syn_spine, out_spine_rds)
message("Saved synthetic spine to: ", out_spine_rds)
################################################################################
# 02: add smoking/vaping starting levels to spine
#
# Inputs:
# - Synthetic spine (England 11–17): age, sex, imd_q, ft_edu
# - STS baseline (16–17): contains sex2 + use_state (from Script 02)
# - ASH headline prevalence for 11–15 (placeholders)
#
# Output:
# - Synthetic population with binary smoke_now and vape_now
################################################################################

library(dplyr)

# ---- Settings ----------------------------------------------------------------
set.seed(1)

#ash vaping data: https://ash.org.uk/resources/view/use-of-e-cigarettes-among-young-people-in-great-britain
#ash smoking data: https://ash.org.uk/uploads/Youth-Smoking-Fact-Sheet.pdf?v=1741882326

# ASH placeholders (11–15), taken from recent ASH report
p_smoke_11_15 <- 0.03     # current smoking
p_vape_11_15  <- 0.05     # current vaping
p_vape_given_smoke <- 0.49  # P(vape | current smoker), from ASH (11–17)

# ---- Paths -------------------------------------------------------------------
#spine built from hse and usoc
spine_path <- "U:/Modelling/R Project/data/data_clean/syn_spine_eng_11_17_N100k.rds"
#sts figures 2022-25
sts_baseline_path <- "U:/Modelling/R Project/data/data_clean/sts_16_17_baseline_2022_2025.rds"
#output path
out_path <- "U:/Modelling/R Project/data/data_clean/syn_pop_eng_11_17_N100k_with_behaviours.rds"

# ---- 1) Load data -------------------------------------------------------------
syn <- readRDS(spine_path)
sts <- readRDS(sts_baseline_path)

# ---- 2) Make sure gender lables are the same (men and women, not male/female etc)
syn <- syn %>%
  mutate(
    sex = as.character(sex),
    sex = case_when(
      sex %in% c("Male", "M") ~ "Male",
      sex %in% c("Female", "F") ~ "Female",
      TRUE ~ NA_character_
    )
  )


sts <- sts %>%
  mutate(
    sex_chr = as.character(sex),
    sex = case_when(
      sex_chr %in% c("Men", "Male") ~ "Male",
      sex_chr %in% c("Women", "Female") ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-sex_chr)

# ---- 3) Prepare STS joint behaviour distribution ------------------------------
#filter to just men/women for first draft
sts_use <- sts %>%
  filter(sex %in% c("Male","Female")) %>%
  filter(!is.na(use_state))

# Build conditional distribution P(use_state | age, sex2)
# (Unweighted for now; if you later want weights, we can add them safely)
sts_probs <- sts_use %>%
  count(age, sex, use_state, name = "n") %>% #count by use_state
  group_by(age, sex) %>% #group by age and sex
  mutate(p = n / sum(n)) %>% #within each gender/age group, calc teh prob of a given state
  ungroup() %>%
  select(age, sex, use_state, p)

# Look up the  probability  for this person’s age and sex, 
# then draw one state from it.
assign_use_state <- function(a, s) {
  sub <- sts_probs %>% filter(age == a, sex == s)
  sample(sub$use_state, size = 1, prob = sub$p)
}


# ---- 4) Assign 11–15 behaviours using ASH placeholders ------------------------
# Calibrate P(vape | non-smoker) so marginal vaping hits 5% in 11–15
# estimate vape given non-smoke based off headline figs
p_vape_given_nonsmoke <- (p_vape_11_15 - p_vape_given_smoke * p_smoke_11_15) / (1 - p_smoke_11_15)

syn <- syn %>%
  mutate(
    smoke_now = case_when(
      age <= 15 ~ rbinom(n(), 1, p_smoke_11_15), #if 15 or under go with ASH prob
      TRUE ~ NA_integer_
    ),
    vape_now = case_when(
      age <= 15 & smoke_now == 1 ~ rbinom(n(), 1, p_vape_given_smoke), #if 15 or under go with ASH prob
      age <= 15 & smoke_now == 0 ~ rbinom(n(), 1, p_vape_given_nonsmoke), #if 15 or under go with ASH prob
      TRUE ~ NA_integer_
    )
  )

# ---- 5) Assign 16–17 behaviours using STS joint distribution ------------------
syn_16_17 <- syn %>%
  filter(age %in% c(16, 17)) %>%
  mutate(
    use_state = mapply(assign_use_state, age, sex),
    smoke_now = if_else(use_state %in% c("Smoker only", "Dual"), 1L, 0L),
    vape_now  = if_else(use_state %in% c("Vaper only", "Dual"), 1L, 0L)
  ) %>%
  select(-use_state)

syn <- syn %>%
  filter(!(age %in% c(16, 17))) %>%
  bind_rows(syn_16_17) %>%
  arrange(age, sex)


# 11–15 prevalence
syn %>%
  filter(age <= 15) %>%
  summarise(
    n = n(),
    smoke_prev = mean(smoke_now),
    vape_prev  = mean(vape_now),
    dual_prev  = mean(smoke_now == 1 & vape_now == 1)
  )

# 16–17 prevalence
syn %>%
  filter(age %in% c(16, 17)) %>%
  summarise(
    n = n(),
    smoke_prev = mean(smoke_now),
    vape_prev  = mean(vape_now),
    dual_prev  = mean(smoke_now == 1 & vape_now == 1)
  )


# ---- 6) Quick checks ----------------------------------------------------------
cat("\n--- 11–15 prevalence (synthetic) ---\n")
cat("Smoking:", round(mean(syn$smoke_now[syn$age <= 15]), 4), "\n")
cat("Vaping: ", round(mean(syn$vape_now[syn$age <= 15]), 4), "\n")
cat("Calibrated P(vape | non-smoker) 11–15:", round(p_vape_given_nonsmoke, 4), "\n")

cat("\n--- 16–17 prevalence (synthetic) ---\n")
print(with(syn %>% filter(age %in% c(16, 17)), tapply(smoke_now, age, mean)))
print(with(syn %>% filter(age %in% c(16, 17)), tapply(vape_now, age, mean)))

cat("\n--- Smoke x Vape (all 11–17) ---\n")
print(with(syn, table(smoke_now, vape_now, useNA = "ifany")))

# ---- 7) Save ------------------------------------------------------------------
saveRDS(syn, out_path)

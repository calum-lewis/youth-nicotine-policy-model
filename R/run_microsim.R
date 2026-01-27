library(dplyr)

source("U:/Modelling/R Project/R/functions/update_education.R")
source("U:/Modelling/R Project/R/functions/snapshot_education.R")

# --- Reproducibility (set once per run / replicate) ---
set.seed(2026)

# --- Load baseline population ---
pop <- readRDS("data/syn_11_17_n100k_init.rds")

# --- Load education transition hazards (estimated from USoc) ---
edu_trans <- readRDS("data/params/usoc_edu_transitions_lmno_16_25.rds")

# Rename non-syntactic hazard columns (with arrows) to clean names used by update_education()
hazards <- edu_trans$band_hazards_wide %>%
  transmute(
    age_band,
    p_mo_notEdu_to_post16 = `p_mo_notEdu→post16`,
    p_mo_post16_to_notEdu = `p_mo_post16→notEdu`
  )

# --- Model horizon ---
T_months <- 120L  # e.g., 10 years

# --- Output folder (create if missing) ---
dir.create("data/outputs", showWarnings = FALSE, recursive = TRUE)

# --- Storage for monthly snapshots ---
snapshots <- vector("list", length = T_months)

# --- Monthly loop ---
for (t in seq_len(T_months)) {
  
  # 1) Update education state at month t
  pop <- update_education(
    pop_df   = pop,
    month_t  = t,
    hazards  = hazards,
    p_post16_edu = 0.714
  )
  
  # 2) Record snapshot for month t (age band x education status)
  snapshots[[t]] <- make_edu_snapshot(pop_df = pop, month_t = t)
}

# --- Bind snapshots into a single time-series table ---
edu_time_series <- bind_rows(snapshots)

# --- Save outputs ---
saveRDS(edu_time_series, "data/outputs/edu_time_series.rds")
saveRDS(pop,            "data/outputs/pop_final.rds")

# --- Quick internal checks (optional prints) ---
cat("\nSaved:\n  data/outputs/edu_time_series.rds\n  data/outputs/pop_final.rds\n")

# Check: within each month + age_band, proportions sum to ~1
check_props <- edu_time_series %>%
  group_by(month, age_band) %>%
  summarise(sum_prop = sum(prop), .groups = "drop")

cat("\nProp-sum check (should be ~1):\n")
print(check_props %>% summarise(min_sum = min(sum_prop), max_sum = max(sum_prop)))

cat("\nPreview (months 1, 12, 120):\n")
print(edu_time_series %>% filter(month %in% c(1L, 12L, 120L)) %>% arrange(month, age_band, education_status), n = 100)


file.exists("data/outputs/edu_time_series.rds")
file.exists("data/outputs/pop_final.rds")

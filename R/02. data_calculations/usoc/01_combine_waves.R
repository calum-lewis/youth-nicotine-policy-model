################################################################################
# 02.data_calculations (or 01.data_cleaning): Combine USoc LMNO indresp + IMD
# Output: long panel file with pidp, wave, age, gender, jbstat, IMD
################################################################################

library(dplyr)

# ---- 1) Load cleaned indresp waves --------------------------------------------
l <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_wave_l_indresp_clean.rds")
m <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_wave_m_indresp_clean.rds")
n <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_wave_n_indresp_clean.rds")
o <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_wave_o_indresp_clean.rds")

usoc_lmno_indresp <- bind_rows(l, m, n, o) |>
  mutate(wave = factor(wave, levels = c("L", "M", "N", "O")))

# ---- 2) Load cleaned IMD (from indall) ----------------------------------------
# ---- 2) Load cleaned IMD and manually add wave --------------------------------
l_imd <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_wave_l_indall_imd_clean.rds") |>
  mutate(wave = "L")

m_imd <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_wave_m_indall_imd_clean.rds") |>
  mutate(wave = "M")

n_imd <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_wave_n_indall_imd_clean.rds") |>
  mutate(wave = "N")

o_imd <- readRDS("U:/Modelling/R Project/data/data_clean/usoc_wave_o_indall_imd_clean.rds") |>
  mutate(wave = "O")

# Now stack them and convert to factor to match your indresp data
imd <- bind_rows(l_imd, m_imd, n_imd, o_imd) |>
  mutate(wave = factor(wave, levels = c("L", "M", "N", "O")))

# ---- 3) Join IMD onto panel ---------------------------------------------------
usoc_lmno_panel <- usoc_lmno_indresp |>
  left_join(imd, by = c("pidp", "wave"))

# ---- 4) Quick checks (optional but recommended) -------------------------------
cat("\nRows by wave:\n")
print(count(usoc_lmno_panel, wave))

cat("\nUnique people (pidp):\n")
print(usoc_lmno_panel |> summarise(n_pidp = n_distinct(pidp)))

cat("\nIMD missingness:\n")
print(usoc_lmno_panel |>
        summarise(
          miss_imd_eng = mean(is.na(imd2019qe_dv))
        ))

# ---- 5) Save ------------------------------------------------------------------
saveRDS(
  usoc_lmno_panel,
  "U:/Modelling/R Project/data/data_clean/usoc_lmno_panel_with_imd.rds"
)

cat("\nSaved: U:/Modelling/R Project/data_clean/usoc_lmno_panel_with_imd.rds\n")

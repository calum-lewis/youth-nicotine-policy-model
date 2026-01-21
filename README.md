# HSE 2022 – Vaping Analysis

This project analyses Health Survey for England (HSE) 2022 data to explore
vaping prevalence and construct a synthetic population with monthly time ticks.

## Project structure

- `R/` – analysis scripts
- `data_raw/` – raw HSE data (not tracked in Git)
- `data_clean/` – cleaned datasets (not tracked)
- `outputs/` – figures and tables

## Workflow

Scripts should be run in the following order:

1. `01_clean_hse2022.R`  
   Cleans raw HSE 2022 data and saves cleaned files to `data_clean/`.

2. `02_make_synth_pop.R`  
   Constructs a synthetic population from cleaned data.

3. `03_add_monthly_ticks.R`  
   Adds monthly time ticks and produces outputs.

Alternatively, run `00_run_all.R` to reproduce the full pipeline.

## Data access

HSE 2022 data are subject to licence restrictions and are **not included** in this repository.
Users must obtain access separately and place files in `data_raw/`.

## Requirements

- R (≥ 4.x)
- Packages listed in scripts

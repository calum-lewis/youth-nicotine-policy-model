# Youth Nicotine Policy Model

This repository contains code for the Youth Nicotine Policy Model, a modelling framework to evaluate the potential impacts of government nicotine policies on smoking and vaping behaviours among young people.

The model covers both combustible tobacco and e-cigarette use. It is intended to support policy evaluation and scenario analysis in high-income settings, with an initial focus on the UK.

## Aims

- Build a reproducible modelling framework to assess youth nicotine use under different policy scenarios
- Explore the effects of access-related and regulatory interventions on smoking and vaping behaviours
- Support evidence-informed discussion of youth nicotine policy

## Repository contents

Scripts are stored in the R folder. The project currently includes:

- 01_clean_hse2022.R: cleaning and preparing HSE 2022 survey data
- 02_make_synth_pop.R: constructing a synthetic population
- 03_add_monthly_ticks.R: adding a monthly time structure and producing intermediate outputs

Additional scripts and documentation will be added as the policy simulation components are developed.

## Data

This repository does not include raw or cleaned data.

If you are reproducing the analysis you will need to:
- obtain appropriate access to the required survey data
- place raw files in data_raw
- run the cleaning scripts locally

Note: restricted or licensed survey microdata are intentionally excluded from version control.

## How to run

Open the RStudio project file and run the scripts in order:

1. R/01_clean_hse2022.R
2. R/02_make_synth_pop.R
3. R/03_add_monthly_ticks.R

## Status

Work in progress. Structure, assumptions, and outputs may change as the project evolves.

## Intended use

This codebase is intended for academic research, policy analysis, and reproducibility. It is not intended for direct clinical or regulatory decision-making without appropriate validation.

## Contact

For questions or collaboration, please contact the repository owner via GitHub.

# libraries
library(dplyr)

# functions
source("U:/Modelling/R Project/R/00. functions/age_bands.R")
source("U:/Modelling/R Project/R/00. functions/hazards_math.R")
source("U:/Modelling/R Project/R/00. functions/nicotine_hazards.R")
source("U:/Modelling/R Project/R/04. sim/update_nicotine.R")

# params
source("U:/Modelling/R Project/R/02. data_calculations/nicotine_hazards_base.R")
source("U:/Modelling/R Project/R/02. data_calculations/nicotine_multipliers.R")
set.seed(1)

# starting person
cig  <- FALSE
vape <- FALSE
age  <- 16
imd  <- 3
edu  <- "post16"

trace <- data.frame(
  month = 0:60,
  age = NA_real_,
  cig_current = NA,
  vape_current = NA,
  use_state = NA_character_
)

# baseline at month 0
trace$age[1] <- age
trace$cig_current[1] <- cig
trace$vape_current[1] <- vape
trace$use_state[1] <- "no_use"

for (m in 1:60) {
  
  out <- update_nicotine_state(
    cig_current = cig,
    vape_current = vape,
    age = age,
    imd = imd,
    edu_state = edu,
    nic_haz_base = nic_haz_base,
    nic_mult_or = nic_mult_or
  )
  
  cig  <- out$cig_current
  vape <- out$vape_current
  
  # age up monthly
  age <- age + 1/12
  
  trace$age[m + 1] <- age
  trace$cig_current[m + 1] <- cig
  trace$vape_current[m + 1] <- vape
  trace$use_state[m + 1] <- as.character(out$use_state)
}

table(trace$use_state)
head(trace, 12)
tail(trace, 12)

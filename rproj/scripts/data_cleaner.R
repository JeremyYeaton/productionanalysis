## LOAD PACKAGES ####
library(dplyr)
library(purrr)

## READ IN DATA ####
# Full data on election results
f0_over_time = read.table("data/master.csv", header=T, sep=",")

xtabs(~condition,f0_over_time)

f0_over_time_clean = f0_over_time %>% 
  filter(series < 60)

xtabs(~obj_id,f0_over_time_clean)

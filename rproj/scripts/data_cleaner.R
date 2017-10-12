## LOAD PACKAGES ####
library(dplyr)
library(purrr)

## READ IN DATA ####
# Full data on election results
f0_over_time = read.table("data/master.csv", header=T, sep=",")

line_max_min = read.table("data/max_min_syll.csv",header=T,sep=",")

subj_meta_data = read.table("data/subj_meta.csv",header = T, sep = ",")

xtabs(~condition,f0_over_time)

aggregate(f0_over_time[,-c(1,3:5,7)], by = list(f0_over_time$subj),
          sd, na.rm = TRUE)

aggregate(f0_over_time[,-c(1,3:5,7)], by = list(f0_over_time$subj),
          mean, na.rm = TRUE)

meta_clean = subj_meta_data %>%
  arrange(subj)

f0_over_time_clean = f0_over_time %>% 
  filter(series < 60)

xtabs(~obj_id,f0_over_time_clean)

aggregate(f0_over_time_clean[,-c(1,3:5,7)], by = list(f0_over_time_clean$subj),
          sd, na.rm = TRUE)

aggregate(f0_over_time_clean[,-c(1,3:5,7)], by = list(f0_over_time_clean$subj),
          mean, na.rm = TRUE)

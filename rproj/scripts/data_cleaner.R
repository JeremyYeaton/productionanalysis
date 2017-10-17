## LOAD PACKAGES ####
library(dplyr)
library(purrr)

## READ IN DATA ####
f0_over_time = read.table("data/master.csv", header=T, sep=",")

line_max_min = read.table("data/max_min_syll.csv",header=T,sep=",")

subj_meta_data = read.table("data/subj_meta.csv",header = T, sep = ",")

## CLEAN UP TABLES TO ONLY INCLUDE 1ST 6 SYLLS #
meta_clean = subj_meta_data %>%
  arrange(subj)

maxmin_clean = line_max_min[, c(1,6:9)] %>% 
  arrange(unique) %>%
  filter(series < 60) %>%
  ungroup()

f0_over_time_clean = f0_over_time %>% 
  filter(series < 60)

# Combine three data frames
data_clean = meta_clean %>%
  inner_join(merge(x = f0_over_time_clean, y = maxmin_clean[, c(1,3:5)], by = "unique", all.x = T)) %>%
  mutate(subj = factor(subj)) %>%
  arrange(unique)

str(data_clean)

xtabs(~subj,meta_clean)

xtabs(~obj_id,f0_over_time_clean)

aggregate(f0_over_time_clean[,-c(1,3:5,7)], by = list(f0_over_time_clean$subj),
          sd, na.rm = TRUE)

aggregate(f0_over_time_clean[,-c(1,3:5,7)], by = list(f0_over_time_clean$subj),
          mean, na.rm = TRUE)

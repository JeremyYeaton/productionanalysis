library(dplyr)
library(purrr)
library(ggplot2)

## SET UP DATAFRAME ####
f0_over_time <- read.table("data/master.csv", header=T, sep=",")

line_max_min <- read.table("data/max_min_syll.csv",header=T,sep=",")

subj_meta_data <- read.table("data/subj_meta.csv",header = T, sep = ",")

# REARRANGE DATA INCLUDING ALL SYLLS 

maxmin_all <- line_max_min[, c(1,6:9)] 

meta_All = subj_meta_data #%>%
  cbind(subj_mean_duration=summAll$`mean(duration, na.rm = TRUE)`) %>%
  cbind(duration_sd=summAll$`sd(duration, na.rm = TRUE)`)

# Combine three data frames
data_All = meta_All %>%
  inner_join(merge(x = f0_over_time, y = maxmin_all[, c(1,3:5)], by = "unique", all.x = T)) %>%
  mutate(subj = factor(subj)) 

summAll <- data_All %>%
  group_by(subj) %>%
  summarize(mean(duration,na.rm=TRUE),
            sd(duration,na.rm=TRUE))

## PLOTTING ####
allseg.plot <- descr_data %>%
  # filter(condition == 'DN' | condition == 'NC') %>%
  ggplot(.,aes(x = series, y = z, color = condition)) +
  geom_smooth()
allseg.plot

countSylls.plot <- f0_over_time %>%
  group_by(condition,series) %>%
  mutate('ct' = demeaned_f0 < 111111110) #%>%
  # summarise(total = sum(ct)) #%>%
  ggplot(data = .,aes(x=series,fill = condition)) +
  geom_bar()

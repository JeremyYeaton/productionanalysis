library(dplyr)
library(purrr)
library(ggplot2)

## SET UP DATAFRAME ####
f0_over_time <- read.table("data/master.csv", header=T, sep=",")

line_max_min <- read.table("data/max_min_syll.csv",header=T,sep=",")

subj_meta_data <- read.table("data/subj_meta.csv",header = T, sep = ",") %>%
  merge(grps,'subj')

serAll<-read.table("data/serAll.csv",header=T,sep=",")

syllNames <- read.table("data/syllNames.csv",header=T,sep=",")

## MASSAGE ####
maxes <- maxmin %>%
  merge(serAll,by="series") %>%
  mutate(u_syll= paste(obj_id,"_",syll_num))

mast<- master_sheet %>%
  merge(serAll,by="series") %>%
  mutate(u_syll = paste(obj_id,"_",syll_num))

master <- maxes[c(7:9,12)] %>%
  merge(.,mast[],by="u_syll")

# Creat syll_name column
descr_data <- master %>%
  ungroup() %>%
  # merge(syllNames,by = 'syll_num') %>%
  mutate("z"=demeaned_f0/sd(demeaned_f0)) %>%
  filter(abs(z)<3)

# REARRANGE DATA INCLUDING ALL SYLLS 
maxmin_all <- line_max_min[, c(1,6:9)] 

meta_All = subj_meta_data %>%
  cbind(subj_mean_duration=summAll$meanDuration) %>%
  cbind(duration_sd=summAll$sdDuration) 

# Combine three data frames
data_All = meta_All %>%
  inner_join(merge(x = f0_over_time, y = maxmin_all[, c(1,3:5)], by = "unique", all.x = T)) %>%
  mutate(subj = factor(subj)) %>%
  merge(grps,'subj')

summAll <- data_All %>%
  group_by(subj) %>%
  summarize(meanDuration = mean(duration,na.rm=TRUE),
            sdDuration = sd(duration,na.rm=TRUE))

## PLOTTING ####
allseg.plot <- descr_data %>%
  merge(grps,'subj') %>%
  filter(grp == 2) %>%
  # filter(series < 71) %>%
  filter(series > 49) %>%
  filter(condition == 'dn' | condition == 'nc') %>%
  ggplot(.,aes(x = series, y = z, color = factor(condition))) +
  geom_smooth()
allseg.plot

countSylls.plot <- f0_over_time %>%
  group_by(condition,series) %>%
  mutate('ct' = trial > 0) %>%
  summarise(total = sum(ct)) %>%
  ggplot(data = .,aes(x=series,y=total,fill = condition)) +
  geom_bar(stat='identity')
countSylls.plot

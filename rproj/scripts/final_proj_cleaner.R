## LOAD PACKAGES####
library(dplyr)

## READ IN DATA ####
master_sheet <- read.table("data/master.csv",header=T, sep=",") %>%
  filter(series<60)
maxmin <- read.table("data/max_min_syll.csv",header=T,sep=",") %>%
  filter(series<60)
subj_meta_data <- read.table("data/subj_meta.csv",header = T, sep = ",")
ser<-read.table("data/sery.csv",header=T,sep=",")

## MASSAGE ####
maxes <- maxmin %>%
  merge(ser,by="series") %>%
  mutate(u_syll= paste(obj_id,"_",syll_num))

mast<- master_sheet %>%
  merge(ser,by="series") %>%
  mutate(u_syll = paste(obj_id,"_",syll_num))

master <- maxes[c(7:9,12)] %>%
  merge(.,mast[],by="u_syll")

# Creat syll_name column
descr_data <- master %>%
  ungroup() %>%
  mutate("syll_name"=ifelse(syll_num == 6, "6_PP1", 
                            ifelse(syll_num==5,"5_rien",
                                   ifelse(syll_num==4,"4_verb",
                                          ifelse(syll_num==3,"3_ne",
                                                 ifelse(syll_num==2,"2_sonne","1_per"))))))
descr_data<- descr_data %>%
  mutate("z"=demeaned_f0/sd(demeaned_f0)) %>%
  filter(abs(z)<3)

## BEHAVIORAL DATA ####
behave <- read.table("data/behave.csv",header= T,sep=",") %>%
  filter(Subject != "107r")

## GROUPINGS ####
grps <- read.table("data/groupings.txt",header=T,sep=",")

lsrlg <- lsrl %>%
  merge(grps,'subj')

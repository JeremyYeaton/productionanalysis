library(ggplot2)
library(dplyr)

## DURATION (BOX) PLOTS ####
duration_density.plot<-ggplot(master) + 
  geom_density(aes(x=duration,color=condition)) +
  labs(title="Overall density of durations",x="Duration")
duration_density.plot

# By syllable
per_dur.plot <- maxmin %>%
  filter(series==54) %>%
  ggplot(.,aes(x=duration)) + geom_density()
per_dur.plot

per_box.plot<- maxes %>%
  filter(syll_num==1) %>%
  ggplot(.,aes(x=series,y=duration,color=condition)) + geom_boxplot()
per_box.plot

sonne_box.plot<- maxes %>%
  filter(syll_num==2) %>%
  ggplot(.,aes(x=series,y=duration,color=condition)) + geom_boxplot() +
  labs(title="Duration by condition on sonne")
sonne_box.plot

ne_box.plot<- maxes %>%
  filter(syll_num==3) %>%
  ggplot(.,aes(x=series,y=duration,color=condition)) + geom_boxplot()
ne_box.plot

verb_box.plot<- maxes %>%
  filter(syll_num==4) %>%
  ggplot(.,aes(x=series,y=duration,color=condition)) + geom_boxplot()
verb_box.plot

rien_box.plot<- maxes %>%
  filter(syll_num==5) %>%
  ggplot(.,aes(x=series,y=duration,color=condition)) + geom_boxplot() +
  labs(title="Duration by condition on rien")
rien_box.plot

pp1_box.plot<- maxes %>%
  filter(syll_num==6) %>%
  ggplot(.,aes(x=series,y=duration,color=condition)) + geom_boxplot() +
  labs(title="Duration by condition on PP1")
pp1_box.plot

## PITCH PLOTS ####
# An overall idea
overall_pitch.plot<- ggplot(master,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  labs(title="f0 over time by condition",x="Time Series",y="Pitch")
overall_pitch.plot

# Box plots by syllable
sonne_f0.box <- descr_data %>%
  filter(syll_num==2) %>%
  ggplot(.,aes(x=syll_name,y=duration,color=condition)) +
  geom_boxplot() +
  labs(title="Pitch range on -sonne")
sonne_f0.box

verb_f0.box<- descr_data %>%
  filter(syll_num==4) %>%
  ggplot(.,aes(x=syll_name,y=duration,color=condition)) +
  geom_boxplot() +
  labs(title="Pitch range on verb")
verb_f0.box

rien_f0.box <- descr_data %>%
  filter(syll_num==5) %>%
  ggplot(.,aes(x=syll_name,y=duration,color=condition)) +
  geom_boxplot() +
  labs(title="Pitch range on rien")
rien_f0.box

pp1_f0.box <- descr_data %>%
  filter(syll_num==6) %>%
  ggplot(.,aes(x=syll_name,y=duration,color=condition)) +
  geom_boxplot() +
  labs(title="Pitch range on PP1")
pp1_f0.box

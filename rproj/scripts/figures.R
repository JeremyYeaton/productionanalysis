## READ IN DATA ####
source("scripts/data_cleaner.R")

## LOAD PACKAGES ####
library(ggplot2)

## TIME NORMALIZED PLOTS ####
combined_normal_f0.plot = ggplot(data_clean, aes(x=series, color = condition)) +
  geom_smooth(aes(y = demeaned_f0)) +
  labs(title="f0 during first 6 syllables of utterance", 
       x = "per sonne ne verb rien", 
       y= "Fundamental Frequency (Hz)")
combined_normal_f0.plot

scaled_normal_f0.plot = ggplot(data_clean, aes(x = series, color = condition)) +
  geom_smooth(aes(y = demeaned_f0/subj_stdev))
scaled_normal_f0.plot

normal_f0_bysubj.plot = ggplot(data_clean, aes(x=series, color = condition)) +
  geom_smooth(aes(y = demeaned_f0)) +
  ylim(c(-100,150)) +
  labs(title="f0 during first 6 syllables of utterance", 
       x = "per sonne ne verb rien", 
       y= "Fundamental Frequency (Hz)") +
  coord_cartesian(xlim=c(40,60), ylim=c(-25,50))
  # + facet_wrap(~subj, ncol = 4, scales = "free")
normal_f0_bysubj.plot

normal_f0_bysubj_scaled.plot = ggplot(data_clean, aes(x=series, color = condition)) +
  geom_smooth(aes(y = raw_f0)) +
  labs(title="f0 during first 6 syllables of utterance", 
       x = "per sonne ne verb rien", 
       y= "Fundamental Frequency (Hz)") +
  facet_wrap(~subj, ncol = 4)
normal_f0_bysubj_scaled.plot

divby_stdev.plot = ggplot(data_clean, aes(x=series, color = condition)) +
  geom_smooth(aes(y = demeaned_f0/subj_stdev)) +
  ylim(c(-3,3)) +
  coord_cartesian(ylim =c(-.75,1.25)) +
  labs(title="f0 during first 6 syllables of utterance", 
       subtitle= "scaled by standard deviations from the mean",
       x = "per sonne ne verb rien", 
       y= "Number of standard deviations from the mean") 
  #facet_wrap(~subj, ncol = 4, scales = "free")
divby_stdev.plot

subj_divby_stdev.plot = ggplot(data_clean, aes(x=series, color = condition)) +
  geom_smooth(aes(y = demeaned_f0/subj_stdev)) +
  #ylim(c(-3,3)) +
  #coord_cartesian(ylim =c(-.75,1.25)) +
  labs(title="f0 during first 6 syllables of utterance", 
       subtitle= "scaled by standard deviations from the mean",
       x = "per sonne ne verb rien", 
       y= "Number of standard deviations from the mean") + 
  facet_wrap(~subj, ncol = 4, scales = "free")
subj_divby_stdev.plot

## SYLL PLOTS ####
syll_duration.plot = ggplot(data=data_clean,na.rm=TRUE, aes(x = series, color = condition)) +
  geom_smooth(aes(y = duration)) +
  facet_wrap(~subj, ncol = 4)
syll_duration.plot

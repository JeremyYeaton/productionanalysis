## READ IN DATA ####
source("scripts/data_cleaner.R")

## LOAD PACKAGES ####
library(ggplot2)
library(RColorBrewer)

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
  coord_cartesian(xlim=c(40,60), ylim=c(-25,50)) +
  facet_wrap(~subj, ncol = 4, scales = "free")
normal_f0_bysubj.plot

normal_f0_bysubj_scaled.plot = ggplot(data_clean, aes(x=series, color = condition)) +
  geom_smooth(aes(y = raw_f0)) +
  labs(title="f0 during first 6 syllables of utterance", 
       x = "per sonne ne verb rien", 
       y= "Fundamental Frequency (Hz)") +
  facet_wrap(~subj, ncol = 4,scales="free")
normal_f0_bysubj_scaled.plot

divby_stdev.plot = ggplot(data_clean, aes(x=series, color = condition)) +
  geom_smooth(aes(y = demeaned_f0/subj_stdev),level=.99) +
  ylim(c(-3,3)) +
  coord_cartesian(ylim =c(-.75,1.25)) +
  labs(title="f0 during first 6 syllables of utterance", 
       subtitle= "scaled by standard deviations from the mean",
       x = "per sonne ne verb rien", 
       y= "Number of standard deviations from the mean") 
  #facet_wrap(~subj, ncol = 4, scales = "free")
divby_stdev.plot

subj_divby_stdev.plot = ggplot(data_clean, aes(x=series, color = condition)) +
  geom_smooth(aes(y = demeaned_f0/subj_stdev),level=.99) +
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

### DURATION PLOTS ####
duration_condition = data_clean %>%
  group_by(subj,condition,series) %>%
  summarize(mean=mean(duration/duration_sd,na.rm=TRUE))

duration_condition<-cbind(duration_condition,new_series=(duration_condition$series + 6)/10)
duration_condition

duration_condition.plot = ggplot(duration_condition,aes(x=new_series,y=mean,color=condition)) +
  geom_point()
duration_condition.plot


dur_pvals.plot = ggplot(dur_pvals_clean,aes(x=as.numeric(series),y=as.numeric(pval))) +
  geom_point(aes(color=test_type.f),size=3)+
  scale_y_reverse(limits=c(.05,0)) +
  facet_wrap(~test_type.f,ncol=3) +
  ggtitle("p-values from t-tests between conditions")

dur_pvals.plot

duration.plot = ggplot(data_clean, aes(x=(series)), color=condition) +
  geom_point(na.rm=TRUE,aes(y=duration/duration_sd,color=condition),position="jitter") +
  #facet_wrap(~subj,ncol=4) +
  ggtitle("Duration stuff")

duration.plot

### GRAPHS WITH P VALUES ####
myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(data_clean$condition)
colScale <- scale_colour_manual(name = "condition",values = myColors)
pvals_graph.plot = ggplot(pvals_for_graph,
                          aes(x= as.numeric(series),
                              color=test_type)) +
  #scale_x_discrete(breaks=c(0,10,20,30,40,50,60)) +
  geom_point(aes(y=pvalue),size=3) +
  scale_y_reverse(lim=c(0.05,0)) +
  facet_wrap(~test_type,ncol=3)

pvals_graph.plot

#F0 and p values for all conditions
data_w_pvals.plot = ggplot(data_clean,aes(as.numeric(series)))+
  geom_smooth(aes(y=(demeaned_f0/subj_stdev),
                  color=condition)) +
  geom_point(data=pvals_for_graph,
             aes(x=as.numeric(series),
                 y=(pvalue*(-25)+2),
                 color=test_type)) +
  scale_y_continuous(lim=c(-3,3),sec.axis=sec_axis(~(.-2)/25,name="P-value"))+
  coord_cartesian(ylim=c(-1,2)) +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  labs(x="Normalized time series",y="Number of SD from Mean") +
  ggtitle("Between condition t-test p-value",subtitle="All conditions together")
data_w_pvals.plot

#NC vs DN plot
clean_subset <- data_clean %>%
  filter(condition=="dn"| condition== "nc")

clean_subset
p_subset <- pvals_for_graph %>%
  filter(test_type=="Negative Concord X Double Negative")

nc_vs_dn_p.plot = ggplot(clean_subset,aes(as.numeric(series)))+
  geom_smooth(aes(y=(demeaned_f0/subj_stdev),
                  color=condition)) +
  geom_point(data=p_subset,
             aes(x=as.numeric(series),
                 y=(pvalue*(-25)+2))) +
  scale_y_continuous(lim=c(-3,3),sec.axis=sec_axis(~(.-2)/25,name="P-value"))+
  coord_cartesian(ylim=c(-1,2)) +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  labs(x="Normalized time series",y="Number of SD from Mean") +
  ggtitle("Between condition t-test p-value",
          subtitle="Critical Conditions: Negative Concord and Double Negation")
nc_vs_dn_p.plot

#NC vs Negob

#NC vs Negsub

#DN vs Negob

#DN vs negsub

#Negsub vs Negob


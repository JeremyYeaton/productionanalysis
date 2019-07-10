library(ggplot2)
library(dplyr)

plot.w <- 24
plot.h <- 16

## OVERALL DURATION PLOT ####
duration.means <- descr_data %>%
  group_by(syll_name,condition) %>%
  summarize("mean_d"=mean(duration)) %>%
  as.data.frame()

overall_dur.plot <- ggplot(descr_data,aes(x=syll_name,y=duration,fill=condition)) + 
  geom_boxplot() +
  #geom_point(data=duration.means,aes(x=syll_name,y=mean_d,color=condition),position="jitter",size=4) +
  labs(x="Syllable",y="Duration (ms)",title="Syllable Duration By Condition")
overall_dur.plot  %>%
  ggsave(plot=.,"figures/overall_dur.jpeg",width=plot.w,height=plot.h,units="cm")

dur_cond.data <- descr_data %>%
  group_by(condition)%>%
  summarize(mean(duration))

dn.mean <- round(dur_cond.data[1,2],2)
nc.mean <- round(dur_cond.data[2,2],2)
negob.mean <- round(dur_cond.data[3,2],2)
negsub.mean <- round(dur_cond.data[4,2],2)


dur_cond.plot <- ggplot(descr_data,aes(x=condition,y=duration,fill=condition)) +
  geom_boxplot() +
  annotate(geom="text", x="dn", y=as.numeric(dn.mean), label=as.character(dn.mean),color="black") +
  annotate(geom="text", x="nc", y=as.numeric(nc.mean), label=as.character(nc.mean),color="black") +
  annotate(geom="text", x="negob", y=as.numeric(negob.mean), label=as.character(negob.mean),color="black") +
  annotate(geom="text", x="negsub", y=as.numeric(negsub.mean), label=as.character(negsub.mean),color="black") +
  labs(x="Condition",y="Duration (ms)",title="Syllable Duration By Condition") 
dur_cond.plot  %>%
  ggsave(plot=.,"figures/dur_cond.jpeg",width=plot.w,height=plot.h,units="cm")
  

dur_bar.plot <- ggplot(descr_data,aes(group,by=syll_name,y=duration,x=condition,fill=syll_name)) +
  geom_col()
dur_bar.plot
## PITCH PLOTS ####
# An overall idea
overall_pitch.plot<- ggplot(descr_data,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  labs(title="F0 over time by condition",x="Time Series",y="F0 (Hz)") +
  annotate(geom="text", x=5, y=-20, label="per",color="black") +
  annotate(geom="text", x=15, y=-20, label="sonne",color="black") +
  annotate(geom="text", x=25, y=-20, label="ne",color="black") +
  annotate(geom="text", x=35, y=-20, label="verb",color="black") +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") 
overall_pitch.plot %>%
  ggsave(plot=.,"figures/overall_pitch.jpeg",width=plot.w,height=plot.h,units="cm")

overall_pitch_z.plot<- ggplot(descr_data,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  geom_vline(xintercept=40) +
  geom_vline(xintercept=59)+
  annotate(geom="text", x=5, y=-20, label="per",color="black") +
  annotate(geom="text", x=15, y=-20, label="sonne",color="black") +
  annotate(geom="text", x=25, y=-20, label="ne",color="black") +
  annotate(geom="text", x=35, y=-20, label="verb",color="black") +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") +
  labs(title="F0 over time by condition",x="Time Series",y="F0 (Hz)")
overall_pitch_z.plot  %>%
  ggsave(plot=.,"figures/overall_pitch_z.jpeg",width=plot.w,height=plot.h,units="cm")


# Faceted pitch
overall_facet.plot <- ggplot(descr_data,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  labs(title="F0 over time by condition",x="Time Series",y="F0 (Hz)") +
  facet_wrap(~subj)
overall_facet.plot %>%
  ggsave(plot=.,"figures/overall_facet.jpeg",width=plot.w,height=plot.h,units="cm")

# Max - min by syllable
max_min_range.plot <- descr_data %>%
  filter(syll_num==2|syll_num==5|syll_num==6) %>%
  #filter(condition=="dn"|condition=="nc") %>%
  ggplot(data=.,aes(x=syll_name,y=max_f0-min_f0,fill=condition)) + 
  geom_boxplot() +
  labs(x="Syllable",y="F0 (Hz)",title="Pitch Range by Syllable (Max F0 - Min F0)") +
  coord_cartesian(ylim=c(200,0))
max_min_range.plot %>%
  ggsave(plot=.,"figures/max_min_range.jpeg",width=plot.w,height=plot.h,units="cm")

# Zoomed pitch curve
f0_rien_zoom.plot <- descr_data %>%
  filter(syll_num==5|syll_num==6) %>%
  ggplot(.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") +
  labs(title="F0 over time by condition",x="Time Series",y="F0 (Hz)")
f0_rien_zoom.plot %>%
  ggsave(plot=.,"figures/f0_zoomed.jpeg",width=plot.w,height=plot.h,units="cm")

dn.top <- descr_data %>% 
  filter(series==48,condition=="dn") %>%
  summarize(mean(demeaned_f0))%>%
  .[,1]
dn.bottom <-descr_data %>% 
  filter(series==59,condition=="dn") %>%
  summarize(mean(demeaned_f0))%>%
  .[,1]
nc.top <- descr_data %>% 
  filter(series==47,condition=="nc") %>%
  summarize(mean(demeaned_f0))%>%
  .[,1]
nc.bottom <- descr_data %>% 
  filter(series==59,condition=="nc") %>%
  summarize(mean(demeaned_f0))%>%
  .[,1]

zoomed_triangles.plot <- descr_data %>%
  filter(syll_num==5|syll_num==6) %>%
  ggplot(.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_segment(aes(y=nc.top,yend=nc.top,x=47,xend=59,color="nc"),linetype="dashed") +
  geom_segment(aes(y=nc.bottom,yend=nc.bottom,x=47,xend=59,color="nc"),linetype="dashed") +
  geom_segment(aes(y=dn.top,yend=dn.top,x=47,xend=59,color="dn"),linetype="dashed") +
  geom_segment(aes(y=dn.bottom,yend=dn.bottom,x=47,xend=59,color="dn"),linetype="dashed") +
  geom_segment(aes(y=dn.top,yend=dn.bottom,x=50,xend=50,color="dn"),arrow=arrow(angle=20,length=unit(.125,"inches"),ends="both",type="closed")) +
  geom_segment(aes(y=nc.top,yend=nc.bottom,x=48,xend=48,color="nc"),arrow=arrow(angle=20,length=unit(.125,"inches"),ends="both",type="closed"))+
  annotate(geom="text", x=57, y=nc.top+1.5, label=round((nc.top + abs(nc.bottom)),2),color="black") +
  annotate(geom="text", x=57, y=dn.top + 1.5, label=round(dn.top + abs(dn.bottom),2),color="black") +
  geom_smooth() +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") +
  labs(title="F0 over time by condition",x="Time Series",y="F0 (Hz)")
zoomed_triangles.plot %>%
  ggsave(plot=.,"figures/zoomed_triangles.jpeg",width=plot.w,height=plot.h,units="cm")


## BEHAVIORAL PLOT ####
# [IGNORE]
behavioral_overview.plot <- behave %>%
  filter(Category == "Critical1 NC"|Category=="Critical2 DN") %>%
  ggplot(.,aes(x=Subject,fill=Column1)) +
  geom_bar()
behavioral_overview.plot

zscored_data <- descr_data %>%
  mutate("z"=demeaned_f0/sd(demeaned_f0)) %>%
  filter(z<3)

ggplot(zscored_data,aes(x=series,y=demeaned_f0)) + geom_point()

ggplot(descr_data,aes(x=condition,y=duration,fill=condition)) + geom_boxplot()
summary(aov(data=descr_data,duration~condition + syll_name))

ggplot(descr_data,aes(x=demeaned_f0,fill=condition)) + geom_density()


ggplot(master,aes(x=series, color=condition)) + geom_density() + geom_density(data=descr_data,aes(x=series,color=condition))

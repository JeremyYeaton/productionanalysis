library(ggplot2)
library(dplyr)
library(gridExtra)

lsrl <- master %>%  
  filter(condition =='nc'|condition=='dn')

plot.w <- 15
plot.h <- 8

clrs = c('darkmagenta','yellow3')
bclrs = c('yellow3','darkmagenta')

## PITCH PLOTS ####
# An overall idea
overall_pitch_lsrl.plot<- ggplot(lsrl,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  scale_color_manual(values=clrs)+
  labs(x="Time Series",y="F0 (Hz)") +
  annotate(geom="text", x=5, y=-20, label="per",color="black") +
  annotate(geom="text", x=15, y=-20, label="sonne",color="black") +
  annotate(geom="text", x=25, y=-20, label="ne",color="black") +
  annotate(geom="text", x=35, y=-20, label="verb",color="black") +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") 
overall_pitch_lsrl.plot
ggsave(plot=overall_pitch_lsrl.plot,'figures/fig_5.jpeg',width=plot.w,height=plot.h,units="cm")

f0_rien_zoom_lsrl.plot <- lsrl %>%
  filter(syll_num==5|syll_num==6) %>%
  ggplot(.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  scale_color_manual(values=clrs)+
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") +
  labs(x="Time Series",y="F0 (Hz)")
f0_rien_zoom_lsrl.plot
#ggsave(plot=f0_rien_zoom_lsrl.plot, 'figures/lsrl_f0_rien_zoom.jpeg',width=plot.w,height=plot.h,units="cm")

dn.top <- lsrl %>% 
  filter(series==48,condition=="dn") %>%
  summarize(mean(demeaned_f0)-2.5)%>%
  .[,1]
dn.bottom <-lsrl %>% 
  filter(series==59,condition=="dn") %>%
  summarize(mean(demeaned_f0)-.5)%>%
  .[,1]
nc.top <- lsrl %>% 
  filter(series==47,condition=="nc") %>%
  summarize(mean(demeaned_f0)-1)%>%
  .[,1]
nc.bottom <- lsrl %>% 
  filter(series==59,condition=="nc") %>%
  summarize(mean(demeaned_f0))%>%
  .[,1]

zoomed_triangles_lsrl.plot <- lsrl %>%
  filter(syll_num==5|syll_num==6) %>%
  ggplot(.,aes(x=series,y=demeaned_f0,color=condition)) + 
  scale_color_manual(values=clrs)+
  geom_segment(aes(y=nc.top,yend=nc.top,x=47,xend=59,color="nc"),linetype="dashed") +
  geom_segment(aes(y=nc.bottom,yend=nc.bottom,x=47,xend=59,color="nc"),linetype="dashed") +
  geom_segment(aes(y=dn.top,yend=dn.top,x=47,xend=59,color="dn"),linetype="dashed") +
  geom_segment(aes(y=dn.bottom,yend=dn.bottom,x=47,xend=59,color="dn"),linetype="dashed") +
  geom_segment(aes(y=dn.top,yend=dn.bottom,x=52,xend=52,color="dn"),arrow=arrow(angle=20,length=unit(.125,"inches"),ends="both",type="closed")) +
  geom_segment(aes(y=nc.top,yend=nc.bottom,x=50,xend=50,color="nc"),arrow=arrow(angle=20,length=unit(.125,"inches"),ends="both",type="closed"))+
  #annotate(geom="text", x=57, y=nc.top+1.5, label=round((nc.top + abs(nc.bottom)),2),color="black") +
  #annotate(geom="text", x=57, y=dn.top + 1.5, label=round(dn.top + abs(dn.bottom),2),color="black") +
  geom_smooth() +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") +
  labs(x="Time Series",y="F0 (Hz)")
zoomed_triangles_lsrl.plot
#ggsave(plot=zoomed_triangles_lsrl.plot,'figures/lsrl_zoomed_triangles.jpeg',width=plot.w,height=plot.h,units="cm")

## Grouped pitch plots ####
group1_lsrl.plot<- lsrlg %>%
  filter(grp==1)%>%
  ggplot(data=.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  scale_color_manual(values=clrs)+
  labs(x="Time Series",y="F0 (Hz)") +
  annotate(geom="text", x=5, y=-20, label="per",color="black") +
  annotate(geom="text", x=15, y=-20, label="sonne",color="black") +
  annotate(geom="text", x=25, y=-20, label="ne",color="black") +
  annotate(geom="text", x=35, y=-20, label="verb",color="black") +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") 
group1_lsrl.plot
ggsave(plot=group1_lsrl.plot,'figures/fig_9.jpeg',width=plot.w,height=plot.h,units="cm")

group2_lsrl.plot<- lsrlg %>%
  filter(grp==2)%>%
  ggplot(data=.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  scale_color_manual(values=clrs)+
  labs(x="Time Series",y="F0 (Hz)") +
  annotate(geom="text", x=5, y=-20, label="per",color="black") +
  annotate(geom="text", x=15, y=-20, label="sonne",color="black") +
  annotate(geom="text", x=25, y=-20, label="ne",color="black") +
  annotate(geom="text", x=35, y=-20, label="verb",color="black") +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") 
group2_lsrl.plot
ggsave(plot=group2_lsrl.plot,'figures/fig_8.jpeg',width=plot.w,height=plot.h,units="cm")

## PEAKS ####
peak_data <- lsrl %>%
  filter(syll_num==5) %>%
  group_by(condition,subj) %>%
  summarize(mean(demeaned_f0))

peaks <-c(6.54,16.90)
cond <- c('nc','dn')
pd <- as.data.frame(peaks) %>%
  cbind('cd'=cond) %>%
  mutate('condition'=ordered(cd,levels=c('nc','dn')))

peak_bar.plot <- ggplot(pd) +
  geom_bar(stat='identity',aes(x=as.ordered(condition),y=peaks,fill=condition)) +
  scale_fill_manual(values=bclrs) +
  labs(x='condition',y="Average peak f0 (Hz)")
peak_bar.plot

mp_peaks.plot <- grid.arrange(peak_bar.plot,f0_rien_zoom_lsrl.plot,nrow=1)
ggsave(plot=mp_peaks.plot,'figures/fig_6.jpeg',width=plot.w,height=plot.h,units="cm")

drop <-c(38.27,52.61)
colnames(x=.,prefix='drop')
ds <- as.data.frame(x=as.matrix(c(38.27,52.61),ncol=1)) %>%
  dimnames()[[2]] <- 'drop'
  cbind('condition'=ordered(cond,levels=c('nc','dn')))
ds

drop_bar.plot <- ggplot(ds) +
  geom_bar(stat='identity',aes(x=condition,y=drop,fill=condition)) +
  scale_fill_manual(values=bclrs) +
  labs(x='condition',y="max f0(rien)-min f0(PP[1]) (Hz)")
drop_bar.plot

mp_drop.plot <- grid.arrange(drop_bar.plot,zoomed_triangles_lsrl.plot,nrow=1)
ggsave(plot=mp_drop.plot,'figures/fig_7.jpeg',width=plot.w,height=plot.h,units="cm")

## BEHAVIORAL FIGURES ####

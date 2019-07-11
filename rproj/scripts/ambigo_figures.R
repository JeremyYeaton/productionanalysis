## LIBRARIES ####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)

## DATA AND CONSTANTS ####
lsrl <- master

behave <- read.table("data/behavior.csv",header=T,sep="\t") %>%
  mutate("subject"=factor(subject),'check_mm'=factor(check_mm),"item"=factor(item)) %>%
  mutate('check_mm'=ordered(check_mm,levels=c('dn','nc_mm','dn_mm','nc'))) %>%
  mutate('nc' = (condition=='nc' & Check==T)|(condition=='dn' & Check==F))

sub_order <- behave %>%
  group_by(subject) %>%
  summarize('tot_nc'=sum(nc)) %>%
  mutate('order'=NA) 
sub_order$order[order(sub_order$tot_nc)] <- 1:nrow(sub_order)

by_sub.beh <- behave %>%
  filter(condition=='nc'|condition=='dn') %>%
  group_by(subject,check_mm) %>%
  summarize("num"=length(item)) %>%
  merge(sub_order,by='subject')

# Plot size and ordering
plot.w <- 18
plot.h <- 10

levels <- c('dn','nc','negob','negsub')

# Set colors
dn_color <- '#F8766D'
nc_color <- '#7CAE00'
mismatch <- c('#F8766D','red1','green1','#7CAE00')
mismatch2 <- c('red1','#F8766D','green1','#7CAE00')
controls <- c('darkorange2','magenta4')
conditions <- c('#F8766D','#7CAE00','#00BFC4','#C77CFF')
conditions2 <- c(dn_color,nc_color)
conditions3 <- c(nc_color,dn_color)

## BEHAVIOR PLOTS ####
#Picture Choice
picture <- read.table('data/picChoiceResults.csv',header = T,sep='\t') %>%
  mutate('reading'=ordered(int,levels=c('nc','dn')),
         'condition'=ordered(condition,levels=c('Pro Pro','Pro DP','DP Pro','DP DP')))
picLabels <- picture %>%
  filter(reading=='dn')
picLabels

picResults.plot <- ggplot() +
  geom_bar(data=picture,stat='identity',aes(fill=reading,x=condition,y=value)) +
  labs(y='proportion DN') +
  scale_fill_manual(values=conditions3) +
  geom_text(data=picLabels,aes(x=condition,label=round(value,2),y=value-.05))
picResults.plot
ggsave(plot=picResults.plot,'../../ambigo/ambigoFigs/picResults.pdf',width=plot.w,height=plot.h,units="cm")

condition <- c('nc','dn','nc','dn')
picPref <- condition %>%
  cbind('lang'=c('fr','fr','en','en')) %>%
  cbind('pref'=c(.43,.57,.379,.621))
condition<- c('nc','dn')
picPrefFr <- as.data.frame(condition) %>%
  cbind('pref'=c(.43,.57))
picPrefFr.plot <- picPrefFr %>%
  ggplot(aes(x=condition,y=pref,fill=condition)) +
  geom_bar(stat="identity") +
  lims(y=c(0,1)) +
  labs(y='proportion',title='French') +
  theme(legend.position = 'none') +
  geom_text(aes(y=pref+.05,label=round(pref,2))) + 
  scale_fill_manual(values=conditions2)
picPrefFr.plot

picPrefEn <- as.data.frame(condition) %>%
  cbind('pref'=c(.379,.621))
picPrefEn.plot <- picPrefEn %>%
  ggplot(aes(x=condition,y=pref,fill=condition)) +
  geom_bar(stat="identity") +
  lims(y=c(0,1)) +
  labs(title='English',y='proportion') +
  theme(legend.position = 'none') +
  geom_text(aes(y=pref+0.05,label=round(pref,2))) + 
  scale_fill_manual(values=conditions2)
picPrefEn.plot
picPrefCombo.plot <- grid.arrange(picPrefFr.plot,picPrefEn.plot,nrow=1)
ggsave(plot=picPrefCombo.plot,'../../ambigo/ambigoFigs/picPrefCombo.pdf',width=plot.w,height=plot.h,units="cm")

picPrefSubj.plot <- read.table('data/picChoiceSubject.csv',header=T,sep='\t') %>%
  ggplot(aes(x=rank)) +
  geom_point(aes(y=X.DN,color='criticals')) +
  geom_point(aes(y=X.Faux,color='controls')) +
  geom_line(aes(y=X.DN,color='criticals')) +
  geom_line(aes(y=X.Faux,color='controls')) +
  scale_color_manual(values=controls) +
  labs(y='proportion correct/DN',x='participant') + 
  theme(axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank())
picPrefSubj.plot
ggsave(plot=picPrefSubj.plot,'../../ambigo/ambigoFigs/picPrefSubj.pdf',width=plot.w,height=plot.h,units="cm")

#Experiment 2a behavior
bySubject.plot <- ggplot(data=by_sub.beh,aes(x=order,fill=check_mm)) +
  geom_bar(aes(y=num),stat="identity") +
  scale_fill_manual(values=mismatch) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(x='participant',y='proportion dn/nc')
bySubject.plot
ggsave(plot=bySubject.plot,'../../ambigo/ambigoFigs/behaveBySubject.pdf',width=plot.w,height=plot.h,units="cm")

bySubjectDelim.plot <- ggplot(data=by_sub.beh,aes(x=order,fill=check_mm)) +
  geom_bar(aes(y=num),stat="identity") +
  scale_fill_manual(values=mismatch) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(x='participant',y='proportion dn/nc') +
  geom_vline(xintercept = c(1.5,21.5))
bySubjectDelim.plot
ggsave(plot=bySubjectDelim.plot,'../../ambigo/ambigoFigs/behaveBySubjectDelim.pdf',width=plot.w,height=plot.h,units="cm")

contVcrit.plot <- behave %>%
  mutate('critical'=(condition=='nc'|condition=='dn')) %>%
  mutate(critical=replace(critical,which(critical==T),'critical')) %>%
  mutate(critical=replace(critical,which(critical==F),'control')) %>%
  group_by(critical) %>%
  summarize('correct'=mean(Check),
            'se' = sd(Check)/sqrt(length(Check))) %>%
  mutate('condition'=critical) %>%
  ggplot(.,aes(x=condition,y=correct,fill=condition,ymin=correct-se,ymax=correct+se)) +
  geom_bar(stat='identity') +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  geom_text(aes(label=round(correct*100,2),y=correct+0.05)) +
  labs(x='condition',y='proportion correct') +
  scale_fill_manual(values=controls)
contVcrit.plot

contVcritSubj.plot <- behave %>%
  mutate('critical'=(condition=='nc'|condition=='dn')) %>%
  mutate(critical=replace(critical,which(critical==T),'critical')) %>%
  mutate(critical=replace(critical,which(critical==F),'control')) %>%
  group_by(subject,critical) %>%
  summarize('correct'=mean(Check)) %>%
  mutate('condition'=critical) %>%
  ggplot(.,aes(x=subject,y=correct,color=condition)) +
  geom_point() +
  labs(x='condition',y='proportion correct') +
  scale_fill_manual(values=controls)
contVcritSubj.plot

dnVnc.plot <- behave %>%
  filter(condition=='dn'|condition=='nc') %>%
  group_by(condition) %>%
  summarize('correct'=mean(Check),
            'se' = sd(Check)/sqrt(length(Check))) %>%
  ggplot(.,aes(x=condition,y=correct,fill=condition,ymin=correct-se,ymax=correct+se)) +
  geom_bar(stat='identity') +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  annotate(geom="text",x=c(1,2), y=c(.8,.92),label=c(72.77,87.05),color="black") +
  labs(x='condition',y='proportion context appropriate') +
  lims(y=c(0,1)) +
  scale_fill_manual(values=conditions2)
dnVnc.plot
behaveCombo.plot <- grid.arrange(contVcrit.plot,dnVnc.plot,nrow=1)
ggsave(plot=behaveCombo.plot,'../../ambigo/ambigoFigs/behaveCombo.pdf',width=plot.w,height=plot.h,units="cm")

behave2compare <- behave %>%
  filter(condition=='dn'|condition=='nc') %>%
  mutate('int'=NA)
behave2compare$int[behave2compare$nc==T] <- 'nc'
behave2compare$int[behave2compare$nc==F] <- 'dn'
behave2compare<-behave2compare %>%
  group_by(int,check_mm) %>%
  summarize('int_mean'=length(nc)/length(behave2compare$nc)) %>%
  mutate(check_mm = ordered(check_mm,levels=c('nc_mm','dn','dn_mm','nc')))
allDNvNC.plot <- behave2compare %>%
  ggplot(.,aes(x=int,y=int_mean,fill=check_mm)) +
  geom_bar(position='stack',stat='identity') +
  lims(y=c(0,1)) +
  scale_fill_manual(values=mismatch2) +
  labs(y='Proportion DN/NC',x='interpretation',title='Context task')
allDNvNC.plot

ints <-c('dn','nc')
frProPro <- ints %>%
  cbind(as.numeric(c(.47,.53))) %>%
  as.data.frame(.)
frProPro.plot <- ggplot(frProPro,aes(x=ints,y=V2,fill=V2)) +
  geom_bar(stat='identity') +
  lims(y=c(0,1)) +
  scale_fill_manual(values=conditions) +
  labs(y='Proportion DN/NC',x='interpretation',title='Context task')
frProPro.plot

sub_orderRev <-sub_order
sub_orderRev$order[order(sub_order$tot_nc)] <- nrow(sub_order):1
dnAndCont.summ <- behave %>%
  mutate('critical'=(condition=='nc'|condition=='dn')) %>%
  mutate(critical=replace(critical,which(critical==T),'critical')) %>%
  mutate(critical=replace(critical,which(critical==F),'control')) %>%
  group_by(subject,critical) %>%
  summarize('correct'=1-mean(Check),'dn'=1-mean(nc)) %>%
  merge(sub_orderRev[,c(1,3)],by='subject') %>%
  melt(id.vars=c("order",'critical')) %>%
  filter((critical=='control' & variable == 'correct')|(critical == 'critical' & variable == 'dn')) %>%
  mutate(value=as.numeric(value))
dnAndCont.summ
dnPrefFr.plot <- ggplot(dnAndCont.summ,aes(x=order,color=critical)) +
  geom_line(aes(y=value)) +
  geom_point(aes(y=value)) +
  lims(y=c(0,1)) + 
  scale_color_manual(values=controls) +
  labs(y='proportion correct/DN',x='participant') + 
  theme(axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank())
dnPrefFr.plot
ggsave(plot=dnPrefFr.plot,'../../ambigo/ambigoFigs/dnPrefBehave.pdf',width=plot.w,height=plot.h,units="cm")

## PITCH PLOTS ####
# An overall idea
overall_pitch_lsrl.plot<- ggplot(lsrl,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  scale_color_manual(values=conditions)+
  labs(x="Time",y="F0 (Hz)") +
  annotate(geom="text", x=c(5,15,25,35,45,55), y=-20, 
           label=c("per",'sonne','ne','verb','rien','PP[1]'),color="black") 
overall_pitch_lsrl.plot
ggsave(plot=overall_pitch_lsrl.plot,'../../ambigo/ambigoFigs/overall_pitch.pdf',width=plot.w,height=plot.h,units="cm")

f0_rien_zoom_lsrl.plot <- lsrlg %>%
  filter(syll_num==5|syll_num==6,grp==1) %>%
  ggplot(.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  scale_color_manual(values=conditions)+
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") +
  labs(x="Time",y="F0 (Hz)")
f0_rien_zoom_lsrl.plot
#ggsave(plot=f0_rien_zoom_lsrl.plot, 'figures/lsrl_f0_rien_zoom.jpeg',width=plot.w,height=plot.h,units="cm")

group2_rien_zoom.plot <- lsrlg %>%
  filter(syll_num==5|syll_num==6,grp==1) %>%
  ggplot(.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  scale_color_manual(values=conditions)+
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") +
  labs(x="Time",y="F0 (Hz)")
group2_rien_zoom.plot
# ggsave(plot=group2_rien_zoom.plot, '../../ambigo/ambigoFigs/group2_rien_zoom.pdf',width=plot.w,height=plot.h,units="cm")

dn.top <- lsrlg %>% 
  filter(series==48,condition=="dn",grp==1) %>%
  summarize(mean(demeaned_f0)-2.5)%>%
  .[,1]
dn.bottom <-lsrlg %>% 
  filter(series==59,condition=="dn",grp==1) %>%
  summarize(mean(demeaned_f0)-.5)%>%
  .[,1]
nc.top <- lsrlg %>% 
  filter(series==47,condition=="nc",grp==1) %>%
  summarize(mean(demeaned_f0)-1)%>%
  .[,1]
nc.bottom <- lsrlg %>% 
  filter(series==59,condition=="nc",grp==1) %>%
  summarize(mean(demeaned_f0))%>%
  .[,1]

zoomed_triangles_lsrl.plot <- lsrlg %>%
  filter(syll_num==5|syll_num==6,grp==1,(condition=='dn'|condition=='nc')) %>%
  ggplot(.,aes(x=series,y=demeaned_f0,color=condition)) + 
  scale_color_manual(values=conditions)+
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
  labs(x="Time",y="F0 (Hz)")
zoomed_triangles_lsrl.plot
#ggsave(plot=zoomed_triangles_lsrl.plot,'figures/lsrl_zoomed_triangles.jpeg',width=plot.w,height=plot.h,units="cm")

## Grouped pitch plots ####
group1_lsrl.plot<- lsrlg %>%
  filter(grp==1)%>%
  ggplot(data=.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  geom_vline(xintercept = c(40,59)) +
  scale_color_manual(values=conditions)+
  labs(x="Time",y="F0 (Hz)") +
  annotate(geom="text", x=5, y=-20, label="per",color="black") +
  annotate(geom="text", x=15, y=-20, label="sonne",color="black") +
  annotate(geom="text", x=25, y=-20, label="ne",color="black") +
  annotate(geom="text", x=35, y=-20, label="verb",color="black") +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") 
group1_lsrl.plot
ggsave(plot=group1_lsrl.plot,'../../ambigo/ambigoFigs/group_rien_full.pdf',width=plot.w,height=plot.h,units="cm")

group2_lsrl.plot<- lsrlg %>%
  filter(grp==2)%>%
  ggplot(data=.,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  scale_color_manual(values=conditions)+
  geom_vline(xintercept = c(0,20)) +
  labs(x="Time",y="F0 (Hz)") +
  annotate(geom="text", x=5, y=-20, label="per",color="black") +
  annotate(geom="text", x=15, y=-20, label="sonne",color="black") +
  annotate(geom="text", x=25, y=-20, label="ne",color="black") +
  annotate(geom="text", x=35, y=-20, label="verb",color="black") +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") 
group2_lsrl.plot
ggsave(plot=group2_lsrl.plot,'../../ambigo/ambigoFigs/group_personne_full.pdf',width=plot.w,height=plot.h,units="cm")

## PEAKS ####
pd <- lsrlg %>%
  filter(syll_num==5,grp==1,(condition=='dn'|condition=='nc')) %>%
  group_by(condition,subj,trial) %>%
  summarize('peak'=max(demeaned_f0),'se'=sd(demeaned_f0)/sqrt(length(demeaned_f0))) %>%
  summarize('meanSubPeak'=mean(peak),'se'=mean(se)) %>%
  summarize('meanPeak'=mean(meanSubPeak),'se'=mean(se)) %>%
  mutate('condition'=ordered(condition,levels=c('dn','nc')))

peak_bar.plot <- ggplot(pd,aes(x=condition,y=meanPeak,fill=condition,ymin=meanPeak-se,ymax=meanPeak+se)) +
  geom_bar(stat='identity') +
  geom_errorbar(position =position_dodge(width=.9),width=0.125) +
  scale_fill_manual(values=conditions2) +
  labs(x='condition',y="Average peak f0 (Hz)") +
  theme(legend.position = 'none')
peak_bar.plot

mp_peaks.plot <- grid.arrange(peak_bar.plot,f0_rien_zoom_lsrl.plot,nrow=1)
ggsave(plot=mp_peaks.plot,'../../ambigo/ambigoFigs/pitchPeak.pdf',width=plot.w,height=plot.h,units="cm")

drop <-c(38.27,52.61)
colnames(x=.,prefix='drop')
ds <- as.data.frame(x=as.matrix(c(38.27,52.61),ncol=1)) %>%
  dimnames()[[2]] <- 'drop'
cbind('condition'=ordered(cond,levels=c('dn','nc')))
ds

ds <- lsrlg %>%
  filter((syll_num==5|syll_num==6),grp==1,(condition=='dn'|condition=='nc')) %>%
  group_by(syll_num,condition,subj,trial) %>%
  summarize('peak'=max(demeaned_f0),'valley'=min(demeaned_f0),'se'=sd(demeaned_f0)/sqrt(length(demeaned_f0))) %>%
  summarize('meanSubPeak'=mean(peak),'meanSubValley'=mean(valley),'se'=mean(se)) %>%
  summarize('meanPeak'=mean(meanSubPeak),'meanValley'=mean(meanSubValley),'se'=mean(se)) %>%
  mutate('condition'=ordered(condition,levels=c('dn','nc'))) %>%
  group_by(condition) %>%
  summarize('drop'=meanPeak[syll_num==5]-meanValley[syll_num==6],'se'=mean(se))

drop_bar.plot <- ggplot(ds,aes(x=condition,y=drop,fill=condition,ymin=drop-se,ymax=drop+se)) +
  geom_bar(stat='identity') +
  geom_errorbar(position =position_dodge(width=.9),width=0.125) +
  scale_fill_manual(values=conditions2) +
  labs(x='condition',y="pitch drop (Hz)") +
  theme(legend.position='none')
drop_bar.plot

mp_drop.plot <- grid.arrange(drop_bar.plot,zoomed_triangles_lsrl.plot,nrow=1)
ggsave(plot=mp_drop.plot,'../../ambigo/ambigoFigs/pitchDrop.pdf',width=plot.w,height=plot.h,units="cm")

## CONSOLE STUFF ####

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
overall_dur.plot  #%>%
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
dur_cond.plot # %>%
ggsave(plot=.,"figures/dur_cond.jpeg",width=plot.w,height=plot.h,units="cm")

dur.summ <- maxmin %>%
  group_by(subj,series,condition) %>%
  summarize('durMean' = mean(duration),
            'seSub' = sd(duration)/sqrt(length(maxmin$duration))) %>%
  group_by(series,condition) %>%
  summarize('dur_mean' = mean(durMean),
            'se' = mean(seSub))


dur_bar.plot <- ggplot(descr_data,aes(group,by=condition,y=duration,x=syll_name,fill=condition)) +
  geom_col(position='dodge') 
dur_bar.plot

dur_bar.plot <- ggplot(dur.summ,aes(x=series,y=dur_mean,fill=condition,ymin=dur_mean-se,ymax=dur_mean+se)) +
  geom_col(position='dodge') +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  labs(x='syllable',y='duration (ms)')
dur_bar.plot
## PITCH PLOTS ####
# An overall idea
overall_pitch.plot<- ggplot(descr_data,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  labs(title="F0 over time by condition",x="Time",y="F0 (Hz)") +
  annotate(geom="text", x=5, y=-20, label="per",color="black") +
  annotate(geom="text", x=15, y=-20, label="sonne",color="black") +
  annotate(geom="text", x=25, y=-20, label="ne",color="black") +
  annotate(geom="text", x=35, y=-20, label="verb",color="black") +
  annotate(geom="text", x=45, y=-20, label="rien",color="black") +
  annotate(geom="text", x=55, y=-20, label="PP[1]",color="black") 
overall_pitch.plot #%>%
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
  labs(title="F0 over time by condition",x="Time",y="F0 (Hz)")
overall_pitch_z.plot  %>%
  ggsave(plot=.,"figures/overall_pitch_z.jpeg",width=plot.w,height=plot.h,units="cm")


# Faceted pitch
overall_facet.plot <- ggplot(descr_data,aes(x=series,y=demeaned_f0,color=condition)) + 
  geom_smooth() +
  labs(title="F0 over time by condition",x="Time",y="F0 (Hz)") +
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
  labs(title="F0 over time by condition",x="Time",y="F0 (Hz)")
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
  labs(title="F0 over time by condition",x="Time",y="F0 (Hz)")
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


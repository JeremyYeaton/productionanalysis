library(dplyr)
library(purrr)
library(ggplot2)
library(viridis)

## SET UP DATAFRAME ####
# serAll<-read.table("data/serAll.csv",header=T,sep=",")

f0_over_time <- read.table("data/master.csv", header=T, sep=",") %>%
  merge(read.table("data/serAll.csv",header=T,sep=","),by="series") %>%
  mutate(u_syll = paste(obj_id,"_",syll_num))

maxMinAll <- read.table("data/max_min_syll.csv",header=T,sep=",")

# grps <- read.table("data/groupings.txt",header=T,sep=",")

subj_meta_data <- read.table("data/subj_meta.csv",header = T, sep = ",") %>%
  merge(read.table("data/groupings.txt",header=T,sep=","),'subj')

syllNames <- read.table("data/syllNames.csv",header=T,sep=",")

# Plot size and ordering
plot.w <- 18
plot.h <- 10

# levels <- c('dn','nc','negob','negsub')
# 
# Set colors
allColors <- plasma(5)
dn_color <- allColors[3]
nc_color <- allColors[1]
negOb_color <- allColors[4]
negSub_color <- allColors[2]
# dn_color <- '#F8766D'
# nc_color <- '#7CAE00'
# mismatch <- c('#F8766D','red1','green1','#7CAE00')
# mismatch2 <- c('red1','#F8766D','green1','#7CAE00')
# controls <- c('darkorange2','magenta4')
# conditions <- c('#F8766D','#7CAE00','#00BFC4','#C77CFF')
# conditions2 <- c(dn_color,nc_color)
# conditions3 <- c(nc_color,dn_color)

## MASSAGE ####
maxes <- maxMinAll %>%
  merge(read.table("data/serAll.csv",header=T,sep=","),by="series") %>%
  mutate(u_syll= paste(obj_id,"_",syll_num))

master <- maxes[c(9,12)] %>%
  merge(f0_over_time,by="u_syll")

metaAll = master %>%
  group_by(subj) %>%
  summarize(meanDuration = mean(duration,na.rm=TRUE),
            sdDuration = sd(duration,na.rm=TRUE)) %>%
  merge(subj_meta_data,'subj')

allData <- master %>%
  merge(metaAll,'subj')

# data_All = meta_All %>%
#   inner_join(merge(x = f0_over_time, y = maxmin_all[, c(1,3:5)], by = "unique", all.x = T)) %>%
#   mutate(subj = factor(subj)) %>%
#   merge(grps,'subj')

# subjSD <- allData %>%
#   group_by(subj) %>%
#   summarise(f0SD = sd(demeaned_f0))

descr_data <- allData %>%
  group_by(subj) %>%
  summarise(f0SD = sd(demeaned_f0)) %>%
  merge(allData,'subj')%>%
  mutate("z"=demeaned_f0/f0SD) %>%
  filter(abs(z)<3)

## N0RMALIZE TIME ON PP ####
allPP <- descr_data %>%
  group_by(trial) %>%
  summarise(totSyll = max(syll_num)) %>%
  merge(descr_data,by='trial') %>%
  filter(syll_num > 5) %>%
  group_by(totSyll)

reSeries <- data.frame(newSer=50:69,
                       y.8 = as.integer(seq.int(50,79,length.out = 20)),
                       y.9 = as.integer(seq.int(50,89,length.out = 20)),
                       y.11 = as.integer(seq.int(50,109,length.out = 20)))
terpo <- allPP %>%
  filter((totSyll == 8 & series %in% reSeries$y.8) |
           (totSyll == 9 & series %in% reSeries$y.9) |
           (totSyll == 11 & series %in% reSeries$y.11))

terpo8 <- terpo %>%
  filter(totSyll == 8) %>%
  mutate(y.8 = series) %>%
  merge(reSeries, by = 'y.8')

terpo9 <- terpo %>%
  filter(totSyll == 9) %>%
  mutate(y.9 = series) %>%
  merge(reSeries, by = 'y.9')

terpo11 <- terpo %>%
  filter(totSyll == 11) %>%
  mutate(y.11 = series) %>%
  merge(reSeries, by = 'y.11')

rescMaster <- descr_data %>%
  group_by(trial) %>%
  summarise(totSyll = max(syll_num)) %>%
  merge(descr_data,by='trial') %>%
  mutate(newSer = series) %>%
  filter(!(totSyll > 7 & syll_num > 5)) %>%
  rbind(terpo8[,2:23]) %>%
  rbind(terpo9[,2:23]) %>%
  rbind(terpo11[,2:23])
rm(allPP,terpo,terpo8,terpo9,terpo11,reSeries)

combControl.df <- rescMaster %>%
  mutate(cond2 = condition)
levels(combControl.df$cond2)[5] <- 'control'
combControl.df$cond2[combControl.df$cond2 == 'negob' | combControl.df$cond2 == 'negsub'] <- 'control'



annY = -1


## PLOTTING ####
allseg.plot <- descr_data %>%
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




# PLOT NORMALIZED DATA
ymini = -1.25
ymaxi = 1.25
terpo1.plot <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(series < 71) %>%
  # filter(series > 49) %>%
  filter(condition == 'dn' | condition == 'nc') %>%
  ggplot(.,aes(x = newSer, y = z, color = condition)) +
  geom_smooth() +
  # geom_vline(xintercept=40) +
  # geom_vline(xintercept=59)+
  # annotate(geom="text", x=5, y=annX, label="per",color="black") +
  # annotate(geom="text", x=15, y=annX, label="sonne",color="black") +
  scale_color_manual(values = c(dn_color,nc_color,negOb_color,negSub_color)) +
  annotate(geom="text", x=9, y=annY, label="subject",color="black") +
  annotate(geom="text", x=25, y=annY, label="ne",color="black") +
  annotate(geom="text", x=35, y=annY, label="verb",color="black") +
  annotate(geom="text", x=45, y=annY, label="object",color="black") +
  annotate(geom="text", x=60, y=annY, label="PP",color="black") +
  labs(x="Time",y="z-scored f0") + # title="f0 -- Group 1",
  coord_cartesian(ylim=c(ymini, ymaxi))
terpo1.plot %>%
  ggsave(plot=.,"figures/interpZscoreG1.jpeg",width=plot.w,height=plot.h,units="cm")

terpo2.plot <- rescMaster %>%
  filter(grp == 2) %>%
  # filter(series < 71) %>%
  # filter(series > 49) %>%
  filter(condition == 'dn' | condition == 'nc') %>%
  ggplot(.,aes(x = newSer, y = z, color = condition)) +
  geom_smooth() +
  # geom_vline(xintercept=40) +
  # geom_vline(xintercept=59)+
  # annotate(geom="text", x=5, y=annX, label="per",color="black") +
  # annotate(geom="text", x=15, y=annX, label="sonne",color="black") +
  scale_color_manual(values = c(dn_color,nc_color,negOb_color,negSub_color)) +
  annotate(geom="text", x=9, y=annY, label="subject",color="black") +
  annotate(geom="text", x=25, y=annY, label="ne",color="black") +
  annotate(geom="text", x=35, y=annY, label="verb",color="black") +
  annotate(geom="text", x=45, y=annY, label="object",color="black") +
  annotate(geom="text", x=60, y=annY, label="PP",color="black") +
  labs(x="Time",y="z-scored f0") + #title="f0 -- Group 2",
  coord_cartesian(ylim=c(ymini, ymaxi))
terpo2.plot %>%
  ggsave(plot=.,"figures/interpZscoreG2.jpeg",width=plot.w,height=plot.h,units="cm")

control.plot <- combControl.df %>%
  filter(grp == 1) %>%
  # filter(series < 71) %>%
  # filter(series > 49) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  ggplot(.,aes(x = newSer, y = z, color = cond2)) +
  geom_smooth() +
  # geom_vline(xintercept=40) +
  # geom_vline(xintercept=59)+
  # annotate(geom="text", x=5, y=annX, label="per",color="black") +
  # annotate(geom="text", x=15, y=annX, label="sonne",color="black") +
  scale_color_manual(values = c(dn_color,nc_color,negOb_color)) +
  annotate(geom="text", x=9, y=annY, label="subject",color="black") +
  annotate(geom="text", x=25, y=annY, label="ne",color="black") +
  annotate(geom="text", x=35, y=annY, label="verb",color="black") +
  annotate(geom="text", x=45, y=annY, label="object",color="black") +
  annotate(geom="text", x=60, y=annY, label="PP",color="black") +
  labs(title="f0 -- Group 1",x="Time",y="z-scored f0") +
  coord_cartesian(ylim=c(ymini, ymaxi))
control.plot

terpoCrit1.plot <- RM1 %>%
  filter(grp == 1) %>%
  # filter((newSer > 15 & newSer <40)) %>%
  # filter((newSer > 45 & newSer <60)) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  ggplot(.,aes(x = newSer, y = z, color = condition)) +
  # geom_smooth(method = 'glm') +
  geom_smooth() +
  # geom_vline(xintercept=47) +
  scale_color_manual(values = c(dn_color,nc_color,negOb_color,negSub_color)) +
  annotate(geom="text", x=9, y=annY, label="subject",color="black") +
  annotate(geom="text", x=25, y=annY, label="ne",color="black") +
  annotate(geom="text", x=35, y=annY, label="verb",color="black") +
  annotate(geom="text", x=45, y=annY, label="object",color="black") +
  annotate(geom="text", x=60, y=annY, label="PP",color="black") +
  labs(x="Time",y="z-scored f0") # title="f0 -- Group 1",
terpoCrit1.plot %>%
  ggsave(plot=.,"figures/interpZscoreG1crit.jpeg",width=plot.w,height=plot.h,units="cm")

asdf <- RM1 %>%
  # filter(newSer %in% seq(0,69,by=5)) %>%
  filter(newSer %in% c(0,14,41,48,59,69)) %>%
  filter(grp == 1) %>%
  filter(condition == 'dn' | condition == 'nc') %>%
  group_by(condition,newSer) %>%
  summarise(zpt = mean(z),
            zsem = sd(z)/sqrt(length(z))) %>%
  ggplot(.,aes(x = newSer, y = zpt)) +
  geom_line(aes(color = condition),size = 1.5) +
  # geom_smooth(aes(color = condition)) +
  scale_color_manual(values = c(dn_color,nc_color)) +
  geom_pointrange(aes(ymin = zpt - zsem, ymax = zpt + zsem)) +
  annotate(geom="text", x=9, y=annY, label="subject",color="black") +
  annotate(geom="text", x=25, y=annY, label="ne",color="black") +
  annotate(geom="text", x=35, y=annY, label="verb",color="black") +
  annotate(geom="text", x=45, y=annY, label="object",color="black") +
  annotate(geom="text", x=60, y=annY, label="PP",color="black") +
  labs(title="f0 -- Group 1",x="Time",y="z-scored f0")
asdf

asdf.data1 <- RM1 %>%
  filter(grp == 1) %>%
  filter(condition == 'nc' | condition == 'dn') %>%
  filter(newSer < 15)

asdf.data2 <- RM1 %>%
  filter(grp == 1) %>%
  filter(condition == 'nc' | condition == 'dn') %>%
  filter(newSer > 13 & newSer < 42)

asdf.data3 <- RM1 %>%
  filter(grp == 1) %>%
  filter(condition == 'nc' | condition == 'dn') %>%
  filter(newSer > 40 & newSer < 49)

asdf.data4 <- RM1 %>%
  filter(grp == 1) %>%
  filter(condition == 'nc' | condition == 'dn') %>%
  filter(newSer > 47 & newSer < 60)

asdf.data5 <- RM1 %>%
  filter(grp == 1) %>%
  filter(condition == 'nc' | condition == 'dn') %>%
  filter(newSer > 58 & newSer < 70)

asdf1 <- ggplot() +
  geom_smooth(data = asdf.data1,
            aes(x = newSer, y = z, color = condition),size = 1.5, method = 'glm') +
  geom_smooth(data = asdf.data2,
              aes(x = newSer, y = z, color = condition),size = 1.5, method = 'glm') +
  geom_smooth(data = asdf.data3,
              aes(x = newSer, y = z, color = condition),size = 1.5, method = 'glm') +
  geom_smooth(data = asdf.data4,
              aes(x = newSer, y = z, color = condition),size = 1.5, method = 'glm') +
  geom_smooth(data = asdf.data5,
              aes(x = newSer, y = z, color = condition),size = 1.5, method = 'glm') +
  # geom_smooth(aes(color = condition)) +
  # geom_pointrange(aes(ymin = zpt - zsem, ymax = zpt + zsem)) +
  annotate(geom="text", x=9, y=annY, label="subject",color="black") +
  annotate(geom="text", x=25, y=annY, label="ne",color="black") +
  annotate(geom="text", x=35, y=annY, label="verb",color="black") +
  annotate(geom="text", x=45, y=annY, label="object",color="black") +
  annotate(geom="text", x=60, y=annY, label="PP",color="black") +
  labs(title="f0 -- Group 1",x="Time",y="z-scored f0")
asdf1

meanbytime <- RM1 %>%
# rescMaster %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  group_by(condition,newSer) %>%
  summarise(zpt = mean(z),
            zsem = sd(z)/sqrt(length(z))) %>%
  ggplot(.,aes(x = newSer, y = zpt + .5)) +
  geom_line(aes(color = condition),size = 1.5) +
  geom_vline(xintercept=14) +
  geom_vline(xintercept=41) +
  geom_vline(xintercept=48)
meanbytime

#####
library(DescTools)
RM2 <- RM1 %>%
  filter(grp == 1) %>%
  filter(condition == 'negob') %>%
  # filter(newSer > 40 & newSer < 61)
  filter(newSer > 5 & newSer < 30)
AUC(RM2$newSer,RM2$z + .5,method = 'spline')

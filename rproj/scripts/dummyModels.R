library(dplyr)
library(ggplot2)
library(lme4)
library(reshape2)
library(ez)
# RM1 <- rescMaster

rescMaster <- RM1 %>%
  mutate(dummy = ifelse(condition == 'dn',1,0)) %>%
  mutate(condition = dummy)

# rescMaster <- RM1


# DN as 1 all else as 0 
cond1.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 13 & newSer < 42) %>%
  lmer(z ~ condition 
       + (1|trial) 
       + (1|subj) 
       ,data = .)

cond2.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 40 & newSer < 49) %>%
  lmer(z ~ condition 
       + (1|trial) 
       + (1|subj) 
       ,data = .)

cond3.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 47 & newSer < 60) %>%
  lmer(z ~ condition 
       + (1|trial) 
       + (1|subj) 
       ,data = .)

condTime1.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 13 & newSer < 42) %>%
  lmer(z ~ condition 
       + newSer
       + (1|trial) 
       + (1|subj) 
       ,data = .)

condTime2.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 40 & newSer < 49) %>%
  lmer(z ~ condition 
       + newSer
       + (1|trial) 
       + (1|subj) 
       ,data = .)

condTime3.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 47 & newSer < 60) %>%
  lmer(z ~ condition 
       + newSer
       + (1|trial) 
       + (1|subj) 
       ,data = .)

anova(cond1.lmer,condTime1.lmer)
anova(cond2.lmer,condTime2.lmer)
anova(cond3.lmer,condTime3.lmer)

condTimeSlope1.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 13 & newSer < 42) %>%
  lmer(z ~ (newSer * condition) 
       + newSer 
       + condition 
       + (1|trial) 
       + (1|subj) 
       ,data = .)

condTimeSlope2.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 40 & newSer < 49) %>%
  lmer(z ~ (newSer * condition) 
       + newSer 
       + condition 
       + (1|trial) 
       + (1|subj) 
       ,data = .)

condTimeSlope3.lmer <- rescMaster %>%
  filter(grp == 1) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  # filter(condition == cond1 | condition == cond2) %>%
  filter(newSer > 47 & newSer < 60) %>%
  lm(z ~ 
       (newSer * condition)
       # + newSer
       # + condition 
       # + (1|trial) 
       # + (1|subj) 
       ,data = .)
summary(condTimeSlope3.lmer)

anova(condTime1.lmer,condTimeSlope1.lmer)
anova(condTime2.lmer,condTimeSlope2.lmer)
anova(condTime3.lmer,condTimeSlope3.lmer)

#####
meanXcond <- RM1 %>%
  filter(grp == 1) %>%
  group_by(condition,newSer) %>%
  summarise(mXc = mean(z))%>%
            # ,mXcSE = sd(z)/sqrt(length(z))) %>%
  dcast(newSer~condition,value.var = 'mXc')

ymini = -.75
ymaxi = 1

ggplot(meanXcond,aes(x=newSer)) +
  geom_line(aes(y = dn-nc), color = 'black', size = 2) +
  geom_line(aes(y = dn-negob), color = 'blue', size = 2) +
  geom_line(aes(y = dn-negsub), color = 'darkred', size = 2) +
  geom_line(aes(y = dn), color = 'darkgreen',size = 2) +
  ylim(ymini,ymaxi)

ggplot(meanXcond,aes(x=newSer)) +
  geom_line(aes(y = nc-dn), color = 'black', size = 2) +
  geom_line(aes(y = nc-negob), color = 'blue', size = 2) +
  geom_line(aes(y = nc-negsub), color = 'red', size = 2) +
  geom_line(aes(y = nc), color = 'darkgreen',size = 2) 
  ylim(ymini,ymaxi)

ggplot(meanXcond,aes(x=newSer)) +
  geom_line(aes(y = negob-dn), color = 'black') +
  geom_line(aes(y = negob-nc), color = 'blue') +
  geom_line(aes(y = negob-negsub), color = 'red') +
  ylim(ymini,ymaxi)

ggplot(meanXcond,aes(x=newSer)) +
  geom_line(aes(y = negsub-dn), color = 'black') +
  geom_line(aes(y = negsub-nc), color = 'blue') +
  geom_line(aes(y = negsub-negob), color = 'red') +
  ylim(ymini,ymaxi)
####
rescMaster <- RM1 %>%
  filter(grp == 1) %>%
  mutate(dummy = ifelse(condition == 'dn',1,0)) %>%
  mutate(condition = factor(dummy),subj = factor(subj))

rescMaster %>%
  filter(newSer < 15) %>%
  ezANOVA(.,z,subj,c(newSer,condition))

rescMaster %>%
  filter(newSer > 13 & newSer < 42) %>%
  ezANOVA(.,z,subj,c(newSer,condition))

rescMaster %>%
  filter(newSer > 40 & newSer < 49) %>%
  ezANOVA(.,z,subj,c(newSer,condition))

rescMaster %>%
  filter(newSer > 47 & newSer < 60) %>%
  ezANOVA(.,z,subj,c(newSer,condition))


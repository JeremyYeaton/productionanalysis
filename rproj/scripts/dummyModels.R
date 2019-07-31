library(reshape2)
# RM1 <- rescMaster

rescMaster <- RM1 %>%
  mutate(dummy = ifelse(condition == 'nc',1,0)) %>%
  mutate(condition = dummy)


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
  lmer(z ~ (newSer * condition) 
       + newSer 
       + condition 
       + (1|trial) 
       + (1|subj) 
       ,data = .)

anova(condTime1.lmer,condTimeSlope1.lmer)
anova(condTime2.lmer,condTimeSlope2.lmer)
anova(condTime3.lmer,condTimeSlope3.lmer)

#####
meanXcond <- RM1 %>%
  group_by(condition,newSer) %>%
  summarise(mXc = mean(z))%>%
            # ,mXcSE = sd(z)/sqrt(length(z))) %>%
  melt(id.vars=c('condition'),
       value.name = 'mXc')
####
rescMaster <- RM1 %>%
  mutate(dummy = ifelse(condition == 'nc',1,0)) %>%
  mutate(condition = dummy) %>%
  filter(grp == 1)

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



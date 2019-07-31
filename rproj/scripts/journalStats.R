library(dplyr)
library(ggplot2)
library(lme4)

## DESCRIPTIVE ####
n_by_condition <- rescMaster %>%
  group_by(condition) %>%
  summarize(n_distinct(obj_id))
n_by_condition

## PAIRWISE BY DURATION BY SYLL ####
dur.lm <- rescMaster %>%
  # filter(syll_num ==2) %>%
  # filter((duration/sd(duration))<3)%>%
  lm(data=.,duration ~ condition)
summary(dur.lm)

## DURATION BY CONDITION ####
dur_cond.lm <- lm(data= rescMaster,duration~syll_name + condition*syll_name)
summary(dur_cond.lm)

ggplot(descr_data,aes(y=duration, x=syll_name,color=condition))+geom_point(position="jitter")

dur.tbl <-descr_data %>%
  group_by("Condition"=condition) %>%
  summarize("mean"=mean(duration),"SD"=sd(duration))

descr_data %>%
  filter(condition="nc"|condition="dn")%>%
  ggplot(.,aes(y=duration,x=condition,fill=condition))+ geom_boxplot() + geom_point(aes(x=condition,y=mean(duration)))

## PAIRWISE BY MAX - MIN BY SYLL ####
maxmin.lm <- lm(data=descr_data,(max_f0-min_f0)~ condition*series + condition + series)
summary(maxmin.lm)

## MAX F0 5 TO MIN F0 6 ####

## LINEAR FOR SUBJECT ####
over46 <- descr_data %>%
  filter(series>46)
rien.model <- lm(data=over46,demeaned_f0 ~ condition * series)
summary(rien.model)

full_utt.lmer <- glm(data=descr_data,demeaned_f0 ~ condition*syll_name + condition +syll_name + subj)
summary(full_utt.lmer)

basic47.lmer <- lm(data=over46,demeaned_f0 ~ series + condition)
summary(basic47.lmer)

series47.lmer <-transform(over46,sery=factor(order(series))) %>%
  lm(data=.,demeaned_f0~ series + condition + series*condition)
summary(series47.lmer)

full.lm <- lm(data=over46,demeaned_f0 ~ series*condition + series + condition)
summary(full.lm)

anova(ser_cond_slope.lmer,full.lm)

series47.glm <-glm(data=over46, demeaned_f0~ series + condition + series*condition + subj)
summary(series47.glm)

15:38
38:47
47:70

# ## LINEAR MIXED EFFECTS MODELS ####
# baseline0.lmer <- lmer(demeaned_f0 ~ series * condition +(1|subj),
#                        data = over46)
# 
# baseline1.lmer <- lmer(demeaned_f0 ~ (1+condition|subj),
#                        data = over46)
# 
# baseline_cond.lmer <-lmer(demeaned_f0 ~ condition + (1+condition|subj),
#                           data = over46)
# anova(baseline_cond.lmer,baseline1.lmer)
# 
# baseline_ser.lmer <- lmer(demeaned_f0 ~ series + (1+condition|subj),
#                           data = over46)
# anova(baseline_ser.lmer,baseline1.lmer)
# 
# ser_cond.lmer <- lmer(demeaned_f0 ~ condition + series +(1+condition|subj),
#                       data = over46)
# anova(ser_cond.lmer,baseline_cond.lmer)
# 
# ser_cond_slope.lmer <- lmer(demeaned_f0 ~ series*condition + condition + series +  (1+condition|subj),
#                             data = over46)
# anova(ser_cond_slope.lmer,ser_cond.lmer)
# summary(ser_cond_slope.lmer)
# 
# ser_cond_slope_sub.lmer <- lmer(demeaned_f0 ~ condition + series + series*condition + (1|subj) + (1+condition|subj),
#                                 data = over46)
# anova(ser_cond_slope_sub.lmer,ser_cond_slope.lmer)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
RM1 <- rescMaster

rescMaster <- rescMaster %>%
  mutate(dummy = ifelse(condition == 'dn',1,0))


# DN as 1 all else as 0 
cond1 = 'negsub'
cond2 = 'negob'
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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test <- rescMaster %>%
  filter(grp == 1) %>%
  filter(condition == 'dn' | condition == 'negob') %>%
  # filter(newSer > 10 & newSer < 40) %>%
  filter(newSer > 45 & newSer < 60) %>%
  # filter(newSer > 50) %>%
  lmer(z ~ (newSer * condition) 
       + newSer 
       + condition 
       + (1|trial) 
       + (1|subj) 
       # + (1 + subj|condition)
       ,data = .)
summary(test)

test1 <- rescMaster %>%
  filter(grp == 1) %>%
  filter(condition == 'nc' | condition == 'negob') %>%
  # filter(newSer > 10 & newSer < 40) %>%
  filter(newSer > 45 & newSer < 60) %>%
  # filter(newSer > 50) %>%
  lmer(z ~ newSer + condition + (1|trial) + (1|subj),data = .)
summary(test1)

test2 <- anova(test,test1)
test2

## SUBJECT VARIANCE TABLE ####
sub_var.tbl <- descr_data %>%
  group_by(subj,syll_name) %>%
  summarize(var(demeaned_f0))
sub_var.tbl

var.tbl <- descr_data %>%
  group_by(condition) %>%
  summarise(var(demeaned_f0))
var.tbl


## OLD STUFF ####

full.mdl.f0 <- lmer(data = data_clean, 
                    raw_f0 ~condition+
                      series +
                      trial +
                      (1+condition|subj),
                    REML = F)

null.mdl.f0 <- lmer(data = data_clean, 
                    raw_f0 ~ series +
                      trial +
                      (1+condition|subj),
                    REML = F)

compare <- anova(null.mdl.f0,full.mdl.f0)
compare
summary(compare)
coef(full.mdl.f0)





##GARBAGE DURATION MODEL ####
full.mdl.dur <- lmer(data = line_max_min, 
                     duration ~ condition + 
                       series +
                       (1|subj) +
                       (1|trial),
                     REML = F)

null.mdl.dur <- lmer(data = line_max_min, 
                     duration ~ series +
                       (1|subj) +
                       (1|trial),
                     REML = F)

compare <- anova(null.mdl.dur,full.mdl.dur)
compare
summary(compare)
coef(full.mdl.dur)
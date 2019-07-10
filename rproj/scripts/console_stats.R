library(dplyr)
library(ggplot2)
library(lme4)

## DESCRIPTIVE ####
n_by_condition <- descr_data %>%
  group_by(condition) %>%
  summarize(n_distinct(obj_id))
n_by_condition

## PAIRWISE BY PITCH BY SYLL ####
descr_data <- zscored_data

## PAIRWISE BY DURATION BY SYLL ####
dur.lm <- descr_data %>%
  filter(syll_num ==2) %>%
  filter((duration/sd(duration))<3)%>%
  lm(data=.,duration ~ condition)
summary(dur.lm)

## DURATION BY CONDITION ####
dur_cond.lm <- lm(data= descr_data,duration~syll_name + condition*syll_name)
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

## LINEAR MIXED EFFECTS MODELS ####
baseline0.lmer <- lmer(demeaned_f0 ~ series * condition +(1|subj),
                     data = over46)

baseline1.lmer <- lmer(demeaned_f0 ~ (1+condition|subj),
                   data = over46)

baseline_cond.lmer <-lmer(demeaned_f0 ~ condition + (1+condition|subj),
                         data = over46)
anova(baseline_cond.lmer,baseline1.lmer)

baseline_ser.lmer <- lmer(demeaned_f0 ~ series + (1+condition|subj),
                         data = over46)
anova(baseline_ser.lmer,baseline1.lmer)

ser_cond.lmer <- lmer(demeaned_f0 ~ condition + series +(1+condition|subj),
                         data = over46)
anova(ser_cond.lmer,baseline_cond.lmer)

ser_cond_slope.lmer <- lmer(demeaned_f0 ~ series*condition + condition + series +  (1+condition|subj),
                          data = over46)
anova(ser_cond_slope.lmer,ser_cond.lmer)
summary(ser_cond_slope.lmer)

ser_cond_slope_sub.lmer <- lmer(demeaned_f0 ~ condition + series + series*condition + (1|subj) + (1+condition|subj),
                            data = over46)
anova(ser_cond_slope_sub.lmer,ser_cond_slope.lmer)

## SUBJECT VARIANCE TABLE ####
sub_var.tbl <- descr_data %>%
  group_by(subj,syll_name) %>%
  summarize(var(demeaned_f0))
sub_var.tbl

var.tbl <- descr_data %>%
  group_by(condition) %>%
  summarise(var(demeaned_f0))
var.tbl

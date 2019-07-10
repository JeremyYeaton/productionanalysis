## LOAD PACKAGES####
library(dplyr)

## DESCRIPTIVE STATS ####
# Some useful tables:
overall_descr.tbl<-summarize(descr_data,"Mean Pitch"=mean(demeaned_f0),"Pitch SD"=sd(demeaned_f0),"Mean Duration"=mean(duration),"Duration SD"=sd(duration))

by_syll_descr.tbl<-descr_data %>%
  group_by("Syllable"=syll_name) %>%
  summarize(.,"Mean Pitch"=mean(demeaned_f0),"Pitch SD"=sd(demeaned_f0),"Mean Duration"=mean(duration),"Duration SD"=sd(duration))

by_condition_descr.tbl <- descr_data %>%
  group_by("Condition"=condition)%>%
  summarize(.,"Mean Pitch"=mean(demeaned_f0),"Pitch SD"=sd(demeaned_f0),"Mean Duration"=mean(duration),"Duration SD"=sd(duration))

by_syll_cond_descr.tbl <- descr_data %>%
  group_by("Syllable"=syll_name,"Condition"=condition) %>%
  summarize(.,"Mean Pitch"=mean(demeaned_f0),"Pitch SD"=sd(demeaned_f0),"Mean Duration"=mean(duration),"Duration SD"=sd(duration))

def.tbl <- matrix(c("nc","dn","negob","negsub",
                    "Negative Concord (Single negation)","Double Negative","Negative Subject (personne only)","Negative Object (rien only)"),
                  ncol=2)
colnames(def.tbl)<- c("Abbreviation","Spell-out")

syll_def.tbl <- matrix(c("per","sonne*","ne","verb","rien*","PP1* (First syllable of prepositional phrase following rien)",
                         "0-9","10-19","20-29","30-39","40-49","50-59"),ncol=2)
colnames(syll_def.tbl)<-c("Syllable","Time range")

## ANOVA ####
duration.aov <- aov(duration ~ condition, data= maxes)
aov.sum<-summary(duration.aov)

## CATEGORICAL REGRESSIONS: DURATION X WORD ####
# Overall
overall_duration.lm <- lm(duration~condition,data=maxmin)
summary(overall_duration.lm)

# Per
per_duration.lm <- maxmin%>%
  filter(series==4) %>%
  lm(duration~condition,data=.)
summary(per_duration.lm)

# Sonne
sonne_duration.lm <- maxmin%>%
  filter(series==14) %>%
  lm(duration~condition,data=.)
summary(sonne_duration.lm)

# Ne
ne_duration.lm <- maxmin%>%
  filter(series==24) %>%
  lm(duration~condition,data=.)
summary(ne_duration.lm)

# Verb
verb_duration.lm <- maxmin%>%
  filter(series==34) %>%
  lm(duration~condition,data=.)
summary(verb_duration.lm)

# Rien
rien_duration.lm <- maxmin%>%
  filter(series==44) %>%
  lm(duration~condition,data=.)
summary(rien_duration.lm)

# PP[1]
pp1_duration.lm <- maxmin%>%
  filter(series==54) %>%
  lm(duration~condition,data=.)
summary(pp1_duration.lm)

## CATEGORICAL REGRESSIONS: PITCH X WORD ####
# Per
per_pitch.lm <- master_sheet %>%
  filter(series <10) %>%
  lm(demeaned_f0~condition,data=.)
summary(per_pitch.lm)

# Sonne
sonne_pitch.lm <- master_sheet %>%
  filter(series >9 & series <20) %>%
  lm(demeaned_f0~condition,data=.)
summary(sonne_pitch.lm)

# Ne
ne_pitch.lm <- master_sheet %>%
  filter(series >19 & series <30) %>%
  lm(demeaned_f0~condition,data=.)
summary(ne_pitch.lm)

# Verb
verb_pitch.lm <- master_sheet %>%
  filter(series >29 & series <40) %>%
  lm(demeaned_f0~condition,data=.)
summary(verb_pitch.lm)

# Rien
rien_pitch.lm <- master_sheet %>%
  filter(series >39 & series <50) %>%
  lm(demeaned_f0~condition,data=.)
summary(rien_pitch.lm)

# PP[1]
pp1_pitch.lm <- master_sheet %>%
  filter(series >49 & series <60) %>%
  lm(demeaned_f0~condition,data=.)
summary(pp1_pitch.lm)

## NONCATEGORICAL REGRESSIONS ####
pitchXduration.lm <- master %>%
  lm(demeaned_f0~duration,data=.)
summary(pitchXduration.lm)

durationXpitch.lm <- master %>%
  lm(duration~demeaned_f0,data=.)
summary(durationXpitch.lm)

## LOGISTIC REGRESSION ####
#Condition X Duration
overall_dur.glm <- glm(condition~duration*syll_num,data=maxes,family=binomial(link="logit"))
summary(overall_dur.glm)

glm_data <- maxes %>%
  mutate(negsub=ifelse(condition=="dn",1,0))


# 4 different logistic regressions to show no difference
sonne_dur_NS.glm <- glm_data%>%
  filter(series==14) %>%
  glm(negsub~duration,data=.,family=binomial(link="logit"))
sonne_dur_glm.sum <-summary(sonne_dur_NS.glm)

rien_dur.glm <- glm_data %>%
  filter(series==44) %>%
  glm(condition~duration,data=.,family=binomial(link="logit"))
rien_dur_glm.sum<-summary(rien_dur.glm)

pp1_dur.glm <- glm_data %>%
  filter(series==54) %>%
  glm(condition~duration,data=.,family=binomial(link="logit"))
pp1_dur_glm.sum<-summary(pp1_dur.glm)
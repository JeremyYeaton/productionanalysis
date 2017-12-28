## READ IN DATA ####
source("scripts/data_cleaner.R")

## LOAD PACKAGES ####
library(tidyr)
library(ez)
library(ggplot2)

#Name some things
conditions <- c('nc','dn','negsub','negob')
#lists which will store t-test values
p_series <- c(0:59,0:59,0:59,0:59,0:59,0:59)
pvalues <- vector("list",360)
condition_vector <- vector("list",360)

## CLEAN THE DATA ####
data_stats = data_clean %>%
  mutate(conditions = factor(condition, 
                             levels = c("nc", "dn","negsub","negob"))) %>%
  mutate(series_fctr = factor(series,levels = 0:59)) %>%
  group_by(series, condition) %>%
  #summarise(series_avg = mean(demeaned_f0, na.rm = T)) %>%
  ungroup()  
data_stats
#filter to groups by condition
data_nc_stats = data_stats %>%
  filter(condition == "nc")
data_dn_stats = data_stats %>%
  filter(condition == "dn")
data_negob_stats = data_stats %>%
  filter(condition == "negob")
data_negsub_stats = data_stats %>%
  filter(condition == "negsub")

## PREPARE T TESTS ####
for (row in 0:59){
  row_ttest_stats = data_stats %>%
    filter(series == row)
  row_data_nc_stats = row_ttest_stats %>%
    filter(condition == "nc")
  row_data_dn_stats = row_ttest_stats %>%
    filter(condition == "dn")
  row_data_negob_stats = row_ttest_stats %>%
    filter(condition == "negob")
  row_data_negsub_stats = row_ttest_stats %>%
    filter(condition == "negsub")
  ncvdn.ttest = t.test(row_data_nc_stats$demeaned_f0,
                       row_data_dn_stats$demeaned_f0,
                       paired =F)
  ncvnegob.ttest = t.test(row_data_nc_stats$demeaned_f0,
                          row_data_negob_stats$demeaned_f0,
                          paired = F)
  ncvnegsub.ttest = t.test(row_data_nc_stats$demeaned_f0,
                           row_data_negsub_stats$demeaned_f0,
                           paired = F)
  dnvnegob.ttest = t.test(row_data_dn_stats$demeaned_f0,
                          row_data_negob_stats$demeaned_f0,
                          paired = F)
  dnvnegsub.ttest = t.test(row_data_dn_stats$demeaned_f0,
                           row_data_negsub_stats$demeaned_f0,
                           paired = F)
  negobvnegsub.ttest = t.test(row_data_negob_stats$demeaned_f0,
                              row_data_negsub_stats$demeaned_f0,
                              paired = F)
  pvalues[[(row + 1)]] <- ncvdn.ttest$p.value
  pvalues[[(row + 61)]] <- ncvnegob.ttest$p.value
  pvalues[[(row + 121)]] <- ncvnegsub.ttest$p.value
  pvalues[[(row + 181)]] <-dnvnegob.ttest$p.value
  pvalues[[(row + 241)]] <-dnvnegsub.ttest$p.value
  pvalues[[(row + 301)]] <- negobvnegsub.ttest$p.value
  condition_vector[[row+1]]<-"Negative Concord X Double Negative"
  condition_vector[[row+61]] <-"Negative Concord X Negative Object Control"
  condition_vector[[row+121]] <- "Negative Concord X Negative Subject Control"
  condition_vector[[row+181]]<- "Double Negative X Negative Object Control"
  condition_vector[[row+241]]<- "Double Negative X Negative Subject Control"
  condition_vector[[row+301]]<- "Negative Object Control X Negative Subject Control"
}
pvalues
condition_vector
pvals_data <- matrix(c(p_series),nrow=360,ncol=1) %>%
  cbind(c(pvalues)) %>%
  cbind(c(condition_vector)) 
colnames(pvals_data) = c("series","pvalue","test_type")
pvals_data

write.table(pvals_data,file="data/ttest_pvals.txt",sep=",")
read.table("data/ttest_pvals.txt",header=TRUE,sep=",")
#unlink("data/ttest_pvals.txt")
pvals_for_graph = read.table("data/ttest_pvals.txt",header=T,sep=",") %>%
  mutate(series = factor(series))
pvals_for_graph

### DURATION T TESTS ####
nc_dur = filter(duration_condition,condition=='nc')
dn_dur = filter(duration_condition,condition=='dn')
negob_dur = filter(duration_condition,condition=='negob')
negsub_dur = filter(duration_condition,condition=='negsub')

rows=c(4,14,24,34,44,54)
duration_pvals <- vector('list',36)
conditions_vector <- vector('list',36)
series_vector <-vector('list',36)
for (row in rows){
  ncvdn.ttest = t.test(filter(data_clean,condition=='nc' & series==row)$duration, 
                       filter(data_clean,condition=='dn' & series==row)$duration,
                       paired=F)$p.value
  ncvnegob.ttest = t.test(filter(data_clean,condition=='nc' & series==row)$duration, 
                          filter(data_clean,condition=='negob' & series==row)$duration,
                          paired=F)$p.value
  ncvnegsub.ttest = t.test(filter(data_clean,condition=='nc' & series==row)$duration, 
                           filter(data_clean,condition=='negsub' & series==row)$duration,
                           paired=F)$p.value
  dnvnegob.ttest = t.test(filter(data_clean,condition=='negob' & series==row)$duration, 
                          filter(data_clean,condition=='dn' & series==row)$duration,
                          paired=F)$p.value
  dnvnegsub.ttest = t.test(filter(data_clean,condition=='negsub' & series==row)$duration, 
                           filter(data_clean,condition=='dn' & series==row)$duration,
                           paired=F)$p.value
  negobvnegsub.ttest = t.test(filter(data_clean,condition=='negob' & series==row)$duration, 
                              filter(data_clean,condition=='negsub' & series==row)$duration,
                              paired=F)$p.value
  duration_pvals[[(row+6)/10]]<-ncvdn.ttest
  duration_pvals[[((row+6)/10)+6]] <- ncvnegob.ttest
  duration_pvals[[((row+6)/10)+12]] <- ncvnegsub.ttest
  duration_pvals[[((row+6)/10)+18]] <- dnvnegob.ttest
  duration_pvals[[((row+6)/10)+24]] <- dnvnegsub.ttest
  duration_pvals[[((row+6)/10)+30]] <- negobvnegsub.ttest
  
  conditions_vector[[(row+6)/10]]<-'NCXDN'
  conditions_vector[[((row+6)/10)+6]] <- 'NCXNegOb'
  conditions_vector[[((row+6)/10)+12]] <- 'NCXNegSub'
  conditions_vector[[((row+6)/10)+18]] <- 'DNXNegOb'
  conditions_vector[[((row+6)/10)+24]] <- 'DNXNegSub'
  conditions_vector[[((row+6)/10)+30]] <- 'NegObXNegSub'
  
  series_vector[[(row+6)/10]]<-row
  series_vector[[((row+6)/10)+6]] <- row
  series_vector[[((row+6)/10)+12]] <- row
  series_vector[[((row+6)/10)+18]] <- row
  series_vector[[((row+6)/10)+24]] <- row
  series_vector[[((row+6)/10)+30]] <- row
}
data_clean
ncvdn.ttest = t.test(filter(data_clean,condition=='nc' & series==44)$duration, 
                     filter(data_clean,condition=='dn' & series==44)$duration,
                     paired=F)
ncvdn.ttest
dur_pvals =c(series_vector) %>%
  cbind(c(duration_pvals)) %>%
  cbind(c(conditions_vector))
colnames(dur_pvals)=c("series","pval","test_type")

dur_pvals_clean <- data.frame(dur_pvals) %>%
  mutate(test_type.f = factor(unlist(test_type)))
dur_pvals

var_subset <- data_clean %>%
  filter(condition=='nc')
summarize(filter(var_subset,condition=='dn'),var(var_subset$demeaned_f0))

varplot.plot = ggplot(data=filter(data_clean,(condition=='nc'|condition=='dn')), aes(x=series,color=condition))+
  geom_smooth(aes(y=(demeaned_f0)))

varplot.plot

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#confidence intervals?
#repeated measures anova
#homogeneity of variance
#normal distro
#no outliers
#anova for a and b and interaction of a and b

data_stats
# ezANOVA
pitch.ezanova = ezANOVA(data.frame(data_clean, na.rm=T),
                            dv = demeaned_f0,
                            wid = obj_id,
                            within_full = .(subj,series),
                            between = condition,
                            type = 3)

pitch.ezanova

duration.lm <- lmer(data=line_max_min,
                    duration~condition + 
                      series + (condition*series)+
                      (1|subj), REML=F)
duration.lm.sum<-summary(duration.lm)
duration.lm.sum

plot(duration.lm)

duration.null <- lmer(data=line_max_min,
                      duration~condition + 
                        series + 
                        (1|subj), REML=F)
plot(duration.null)

plot(anova(duration.null,duration.lm))

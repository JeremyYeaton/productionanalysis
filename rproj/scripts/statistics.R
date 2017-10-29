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


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
confidence intervals?
repeated measures anova
homogeneity of variance
normal distro
no outliers
anova for a and b and interaction of a and b

data_stats
# ezANOVA
pitch.ezanova = ezANOVA(data.frame(data_stats, na.rm=T),
                            dv = demeaned_f0,
                            wid = obj_id,
                            within = subj,
                            between = condition,
                            type = 3)

pitch.ezanova
## ORAGANIZE DATA ####
# Make data for statistics
data_stats = data_clean %>%
  mutate(civil_war = factor(civil_war, levels = c("union", "confederacy"))) %>%
  group_by(state, incumbent_party, civil_war) %>%
  summarise(perc_incumbent_mean = mean(perc_votes_incumbent, na.rm = T)) %>%
  ungroup()

# Check if incumbent party is within-state
xtabs(~state+incumbent_party, data_stats)

# Check if civil war is within-state
xtabs(~state+civil_war, data_stats)

# ezANOVA
incumbent.ezanova = ezANOVA(data.frame(data_stats),
                            dv = perc_incumbent_mean,
                            wid = state,
                            within = incumbent_party,
                            between = civil_war,
                            type = 3)

incumbent.ezanova

# Prepare data for t-test
data_union_stats = data_stats %>%
  filter(civil_war == "union") %>%
  spread(incumbent_party, perc_incumbent_mean)

data_confederacy_stats = data_stats %>%
  filter(civil_war == "confederacy") %>%
  spread(incumbent_party, perc_incumbent_mean)

data_democrat_stats = data_stats %>%
  filter(incumbent_party == "democrat")

data_republican_stats = data_stats %>%
  filter(incumbent_party == "republican")

## FOLLOW-UP T-TESTS ####
# Effect of incumbent party, separated by civil war
incumbent_union.ttest = t.test(data_union_stats$democrat,
                               data_union_stats$republican,
                               paired = T)
incumbent_union.ttest

incumbent_confederacy.ttest = t.test(data_confederacy_stats$democrat,
                                     data_confederacy_stats$republican,
                                     paired = T)
incumbent_confederacy.ttest

# Effect of incumbent party, separated by civil war
incumbent_democrat.ttest = t.test(perc_incumbent_mean ~ civil_war,
                                  paired = F,
                                  data = data_democrat_stats)
incumbent_democrat.ttest

incumbent_republican.ttest = t.test(perc_incumbent_mean ~ civil_war,
                                    paired = F,
                                    data = data_republican_stats)
incumbent_republican.ttest

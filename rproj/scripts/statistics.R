## READ IN DATA ####
source("scripts/data_cleaner.R")

## LOAD PACKAGES ####
library(tidyr)
library(ez)

#Name some things
conditions <- c('nc','dn','negsub','negob')
#lists which will store t-test values
ncvdn <- vector("list",60)
ncvnegob <- vector("list",60)
ncvnegsub <- vector("list",60)
dnvnegob <- vector("list",60)
dnvnegsub <- vector("list",60)
negobvnegsub <- vector("list",60)

## CLEAN THE DATA ####
data_stats = data_clean %>%
  mutate(conditions = factor(condition, levels = c("nc", "dn","negsub","negob"))) %>%
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
  ncvdn[[row + 1]] <- ncvdn.ttest$p.value
  ncvnegob[[row + 1]] <- ncvnegob.ttest$p.value
  ncvnegsub[[row + 1]] <- ncvnegsub.ttest$p.value
  dnvnegob[[row +1]] <- dnvnegob.ttest$p.value
  dnvnegsub[[row + 1]] <- dnvnegsub.ttest$p.value
  negobvnegsub[[row + 1]] <- negobvnegsub.ttest$p.value
}
pvals_data = cbind(data.frame(c(0:60),c(ncvdn),
                              c(ncvnegob),
                              c(ncvnegsub)),
                   c(dnvnegob),
                   c(dnvnegsub),
                   c(negobvnegsub))
pvals_data
colnames(pvals_data) <- c("series","ncvdn","ncvnegob","ncvnegsub","dnvnegob","dnvnegsub","negobvnegsub")
ncvdn
ncvnegob
ncvnegsub
dnvnegob
negobvnegsub
,ncvnegob,ncvnegsub,dnvnegob,dnvnegsub,negobvnegsub
pvals_data = data.frame(series = c(0:59),nc_v_dn = c(ncvdn))
pvals_data

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

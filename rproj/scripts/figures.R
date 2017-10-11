## READ IN DATA ####
source("scripts/data_cleaner.R")

## LOAD PACKAGES ####
library(ggplot2)

## ORGANIZE DATA ####
time_figs_data = f0_over_time_clean 



data_figs = data_clean %>%
  mutate(civil_war = factor(civil_war,
                            levels = c("union", "confederacy"),
                            labels = c("Union", "Confederacy"))) %>%
  mutate(incumbent_party = factor(incumbent_party,
                                  levels = c("democrat", "republican"),
                                  labels = c("Democrat", "Republican")))

# Average data over years but not states
data_figs_state_sum = data_figs %>%
  group_by(state, incumbent_party, civil_war) %>%
  summarise(perc_incumbent_mean = 
              mean(perc_votes_incumbent, na.rm = T)) %>%
  ungroup ()


## MAKE FIGURES ####
#Histogram of full data set
incumbent_histogram_full.plot = ggplot(data_figs, aes(x = perc_votes_incumbent,
                                                      fill = incumbent_party)) + 
  geom_histogram(bins = 10) +
  facet_grid(incumbent_party ~ civil_war) +
  scale_fill_manual(values = c("blue","red"))

pdf("figures/incumbent_histogram_full.pdf")
incumbent_histogram_full.plot
dev.off()

# Histogram of data averaged over years
incumbent_histogram_sum.plot = ggplot(data_figs_state_sum, aes(x = perc_incumbent_mean,
                                                               fill = incumbent_party)) +
  geom_histogram(bins = 10) +
  facet_grid(incumbent_party ~ civil_war) +
  scale_fill_manual(values = c("blue","red"))

pdf("figures/incumbent_histogram_sum.pdf")
incumbent_histogram_sum.plot
dev.off()

# Boxplot
incumbent_boxplot.plot = ggplot(data_figs_state_sum, aes(x = civil_war,
                                                         y = perc_incumbent_mean,
                                                         fill = incumbent_party)) +
  geom_boxplot() +
  ylim(0,100) +
  geom_hline(yintercept = 50) +
  scale_fill_manual(values = c("blue", "red"))

pdf("figures/incumbent_boxplot.pdf")
incumbent_boxplot.plot
dev.off()


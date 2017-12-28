##STATS WITH MATT####
#Make dataframe
zscored_data <- data_clean %>%
  mutate(zscore = demeaned_f0/ subj_stdev) %>%
  filter(zscore<3,zscore>-3)

rien.model <- lm(data=zscored_data,zscore ~ condition * series)
summary(rien.model)
plot(rien.model)

zscore<- ts(zscored_data$series)

plot(zscore)


panel_data <- zscored_data

series50.lm <-lm(data=zscored_data[zscored_data$series>=50,], zscore~ series + condition + series*condition)

summary(series50.lm)

plot(series50.lm)

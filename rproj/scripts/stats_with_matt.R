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

series50.lm <-lm(data=zscored_data[zscored_data$series>=47,], 
                 zscore~ series + condition + series*condition)

summary(series50.lm)

plot(series50.lm)


#Duration stuff?
duration_data <- line_max_min %>%
  mutate(syll_num = (series-4)/10) %>%
  filter(syll_num<6)
duration_data

duration.lm1 <- lm(data=duration_data, 
                   duration~ condition + syll_num +condition*syll_num)
summary(duration.lm1)

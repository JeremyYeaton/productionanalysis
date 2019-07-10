##DATA FRAME WITHOUT NEM####
data_clean

aime_set <- c(1,9,17,25)

aime_free <- data_clean %>%
  filter(!trial %in% aime_set)

aime_free

scaled_normal_f0.plot = ggplot(aime_free, aes(x = series, color = condition)) +
  geom_smooth(aes(y = demeaned_f0/subj_stdev),se=T) +
  ylab("f0 relative to subject range") +
  theme(legend.position="bottom")
scaled_normal_f0.plot

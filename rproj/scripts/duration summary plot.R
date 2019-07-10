durations <- line_max_min %>%
  group_by(condition,series) %>%
  filter(series<60)

durations

sum_d<-summarize(durations,mean=mean(duration))

ggplot(sum_d,aes(x=series,y=mean,color=condition)) + geom_point(size=4)

sum_d

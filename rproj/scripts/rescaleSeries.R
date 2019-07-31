allPP <- descr_data %>%
  group_by(trial) %>%
  summarise(totSyll = max(syll_num)) %>%
  merge(descr_data,by='trial') %>%
  filter(syll_num > 5) %>%
  group_by(totSyll)

reSeries <- data.frame(newSer=50:69,
                       y.8 = as.integer(seq.int(50,79,length.out = 20)),
                       y.9 = as.integer(seq.int(50,89,length.out = 20)),
                       # y.10 = as.integer(seq.int(50,99,length.out = 20)),
                       y.11 = as.integer(seq.int(50,109,length.out = 20)))
terpo <- allPP %>%
  filter((totSyll == 8 & series %in% reSeries$y.8) |
           (totSyll == 9 & series %in% reSeries$y.9) |
           # (totSyll == 10 & series %in% reSeries$y.10) |
           (totSyll == 11 & series %in% reSeries$y.11))
terpo8 <- terpo %>%
  filter(totSyll == 8) %>%
  mutate(y.8 = series) %>%
  merge(reSeries, by = 'y.8')

terpo9 <- terpo %>%
  filter(totSyll == 9) %>%
  mutate(y.9 = series) %>%
  merge(reSeries, by = 'y.9')

# terpo10 <- terpo %>%
#   filter(totSyll == 10) %>%
#   mutate(y.10 = series) %>%
#   merge(reSeries, by = 'y.10')

terpo11 <- terpo %>%
  filter(totSyll == 11) %>%
  mutate(y.11 = series) %>%
  merge(reSeries, by = 'y.11') %>%
  select()

rescMaster <- descr_data %>%
  group_by(trial) %>%
  summarise(totSyll = max(syll_num)) %>%
  merge(descr_data,by='trial') %>%
  mutate(newSer = series) %>%
  filter(!(totSyll > 7 & syll_num > 5)) %>%
  rbind(terpo8[,2:18]) %>%
  rbind(terpo9[,2:18]) %>%
  rbind(terpo11[,2:18])

annX = -1
  
terpo.plot <- rescMaster %>%
  merge(grps,'subj') %>%
  # filter(grp == 1) %>%
  # filter(series < 71) %>%
  # filter(series > 49) %>%
  # filter(condition == 'dn' | condition == 'nc') %>%
  ggplot(.,aes(x = newSer, y = z, color = condition)) +
  geom_smooth() +
  # geom_vline(xintercept=40) +
  # geom_vline(xintercept=59)+
  # annotate(geom="text", x=5, y=annX, label="per",color="black") +
  # annotate(geom="text", x=15, y=annX, label="sonne",color="black") +
  annotate(geom="text", x=9, y=annX, label="subject",color="black") +
  annotate(geom="text", x=25, y=annX, label="ne",color="black") +
  annotate(geom="text", x=35, y=annX, label="verb",color="black") +
  annotate(geom="text", x=45, y=annX, label="object",color="black") +
  annotate(geom="text", x=59, y=annX, label="prepositional phrase",color="black") +
  labs(title="F0 over time by condition",x="Time",y="z-scored f0")
terpo.plot






# make up some data 
foo <- data.frame(x= 1:115, y=jitter(sin(1:115/10), 1500)) 
plot(foo) 

# use approx for interpolation 
bar <- approx(foo, n=20) 
lines(bar, col='red', lwd=2) 

# or use spline for interpolation 
bar <- spline(foo, n=30) 
lines(bar, col='green', lwd=2) 

# or fit a loess curve 
# had to play with span to make it look ok 
model <- loess(y~x, foo, span=.25)   
x <- seq(1, 115, length.out=20) 
bar <- predict(model, newdata=data.frame(x=x, y=NA)) 
lines(50:69, bar, col='blue', lwd=2) 

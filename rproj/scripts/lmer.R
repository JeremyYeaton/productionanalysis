library(lme4)

full.mdl.f0 <- lmer(data = data_clean, 
                 raw_f0 ~condition+
                   series +
                   trial +
                   (1+condition|subj),
                 REML = F)

null.mdl.f0 <- lmer(data = data_clean, 
                 raw_f0 ~ series +
                   trial +
                   (1+condition|subj),
                 REML = F)

compare <- anova(null.mdl.f0,full.mdl.f0)
compare
summary(compare)
coef(full.mdl.f0)





##GARBAGE DURATION MODEL ####
full.mdl.dur <- lmer(data = line_max_min, 
                 duration ~ condition + 
                   series +
                   (1|subj) +
                   (1|trial),
                 REML = F)

null.mdl.dur <- lmer(data = line_max_min, 
                     duration ~ series +
                       (1|subj) +
                       (1|trial),
                 REML = F)

compare <- anova(null.mdl.dur,full.mdl.dur)
compare
summary(compare)
coef(full.mdl.dur)
library(ggplot2)
library(dplyr)

#NC vs DN plot
clean_subset <- data_clean %>%
  filter(condition=="dn"| condition== "nc")

clean_subset
p_subset <- pvals_for_graph %>%
  filter(test_type=="Negative Concord X Double Negative")

trial_num = 3
subj_num = 101


nc_vs_dn_master.plot = ggplot(clean_subset,aes(as.numeric(series)))+
  scale_y_continuous(lim=c(-3,3),sec.axis=sec_axis(~(.-1.25)/25,name="P-value"))+
  #coord_cartesian(ylim=c(-.75,1.25)) +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  labs(x="Normalized time series",y="Number of SD from Mean") +
  ggtitle("Between condition t-test p-value",
          subtitle="Critical Conditions: Negative Concord and Double Negation") +
  geom_smooth(aes(y=(demeaned_f0/subj_stdev),
                  color=condition)) +
  #geom_point(data=p_subset,aes(x=as.numeric(series),y=(pvalue*(-25)+1.25)),size=2) +
  geom_smooth(data=filter(clean_subset,subj==subj_num & trial==trial_num),
              aes(x=as.numeric(series),
                  y=(demeaned_f0/subj_stdev),color=as.character(trial)),se=FALSE)+
  geom_smooth(data=filter(clean_subset,subj==subj_num & trial==(trial_num)+8),
              aes(x=as.numeric(series),
                  y=(demeaned_f0/subj_stdev),color=as.character(trial)),se=FALSE)+
  geom_point(data=filter(clean_subset,subj==subj_num & trial==trial_num),
           aes(x=as.numeric(series),
               y=(demeaned_f0/subj_stdev),color=as.character(trial)),se=FALSE)+
  geom_point(data=filter(clean_subset,subj==subj_num & trial==(trial_num)+8),
             aes(x=as.numeric(series),
                 y=(demeaned_f0/subj_stdev),color=as.character(trial)),se=FALSE)

nc_vs_dn_master.plot

nc_vs_dn_master1.plot = ggplot(clean_subset,aes(as.numeric(series)))+
  scale_y_continuous(lim=c(-3,3),sec.axis=sec_axis(~(.-1.25)/25,name="P-value"))+
  coord_cartesian(ylim=c(-.75,1.25)) +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  labs(x="Normalized time series",y="Number of SD from Mean") +
  ggtitle("",
          subtitle="Critical Conditions: Negative Concord and Double Negation") +
  geom_smooth(aes(y=(demeaned_f0/subj_stdev),
                  color=condition)) +
  #geom_point(data=p_subset,aes(x=as.numeric(series),y=(pvalue*(-25)+1.25)),size=2) +
  geom_smooth(data=filter(clean_subset,trial==trial_num),
              aes(x=as.numeric(series),
                  y=(demeaned_f0/subj_stdev),color=as.character(trial)),se=FALSE)+
  geom_smooth(data=filter(clean_subset,trial==(trial_num)+8),
              aes(x=as.numeric(series),
                  y=(demeaned_f0/subj_stdev),color=as.character(trial)),se=FALSE)+
  geom_point(data=filter(clean_subset,trial==trial_num),
             aes(x=as.numeric(series),
                 y=(demeaned_f0/subj_stdev),color=as.character(trial)),se=FALSE)+
  geom_point(data=filter(clean_subset,trial==(trial_num)+8),
             aes(x=as.numeric(series),
                 y=(demeaned_f0/subj_stdev),color=as.character(trial)),se=FALSE)

nc_vs_dn_master1.plot

sum_of_items.plot= ggplot(data_clean,aes(x=trial)) +
  geom_histogram()

sum_of_items.plot

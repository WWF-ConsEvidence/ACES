library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(lme4)
library(rstanarm)
library(ggthemes)
dat_long<-read_csv("~/CARE-WWF/Data/Harmonized_longitudinal_data_3_28_2019.csv")
dat_2018<-read_csv("~/CARE-WWF/Data/Cleaned_2018_only_data.csv")
long_relevant<-dat_long%>%subset(Community %in% c(unique(dat_2018$Community)) )
#comment next line if you want to compare the all communities.
dat_long<-long_relevant

#make overall treatment variables
dat_long$Treatment<-as.character(ifelse(dat_long$Cons_Intervention_gen == 1 & dat_long$Devel_Intervention_gen == 1,"Both (n=3)",
                                 ifelse(dat_long$Cons_Intervention_gen == 1 & dat_long$Devel_Intervention_gen == 0,"Conservation (n=2)",
                                 ifelse(dat_long$Cons_Intervention_gen == 0 & dat_long$Devel_Intervention_gen == 1,"Development (n=2)","None (n=1)"))))

ggplot(dat_long,aes(x=as.integer(Year),y=Asset_weight_sum,color=Treatment))+
  stat_summary(fun.y=mean,geom="line",size=3, aes(color= Treatment))+
  stat_summary(fun.y=mean,geom="point",size=4, aes(color =Treatment))+
  stat_summary(fun.y = mean,
               fun.ymin = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.ymax = function(x) mean(x) +sd(x)/sqrt(length(x)), 
               geom = "pointrange",size=3, aes(color= Treatment)) +
theme_few()+
  scale_color_manual(name="Community Intervention",values=c("#7b3294","#c2a5cf","#92c5de","#0571b0"))+
  xlab("Year")+
  ylab("Weighted Asset Index")+
  scale_x_continuous(breaks=c(2008,2014, 2018))+
  ggtitle("Change in Household Asset Index in Moma & Angoche\nDistricts 2008 (n=184), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=20),legend.key.size = unit(1, "cm"),
        
       #legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"))



ggplot(dat_long,aes(x=as.integer(Year),y=Weighted_Dietary_Diversity,color=Treatment))+
  stat_summary(fun.y=mean,geom="line",size=3, aes(color= Treatment))+
  stat_summary(fun.y=mean,geom="point",size=5, aes(color= Treatment))+
  stat_summary(fun.y = mean,
               fun.ymin = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.ymax = function(x) mean(x) +sd(x)/sqrt(length(x)), 
               geom = "pointrange",size=3, aes(color=Treatment)) +
  theme_few()+
  scale_color_manual(name="Community Intervention",values=c("#7b3294","#c2a5cf","#92c5de","#0571b0"))+
  xlab("Year")+
  ylab("Weighted Dietary Diversity")+
  scale_x_continuous(breaks=c(2008,2014, 2018))+
  ggtitle("Change in Household Weighted Dietary Diversity\nin Moma & Angoche Districts\n2008 (n=184), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(1, "cm"))

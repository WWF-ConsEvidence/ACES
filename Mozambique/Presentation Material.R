
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

#RQ1A. What changes did communities experience in food security and wealth?  

#RQ1B. How did those changes differ between those that participated in both CBNRM and development 
#interventions compared with those that participated in one or none? 

#RQ2. To what extent are community-managed fisheries, mangroves, and forest interventions correlated 
#with changes in community food security and wealth? 

#RQ3. To what extent do changes in food security and wealth differ between 
#women and men (or female-headed households and male-headed households)? 

#RQ4. To what extent do changes in food security and wealth between individuals 
#that participated in both CBNRM and development interventions compared with those that participated in one or none? 

##############################################################################################################################

#Community changes overall

#Change in assets
x<-dat_long %>% 
  dplyr::select(Year, Assets)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=as.integer(Assets), fill=as.character(Year)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#1f78b4","#b2df8a","#a6cee3"))+
  xlab("Year")+
  ylab("Reported Number of Household Assets")+
  ggtitle("No Total Yearly Change in Reported Number of Household Assets\nin Moma & Angoche Districts")+
  theme(text=element_text(size=20),legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"))


#Change in Dietary Diversity
x<-dat_long %>% 
  dplyr::select(Year, Food_Groups)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=as.integer(Food_Groups), fill=as.character(Year)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#1f78b4","#b2df8a","#a6cee3"))+
  xlab("Year")+
  ylab("Food Groups Eaten in Last 24 Hours")+
  ggtitle("No Total Yearly Change in Reported Dietary Diversity\nin Moma & Angoche Districts")+
  theme(text=element_text(size=20),legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"))

#Plain asset and diet breakdown by gender
x<-dat_long %>% 
  dplyr::select(Year, Food_Groups, Gender)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=as.integer(Food_Groups), fill=as.character(Gender)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#b2df8a","#1f78b4"), labels=c("Female","Male"), name="Gender")+
  xlab("Year")+
  ylab("Food Groups Eaten in Last 24 Hours")+
  ggtitle("Yearly Dietary Diversity Difference by Gender")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))
#Assets
x<-dat_long %>% 
  dplyr::select(Year, Assets, Gender)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=as.integer(Assets), fill=as.character(Gender)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#b2df8a","#1f78b4"), labels=c("Female","Male"), name="Gender")+
  xlab("Year")+
  ylab("Household Assets")+
  ggtitle("Yearly Household Asset Difference by Gender")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))

#days seafood
x<-dat_long %>% 
  dplyr::select(Year, Days_seafood, Gender)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=as.integer(Days_seafood), fill=as.character(Gender)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#b2df8a","#1f78b4"), labels=c("Female","Male"), name="Gender")+
  xlab("Year")+
  ylab("Days Ate Seafood in Last Week")+
  ggtitle("Days Households Ate Seafood in Past\nWeek by Gender")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))


#Communities that had interventions vs those that didnt
x<-dat_long %>% 
  dplyr::select(Year, Food_Groups,Cons_Intervention_gen )
x<-na.omit(x)
x<-x[x$Year !=2014,]
ggplot(data=x, aes(x=as.character(Year), y=as.integer(Food_Groups), fill=as.character(Cons_Intervention_gen)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(name="Community Conservation\nIntervention",values=c("#e9a3c9","#4d9221"),
                                                                         labels=c("No (n=3)","Yes (n=5)"))+
  xlab("Year")+
  ylab("Food Groups Eaten in Last 24 Hours")+
  ggtitle("Changes in Household Dietary Diversity in Communities with\nand without Conservation Interventions between 2008 and 2018")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))



#Fish NTZ and Days Seafood
f<-dat_long%>%dplyr::select(Fish_NTZ,Days_seafood, Year, Community)
f<-na.omit(f) 
f$Days_seafood<-as.integer(f$Days_seafood)
ggplot(data=f, aes(x=as.character(Year), y=as.integer(Days_seafood), fill=as.character(Fish_NTZ)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(name="Fish NTZ",values=c("#e9a3c9","#4d9221"),
                    labels=c("No (n=5)","Yes (n=3"))+
  xlab("Year")+
  ylab("Days Ate Seafood Last Week")+
  ggtitle("Changes in Household Seafood Consumption in Communities with\nand without Fish No-Take Zones between 2014 and 2018")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))

#VSLA and food groups ###Insignificant
f<-dat_long%>%dplyr::select(VSLA,Food_Groups, Year)
f<-na.omit(f) 
f<-f[f$Year !=2014,]
f$Food_Groups<-as.integer(f$Food_Groups)
ggplot(data=f, aes(x=as.character(Year), y=Food_Groups, fill=as.character(VSLA)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(name="VSLA",values=c("#e9a3c9","#4d9221"),
                    labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Food Groups Eaten in Last 48 Hours")+
  ggtitle("VSLA effect on Household Dietary Diversity\nin Moma & Angoche Districts 2008 (n=184), 2018 (n=469)")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))



#now on the household level

#effect of borrowing
x<-dat_long %>% 
  dplyr::select( Assets,Borrow,Gender )
x<-na.omit(x)
ggplot(data=x, aes(x=as.logical(Borrow), y=as.integer(Assets), fill=as.character(Gender)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(name="Gender",values=c("#b2df8a","#1f78b4"),
                    labels=c("Female","Male"))+
  xlab("Household Borrowed Money in the Last Year")+
  ylab("Household Assets Reported")+
  scale_x_discrete(labels=c("No","Yes"))+
  ggtitle("Access to Credit and Household Assets by Gender (n=642)")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))

#same plot as above but no gender
x<-dat_long %>% 
  dplyr::select( Asset_weight_sum,Borrow, Year )
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=Asset_weight_sum, fill=as.logical(Borrow)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(name="Borrowed Money\nin Last Year",values=c("#e9a3c9","#4d9221"),
                    labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Weighted Household Assets Index")+
  ggtitle("Correlation Between Borrowing Money and Household Assets")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))

#now assets
x<-dat_long%>%filter(Association_Alliance==0 | Association_Alliance==1)
ggplot(x, aes(x=as.character(Year), y=Assets, fill=as.logical(Association_Alliance))) +
  geom_boxplot()+theme_few()+ scale_fill_manual(name="Member of an\nAssociation Likely\nReached by Alliance",values=c("#e9a3c9","#4d9221"),
                                                labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Reported Household Assets")+
  ggtitle("Correlation between Membership in a Community Association\nLikely Reached by the Alliance\nand Household Assets in Moma & Angoche Districts\n2008 (n=51), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))

#Dietary Diversity
x<-dat_long%>%filter(Association_Alliance==0 | Association_Alliance==1)
ggplot(x, aes(x=as.character(Year), y=Food_Groups, fill=as.logical(Association_Alliance))) +
  geom_boxplot()+theme_few()+ scale_fill_manual(name="Member of an\nAssociation Likely\nReached by Alliance",values=c("#e9a3c9","#4d9221"),
                                                labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Food Groups Eaten in Last 24 Hours")+
  ggtitle("Correlation between Membership in a Community Association\nLikely Reached by the Alliance\nand Household Dietary Diversity in Moma & Angoche Districts\n2008 (n=51), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))

#FFS and food security

x<-dat_2018 %>% 
  dplyr::select(Food_prov,FFS)
x<-na.omit(x)
x$FFS<-as.logical(x$FFS)
x<-x%>%group_by(FFS)%>%summarise(pct=sum(Food_prov)/n())
x$pct_rev<-1.00-x$pct

ggplot(data=x, aes(x=FFS, y=pct_rev, color=FFS,label=paste(sprintf("%0.0f", round((pct_rev*100), digits = 0)),"%")))+
  geom_point(size=35)+theme_few()+
  geom_text(aes(x=FFS,y=pct_rev),color="black", size=10)+
  geom_segment(aes(x=FFS,xend=FFS,y=0.25,yend=(pct_rev-0.03),color=FFS), size=10)+
  scale_y_continuous(limits=c(0.25,1.00),labels = scales::percent_format(accuracy = 1))+
   scale_color_manual(name="Community had\nFarmer Field School",values=c("#e9a3c9","#4d9221"),
                      labels=c("No (n=3)","Yes (n=5)"))+
 # xlab("Community had Farmer Field School")+
  scale_x_discrete( name=("Community had Farmer Field School"),labels=c("No","Yes"))+
  ylab("Percent of Population Reporting Year-Round Food Security")+
  ggtitle("Correlation Between Farmer Field School\nand 2018 Reported Food Security")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"))

#If their household participated in an intervention directly
dat_2018$FFS_Part<-dat_long[dat_long$Year==2018,]$FFS_Participation
dat_2018$Direct_particip<-as.logical(ifelse(dat_2018$FFS_Part == "1" | 
                                              dat_2018$Mang_replant=="Members_of_household"|
                                              dat_2018$Mang_replant=="I_participated"|
                                              dat_2018$Mang_select_extr=="I_practice" |
                                              dat_2018$Mang_select_extr== "Members_of_household",1,0))
dat_2018$Food_Groups<-dat_long[dat_long$Year==2018,]$Food_Groups
#Food Groups
x<-dat_2018%>%filter(Direct_particip==0 | Direct_particip==1)
ggplot(x, aes(x=as.logical(Direct_particip), y=Food_Groups, fill=as.logical(Direct_particip))) +
  geom_boxplot()+theme_few()+ scale_fill_manual(name="Member of an\nAssociation Likely\nReached by Alliance",values=c("#e9a3c9","#4d9221"),
                                                labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Food Groups Eaten in Last 24 Hours")+
  ggtitle("Correlation between Membership in a community Association\nLikely Reached by the Alliance\nand Household Dietary Diversity in Moma & Angoche Districts\n2008 (n=51), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=20),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
        legend.key.size = unit(2, "cm"))



#now do some statistics to quantify what we are seeing visually


fit<-glm(as.integer(Days_seafood)~as.logical(Fish_NTZ), family="poisson", data=dat_long)
summary(fit)
coef(fit)
exp(0.132575 )


fit<-glmer(Food_prov~as.logical(FFS)+ (1|Gender),
           family="binomial",data=dat_2018, control = glmerControl(optimizer ="Nelder_Mead"))
summary(fit)


fit<-glm(Assets~as.logical(Borrow), family="poisson", data=dat_long[dat_long$Gender=="Female",])
summary(fit)
coef(fit)

fit<-glm(Assets~Association_Alliance, family="poisson", data=dat_long)
summary(fit)
coef(fit)

#*
fit<-glm(Food_Groups~Association_Alliance, family="poisson", data=dat_long[dat_long$Year==2018,])
summary(fit)
coef(fit)


####weighted assets
x<-dat_long %>% 
  dplyr::select(Year, Asset_weight_sum, Devel_Intervention_gen)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=Asset_weight_sum, fill=as.character(Devel_Intervention_gen)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#e9a3c9","#4d9221"),name="Development Intervention",labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Weighted Household Asset Index")+
  ggtitle("Change in Household Asset Index in Moma & Angoche Districts\n2008 (n=184), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold") ,legend.key.size = unit(2, "cm"))



x<-dat_long %>% 
  dplyr::select(Year, Asset_weight_sum, Cons_Intervention_gen)
x<-na.omit(x)
x<-x[x$Year !=2014,]
ggplot(data=x, aes(x=as.character(Year), y=Asset_weight_sum, fill=as.character(Cons_Intervention_gen)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#e9a3c9","#4d9221"),name="Conservation Intervention",labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Weighted Household Asset Index")+
  ggtitle("Change in Household Asset Index in Moma & Angoche Districts\n2008 (n=184), 2018 (n=469)")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold") ,legend.key.size = unit(2, "cm"))

x<-dat_long %>% 
  dplyr::select(Year, Asset_weight_sum, Association_Alliance)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=Asset_weight_sum, fill=as.character(Association_Alliance)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#e9a3c9","#4d9221"),name="In Alliance Reached\nCommunity Association",labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Weighted Household Asset Index")+
  ggtitle("Change in Household Asset Index in Moma & Angoche Districts\n2008 (n=51), 2014 (n=135), 2018 (n=469)")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold") ,legend.key.size = unit(2, "cm"))

##Weighted Asset Index
x<-dat_long %>% 
  dplyr::select(Year, Asset_weight_sum, Fish_NTZ)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=Asset_weight_sum, fill=as.logical(Fish_NTZ)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(name="Fish NTZ\nin Community",values=c("#e9a3c9","#4d9221"),
labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Weighted Asset Index")+
  ggtitle("HouseholdAsset Index Correlated with Fish NTZ\nin Moma & Angoche Districts")+
  theme(text=element_text(size=20) ,legend.key.size = unit(2, "cm"),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"))

x<-dat_long %>% 
  dplyr::select(Year, Asset_weight_sum, Cons_Intervention_gen)
x<-na.omit(x)
x<-x[x$Year!=2014,]
ggplot(data=x, aes(x=as.character(Year), y=Asset_weight_sum, fill=as.logical(Cons_Intervention_gen)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(name="Conservation Intervention\nin Community",values=c("#e9a3c9","#4d9221"),
                    labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Weighted Asset Index")+
  ggtitle("HouseholdAsset Index Correlated with Conservation Intervention\nin Moma & Angoche Districts")+
  theme(text=element_text(size=20), legend.key.size = unit(2, "cm"),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"))


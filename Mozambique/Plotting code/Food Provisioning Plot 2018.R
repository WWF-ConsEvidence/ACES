library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
dat2018 <- read_csv("~/CARE-WWF/2018Data.csv")
fp2018<-dat2018[ ,c('7_5','7_7') ]
fp2018$`7_5`<-as.character(fp2018$`7_5`)
fp2018$`7_7`<-as.character(fp2018$`7_7`)
fp2018$change <- ifelse(fp2018$`7_7` == 2, "Worsened", 
                        ifelse(fp2018$`7_7` == 1, "Worsened",
                               ifelse(fp2018$`7_7`==3, "Stayed the same",
                                      ifelse(fp2018$`7_7` ==4, "Improved", 
                                             ifelse(fp2018$`7_7`==5, "Improved",NA)))))
                                               # above / below avg flag
fp2018[fp2018$`7_5`=="1",1]<-"Inadequate"
fp2018[fp2018$`7_5`=="2",1]<-"Adequate"
##

##

#single plot
ggplot(fp2018, aes(x=`7_5`)) + 
  geom_bar(aes(fill = change,y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")+
  theme_classic()
#

#split plots 1
df<-fp2018 %>%
  group_by(`7_5`) %>%
  summarise(counts = n())%>%
  mutate(prop = round(counts*100/sum(counts), 1))

ggplot(df, aes(x=`7_5`, y=prop,label=paste(sprintf("%0.0f", round((prop), digits = 0)),"%"))) + 
  geom_point( aes(color=`7_5`), size=45)+
  scale_color_manual(values=c("#33a02c","#1f78b4"))+
  geom_segment(aes(color = `7_5`,y = 0, 
                   x = `7_5`, 
                   yend = prop, 
                   xend = `7_5`), 
               size=6) +
  ylim(0, 80)+
  geom_text(color="white", size=10)+
  ylab("relative frequencies")+
  theme_classic()+
  theme(legend.position="none")


#SplitPlots2
df<-na.omit(fp2018)
df<-df %>%
  group_by(change) %>%
  summarise(counts = n())%>%
  mutate(prop = round(counts*100/sum(counts), 1))

ggplot(df, aes(x=change, y=prop,label=paste(sprintf("%0.0f", round((prop), digits = 0)),"%"))) + 
  geom_point( aes(color=change), size=45)+
  scale_color_manual(values=c("#33a02c","#a6cee3","#1f78b4"))+
  geom_segment(aes(color = change,y = 0, 
                   x = change, 
                   yend = prop, 
                   xend = change), 
               size=6) +
  ylim(0, 80)+
  geom_text(color=c("white","black","white"), size=10)+
  ylab("relative frequencies")+
  theme_classic()+
  theme(legend.position="none")
 
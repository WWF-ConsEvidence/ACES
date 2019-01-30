library(haven)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
#Load full 2008 data
dat2008 <- read_sav("~/CARE-WWF/CARE.Basic.Data.Set.Clean (1).sav")
#subset 2008 data to only include diet data
dd2008<-dat2008[ , grepl( "7_4" , names( dat2008 ) ) ]
#Load full 2018 data
dat2018 <- read_csv("~/CARE-WWF/2018Data.csv")
#subset 2018 data to only include diet data
dd2018<-dat2018[ , grepl( "7_3" , names( dat2018 ) ) ]
#subset to only columns we need
dd2018<-dd2018[,2:26]
#Load 2014 Diet data
dd2014 <- read_csv("~/CARE-WWF/2014Data_Diet.csv")
#subset to only columns we need
dd2014<-dd2014[,2:26]
#Make all "2"s into "0"s in all datasets
dd2018[dd2018=="2"]<-0
dd2008[dd2008=="2"]<-0
dd2014[dd2014=="2"]<-0
#Make anything greater than 2 into NA
ad2018[ad2018 >= 3]<-NA
ad2008[ad2008 >=3]<-NA
ad2008[ad2008 >=3]<-NA
#sum by rows 
sums2008<-as.data.frame(apply(dd2008,1,sum,na.rm=TRUE))
sums2014<-as.data.frame(apply(dd2014,1,sum,na.rm=TRUE))
sums2018<-as.data.frame(apply(dd2018,1,sum,na.rm=TRUE))

#add a year column to each & change sum name
colnames(sums2008)[1]<-"Diet"
sums2008$Year<-rep(2008,nrow(sums2008))

colnames(sums2014)[1]<-"Diet"
sums2014$Year<-rep(2014,nrow(sums2014))

colnames(sums2018)[1]<-"Diet"
sums2018$Year<-rep(2018,nrow(sums2018))

#Bind the data
dddata<-rbind(sums2008,sums2014,sums2018)
dddata$Year<-as.character(dddata$Year)
#Create boxplots
bp<-ggplot(dddata, aes(x=Year, y=Diet, fill=Year)) +
  geom_boxplot()
bp+scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a"))+
  theme_classic()+
  theme(legend.position="none")

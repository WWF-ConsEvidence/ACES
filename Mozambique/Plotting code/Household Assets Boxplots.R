library(haven)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
#Load full 2008 data
dat2008 <- read_sav("~/CARE-WWF/CARE.Basic.Data.Set.Clean (1).sav")
#subset 2008 data to only include asset data
ad2008<-dat2008[ , grepl( "3_1" , names( dat2008 ) ) ]
#Remove sickle, not in 2014 data
ad2008<-ad2008[,-13]

#Load full 2018 data
dat2018 <- read_csv("~/CARE-WWF/2018Data.csv")
#subset 2018 data to only include diet data
ad2018<-dat2018[ , grepl( "3_1" , names( dat2018 ) ) ]
#subset to only columns which match for all years
ad2018<-ad2018[,1:27]
#Remove sickle, not in 2014 data
ad2018<-ad2018[,-13]

#Load 2014 asset data
ad2014 <- read_csv("~/CARE-WWF/2014Asset_Data.csv")
#make the two mattress columns into one
ad2014$mattress<-ad2014[,3]+ad2014[,28]  
ad2014$mattress <- ifelse(ad2014$mattress == 4,2,1)
#remove original mattress columns
ad2014<-ad2014[,c(-3,-28)]
#Remove Wall Clock
ad2014<-ad2014[,-27]
#Remove washing machine
ad2014<-ad2014[,-25]
#Remove hoe
ad2014<-ad2014[,-28]
#Remove motorboat
ad2014<-ad2014[,-26]
#remove head of household
ad2014<-ad2014[,-1]

#Make all "2"s into "0"s in all datasets
ad2018[ad2018=="2"]<-0
ad2008[ad2008=="2"]<-0
ad2014[ad2014=="2"]<-0

#Make anything greater than 2 into NA
ad2018[ad2018 >= 3]<-NA
ad2008[ad2008 >=3]<-NA
ad2008[ad2008 >=3]<-NA
#sum by rows 
sums2008<-as.data.frame(apply(ad2008,1,sum,na.rm=TRUE))
sums2014<-as.data.frame(apply(ad2014,1,sum,na.rm=TRUE))
sums2018<-as.data.frame(apply(ad2018,1,sum,na.rm=TRUE))

#add a year column to each & change sum name
colnames(sums2008)[1]<-"Assets"
sums2008$Year<-rep(2008,nrow(sums2008))

colnames(sums2014)[1]<-"Assets"
sums2014$Year<-rep(2014,nrow(sums2014))

colnames(sums2018)[1]<-"Assets"
sums2018$Year<-rep(2018,nrow(sums2018))

#Bind the all assetdata
aadata<-rbind(sums2008,sums2014,sums2018)
aadata$Year<-as.character(aadata$Year)

#Create boxplots
bp<-ggplot(aadata, aes(x=Year, y=Assets, fill=Year)) +
  geom_boxplot()
bp+scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a"))+
  theme_classic()+
  theme(legend.position="none")








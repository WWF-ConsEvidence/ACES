#Load full 2008 data
dat2008 <- read_sav("~/CARE-WWF/CARE.Basic.Data.Set.Clean (1).sav")
#subset 2008 data to only include diet data
dd2008<-dat2008[ , grepl( "7_4" , names( dat2008 ) ) ]
#Load full 2018 data
dat2018 <- read_csv("~/CARE-WWF/2018Data.csv")
#subset 2018 data to only include diet data
library(haven)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
dd2018<-dat2018[ , grepl( "7_3" , names( dat2018 ) ) ]
#subset to only columns we need
dd2018<-dd2018[,2:26]
#Load 2014 Diet data
dd2014 <- read_csv("~/CARE-WWF/Data/2014Data_Diet.csv")
#subset to only columns we need
dd2014<-dd2014[,2:26]
#Make all "2"s into "0"s in all datasets
dd2018[dd2018=="2"]<-0
dd2008[dd2008=="2"]<-0
dd2014[dd2014=="2"]<-0
#Make anything greater than 2 into NA
dd2018[dd2018 >= 3]<-NA
dd2014[dd2014 >=3]<-NA
dd2008[dd2008 >=3]<-NA
#sum by rows 
sums2008<-as.data.frame(apply(dd2008,1,sum,na.rm=TRUE))
sums2014<-as.data.frame(apply(dd2014,1,sum,na.rm=TRUE))
sums2018<-as.data.frame(apply(dd2018,1,sum,na.rm=TRUE))

#add a year column to each & change sum name
colnames(sums2008)[1]<-"Food_Groups"
sums2008$Year<-rep(2008,nrow(sums2008))

colnames(sums2014)[1]<-"Food_Groups"
sums2014$Year<-rep(2014,nrow(sums2014))

colnames(sums2018)[1]<-"Food_Groups"
sums2018$Year<-rep(2018,nrow(sums2018))

#Add everything together
diet2008<-cbind(sums2008,dd2008)
diet2014<-cbind(sums2014,dd2014)
diet2018<-cbind(sums2018,dd2018)

#Make column names match
#2008
names(diet2008)[3]= "Grains"
names(diet2008)[4]= "Manufactured Cereals"
names(diet2008)[5]= "Banana"
names(diet2008)[6]= "Potato"
names(diet2008)[7]= "Sweet Potato"
names(diet2008)[8]= "Pumpkin/Carrot"
names(diet2008)[9]= "Sugar"
names(diet2008)[10]= "Beans/Lentils"
names(diet2008)[11]= "Nuts"
names(diet2008)[12]= "Vegetables"
names(diet2008)[13]= "Dark Leafy Greens"
names(diet2008)[14]= "Mango/Papaya"
names(diet2008)[15]= "Wild Fruits"
names(diet2008)[16]= "Red Meat"
names(diet2008)[17]= "White Meat"
names(diet2008)[18]= "Pork"
names(diet2008)[19]= "Organs"
names(diet2008)[20]= "Eggs"
names(diet2008)[21]= "Fish"
names(diet2008)[22]= "Oils/Fats"
names(diet2008)[23]= "Dairy"
names(diet2008)[24]= "CSB"
names(diet2008)[25]= "Seeds"
names(diet2008)[26]= "Salt"
names(diet2008)[27]= "Inects"

#2014
names(diet2014)[3]= "Grains"
names(diet2014)[4]= "Manufactured Cereals"
names(diet2014)[5]= "Banana"
names(diet2014)[6]= "Potato"
names(diet2014)[7]= "Sweet Potato"
names(diet2014)[8]= "Pumpkin/Carrot"
names(diet2014)[9]= "Sugar"
names(diet2014)[10]= "Beans/Lentils"
names(diet2014)[11]= "Nuts"
names(diet2014)[12]= "Vegetables"
names(diet2014)[13]= "Dark Leafy Greens"
names(diet2014)[14]= "Mango/Papaya"
names(diet2014)[15]= "Wild Fruits"
names(diet2014)[16]= "Red Meat"
names(diet2014)[17]= "White Meat"
names(diet2014)[18]= "Pork"
names(diet2014)[19]= "Organs"
names(diet2014)[20]= "Eggs"
names(diet2014)[21]= "Fish"
names(diet2014)[22]= "Oils/Fats"
names(diet2014)[23]= "Dairy"
names(diet2014)[24]= "CSB"
names(diet2014)[25]= "Seeds"
names(diet2014)[26]= "Salt"
names(diet2014)[27]= "Inects"

#2018
names(diet2018)[3]= "Grains"
names(diet2018)[4]= "Manufactured Cereals"
names(diet2018)[5]= "Banana"
names(diet2018)[6]= "Potato"
names(diet2018)[7]= "Sweet Potato"
names(diet2018)[8]= "Pumpkin/Carrot"
names(diet2018)[9]= "Sugar"
names(diet2018)[10]= "Beans/Lentils"
names(diet2018)[11]= "Nuts"
names(diet2018)[12]= "Vegetables"
names(diet2018)[13]= "Dark Leafy Greens"
names(diet2018)[14]= "Mango/Papaya"
names(diet2018)[15]= "Wild Fruits"
names(diet2018)[16]= "Red Meat"
names(diet2018)[17]= "White Meat"
names(diet2018)[18]= "Pork"
names(diet2018)[19]= "Organs"
names(diet2018)[20]= "Eggs"
names(diet2018)[21]= "Fish"
names(diet2018)[22]= "Oils/Fats"
names(diet2018)[23]= "Dairy"
names(diet2018)[24]= "CSB"
names(diet2018)[25]= "Seeds"
names(diet2018)[26]= "Salt"
names(diet2018)[27]= "Inects"


harmDiet<-rbind(diet2008,diet2014,diet2018)
write.csv(harmDiet,file="Harmonized_Diet_Data.csv")


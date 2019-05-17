library(haven)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
dat2008 <- read_sav("~/CARE-WWF/Data/CARE.Basic.Data.Set.Clean (1).sav")
dat2014_diet <- read_csv("~/CARE-WWF/Data/2014Data_Diet.csv")
dat2014_asset <- read_csv("~/CARE-WWF/Data/2014Asset_Data.csv")
dat2014_agri <- read_csv("~/CARE-WWF/Data/2014Agriculture_Data.csv")
dat2014_dem <- read_csv("~/CARE-WWF/Data/2014Demographic_Data.csv")
dat2014_liveli <- read_csv("~/CARE-WWF/Data/2014Livelihood_Data.csv")
dat2018 <- read_csv("~/CARE-WWF/Data/2018Data.csv")

# Clean & harmonize the asset data
#Load full 2008 data
dat2008 <- read_sav("~/CARE-WWF/Data/CARE.Basic.Data.Set.Clean (1).sav")
#subset 2008 data to only include asset data
ad2008<-dat2008[ , grepl( "3_1" , names( dat2008 ) ) ]
#Remove sickle, not in 2014 data
ad2008<-ad2008[,-13]
#remove disk harrow, not in 2014 data
ad2008<-ad2008[,-15]

#Load full 2018 data
dat2018 <- read_csv("~/CARE-WWF/Data/2018Data.csv")
#subset 2018 data to only include diet data
ad2018<-dat2018[ , grepl( "3_1" , names( dat2018 ) ) ]
#subset to only columns which match for all years
ad2018<-ad2018[,1:27]
#Remove sickle, not in 2014 data
ad2018<-ad2018[,-13]
#remove disk harrow, not in 2014 data
ad2018<-ad2018[,-15]

#Load 2014 asset data
ad2014 <- read_csv("~/CARE-WWF/Data/2014Asset_Data.csv")
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
#remove quitanta
ad2014<-ad2014[,-24]

#Make all "2"s into "0"s in all datasets
ad2018[ad2018=="2"]<-0
ad2008[ad2008=="2"]<-0
ad2014[ad2014=="2"]<-0

#Make anything greater than 2 into NA
ad2018[ad2018 >= 3]<-NA
ad2008[ad2008 >=3]<-NA
ad2014[ad2014 >=3]<-NA
# Set the column names 2018
names(ad2018)[which(names(ad2018)=="3_1A")]= "Bicycle"
names(ad2018)[which(names(ad2018)=="3_1B")]= "Radio"
names(ad2018)[which(names(ad2018)=="3_1C")]= "Bed"
names(ad2018)[which(names(ad2018)=="3_1D")]= "Cellphone"
names(ad2018)[which(names(ad2018)=="3_1E")]= "Watch"
names(ad2018)[which(names(ad2018)=="3_1F")]= "Stove"
names(ad2018)[which(names(ad2018)=="3_1G")]= "Chair"
names(ad2018)[which(names(ad2018)=="3_1H")]= "Table"
names(ad2018)[which(names(ad2018)=="3_1I")]= "Cups & plates"
names(ad2018)[which(names(ad2018)=="3_1J")]= "Gas"
names(ad2018)[which(names(ad2018)=="3_1K")]= "Television"
names(ad2018)[which(names(ad2018)=="3_1L")]= "Axe"
names(ad2018)[which(names(ad2018)=="3_1M")]= "Sickle"
names(ad2018)[which(names(ad2018)=="3_1N")]= "Machete"
names(ad2018)[which(names(ad2018)=="3_1O")]= "Mortar & pestle"
names(ad2018)[which(names(ad2018)=="3_1P")]= "Disk harrow"
names(ad2018)[which(names(ad2018)=="3_1Q")]= "Wagon"
names(ad2018)[which(names(ad2018)=="3_1R")]= "Sewing Machine"
names(ad2018)[which(names(ad2018)=="3_1S")]= "Fishing rod"
names(ad2018)[which(names(ad2018)=="3_1T")]= "Fishing net"
names(ad2018)[which(names(ad2018)=="3_1U")]= "Harpoon"
names(ad2018)[which(names(ad2018)=="3_1V")]= "Canoe"
names(ad2018)[which(names(ad2018)=="3_1W")]= "Canoe Moma"
names(ad2018)[which(names(ad2018)=="3_1X")]= "Raft"
names(ad2018)[which(names(ad2018)=="3_1Y")]= "Tractor"
names(ad2018)[which(names(ad2018)=="3_1Z")]= "Motorcycle"
names(ad2018)[which(names(ad2018)=="3_1AA")]= "Mattress"
# Set the column names 2008
names(ad2008)[which(names(ad2008)=="@3_1A")]= "Bicycle"
names(ad2008)[which(names(ad2008)=="@3_1B")]= "Radio"
names(ad2008)[which(names(ad2008)=="@3_1C")]= "Bed"
names(ad2008)[which(names(ad2008)=="@3_1D")]= "Cellphone"
names(ad2008)[which(names(ad2008)=="@3_1E")]= "Watch"
names(ad2008)[which(names(ad2008)=="@3_1F")]= "Stove"
names(ad2008)[which(names(ad2008)=="@3_1G")]= "Chair"
names(ad2008)[which(names(ad2008)=="@3_1H")]= "Table"
names(ad2008)[which(names(ad2008)=="@3_1I")]= "Cups & plates"
names(ad2008)[which(names(ad2008)=="@3_1J")]= "Gas"
names(ad2008)[which(names(ad2008)=="@3_1K")]= "Television"
names(ad2008)[which(names(ad2008)=="@3_1L")]= "Axe"
names(ad2008)[which(names(ad2008)=="@3_1M")]= "Sickle"
names(ad2008)[which(names(ad2008)=="@3_1N")]= "Machete"
names(ad2008)[which(names(ad2008)=="@3_1O")]= "Mortar & pestle"
names(ad2008)[which(names(ad2008)=="@3_1P")]= "Disk harrow"
names(ad2008)[which(names(ad2008)=="@3_1Q")]= "Wagon"
names(ad2008)[which(names(ad2008)=="@3_1R")]= "Sewing Machine"
names(ad2008)[which(names(ad2008)=="@3_1S")]= "Fishing rod"
names(ad2008)[which(names(ad2008)=="@3_1T")]= "Fishing net"
names(ad2008)[which(names(ad2008)=="@3_1U")]= "Harpoon"
names(ad2008)[which(names(ad2008)=="@3_1V")]= "Canoe"
names(ad2008)[which(names(ad2008)=="@3_1W")]= "Canoe Moma"
names(ad2008)[which(names(ad2008)=="@3_1X")]= "Raft"
names(ad2008)[which(names(ad2008)=="@3_1Y")]= "Tractor"
names(ad2008)[which(names(ad2008)=="@3_1Z")]= "Motorcycle"
names(ad2008)[which(names(ad2008)=="@3_1AA")]= "Mattress"
# Set column names 2014
names(ad2014)[which(names(ad2014)=="bicicleta")]= "Bicycle"
names(ad2014)[which(names(ad2014)=="Rádio")]= "Radio"
names(ad2014)[which(names(ad2014)=="Cama")]= "Bed"
names(ad2014)[which(names(ad2014)=="Telemovel")]= "Cellphone"
names(ad2014)[which(names(ad2014)=="Relogio de pulso")]= "Watch"
names(ad2014)[5]= "Stove"
names(ad2014)[which(names(ad2014)=="cadeira")]= "Chair"
names(ad2014)[which(names(ad2014)=="Mesa")]= "Table"
names(ad2014)[which(names(ad2014)=="copos e pratos")]= "Cups & plates"
names(ad2014)[which(names(ad2014)=="candieiro")]= "Gas"
names(ad2014)[6]= "Television"
names(ad2014)[which(names(ad2014)=="machado")]= "Axe"
names(ad2014)[which(names(ad2014)=="catana")]= "Machete"
names(ad2014)[14]= "Mortar & pestle"
names(ad2014)[which(names(ad2014)=="3_1P")]= "Disk harrow"
names(ad2014)[23]= "Wagon"
names(ad2014)[which(names(ad2014)=="maquina de costura")]= "Sewing Machine"
names(ad2014)[which(names(ad2014)=="vara de pesca")]= "Fishing rod"
names(ad2014)[which(names(ad2014)=="rede de pesca")]="Fishing net"
names(ad2014)[17]= "Harpoon"
names(ad2014)[which(names(ad2014)=="canoa")]= "Canoe"
names(ad2014)[which(names(ad2014)=="barco de vela")]= "Canoe Moma"
names(ad2014)[which(names(ad2014)=="jangada")]= "Raft"
names(ad2014)[which(names(ad2014)=="trator")]= "Tractor"
names(ad2014)[which(names(ad2014)=="motorizada")]= "Motorcycle"
names(ad2014)[which(names(ad2014)=="mattress")]= "Mattress"



#sum by rows 
sums2008<-as.data.frame(apply(ad2008,1,sum,na.rm=TRUE))
sums2014<-as.data.frame(apply(ad2014,1,sum,na.rm=TRUE))
sums2018<-as.data.frame(apply(ad2018,1,sum,na.rm=TRUE))

colnames(sums2008)[1]<-"Assets"
sums2008$Year<-rep(2008,nrow(sums2008))

colnames(sums2014)[1]<-"Assets"
sums2014$Year<-rep(2014,nrow(sums2014))

colnames(sums2018)[1]<-"Assets"
sums2018$Year<-rep(2018,nrow(sums2018))

#Add everything together
assets2008<-cbind(sums2008,ad2008)
assets2014<-cbind(sums2014,ad2014)
assets2018<-cbind(sums2018,ad2018)

harmAssets<-rbind(assets2008,assets2014,assets2018)

harmAssets$Asset_weight_sum<-(harmAssets$Chair *0.05)+(harmAssets$Radio*0.1)+(harmAssets$Table*0.15)+(harmAssets$Bed*0.15)+
  (harmAssets$`Cups & plates`*0.25)+ (harmAssets$Watch*0.3)+ (harmAssets$Gas*0.3)+ (harmAssets$Mattress*0.4)+ (harmAssets$Cellphone*0.5)+
  (harmAssets$Stove*0.75)+ (harmAssets$Television*1.0)+ (harmAssets$Machete*0.05)+ (harmAssets$Axe*0.05)+ (harmAssets$`Mortar & pestle`*0.1)+
  (harmAssets$Harpoon*0.3)+ (harmAssets$`Fishing net`*0.4)+ (harmAssets$`Fishing rod`*0.3)+ (harmAssets$Bicycle*0.4)+ (harmAssets$Wagon*0.65)+
  (harmAssets$`Sewing Machine`*0.7)+ (harmAssets$Canoe*0.85)+ (harmAssets$`Canoe Moma`*0.7)+ (harmAssets$Raft*0.6)+ (harmAssets$Motorcycle*0.9)+
  (harmAssets$Tractor*1.0)

harmAssets$TotalAssetValue<-(harmAssets$Television *2500)+(harmAssets$Radio*500)+(harmAssets$Cellphone*500)+(harmAssets$`Sewing Machine`*7000)+
  (harmAssets$Axe*200)+ (harmAssets$Machete*200)+ (harmAssets$`Fishing rod`*100)+ (harmAssets$`Fishing net`*1000)+ (harmAssets$Harpoon*500)+
  (harmAssets$Raft*1000)+ (harmAssets$Canoe*5000)+ (harmAssets$`Canoe Moma`*15000)+ (harmAssets$Tractor*30000)+ (harmAssets$Wagon*5000)+
  (harmAssets$Bed*4000)+ (harmAssets$Mattress*1500)+ (harmAssets$Table*1500)+ (harmAssets$Chair*400)+ (harmAssets$`Cups & plates`*500)+
  (harmAssets$Stove*5000)+ (harmAssets$Watch*100)+ (harmAssets$Gas*100)+ (harmAssets$`Mortar & pestle`*200)

harmAssets$ProductiveAssetValue<-(harmAssets$Television *2500)+(harmAssets$Radio*500)+(harmAssets$Cellphone*500)+(harmAssets$`Sewing Machine`*7000)+
  (harmAssets$Axe*200)+ (harmAssets$Machete*200)+ (harmAssets$`Fishing rod`*100)+ (harmAssets$`Fishing net`*1000)+ (harmAssets$Harpoon*500)+
  (harmAssets$Raft*1000)+ (harmAssets$Canoe*5000)+ (harmAssets$`Canoe Moma`*15000)+ (harmAssets$Tractor*30000)+ (harmAssets$Wagon*5000)

harmAssets$NonProductiveAssetValue<- (harmAssets$Bed*4000)+ (harmAssets$Mattress*1500)+ (harmAssets$Table*1500)+ (harmAssets$Chair*400)+ 
  (harmAssets$`Cups & plates`*500)+
  (harmAssets$Stove*5000)+ (harmAssets$Watch*100)+ (harmAssets$Gas*100)+ (harmAssets$`Mortar & pestle`*200)
 
  
write.csv(harmAssets,file="Harmonized_Assets_Data.csv")


library(haven)
library(dplyr)
#this next line will be different for different people
dat2008 <- read_sav("~/CARE-WWF/CARE.Basic.Data.Set.Clean (1).sav")
View(dat)
write.csv(dat2008,file="2008Data.csv")

#Lets subset so we are only using the househild asset index data
AI2008<-dat2008[ , grepl( "3_1" , names( dat2008 ) ) ]
colnames(AI2008)<-gsub("@","Q",colnames(AI2008))
x<-apply(AI2008,2,sum,na.rm=TRUE)
props2008<-x/1566 

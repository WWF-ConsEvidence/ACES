library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
dat2018 <- read_csv("~/CARE-WWF/2018Data.csv")
#subset to just the monthly provisioning data we want
mpdat<-dat2018[ , grepl( "7_6" , names( dat2018 ) ) ]
#Make all 0's (meaning they had adequade food provisioning for all months) 2's to indicate they had adequate food for that month
mpdat[mpdat=="0"]<-2
#Make all 1's 0's, indicatibng that they didn't have adequate FP that month
mpdat[mpdat=="1"]<-0
#now make all 2's, indicating they had inadequate food provisioning thatmonth, a 1
mpdat[mpdat=="2"]<-1
#Now let's sum by rows
mpsums<-as.data.frame(apply(mpdat,1,sum,na.rm=TRUE))
colnames(mpsums)[1]<-"Months"

#Now lets plot
ggplot(data=mpsums,aes(x=Months))+
  geom_histogram(aes(y=..density..),fill="#1f78b4",bins=12,col="black")+
  scale_x_continuous(breaks=c(0:12))+
  scale_y_continuous(labels=scales::percent)+
  theme_classic()
_
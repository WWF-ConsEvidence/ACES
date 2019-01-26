library(readr)
library(haven)
library(dplyr)
library(tibble)
library(ggplot2)
#2018 data
dat2018 <- read_csv("~/CARE-WWF/2018Data.csv")
AI2018<-dat2018[ , grepl( "3_1" , names( dat2018 ) ) ]
AI2018[AI2018=="2"]<-0
x<-apply(AI2018,2,sum,na.rm=TRUE)
props2018<-x/469
props2018<-props2018[1:27]
#2008 Data
#this next line will be different for different people
dat2008 <- read_sav("~/CARE-WWF/CARE.Basic.Data.Set.Clean (1).sav")
str(dat2018)
View(dat)
write.csv(dat2008,file="2008Data.csv")

#Lets subset so we are only using the househild asset index data
AI2008<-dat2008[ , grepl( "3_1" , names( dat2008 ) ) ]
colnames(AI2008)<-gsub("@","Q",colnames(AI2008))
x<-apply(AI2008,2,sum,na.rm=TRUE)
props2008<-x/1566 




propchange<-as.data.frame(props2018-props2008)
#rename
row.names(propchange)[which(row.names(propchange)=="3_1A")]= "Bicycle"
row.names(propchange)[which(row.names(propchange)=="3_1B")]= "Radio"
row.names(propchange)[which(row.names(propchange)=="3_1C")]= "Bed"
row.names(propchange)[which(row.names(propchange)=="3_1D")]= "Cellphone"
row.names(propchange)[which(row.names(propchange)=="3_1E")]= "Watch"
row.names(propchange)[which(row.names(propchange)=="3_1F")]= "Stove"
row.names(propchange)[which(row.names(propchange)=="3_1G")]= "Chair"
row.names(propchange)[which(row.names(propchange)=="3_1H")]= "Table"
row.names(propchange)[which(row.names(propchange)=="3_1I")]= "Cups & plates"
row.names(propchange)[which(row.names(propchange)=="3_1J")]= "Gas"
row.names(propchange)[which(row.names(propchange)=="3_1K")]= "Television"
row.names(propchange)[which(row.names(propchange)=="3_1L")]= "Axe"
row.names(propchange)[which(row.names(propchange)=="3_1M")]= "Sickle"
row.names(propchange)[which(row.names(propchange)=="3_1N")]= "Catana"
row.names(propchange)[which(row.names(propchange)=="3_1O")]= "Mortar & pestle"
row.names(propchange)[which(row.names(propchange)=="3_1P")]= "Disk harrow"
row.names(propchange)[which(row.names(propchange)=="3_1Q")]= "Wagon"
row.names(propchange)[which(row.names(propchange)=="3_1R")]= "Sewing Machine"
row.names(propchange)[which(row.names(propchange)=="3_1S")]= "Fishing rod"
row.names(propchange)[which(row.names(propchange)=="3_1T")]= "Fishing net"
row.names(propchange)[which(row.names(propchange)=="3_1U")]= "Harpoon"
row.names(propchange)[which(row.names(propchange)=="3_1V")]= "Canoe"
row.names(propchange)[which(row.names(propchange)=="3_1W")]= "Canoe Moma"
row.names(propchange)[which(row.names(propchange)=="3_1X")]= "Raft"
row.names(propchange)[which(row.names(propchange)=="3_1Y")]= "Tractor"
row.names(propchange)[which(row.names(propchange)=="3_1Z")]= "Motorcycle"
row.names(propchange)[which(row.names(propchange)=="3_1AA")]= "Mattress"

#
colnames(propchange)[1]<-"Change"
propchange$Outcome <- ifelse(propchange$Change < 0, "decreased", "increased")  # above / below avg flag
propchange <- propchange[order(propchange$Change), ]  # sort
propchange<- propchange %>% tibble::rownames_to_column("Asset")
propchange$Asset <- factor(propchange$Asset, levels = propchange$Asset)






###


ggplot(propchange, aes(x=Asset, y=Change, label=Change)) + 
  geom_bar(stat='identity', aes(fill=Outcome), width=.5)  +
  scale_fill_manual(name="Outcome", 
                    labels = c("Decreased from 2008","Increased from 2008"), 
                    values = c("increased"="#00ba38", "decreased"="#f8766d")) + 
  labs(title = "Change in proportion of households reporting assets from 2008 to 2018") + 
  coord_flip()+theme_bw()


#Lolipop chart
ggplot(propchange, aes(x=Asset, y=Change, label=paste(sprintf("%0.0f", round((Change*100), digits = 0)),"%"))) + 
  geom_point(stat='identity', fill="black", size=14)  +
  geom_segment(aes(y = 0, 
                   x = Asset, 
                   yend = Change, 
                   xend = Asset), 
               color = "black",size=2) +
  scale_y_continuous(labels=scales::percent) +
  geom_text(color="white", size=4) +
  labs(title="Diverging Lollipop Chart") + 
  coord_flip()+
  theme_bw()

#Colored
ggplot(propchange, aes(x=Asset, y=Change, label=paste(sprintf("%0.0f", round((Change*100), digits = 0)),"%"))) + 
  geom_point(stat='identity', aes(color=Outcome), size=14)  +
  geom_segment(aes(color = Outcome,y = 0, 
                   x = Asset, 
                   yend = Change, 
                   xend = Asset), 
               size=2) +
  scale_y_continuous(labels=scales::percent) +
  scale_color_manual(values=c("#984ea3", "#377eb8"))+
  geom_text(color="white", size=4) +
  labs(title="Diverging Lollipop Chart") + 
  coord_flip()+
  theme_bw()

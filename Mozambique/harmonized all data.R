library(readr)
harm_diet<-read_csv("~/CARE-WWF/Data/Harmonized_Diet_Data.csv")
harm_diet$Year<-NULL
harm_assets<-read_csv("~/CARE-WWF/Data/Harmonized_Assets_Data.csv")
harm_assets$Year<-NULL
harm_misc<-read_csv("~/CARE-WWF/Data/MiscDatAll.csv")
harm_misc$Year<-NULL
harm_dem<-read_csv("~/CARE-WWF/Data/Harmonized_Demographic_Data.csv")
Full_data<-merge(harm_diet,harm_assets,by="X1")
Full_data<-merge(harm_dem,Full_data,by="X1")
Full_data<-merge(Full_data,harm_misc,by="X1")
#updated 2/26/2019
write.csv(Full_data,file="Harmonized_longitudinal_data_2_26_2019.csv")
View(Full_data)


#Let's take a crack at some plots for fun
mdldat<-Full_data[c(2,3,4,5,6,7,8,9,35)]
mdldat<-na.omit(mdldat)
mdldat$Literacy<-as.logical(mdldat$Literacy)
mdldat$Community_Group<-as.logical(mdldat$Community_Group)
mdldat$Cons_Intervention<-as.logical(mdldat$Cons_Intervention)
mdldat$Devel_Intervention<-as.logical(mdldat$Devel_Intervention)
mdldat$Year<-as.character(mdldat$Year)
library(ggplot2)
ggplot(data=mdldat[mdldat$Year == c("2008" , "2018"),], aes(x=as.character(Year), y=Food_Groups,fill=Cons_Intervention))+
  geom_boxplot()+theme_bw()

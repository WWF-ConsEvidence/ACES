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
#updated 4/23/2019
write.csv(Full_data,file="Harmonized_longitudinal_data_4_23_2019.csv")
View(Full_data)



library(haven)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
library(splitstackshape)
dat2008 <- read_sav("~/CARE-WWF/Data/CARE.Basic.Data.Set.Clean (1).sav")
colnames(dat2008)<-gsub("@","Q",colnames(dat2008))
nd2008 <- read_sav("~/CARE-WWF/Data/CARE.Final.Data.Set.2008.sav")
Gender2008<-nd2008[48]
dat2014_diet <- read_csv("~/CARE-WWF/Data/2014Data_Diet.csv")
dat2014_asset <- read_csv("~/CARE-WWF/Data/2014Asset_Data.csv")
dat2014_agri <- read_csv("~/CARE-WWF/Data/2014Agriculture_Data.csv")
dat2014_agri <-dat2014_agri[-292,]
dat2014_dem <- read_csv("~/CARE-WWF/Data/2014Demographic_Data.csv")
dat2014_liveli <- read_csv("~/CARE-WWF/Data/2014Livelihood_Data.csv")
dat2018 <- read_csv("~/CARE-WWF/Data/2018Data.csv")
colnames(dat2018)<-paste("Q", colnames(dat2018))
colnames(dat2018)<-gsub(" ","_",colnames(dat2018))
#harmonize community & district data
names(dat2008)[14]="Community_code"
Community2008 <- ifelse(dat2008$Community_code == 241601, "Fuzi", 
             ifelse(dat2008$Community_code == 41602, "Moebase",
             ifelse(dat2008$Community_code==41603, "Txotxo",
             ifelse(dat2008$Community_code ==41604, "Monudo/Murudo", 
             ifelse(dat2008$Community_code ==41605, "Namige", 
             ifelse(dat2008$Community_code ==41606, "Solulo", 
             ifelse(dat2008$Community_code ==41607, "Magene", 
             ifelse(dat2008$Community_code ==41608, "Murateia", 
             ifelse(dat2008$Community_code ==41609, "Namaquete", 
             ifelse(dat2008$Community_code ==31101, "Mtopa", 
             ifelse(dat2008$Community_code ==31102, "Hori", 
             ifelse(dat2008$Community_code ==31103, "Mecane (Oua)", 
             ifelse(dat2008$Community_code ==31104, "Nacocolo", 
             ifelse(dat2008$Community_code ==31105, "Pacone", 
             ifelse(dat2008$Community_code ==31106, "Ampuitine",
             ifelse(dat2008$Community_code ==31107, "Corane",
             ifelse(dat2008$Community_code ==31108, "Lalaue",
             ifelse(dat2008$Community_code ==31109, "Mavele",
             ifelse(dat2008$Community_code ==31110, "Mucoroma",
             ifelse(dat2008$Community_code ==31111, "Nacalela",
             ifelse(dat2008$Community_code ==31112, "Nambui",
             ifelse(dat2008$Community_code ==31113, "Namichir",
             ifelse(dat2008$Community_code ==31114, "Nanheua",
             ifelse(dat2008$Community_code ==31115, "Natere",
             ifelse(dat2008$Community_code ==31116, "Tipane",
             ifelse(dat2008$Community_code ==30101, "Iarupa",
             ifelse(dat2008$Community_code ==30102, "Mituco",
             ifelse(dat2008$Community_code ==30103, "Macogone",
             ifelse(dat2008$Community_code ==30104, "Munar",
             ifelse(dat2008$Community_code ==30105, "Namiepe",
             ifelse(dat2008$Community_code ==30106, "Nauluco",
             ifelse(dat2008$Community_code ==30107, "Pulizica",
             ifelse(dat2008$Community_code ==30108, "Naheco",
             ifelse(dat2008$Community_code ==30109, "Namame",NA))))))))))))))))))))))))))))))))))
             
        
Community2018 <-ifelse(dat2018[7]==1,"Nauluco",
                ifelse(dat2018[7]==2,"Namiepe",
                 ifelse(dat2018[7]==3,"Pulizica",
                 ifelse(dat2018[7]==4,"Namame",
                 ifelse(dat2018[7]==5,"Macogone",
                 ifelse(dat2018[7]==6,"Manene",
                 ifelse(dat2018[7]==8,"Corane",
                 ifelse(dat2018[7]==9,"Mingolene", NA))))))))
                            
Community2014<-ifelse(dat2014_dem$village=="nacololo","Nacololo",
        ifelse(dat2014_dem$village=="mucuto","Mucuto",
       ifelse(dat2014_dem$village=="thapua","Thapua",
        ifelse(dat2014_dem$village=="manene","Manene",
        ifelse(dat2014_dem$village=="corane","Corane",
        ifelse(dat2014_dem$village=="mingolene","Mingolene", NA))))))



##Make 2008 DF
DemDat2008<-as.data.frame(cbind(Community2008,Gender2008,dat2008[21],dat2008[170]))
names(DemDat2008)[1:4]= c("Community","Gender","Literacy","Community_Group")
DemDat2008$Literacy <- ifelse(DemDat2008$Literacy == 0, 0, 
                              ifelse(DemDat2008$Literacy == 1, 1,NA))
DemDat2008$Community_Group <- ifelse(DemDat2008$Community_Group == 2, 0, 
                              ifelse(DemDat2008$Community_Group == 1, 1,NA))
DemDat2008$Gender <- ifelse(DemDat2008$Gender == 1, "Male", 
                            ifelse(DemDat2008$Gender == 2, "Female",NA))
DemDat2008$Year<-rep(2008,nrow(DemDat2008))

DemDat2008$Income_source<-ifelse(dat2008$Q4_51 == 1,"Subsist_crops",
                          ifelse(dat2008$Q4_51==2,"Cash_crops",
                          ifelse(dat2008$Q4_51==3, "Hort_crops",
                          ifelse(dat2008$Q4_51==4, "Fruit",
                          ifelse(dat2008$Q4_51==5,"Forraged_products",
                          ifelse(dat2008$Q4_51==6, "Informal_work",
                          ifelse(dat2008$Q4_51==7,"Fish_sale_local",
                          ifelse(dat2008$Q4_51==8,"Fish_sale_external",
                          ifelse(dat2008$Q4_51==9,"Fish_processing",
                          ifelse(dat2008$Q4_51==10,"Animal_husbandry",
                          ifelse(dat2008$Q4_51==11,"Firewood_charcoal",
                          ifelse(dat2008$Q4_51==12,"Sell_drinks",
                          ifelse(dat2008$Q4_51==13,"Transportation",
                          ifelse(dat2008$Q4_51 ==14,"Formal_work",
                          ifelse(dat2008$Q4_51==16,"Construction_materials",
                          ifelse(dat2008$Q4_51==17, "Rent_draft_animals",
                          ifelse(dat2008$Q4_51 == 18,"Monthly_pension",
                          ifelse(dat2008$Q4_51==19,"Remittances",
                          ifelse(dat2008$Q4_51==20,"Informal_formal_business",
                          ifelse(dat2008$Q4_51==15,"Informal_work",NA))))))))))))))))))))
                                               
DemDat2008$Food_source <-rep(NA, length=nrow(dat2008))   


DemDat2008$Education_HH <- ifelse(dat2008$Q1_10 == 1, "None",
                           ifelse(dat2008$Q1_10 ==2, "Incomplete_primary",
                           ifelse(dat2008$Q1_10 == 3,"Complete_primary",
                           ifelse(dat2008$Q1_10 ==4, "Incomplete_secondary",
                           ifelse(dat2008$Q1_10 ==5, "Complete_secondary_or_higher",
                           ifelse(dat2008$Q1_10 == 6,"Illiterate", NA))))))

DemDat2008$Education_spouse <- ifelse(dat2008$Q1_13 == 1, "None",
                               ifelse(dat2008$Q1_13 ==2, "Incomplete_primary",
                               ifelse(dat2008$Q1_13 == 3,"Complete_primary",
                               ifelse(dat2008$Q1_13 ==4, "Incomplete_secondary",
                               ifelse(dat2008$Q1_13 ==5, "Complete_secondary_or_higher",
                               ifelse(dat2008$Q1_13 == 6,"Illiterate", NA))))))


#Make 2014 DF
DemDat2014<-as.data.frame(cbind(Community2014,dat2014_dem$Gender,dat2014_dem$`1.5`,dat2014_agri$`4.22`))
names(DemDat2014)[1:4]= c("Community","Gender","Literacy","Community_Group")
DemDat2014$Gender <- ifelse(DemDat2014$Gender == 1, "Male", 
                        ifelse(DemDat2014$Gender == 2, "Female",NA))
DemDat2014$Literacy <- ifelse(DemDat2014$Literacy == 2, 0, 
                            ifelse(DemDat2014$Literacy == 1, 1,NA))
DemDat2014$Community_Group <- ifelse(DemDat2014$Community_Group == 2, 0, 
                                     ifelse(DemDat2014$Community_Group == 1, 1,NA))
DemDat2014$Year<-rep(2014,nrow(DemDat2014))

DemDat2014$Income_source<-ifelse(dat2014_liveli$`2.5` == 1,"Subsist_crops",
                          ifelse(dat2014_liveli$`2.5`==2,"Cash_crops",
                          ifelse(dat2014_liveli$`2.5`==3, "Hort_crops",
                          ifelse(dat2014_liveli$`2.5`==4, "Fruit",
                          ifelse(dat2014_liveli$`2.5`==5,"Forraged_products",
                          ifelse(dat2014_liveli$`2.5`==6, "Informal_work",
                          ifelse(dat2014_liveli$`2.5`==7, "Informal_work",
                          ifelse(dat2014_liveli$`2.5`==8,"Fish_sale_local",
                          ifelse(dat2014_liveli$`2.5`==9,"Fish_sale_external",
                          ifelse(dat2014_liveli$`2.5`==10,"Fish_processing",
                          ifelse(dat2014_liveli$`2.5`==11,"Animal_husbandry",
                          ifelse(dat2014_liveli$`2.5`==12,"Firewood_charcoal",
                          ifelse(dat2014_liveli$`2.5`==13,"Sell_drinks",
                          ifelse(dat2014_liveli$`2.5`==14,"Transportation",
                          ifelse(dat2014_liveli$`2.5` ==15,"Formal_work",
                          ifelse(dat2014_liveli$`2.5`==16,"Construction_materials",
                          ifelse(dat2014_liveli$`2.5`==17, "Rent_draft_animals",
                          ifelse(dat2014_liveli$`2.5` == 18,"Monthly_pension",
                          ifelse(dat2014_liveli$`2.5`==19,"Remittances",
                          ifelse(dat2014_liveli$`2.5`==20,"Informal_formal_business",
                          ifelse(grepl("25",dat2014_liveli$`2.5`),"Other",
                          ifelse(dat2014_liveli$`2.5`==22,"Other",
                          ifelse(dat2014_liveli$`2.5`==23,"Other",
                          ifelse(dat2014_liveli$`2.5`==24,"Other",
                           ifelse(dat2014_liveli$`2.5`==21,"Informal_formal_business","NA")))))))))))))))))))))))))

DemDat2014$Food_source<- ifelse(dat2014_liveli$`2.6` == 1,"Subsist_crops",
                         ifelse(dat2014_liveli$`2.6`==2,"Cash_crops",
                         ifelse(dat2014_liveli$`2.6`==3, "Hort_crops",
                         ifelse(dat2014_liveli$`2.6`==4, "Fruit",
                         ifelse(dat2014_liveli$`2.6`==5,"Forraged_products",
                         ifelse(dat2014_liveli$`2.6`==6, "Informal_work",
                         ifelse(dat2014_liveli$`2.6`==7, "Informal_work",
                         ifelse(dat2014_liveli$`2.6`==8,"Fish_sale_local",
                         ifelse(dat2014_liveli$`2.6`==9,"Fish_sale_external",
                         ifelse(dat2014_liveli$`2.6`==10,"Fish_processing",
                         ifelse(dat2014_liveli$`2.6`==11,"Animal_husbandry",
                         ifelse(dat2014_liveli$`2.6`==12,"Firewood_charcoal",
                         ifelse(dat2014_liveli$`2.6`==13,"Sell_drinks",
                         ifelse(dat2014_liveli$`2.6`==14,"Transportation",
                         ifelse(dat2014_liveli$`2.6` ==15,"Formal_work",
                         ifelse(dat2014_liveli$`2.6`==16,"Construction_materials",
                         ifelse(dat2014_liveli$`2.6`==17, "Rent_draft_animals",
                         ifelse(dat2014_liveli$`2.6` == 18,"Monthly_pension",
                         ifelse(dat2014_liveli$`2.6`==19,"Remittances",
                         ifelse(dat2014_liveli$`2.6`==20,"Informal_formal_business",
                         ifelse(grepl("25",dat2014_liveli$`2.6`),"Other",
                         ifelse(dat2014_liveli$`2.6`==22,"Artisan",
                         ifelse(dat2014_liveli$`2.6`==23,"Offers",
                         ifelse(dat2014_liveli$`2.6`==24,"Other",
                         ifelse(dat2014_liveli$`2.6`==21,"Informal_formal_business","NA")))))))))))))))))))))))))

DemDat2014$Education_HH<-rep(NA,length=nrow(DemDat2014))
DemDat2014$Education_spouse<-rep(NA,length=nrow(DemDat2014))
#####

#make 2018 DF##

DemDat2018<-cbind(Community2018,dat2018[13],dat2018[17], dat2018[279])
names(DemDat2018)[1:4]= c("Community","Gender","Literacy","Community_Group")
DemDat2018$Gender <- ifelse(DemDat2018$Gender == 1, "Male", 
                            ifelse(DemDat2018$Gender == 2, "Female",NA))
DemDat2018$Literacy <- ifelse(DemDat2018$Literacy == 2, 0, 
                              ifelse(DemDat2018$Literacy == 1, 1,NA))
DemDat2018$Community_Group <- ifelse(DemDat2018$Community_Group == 2, 0, 
                                     ifelse(DemDat2018$Community_Group == 1, 1,NA))
DemDat2018$Year<-rep(2018,nrow(DemDat2018))

DemDat2018$Income_source<-ifelse(dat2018[34] == 1,"Subsist_crops",
   ifelse(dat2018[34]==2,"Cash_crops",
   ifelse(dat2018[34]==3, "Hort_crops",
   ifelse(dat2018[34]==4, "Fruit",
   ifelse(dat2018[34]==5,"Forraged_products",
   ifelse(dat2018[34]==6, "Informal_work",
   ifelse(dat2018[34]==7, "Fish_sale_local",
   ifelse(dat2018[34]==8,"Fish_sale_external",
   ifelse(dat2018[34]==9,"Fish_processing",
   ifelse(dat2018[34]==10,"Animal_husbandry",
   ifelse(dat2018[34]==11,"Firewood_charcoal",
   ifelse(dat2018[34]==12,"Sell_drinks",
   ifelse(dat2018[34]==13,"Transportation",
   ifelse(dat2018[34] ==14,"Formal_work",
   ifelse(dat2018[34]==15,"Construction_materials",
   ifelse(dat2018[34]==16, "Rent_draft_animals",
   ifelse(dat2018[34] == 17,"Monthly_pension",
   ifelse(dat2018[34]==18,"Remittances",
   ifelse(dat2018[34]==88,"None",
   ifelse(dat2018[34]==19,"Informal_formal_business",
   ifelse(dat2018[34]==20,"Informal_formal_business","Other")))))))))))))))))))))
            
DemDat2018$Food_source<-as.character(ifelse(dat2018[35] == 1,"Subsist_crops",
                          ifelse(dat2018[35]==2,"Cash_crops",
                          ifelse(dat2018[35]==3, "Hort_crops",
                          ifelse(dat2018[35]==4, "Fruit",
                          ifelse(dat2018[35]==5,"Forraged_products",
                          ifelse(dat2018[35]==6, "Informal_work",
                          ifelse(dat2018[35]==7, "Fish_sale_local",
                          ifelse(dat2018[35]==8,"Fish_sale_external",
                          ifelse(dat2018[35]==9,"Fish_processing",
                          ifelse(dat2018[35]==10,"Animal_husbandry",
                          ifelse(dat2018[35]==11,"Firewood_charcoal",
                          ifelse(dat2018[35]==12,"Sell_drinks",
                          ifelse(dat2018[35]==13,"Transportation",
                          ifelse(dat2018[35] ==14,"Formal_work",
                          ifelse(dat2018[35]==15,"Construction_materials",
                          ifelse(dat2018[35]==16, "Rent_draft_animals",
                          ifelse(dat2018[35] == 17,"Monthly_pension",
                          ifelse(dat2018[35]==18,"Remittances",
                          ifelse(dat2018[35]==19,"Informal_formal_business",
                          ifelse(dat2018[35]==88,"None",
                          ifelse(dat2018[35]==21,"Artisan",
                          ifelse(dat2018[35]==22,"Offers",
                          ifelse(dat2018[35]==20,"Informal_formal_business","Other")))))))))))))))))))))))) 

DemDat2018$Education_HH <- as.character(ifelse(dat2018[18] == 1, "None",
                           ifelse(dat2018[18] ==2, "Incomplete_primary",
                           ifelse(dat2018[18]== 3,"Complete_primary",
                           ifelse(dat2018[18] ==4, "Incomplete_secondary",
                           ifelse(dat2018[18] ==5, "Complete_secondary_or_higher",
                           ifelse(dat2018[18] == 6,"Illiterate", NA)))))))

DemDat2018$Education_spouse <- as.character(ifelse(dat2018[22] == 1, "None",
                               ifelse(dat2018[22] ==2, "Incomplete_primary",
                               ifelse(dat2018[22] == 3,"Complete_primary",
                               ifelse(dat2018[22] ==4, "Incomplete_secondary",
                               ifelse(dat2018[22] ==5, "Complete_secondary_or_higher",
                               ifelse(dat2018[22] == 6,"Illiterate", NA)))))))

#DemDat2018$Association<-as.character()

###
harmdem<-rbind(DemDat2008,DemDat2014,DemDat2018)
#Make logical variables for general conservation or development intervention
harmdem$Cons_Intervention_gen<-ifelse(harmdem$Community == "Nauluco" ,0,
                           ifelse(harmdem$Community == "Namame", 0,
                           ifelse(harmdem$Community == "Pulizica",1,
                           ifelse(harmdem$Community == "Namiepe",1,
                           ifelse(harmdem$Community == "Macogone",0,
                           ifelse(harmdem$Community == "Manene",1,
                           ifelse(harmdem$Community == "Corane",1,
                           ifelse(harmdem$Community == "Mingolene",1,NA))))))))

harmdem$Devel_Intervention_gen<-ifelse(harmdem$Community == "Nauluco" ,0,
                                  ifelse(harmdem$Community == "Namame", 1,
                                  ifelse(harmdem$Community == "Pulizica",0,
                                  ifelse(harmdem$Community == "Namiepe",1,
                                  ifelse(harmdem$Community == "Macogone",1,
                                  ifelse(harmdem$Community == "Manene",1,
                                  ifelse(harmdem$Community == "Corane",1,
                                  ifelse(harmdem$Community == "Mingolene",0,NA))))))))

#Make variables for the more specific interventions
harmdem$Fish_NTZ<-ifelse(harmdem$Community == "Nauluco" ,0,
                                      ifelse(harmdem$Community == "Namame", 0,
                                      ifelse(harmdem$Community == "Pulizica",1,
                                      ifelse(harmdem$Community == "Namiepe",0,
                                      ifelse(harmdem$Community == "Macogone",0,
                                      ifelse(harmdem$Community == "Manene",0,
                                      ifelse(harmdem$Community == "Corane",1,
                                      ifelse(harmdem$Community == "Mingolene",1,NA))))))))

harmdem$FFS<-ifelse(harmdem$Community == "Nauluco" ,0,
                         ifelse(harmdem$Community == "Namame", 1,
                         ifelse(harmdem$Community == "Pulizica",0,
                         ifelse(harmdem$Community == "Namiepe",1,
                         ifelse(harmdem$Community == "Macogone",1,
                         ifelse(harmdem$Community == "Manene",1,
                         ifelse(harmdem$Community == "Corane",1,
                         ifelse(harmdem$Community == "Mingolene",0,NA))))))))

harmdem$CGRN_Miombo<-ifelse(harmdem$Community == "Nauluco" ,0,
                    ifelse(harmdem$Community == "Namame", 0,
                    ifelse(harmdem$Community == "Pulizica",0,
                    ifelse(harmdem$Community == "Namiepe",1,
                    ifelse(harmdem$Community == "Macogone",0,
                    ifelse(harmdem$Community == "Manene",0,
                    ifelse(harmdem$Community == "Corane",1,
                    ifelse(harmdem$Community == "Mingolene",0,NA))))))))

harmdem$CGRN_Mangrove<-ifelse(harmdem$Community == "Nauluco" ,0,
                            ifelse(harmdem$Community == "Namame", 0,
                            ifelse(harmdem$Community == "Pulizica",1,
                            ifelse(harmdem$Community == "Namiepe",0,
                            ifelse(harmdem$Community == "Macogone",0,
                            ifelse(harmdem$Community == "Manene",1,
                            ifelse(harmdem$Community == "Corane",1,
                            ifelse(harmdem$Community == "Mingolene",1,NA))))))))

harmdem$VSLA<-ifelse(harmdem$Community == "Nauluco" ,0,
                              ifelse(harmdem$Community == "Namame", 1,
                              ifelse(harmdem$Community == "Pulizica",0,
                              ifelse(harmdem$Community == "Namiepe",1,
                              ifelse(harmdem$Community == "Macogone",1,
                              ifelse(harmdem$Community == "Manene",0,
                              ifelse(harmdem$Community == "Corane",0,
                              ifelse(harmdem$Community == "Mingolene",0,NA))))))))

#community associations that are indicative of CARE-WWF alliance
Association_Alliance_2008<-ifelse((is.na(dat2008$Q6_2A) == TRUE) &
                                (is.na(dat2008$Q6_2B) == TRUE) & 
                                (is.na(dat2008$Q6_2C) == TRUE), NA,
                                    ifelse(dat2008$Q6_2A  %in% c(1,5,6,7,11,16)|
                                dat2008$Q6_2B  %in% c(1,5,6,7,11,16)|
                                dat2008$Q6_2C  %in% c(1,5,6,7,11,16),1,0))

#2014 and 2018 have '17' which was not present in 2008 survey

Association_Alliance_2014<-gsub("[^0-9.,]","", dat2014_agri$`4.23`)
Association_Alliance_2014<-gsub("[.]",",", Association_Alliance_2014)
Association_Alliance_2014<-as.data.frame(Association_Alliance_2014)
x<-cSplit(Association_Alliance_2014, "Association_Alliance_2014", sep=",")
Association_Alliance_2014<-ifelse((is.na(x$Association_Alliance_2014_1) == TRUE) &
                                    (is.na(x$Association_Alliance_2014_2) == TRUE) & 
                                    (is.na(x$Association_Alliance_2014_3) == TRUE), NA,
                                  ifelse(x$Association_Alliance_2014_1  %in% c(1,5,6,7,11,16,17)|
                                           x$Association_Alliance_2014_2  %in% c(1,5,6,7,11,16,17)|
                                           x$Association_Alliance_2014_3  %in% c(1,5,6,7,11,16,17),1,0))

                               


Association_Alliance_2018<-ifelse((is.na(dat2018$Q_10_2_A ) == TRUE) &
                                (is.na(dat2018$Q_10_2_B ) == TRUE) & 
                                (is.na(dat2018$Q_10_2_C ) == TRUE) & 
                                (is.na(dat2018$Q_10_2_D ) == TRUE), NA,
                              ifelse(dat2018$Q_10_2_A   %in% c(1,5,6,7,11,16,17)|
                                       dat2018$Q_10_2_B   %in% c(1,5,6,7,11,16,17)|
                                       dat2018$Q_10_2_C  %in% c(1,5,6,7,11,16,17)|
                                       dat2018$Q_10_2_D   %in% c(1,5,6,7,11,16,17),1,0))


harmdem$Association_Alliance<-c(Association_Alliance_2008,Association_Alliance_2014,Association_Alliance_2018)

                                                              
write.csv(harmdem, file="Harmonized_Demographic_Data.csv")











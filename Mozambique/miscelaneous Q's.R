library(readr)
library(haven)
dat2014_liveli <- read_csv("~/CARE-WWF/Data/2014Livelihood_Data.csv")
dat2008 <- read_sav("~/CARE-WWF/Data/CARE.Basic.Data.Set.Clean (1).sav")
colnames(dat2008)<-gsub("@","Q",colnames(dat2008))
dat2018 <- read.csv("~/CARE-WWF/Data/2018Data.csv",stringsAsFactors = FALSE)
dat2014_fish<-read_csv("~/CARE-WWF/Data/2014Fishing_fishing.csv")
dat2014_fish<-dat2014_fish[-(292:306),]
dat2014_fish_Reg<-read_csv("~/CARE-WWF/Data/2014Fishing_Regulations_Data.csv")
dat2014_diet<-read_csv("~/CARE-WWF/Data/2014Data_Diet.csv")
dat2014_agri<-read_csv("~/CARE-WWF/Data/2014Agriculture_Data.csv")
dat2014_agri <-dat2014_agri[-292,]

# Just gonna go down the list for the predictors that are not complete from all 3 years.

#Economic Decision making 

#2008
Econ_dec_2008<-rep(NA, length= nrow(dat2008))

#2014
Econ_dec_2014<-as.character(ifelse(dat2014_liveli$`2.9` == "1", "head",
                          ifelse(dat2014_liveli$`2.9` == "2", "spouse",
                                 ifelse(dat2014_liveli$`2.9` == "3", "together",NA))))

#2018
Econ_dec_2018<-as.character(ifelse(dat2018[276] == 1, "head",
                      ifelse(dat2018[276] == 2, "spouse",
                             ifelse(dat2018[276] == 3, "together",NA))))


# change in livlihood
#2008
Chng_livli_2008<-rep(NA, length= nrow(dat2008))

#2014
Chng_livli_2014<-ifelse(dat2014_liveli$`2.7` == "1", 1,
                      ifelse(dat2014_liveli$`2.7` == "2", 0,NA))
                            

#2018
Chng_livli_2018<-ifelse(dat2018[36] == 1,1,
                      ifelse(dat2018[36] == 2, 0,NA))


# Number of days ate seafood in the last week
#2008
Days_seafood_2008<-rep(NA, length= nrow(dat2008))

#2014
Days_seafood_2014<-ifelse(dat2014_diet$`6.2` == "1", 1,
                   ifelse(dat2014_diet$`6.2` == "2", 2,
                   ifelse(dat2014_diet$`6.2` == "3", 3,
                   ifelse(dat2014_diet$`6.2` == "4", 4,
                   ifelse(dat2014_diet$`6.2` == "5", 5,
                   ifelse(dat2014_diet$`6.2` == "6", 6,
                   ifelse(dat2014_diet$`6.2` == "7", 7,
                   ifelse(dat2014_diet$`6.2` == "0", 0,NA))))))))


#2018
Days_seafood_2018<-dat2018[237]

#Fishing Frequency
#2008
Fish_freq_2008<-rep(NA, length= nrow(dat2008))

#2014
Fish_freq_2014<-ifelse(dat2014_fish$`5.3` == "1", 1,
                   ifelse(dat2014_fish$`5.3` == "2", 2,
                   ifelse(dat2014_fish$`5.3` == "3", 3,
                   ifelse(dat2014_fish$`5.3` == "4", 4,
                   ifelse(dat2014_fish$`5.3` == "5", 5,
                   ifelse(dat2014_fish$`5.3` == "6", 6,
                   ifelse(dat2014_fish$`5.3` == "7", 7,
                   ifelse(dat2014_fish$`5.3` == "0", 0,NA))))))))


#2018
Fish_freq_2018<-dat2018[139]


#Fish sanctuaries impact
#2008
Fish_sanct_impct_2008<-rep(NA, length= nrow(dat2008))
#2014
Fish_sanct_impct_2014<-as.character(ifelse(dat2014_fish_Reg$`7.15` == 1, "benefited",
                      ifelse(dat2014_fish_Reg$`7.15` == 2, "harmed",
                             ifelse(dat2014_fish_Reg$`7.15` == 3, "none",NA))))
#2018
Fish_sanct_impct_2018<-as.character(ifelse(dat2018[300] == 1, "benefited",
                              ifelse(dat2018[300] == 2, "harmed",
                                     ifelse(dat2018[300] == 3, "none",NA))))

#Knowledge of fishing prohibitions
#2008
Prohb_fish_2008<-rep(NA, length= nrow(dat2008))
#2014
Prohb_fish_2014<-as.character(ifelse(dat2014_fish_Reg$`7.5` == 1.0, "Yes",
                              ifelse(dat2014_fish_Reg$`7.5` == 2.0, "No",NA)))
                                    
#2018
Prohb_fish_2018<-as.character(ifelse(dat2018[169] == 1, "Yes",
                        ifelse(dat2018[169] == 2, "No",NA)))


#Do species exist that can not be fished?
#2008
No_fish_spec_2008<-rep(NA, length= nrow(dat2008))
#2014
No_fish_spec_2014<-as.character(ifelse(dat2014_fish_Reg$`7.8` == 1, "Yes",
                        ifelse(dat2014_fish_Reg$`7.8` == 2, "No",NA)))

#2018
No_fish_spec_2018<-as.character(ifelse(dat2018[179] == 1, "Yes",
                        ifelse(dat2018[179] == 2, "No",NA)))


#Coastal forest use
#Does your household use coastal forests (Yes or no)

#2008
CF_USE_YN_2008<-rep(NA, length= nrow(dat2008))

#2014
CF_USE_YN_2014<-ifelse(is.na(dat2014_fish$`5.17`), 0,
                       ifelse(dat2014_fish$`5.17` == 0.0, 0,1))
#2018
CF_USE_YN_2018<-ifelse(dat2018[186] ==1,1,0)


#How do you use coastal forests
#Construction
#2008
CF_USE_Construction_2008<-rep(NA, length= nrow(dat2008))
#2014
CF_USE_Construction_2014<-ifelse(grepl("1",dat2014_fish$`5.17`),1,0)
#2018
x<-dat2018[187]+dat2018[188]
CF_USE_Construction_2018<-ifelse(x == 0, 0,
                          ifelse(x ==4, 0, 1))
#Honey
#2008
CF_USE_Honey_2008<-rep(NA, length= nrow(dat2008))
#2014
CF_USE_Honey_2014<-ifelse(grepl("2",dat2014_fish$`5.17`),1,0)
#2018
CF_USE_Honey_2018<-ifelse(dat2018[189]==1,1,
                          ifelse(dat2018[189] ==2,0,NA))
#Medicine
#2008
CF_USE_Medicine_2008<-rep(NA, length= nrow(dat2008))
#2014
CF_USE_Medicine_2014<-ifelse(grepl("3",dat2014_fish$`5.17`),1,0)
#2018
CF_USE_Medicine_2018<-ifelse(dat2018[190]==1,1,
                             ifelse(dat2018[190] ==2,0,NA))
#Fruit
#2008
CF_USE_Fruit_2008<-rep(NA, length= nrow(dat2008))
#2014
CF_USE_Fruit_2014<-ifelse(grepl("4",dat2014_fish$`5.17`),1,0)
#2018
CF_USE_Fruit_2018<-ifelse(dat2018[191]==1,1,
                          ifelse(dat2018[191] ==2,0,NA))
#other
#2008
CF_USE_Other_2008<-rep(NA, length= nrow(dat2008))
#2014
CF_USE_Other_2014<-ifelse(grepl("5",dat2014_fish$`5.17`),1,0)
#2018
a<-ifelse(dat2018[192] ==1,1,0)
b<-ifelse(dat2018[193] ==1,1,0)
c<-ifelse(dat2018[194] ==1,1,0)
d<-ifelse(is.na(dat2018[195]),0,
                ifelse(dat2018[195] == 0,0,1))
y<-a+b+c+d
CF_USE_Other_2018<-ifelse(y==0,0,1)

#Mangrove use
#Does your household use mangroves (Yes or no)
#2008
Mang_USE_YN_2008<-rep(NA, length= nrow(dat2008))

#2014
Mang_USE_YN_2014<-ifelse(is.na(dat2014_fish$`5.16`), 0,
                         ifelse(dat2014_fish$`5.16` ==9,0,
                       ifelse(dat2014_fish$`5.16` == 0.0, 0,1)))
#2018
Mang_USE_YN_2018<-ifelse(dat2018[196] ==1,1,
                         ifelse(dat2018[196]==2,0,NA))

#How do you use mangroves?

#Firewood
#2008
Mang_USE_Firewood_2008<-rep(NA, length= nrow(dat2008))
#2014
Mang_USE_Firewood_2014<-ifelse(grepl("1",dat2014_fish$`5.16`),1,0)
#2018
Mang_USE_Firewood_2018<-ifelse(dat2018[201] ==1,1,
                               ifelse(dat2018[201]==2,0,NA))
#Home_construction
#2008
Mang_USE_Home_2008<-rep(NA, length= nrow(dat2008))
#2014
Mang_USE_Home_2014<-ifelse(grepl("2",dat2014_fish$`5.16`),1,0)
#2018
Mang_USE_Home_2018<-ifelse(dat2018[197] ==1,1,
                           ifelse(dat2018[197]==2,0,NA))

#Crab and Snail collection
#2008
Mang_USE_CrabSnail_2008<-rep(NA, length= nrow(dat2008))
#2014
cr<-ifelse(grepl("3",dat2014_fish$`5.16`),1,0)
sn<-ifelse(grepl("4",dat2014_fish$`5.16`),1,0)
crsn<-cr+sn
Mang_USE_CrabSnail_2014<-ifelse(crsn == 0,0,1)
#2018
Mang_USE_CrabSnail_2018<-ifelse(dat2018[202] ==1,1,
                                ifelse(dat2018[202]==2,0,NA))

#Fish production
#2008
Mang_USE_FishProd_2008<-rep(NA, length= nrow(dat2008))
#2014
Mang_USE_FishProd_2014<-ifelse(grepl("5",dat2014_fish$`5.16`),1,0)
#2018
Mang_USE_FishProd_2018<-ifelse(dat2018[203] ==1,1,
                               ifelse(dat2018[203]==2,0,NA))

#Medicine
#2008
Mang_USE_medicine_2008<-rep(NA, length= nrow(dat2008))
#2014
Mang_USE_medicine_2014<-ifelse(grepl("7",dat2014_fish$`5.16`),1,0)
#2018
Mang_USE_medicine_2018<-ifelse(dat2018[199] ==1,1,
                               ifelse(dat2018[199]==2,0,NA))

#Honey
#2008
Mang_USE_Honey_2008<-rep(NA, length= nrow(dat2008))
#2014
Mang_USE_Honey_2014<-ifelse(grepl("8",dat2014_fish$`5.16`),1,0)
#2018
Mang_USE_Honey_2018<-ifelse(dat2018[198] ==1,1,
                            ifelse(dat2018[198]==2,0,NA))


#Fish Trap construction
#2008
Mang_USE_FishTrap_2008<-rep(NA, length= nrow(dat2008))
#2014
Mang_USE_FishTrap_2014<-ifelse(grepl("6",dat2014_fish$`5.16`),1,0)
#2018
Mang_USE_FishTrap_2018<-ifelse(dat2018[204] ==1,1,
                        ifelse(dat2018[204]==2,0,NA))

#other
#2008
Mang_USE_Other_2008<-rep(NA, length= nrow(dat2008))
#2014
Mang_USE_Other_2014<-ifelse(grepl("10",dat2014_fish$`5.16`),1,0)
#2018
Mang_USE_Other_2018<-ifelse(dat2018[205] ==1,1,
                     ifelse(dat2018[205]==2,0,NA))


#AGRICULTURE
#Crop rotation
#2008
Crop_Rotation_2008<-rep(NA, length= nrow(dat2008))
#2014
Crop_Rotation_2014<-ifelse(dat2014_agri$`4.10` == 1,1,
                    ifelse(dat2014_agri$`4.10`==2,0,NA))
#2018
Crop_Rotation_2018<-ifelse(dat2018[204]==1,1,
                    ifelse(dat2018[204]==2,0,NA))

#Legumes between rainy seasons
#2008
Legumes_BRS_2008<-ifelse(dat2008$Q2_14 ==1,1,
                  ifelse(dat2008$Q2_14==2,0,NA))
                  
#2014
Legumes_BRS_2014<-rep(NA, length= nrow(dat2014_agri))
#2018
Legumes_BRS_2018<-ifelse(dat2018[121] ==1,1,
                  ifelse(dat2018[121]==2,0,NA))

#Minimum tillage
#2008
Minimum_tillage_2008<-rep(NA, length= nrow(dat2008))
#2014
Minimum_tillage_2014<-ifelse(dat2014_agri$`4.7` == 1,1,
                      ifelse(dat2014_agri$`4.7`==2,0,NA))
#2018
Minimum_tillage_2018<-ifelse(dat2018[122]==1,1,
                       ifelse(dat2018[122]==2,0,NA))

#Mulching
#2008
Mulching_2008<-ifelse(dat2008$Q2_12 ==1,1,
               ifelse(dat2008$Q2_12==2,0,NA))
#2014
Mulching_2014<-ifelse(dat2014_agri$`4.8` == 1,1,
               ifelse(dat2014_agri$`4.8`==2,0,NA))
#2018
Mulching_2018<-ifelse(dat2018[123]==1,1,
               ifelse(dat2018[123]==2,0,NA))

#Live cover
#2008
Live_cover_2008<-rep(NA, length= nrow(dat2008))
#2014
Live_cover_2014<-ifelse(dat2014_agri$`4.9` == 1,1,
                 ifelse(dat2014_agri$`4.9`==2,0,NA))
#2018
Live_cover_2018<-ifelse(dat2018[124]==1,1,
                 ifelse(dat2018[124]==2,0,NA))

#Use of organic fertilizer
#2008
Organic_Fertilizer_2008<-ifelse(dat2008$Q2_13 ==1,1,
                         ifelse(dat2008$Q2_13 ==2,0,NA))
#2014
Organic_Fertilizer_2014<-ifelse(dat2014_agri$`4.8` == 1,1,
                         ifelse(dat2014_agri$`4.8`==2,0,NA))
#2018
Organic_Fertilizer_2018<-rep(NA, length= nrow(dat2018))

#Enhance soil fertility in any way. This is juat a combination of the last two
Any_fert_2008 <- Organic_Fertilizer_2008
Any_fert_2014 <- apply(cbind(Live_cover_2014, Organic_Fertilizer_2014), 1, function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm=T)))
Any_fert_2014 <- ifelse(Any_fert_2014 == 1,1,
                 ifelse(Any_fert_2014 ==2,1,
                 ifelse(Any_fert_2014==0,0,NA)))
Any_fert_2018 <- Live_cover_2018


#greatest agricultural challenges...come back to this cant do it today *******************************

#Received credit
#2008
Borrow_2008<-ifelse(dat2008$Q6_3 ==1,1,
             ifelse(dat2008$Q6_3 ==2,0,NA))
#2014
Borrow_2014<-rep(NA, length= nrow(dat2014_agri))
#2018  
Borrow_2018<-ifelse(dat2018[76]==1,1,
             ifelse(dat2018[76]==2,0,NA))
  
#Credit from where...A
#2008
Credit_from_A_2008<-as.character(ifelse(dat2008$Q6_4A ==1,"Bank",
                    ifelse(dat2008$Q6_4A ==2, "Informal_credit",
                    ifelse(dat2008$Q6_4A ==3, "Informal_credit",
                    ifelse(dat2008$Q6_4A ==7, "Informal_credit",
                    ifelse(dat2008$Q6_4A ==8, "Informal_credit",
                    ifelse(dat2008$Q6_4A ==4, "Friends_family",
                    ifelse(dat2008$Q6_4A ==6, "Religious_institution",
                    ifelse(dat2008$Q6_4A ==5, "Other",NA)))))))))
                

#2014
Credit_from_A_2014<-rep(NA, length=nrow(dat2014_agri))

#2018
Credit_from_A_2018<-as.character(ifelse(dat2018[77] ==1,"Bank",
                    ifelse(dat2018[77] ==2, "Informal_credit",
                    ifelse(dat2018[77] ==3, "Informal_credit",
                    ifelse(dat2018[77] ==7, "Other",
                    ifelse(dat2018[77] ==4, "Friends_family",
                    ifelse(dat2018[77] ==6, "Religious_institution",
                    ifelse(dat2018[77] ==5, "Other",NA))))))))


#Credit from where...B
#2008
Credit_from_B_2008<-as.character(ifelse(dat2008$Q6_4B ==1,"Bank",
                    ifelse(dat2008$Q6_4B ==2, "Informal_credit",
                    ifelse(dat2008$Q6_4B ==3, "Informal_credit",
                    ifelse(dat2008$Q6_4B ==7, "Informal_credit",
                    ifelse(dat2008$Q6_4B ==8, "Informal_credit",
                    ifelse(dat2008$Q6_4B ==4, "Friends_family",
                    ifelse(dat2008$Q6_4B ==6, "Religious_institution",
                    ifelse(dat2008$Q6_4B ==5, "Other",NA)))))))))

#2014
Credit_from_B_2014<-rep(NA, length=nrow(dat2014_agri))

#2018
Credit_from_B_2018<-as.character(ifelse(dat2018[78] ==1,"Bank",
                    ifelse(dat2018[78] ==2, "Informal_credit",
                    ifelse(dat2018[78] ==3, "Informal_credit",
                    ifelse(dat2018[78] ==7, "Other",
                    ifelse(dat2018[78] ==4, "Friends_family",
                    ifelse(dat2018[78] ==6, "Religious_institution",
                    ifelse(dat2018[78] ==5, "Other",NA))))))))
  
#Credit from where...C
#2008
Credit_from_C_2008<-as.character(ifelse(dat2008$Q6_4C ==1,"Bank",
                    ifelse(dat2008$Q6_4C ==2, "Informal_credit",
                    ifelse(dat2008$Q6_4C ==3, "Informal_credit",
                    ifelse(dat2008$Q6_4C ==7, "Informal_credit",
                    ifelse(dat2008$Q6_4C ==8, "Informal_credit",
                    ifelse(dat2008$Q6_4C ==4, "Friends_family",
                    ifelse(dat2008$Q6_4C ==6, "Religious_institution",
                    ifelse(dat2008$Q6_4C ==5, "Other",NA)))))))))
  
#2014
Credit_from_C_2014<-rep(NA, length=nrow(dat2014_agri))

#2018
Credit_from_C_2018<-as.character(ifelse(dat2018[79] ==1,"Bank",
                    ifelse(dat2018[79] ==2, "Informal_credit",
                    ifelse(dat2018[79] ==3, "Informal_credit",
                    ifelse(dat2018[79] ==7, "Other",
                    ifelse(dat2018[79] ==4, "Friends_family",
                    ifelse(dat2018[79] ==6, "Religious_institution",
                    ifelse(dat2018[79] ==5, "Other",NA))))))))
  
#credit reason
#2008
Credit_reason_2008<-as.character(ifelse(dat2008$Q6_5 == 1,"New_home",
                    ifelse(dat2008$Q6_5 ==2, "Home_repair",
                    ifelse(dat2008$Q6_5 ==3, "Other_construction",
                    ifelse(dat2008$Q6_5 ==4, "Land",
                    ifelse(dat2008$Q6_5 ==5, "Fish_equiptment",
                    ifelse(dat2008$Q6_5 == 6, "Food",
                    ifelse(dat2008$Q6_5 ==7, "Agricultural_inputs",
                    ifelse(dat2008$Q6_5 ==8, "Animals_medicine",
                    ifelse(dat2008$Q6_5 ==9, "Other_goods",
                    ifelse(dat2008$Q6_5 ==10, "Pride_wedding",
                    ifelse(dat2008$Q6_5 ==11, "Health_medicine",
                    ifelse(dat2008$Q6_5 ==12, "Funeral",
                    ifelse(dat2008$Q6_5 == 13,"Business",NA))))))))))))))
#2014
Credit_reason_2014<-rep(NA, length=nrow(dat2014_agri))

#2018
Credit_reason_2018<-as.character(ifelse(dat2018[80] == 1,"New_home",
                     ifelse(dat2018[80] ==2, "Home_repair",
                     ifelse(dat2018[80] ==3, "Other_construction",
                     ifelse(dat2018[80] ==4, "Land",
                     ifelse(dat2018[80] ==5, "Fish_equiptment",
                     ifelse(dat2018[80] == 6, "Food",
                     ifelse(dat2018[80] ==7, "Agricultural_inputs",
                     ifelse(dat2018[80] ==8, "Animals_medicine",
                     ifelse(dat2018[80] ==9, "Other",
                     ifelse(dat2018[80] == 14, "Other",
                     ifelse(dat2018[80] ==10, "Pride_wedding",
                     ifelse(dat2018[80] ==11, "Health_medicine",
                     ifelse(dat2018[80] ==12, "Funeral",
                     ifelse(dat2018[80] == 13,"Business",NA)))))))))))))))


#Participation in FFS
#2008
FFS_Participation_2008<-rep(NA, length= nrow(dat2008))
#2014
FFS_Participation_2014<-ifelse(dat2014_agri$`4.19` == 1,1,
                        ifelse(dat2014_agri$`4.19`==2,0,NA))
#2018
FFS_Participation_2018<-ifelse(dat2018[293]==1,1,
                        ifelse(dat2018[293]==2,1,
                        ifelse(dat2018[293]==3,0,
                        ifelse(dat2018[293]==4,0,NA))))
#Add everything together
Year_2008<-rep(as.character(2008),length=nrow(dat2008))
Year_2014<-rep(as.character(2014),length=nrow(dat2014_agri))
Year_2018<-rep(as.character(2018),length=nrow(dat2018))

MiscDat2008<-as.data.frame(cbind(Year_2008,Econ_dec_2008,Chng_livli_2008,Days_seafood_2008,
                                 Fish_freq_2008,Fish_sanct_impct_2008,Prohb_fish_2008,
                                 No_fish_spec_2008,CF_USE_YN_2008, CF_USE_Construction_2008, 
                                 CF_USE_Honey_2008,CF_USE_Medicine_2008, CF_USE_Fruit_2008,
                                 CF_USE_Other_2008,Mang_USE_YN_2008, Mang_USE_Firewood_2008,
                                 Mang_USE_Home_2008, Mang_USE_CrabSnail_2008, Mang_USE_FishProd_2008,
                                 Mang_USE_medicine_2008, Mang_USE_Honey_2008,Mang_USE_FishTrap_2008,
                                 Mang_USE_Other_2008,Crop_Rotation_2008, Legumes_BRS_2008, 
                                 Minimum_tillage_2008,Mulching_2008, Live_cover_2008,
                                 Organic_Fertilizer_2008,Any_fert_2008, Borrow_2008,Credit_from_A_2008,
                                 Credit_from_B_2008,Credit_from_C_2008,Credit_reason_2008, FFS_Participation_2008),stringsAsFactors = FALSE)

MiscDat2018<-as.data.frame(cbind(Year_2018,Econ_dec_2018,Chng_livli_2018,Days_seafood_2018,
                                 Fish_freq_2018,Fish_sanct_impct_2018,Prohb_fish_2018,
                                 No_fish_spec_2018,CF_USE_YN_2018, CF_USE_Construction_2018, 
                                 CF_USE_Honey_2018,CF_USE_Medicine_2018, CF_USE_Fruit_2018,
                                 CF_USE_Other_2018,Mang_USE_YN_2018, Mang_USE_Firewood_2018,
                                 Mang_USE_Home_2018, Mang_USE_CrabSnail_2018, Mang_USE_FishProd_2018,
                                 Mang_USE_medicine_2018, Mang_USE_Honey_2018,Mang_USE_FishTrap_2018,
                                 Mang_USE_Other_2018,Crop_Rotation_2018, Legumes_BRS_2018, 
                                 Minimum_tillage_2018,Mulching_2018, Live_cover_2018,
                                 Organic_Fertilizer_2018,Any_fert_2018, Borrow_2018,Credit_from_A_2018,
                                 Credit_from_B_2018,Credit_from_C_2018, Credit_reason_2018, FFS_Participation_2018,stringsAsFactors = FALSE))

MiscDat2014<-as.data.frame(cbind(Year_2014,Econ_dec_2014,Chng_livli_2014,Days_seafood_2014,
                                 Fish_freq_2014,Fish_sanct_impct_2014,Prohb_fish_2014,
                                 No_fish_spec_2014,CF_USE_YN_2014, CF_USE_Construction_2014, 
                                 CF_USE_Honey_2014,CF_USE_Medicine_2014, CF_USE_Fruit_2014,
                                 CF_USE_Other_2014,Mang_USE_YN_2014, Mang_USE_Firewood_2014,
                                 Mang_USE_Home_2014, Mang_USE_CrabSnail_2014, Mang_USE_FishProd_2014,
                                 Mang_USE_medicine_2014, Mang_USE_Honey_2014,Mang_USE_FishTrap_2014,
                                 Mang_USE_Other_2014,Crop_Rotation_2014, Legumes_BRS_2014, 
                                 Minimum_tillage_2014,Mulching_2014, Live_cover_2014,
                                 Organic_Fertilizer_2014,Any_fert_2014, Borrow_2014,Credit_from_A_2014,
                                 Credit_from_B_2014,Credit_from_C_2014, Credit_reason_2014,FFS_Participation_2014),stringsAsFactors = FALSE)

names(MiscDat2008)[1:36]= c("Year","Econ_dec","Chng_livli","Days_seafood",
                            "Fish_freq","Fish_sanct_impct","Prohb_fish",
                            "No_fish_spec","CF_USE_YN", "CF_USE_Construction", 
                            "CF_USE_Honey","CF_USE_Medicine", "CF_USE_Fruit",
                            "CF_USE_Other","Mang_USE_YN", "Mang_USE_Firewood",
                            "Mang_USE_Home", "Mang_USE_CrabSnail", "Mang_USE_FishProd",
                            "Mang_USE_medicine", "Mang_USE_Honey","Mang_USE_FishTrap",
                            "Mang_USE_Other","Crop_Rotation", "Legumes_BRS", 
                            "Minimum_tillage","Mulching", "Live_cover",
                            "Organic_Fertilizer","Any_fert" ,"Borrow", "Credit_from_A",
                            "Credit_from_B","Credit_from_C","Credit_reason","FFS_Participation")

names(MiscDat2014)[1:36]= c("Year","Econ_dec","Chng_livli","Days_seafood",
                            "Fish_freq","Fish_sanct_impct","Prohb_fish",
                            "No_fish_spec","CF_USE_YN", "CF_USE_Construction", 
                            "CF_USE_Honey","CF_USE_Medicine", "CF_USE_Fruit",
                            "CF_USE_Other","Mang_USE_YN", "Mang_USE_Firewood",
                            "Mang_USE_Home", "Mang_USE_CrabSnail", "Mang_USE_FishProd",
                            "Mang_USE_medicine", "Mang_USE_Honey","Mang_USE_FishTrap",
                            "Mang_USE_Other","Crop_Rotation", "Legumes_BRS", 
                            "Minimum_tillage","Mulching", "Live_cover",
                            "Organic_Fertilizer", "Any_fert" ,"Borrow", "Credit_from_A",
                            "Credit_from_B","Credit_from_C","Credit_reason","FFS_Participation")

names(MiscDat2018)[1:36]= c("Year","Econ_dec","Chng_livli","Days_seafood",
                            "Fish_freq","Fish_sanct_impct","Prohb_fish",
                            "No_fish_spec","CF_USE_YN", "CF_USE_Construction", 
                            "CF_USE_Honey","CF_USE_Medicine", "CF_USE_Fruit",
                            "CF_USE_Other","Mang_USE_YN", "Mang_USE_Firewood",
                            "Mang_USE_Home", "Mang_USE_CrabSnail", "Mang_USE_FishProd",
                            "Mang_USE_medicine", "Mang_USE_Honey","Mang_USE_FishTrap",
                            "Mang_USE_Other","Crop_Rotation", "Legumes_BRS", 
                            "Minimum_tillage","Mulching", "Live_cover",
                            "Organic_Fertilizer", "Any_fert" ,"Borrow","Credit_from_A",
                            "Credit_from_B","Credit_from_C","Credit_reason", "FFS_Participation")

MiscDatAll<-rbind(MiscDat2008,MiscDat2014,MiscDat2018)
write.csv(MiscDatAll, file="MiscDatAll.csv")

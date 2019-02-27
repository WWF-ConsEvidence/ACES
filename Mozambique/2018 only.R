#Let's look at data that was collected in 2018 only 
dat2018 <- read_csv("~/CARE-WWF/Data/2018Data.csv")

#Make the df and add Womens economic decision making
Only2018<-as.data.frame(dat2018$'9_2')
colnames(Only2018)[1]<-"WEDM"
Only2018$WEDM<-ifelse(Only2018$WEDM == 1,"Increased",
                        ifelse(Only2018$WEDM == 2, "Same",
                               ifelse(Only2018$WEDM == 3,"Decreased",NA)))

#Food provisioning 
fp2018<-dat2018[ ,c('7_5','7_7') ]
fp2018$`7_5`<-as.character(fp2018$`7_5`)
fp2018$`7_7`<-as.character(fp2018$`7_7`)
fp2018$change <- ifelse(fp2018$`7_7` == 2, "Worsened", 
                 ifelse(fp2018$`7_7` == 1, "Worsened",
                 ifelse(fp2018$`7_7`==3, "Stayed the same",
                 ifelse(fp2018$`7_7` ==4, "Improved", 
                 ifelse(fp2018$`7_7`==5, "Improved",NA)))))


Only2018$Food_prov<-fp2018[[1]]
Only2018$Food_prov<-ifelse(Only2018$Food_prov=="2",0,1)
Only2018$Food_prov_change<-fp2018[[3]]

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

#add months of adequate fp
Only2018$Months_adequate_fp<-mpsums$Months


#fish change over time
Only2018$Fish_change_quantity<-ifelse(dat2018[[166]]==1,"More",
                                      ifelse(dat2018[166]==2,"Less",
                                                    ifelse(dat2018[166]==3,"Same",NA)))
Only2018$Fish_change_species<-ifelse(dat2018[[167]]==1,"More",
                                     ifelse(dat2018[167]==2,"Less",
                                            ifelse(dat2018[167]==3,"Same",NA)))
Only2018$Fish_change_size<-ifelse(dat2018[[168]]==1,"More",
                                  ifelse(dat2018[168]==2,"Less",
                                         ifelse(dat2018[168]==3,"Same",NA)))

#Make variables for community and interventions
Only2018$Community <-as.character(ifelse(dat2018[7]==1,"Nauluco",
                                    ifelse(dat2018[7]==2,"Namiepe",
                                    ifelse(dat2018[7]==3,"Pulizica",
                                    ifelse(dat2018[7]==4,"Namame",
                                    ifelse(dat2018[7]==5,"Macogone",
                                    ifelse(dat2018[7]==6,"Manene",
                                    ifelse(dat2018[7]==8,"Corane",
                                    ifelse(dat2018[7]==9,"Mingolene", NA)))))))))

Only2018$Cons_Intervention_gen<-as.integer(ifelse(Only2018$Community == "Nauluco" ,0,
                                ifelse(Only2018$Community == "Namame", 0,
                                ifelse(Only2018$Community == "Pulizica",1,
                                ifelse(Only2018$Community == "Namiepe",1,
                                ifelse(Only2018$Community == "Macogone",0,
                                ifelse(Only2018$Community == "Manene",1,
                                ifelse(Only2018$Community == "Corane",1,
                                ifelse(Only2018$Community == "Mingolene",1,NA)))))))))

Only2018$Devel_Intervention_gen<-as.integer(ifelse(Only2018$Community == "Nauluco" ,1,
                                 ifelse(Only2018$Community == "Namame", 1,
                                 ifelse(Only2018$Community == "Pulizica",0,
                                 ifelse(Only2018$Community == "Namiepe",1,
                                 ifelse(Only2018$Community == "Macogone",1,
                                 ifelse(Only2018$Community == "Manene",1,
                                 ifelse(Only2018$Community == "Corane",1,
                                 ifelse(Only2018$Community == "Mingolene",0,NA)))))))))

#Make variables for the more specific interventions

#Fish No take zone
Only2018$Fish_NTZ<-as.integer(ifelse(Only2018$Community == "Nauluco" ,0,
                   ifelse(Only2018$Community == "Namame", 0,
                   ifelse(Only2018$Community == "Pulizica",1,
                   ifelse(Only2018$Community == "Namiepe",0,
                   ifelse(Only2018$Community == "Macogone",0,
                   ifelse(Only2018$Community == "Manene",0,
                   ifelse(Only2018$Community == "Corane",1,
                   ifelse(Only2018$Community == "Mingolene",1,NA)))))))))

#Farmer Field Schools
Only2018$FFS<-ifelse(Only2018$Community == "Nauluco" ,0,
              ifelse(Only2018$Community == "Namame", 1,
              ifelse(Only2018$Community == "Pulizica",0,
              ifelse(Only2018$Community == "Namiepe",1,
              ifelse(Only2018$Community == "Macogone",1,
              ifelse(Only2018$Community == "Manene",1,
              ifelse(Only2018$Community == "Corane",1,
              ifelse(Only2018$Community == "Mingolene",0,NA))))))))

#Community based coastal forest management
Only2018$CGRN_Miombo<-ifelse(Only2018$Community == "Nauluco" ,0,
                      ifelse(Only2018$Community == "Namame", 0,
                      ifelse(Only2018$Community == "Pulizica",0,
                      ifelse(Only2018$Community == "Namiepe",1,
                      ifelse(Only2018$Community == "Macogone",0,
                      ifelse(Only2018$Community == "Manene",0,
                      ifelse(Only2018$Community == "Corane",1,
                      ifelse(Only2018$Community == "Mingolene",0,NA))))))))

#Community based mangrove management
Only2018$CGRN_Mangrove<-ifelse(Only2018$Community == "Nauluco" ,0,
                        ifelse(Only2018$Community == "Namame", 0,
                        ifelse(Only2018$Community == "Pulizica",1,
                        ifelse(Only2018$Community == "Namiepe",0,
                        ifelse(Only2018$Community == "Macogone",0,
                        ifelse(Only2018$Community == "Manene",1,
                        ifelse(Only2018$Community == "Corane",1,
                        ifelse(Only2018$Community == "Mingolene",1,NA))))))))

#Village savings and loan program
Only2018$VSLA<-ifelse(Only2018$Community == "Nauluco" ,0,
                ifelse(Only2018$Community == "Namame", 1,
                ifelse(Only2018$Community == "Pulizica",0,
                ifelse(Only2018$Community == "Namiepe",1,
                ifelse(Only2018$Community == "Macogone",1,
                ifelse(Only2018$Community == "Manene",0,
                ifelse(Only2018$Community == "Corane",0,
                ifelse(Only2018$Community == "Mingolene",0,NA))))))))


#Language 
Only2018$Language<-ifelse(dat2018[[28]]==1,"Macua",
                   ifelse(dat2018[28]==2,"Koti",
                   ifelse(dat2018[28]==3,"Portuguese","Other")))


#religion
Only2018$Religion<-ifelse(dat2018[[14]]==1,"Christian",
                   ifelse(dat2018[[14]]==2,"Muslim",
                   ifelse(dat2018[[14]]==6,"Traditional_beliefs",
                   ifelse(dat2018[[14]] == 7,"Athiest","Other"))))

#Knowledge of mangrove restrictions
Only2018$Mang_prohb<-ifelse(dat2018[[207]]==0,NA,
                     ifelse(dat2018[[207]]==2,0,
                     ifelse(dat2018[[207]]==1,1,NA)))


#Did you apply the practices from FFS?
Only2018$Apply_FFS<-ifelse(dat2018[[294]]==0,NA,
                    ifelse(dat2018[[294]]==2,0,
                    ifelse(dat2018[[294]]==1,1,NA)))


#Mangrove replanting
Only2018$Mang_replant<-ifelse(dat2018[[297]]==1,"I_participated",
                       ifelse(dat2018[[297]]==2,"Members_of_household",
                       ifelse(dat2018[[297]]==3,"People_in_community",
                       ifelse(dat2018[[297]] == 4,"No_one_I_know",NA))))

#Selective extraction from Mangroves
Only2018$Mang_select_extr<-ifelse(dat2018[[298]]==1,"I_practice",
                              ifelse(dat2018[[298]]==2,"Members_of_household",
                                     ifelse(dat2018[[298]]==3,"We_cut_mangroves_but_non_selective",
                                            ifelse(dat2018[[298]] == 4,"We_dont cut","Dont_know_what_select_extract_is"))))



write.csv(Only2018, file="Cleaned_2018_only_data.csv")















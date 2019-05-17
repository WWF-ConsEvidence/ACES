#stacked bar for productive vs unproductive assets
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(plyr)
dat_long<-read_csv("~/CARE-WWF/Data/Harmonized_longitudinal_data_4_23_2019.csv")
dat_2018<-read_csv("~/CARE-WWF/Data/Cleaned_2018_only_data.csv")
dat_long<-dat_long%>%subset(Community %in% c(unique(dat_2018$Community)) )

x<-dat_long %>% 
  dplyr::select(Year, TotalAssetValue, Devel_Intervention_gen)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=TotalAssetValue,fill=as.character(Devel_Intervention_gen)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#e9a3c9","#4d9221"),name="Development Intervention",labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Total Asset Value")+
  ggtitle("Change in Total Household Asset Value in Moma & Angoche Districts\n2008 (n=184), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold") ,legend.key.size = unit(2, "cm"))

x<-dat_long %>% 
  dplyr::select(Year, ProductiveAssetValue,Devel_Intervention_gen)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=ProductiveAssetValue,fill=as.character(Devel_Intervention_gen)))+
  geom_boxplot()+theme_few()+
   scale_fill_manual(values=c("#e9a3c9","#4d9221"),name="Development Intervention",labels=c("No","Yes"))+
  xlab("Year")+
  ylim(0,25000)+
  ylab("Productive Asset Value")+
  ggtitle("Change in Productive Household Asset Value in Moma & Angoche Districts\n2008 (n=184), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold") ,legend.key.size = unit(2, "cm"))

x<-dat_long %>% 
  dplyr::select(Year, NonProductiveAssetValue, Devel_Intervention_gen)
x<-na.omit(x)
ggplot(data=x, aes(x=as.character(Year), y=NonProductiveAssetValue,fill=as.character(Devel_Intervention_gen)))+
  geom_boxplot()+theme_few()+
  scale_fill_manual(values=c("#e9a3c9","#4d9221"),name="Development Intervention",labels=c("No","Yes"))+
  xlab("Year")+
  ylab("Non-Productive Asset Value")+
  ggtitle("Change in Non-Productive Household Asset Value in Moma & Angoche Districts\n2008 (n=184), 2014 (n=136), 2018 (n=469)")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold") ,legend.key.size = unit(2, "cm"))

##split productive/non-productive bars no whiskers or outliers
x<-dat_long%>%dplyr::select(Year, NonProductiveAssetValue, ProductiveAssetValue)
x_l<-tidyr::gather(x, Type, Value, NonProductiveAssetValue:ProductiveAssetValue, factor_key=F)
x_l$Type<-revalue(x_l$Type,c("NonProductiveAssetValue"="NonProductive","ProductiveAssetValue"="Productive"))



#plot using these
p<-ggplot(x_l, aes(x = as.character(Year),y = Value, fill = Type))
p+geom_boxplot(outlier.shape = NA, coef = 0,alpha=0.7)+
  scale_y_continuous(name = "Value (MZN)",
                     breaks = seq(0, 8000, 2000),
                     limits=c(0, 8000)) +
  scale_x_discrete(name = "Year", breaks=c("2008","2014","2018")) +
  ggtitle("Asset Value by Year") +
  theme_few() +
  theme(plot.title = element_text(size = 30, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", color="black"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,color="black"),axis.text.y=element_text(color="black")) +
  scale_fill_brewer(palette = "Accent")+
  theme(text=element_text(size=30),
        plot.title = element_text(lineheight=.8, face="bold", size=25, color="black") ,
        legend.key.size = unit(1.5, "cm"))
  
#dont split prod and non prod
x<-dat_long%>%dplyr::select(Year, TotalAssetValue)
p<-ggplot(x, aes(x = as.character(Year),y =TotalAssetValue))
p+geom_boxplot(outlier.shape = NA, coef = 0,alpha=0.7, fill="skyblue")+
  scale_y_continuous(name = "Value (MZN)",
                     breaks = seq(0, 8000, 2000),
                     limits=c(0, 8000)) +
  scale_x_discrete(name = "Year", breaks=c("2008","2014","2018")) +
  ggtitle("Total Asset Value by Year") +
  theme_few() +
  theme(plot.title = element_text(size = 30, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", color="black"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,color="black"), axis.text.y=element_text(color="black")) +
  theme(text=element_text(size=30),
        plot.title = element_text(lineheight=.8, face="bold", size=25, color="black") ,
        legend.key.size = unit(1.5, "cm"))
#points and sd
pd <- position_dodge(0.1)
x_ls<-summarySE(na.omit(x_l), measurevar="Value",groupvars=c("Type","Year"))
ggplot(x_ls, aes(x=as.character(Year), y=Value,  group=Type, color=Type)) + 
  geom_errorbar(aes(ymin=Value-sd(Value), ymax=Value+sd(Value)), colour="black", width=0, position=pd, size=1.5) +
  geom_point(position=pd, size=10)+
  geom_line(position=pd, size=3) +
  scale_color_manual(name="Asset Type",values=c("#e7298a","#8da0cb"))+
  scale_x_discrete(name = "Year")+
  scale_y_continuous(name = "Cumulative Asset Value (MZN)")+
  ggtitle("Yearly Change in Mean Household Asset Value") +
  theme_few() +
  theme(plot.title = element_text(size = 30, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", color="black"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,color="black"), axis.text.y=element_text(color="black")) +
  theme(text=element_text(size=30),
        plot.title = element_text(lineheight=.8, face="bold", size=25, color="black") ,
        legend.key.size = unit(1.5, "cm"))
  

#2018 data descriptives --dataset it lives in
#income source --long data
unique(dat_long[dat_long$Year==2018,]$Income_source)
x<-dat_long[dat_long$Year==2018,]
x<-x%>%select(Income_source)
dat_IS<-x%>%dplyr::group_by(Income_source)%>% dplyr::summarise(perc=n()/nrow(x))
dat_IS$Income_source<-revalue(dat_IS$Income_source,c("Cash_crops"="Cash crops","Firewood_charcoal"= "Firewood/Charcoal",
                                                     "Fish_sale_external"="External fish sale","Fish_sale_local"="Local fish sale",
                                                     "Formal_work"="Formal work","Informal_formal_business"="Business","Informal_work"="Informal work",
                                                     "Subsist_crops"="Subsistence crops"))
#food source  --long data
unique(dat_long[dat_long$Year==2018,]$Food_source)
x<-dat_long[dat_long$Year==2018,]
x<-x%>%select(Food_source)
dat_FS<-x%>%dplyr::group_by(Food_source)%>% dplyr::summarise(perc=n()/nrow(x))
dat_FS$Food_source<-revalue(dat_FS$Food_source,c("Cash_crops"="Cash crops","Fish_sale_local"="Local fish sale",
                                                   "Informal_work"="Informal work",
                                                     "Subsist_crops"="Subsistence crops",
                                                 "Artisan" ="Craftwork"))
#gender --both
unique(dat_long[dat_long$Year==2018,]$Gender)
x<-dat_long[dat_long$Year==2018,]
x<-x%>%select(Gender)
dat_G<-x%>%dplyr::group_by(Gender)%>% dplyr::summarise(perc=n()/nrow(x))
#Portugese literacy -- 2018
unique(dat_2018$Literacy)
x<-dat_2018
x<-x%>%select(Literacy)
x<-na.omit(x)
dat_PL<-x%>%dplyr::group_by(Literacy)%>% dplyr::summarise(perc=n()/nrow(x))
#language spoken at home -- 2018
unique(dat_2018$Language)
x<-dat_2018
x<-x%>%select(Language)
x<-na.omit(x)
dat_L<-x%>%dplyr::group_by(Language)%>% dplyr::summarise(perc=n()/nrow(x))
##educational attainment --long
unique(dat_long[dat_long$Year==2018,]$Education_HH)
x<-dat_long[dat_long$Year==2018,]
x<-x%>%select(Education_HH)
x<-na.omit(x)
dat_EA<-x%>%dplyr::group_by(Education_HH)%>% dplyr::summarise(perc=n()/nrow(x))
dat_EA$Education_HH<-revalue(dat_EA$Education_HH,c("Complete_primary"="Primary","Complete_secondary_or_higher"="Secondary or higher",
                                                   "Incomplete_primary"="Incomplete primary","Incomplete_secondary"="Incomplete secondary"))
#Religion --2018
unique(dat_2018$Religion)
x<-dat_2018
x<-x%>%select(Religion)
x<-na.omit(x)
dat_R<-x%>%dplyr::group_by(Religion)%>% dplyr::summarise(perc=n()/nrow(x))
dat_R$Religion<-revalue(dat_R$Religion,c("Traditional_beliefs"="Traditional beliefs"))




#2014 - 2018 changes in mangrove use 

x<-dat_long[dat_long$Year==2014,]
x<-select(x,Mang_USE_YN)
x<-x%>%dplyr::group_by(Mang_USE_YN)%>% dplyr::summarise(perc=n()/nrow(x)*100)
bp<- ggplot(x, aes(x="", y=perc, fill=Mang_USE_YN))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)

#creat a blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie + scale_fill_brewer(palette="Accent", name="mang_use", labels=c("No","Yes")) + blank_theme +
  
  theme(axis.text.x=element_blank())


#types of use 2014
x<-dat_long[dat_long$Year==2014,]

##
Firewood<-sum(as.numeric(x$Mang_USE_Firewood))
Crab_Snail<-sum(as.numeric(x$Mang_USE_CrabSnail))
Home_Construction<-sum(as.numeric(x$Mang_USE_Home))
Fish_Production<-sum(as.numeric(x$Mang_USE_FishProd))
Medicine<-sum(as.numeric(x$Mang_USE_medicine))
Honey<-sum(as.numeric(x$Mang_USE_Honey))
Fish_Trap<-sum(as.numeric(x$Mang_USE_FishTrap))
Other<-sum(as.numeric(x$Mang_USE_Other))

x<-as.data.frame(rbind(Firewood,Crab_Snail, Home_Construction, Fish_Production,Medicine,Honey,Fish_Trap,Other))

x<-setDT(x, keep.rownames = TRUE)[]
names(x)[1:2]<-c("Use","Num")
labs<-paste(format((x$Num/sum(x$Num))*100,digits=2), "%", sep="")
x$labs<-labs
x<-x[x$Num!=0,]
#plot
df <- x %>% 
  mutate(end = 2 * pi * cumsum(Num)/sum(Num),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
collss<-c("Firewood"="#7fc97f","Crab_Snail"="#beaed4","Home_Construction"="#fdc086", "Fish_Production"="#ffff99","Medicine"="#386cb0","Honey"="#f0027f")
ggplot(df) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = Use)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = labs,
                hjust = hjust, vjust = vjust), size=6) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.4),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)+theme_void()+ 
  scale_fill_manual(values =collss, name="Mangrove Use") + blank_theme +
  theme(axis.text.x=element_blank(),text=element_text(size=30),plot.title = element_text(lineheight=.8, face="bold", size=25) ,legend.key.size = unit(1.5, "cm"))+
  ggtitle("Mangrove Use in 2014")

  

#types of use 2018
x<-dat_long[dat_long$Year==2018,]
#or both years
x<-dat_long[dat_long$Year==2014|dat_long$Year==2018,]
##
Firewood<-sum(na.omit(as.numeric(x$Mang_USE_Firewood)))
Crab_Snail<-sum(na.omit(as.numeric(x$Mang_USE_CrabSnail)))
Home_Construction<-sum(na.omit(as.numeric(x$Mang_USE_Home)))
Fish_Production<-sum(na.omit(as.numeric(x$Mang_USE_FishProd)))
Medicine<-sum(na.omit(as.numeric(x$Mang_USE_medicine)))
Honey<-sum(na.omit(as.numeric(x$Mang_USE_Honey)))
Fish_Trap<-sum(na.omit(as.numeric(x$Mang_USE_FishTrap)))
Other<-sum(na.omit(as.numeric(x$Mang_USE_Other)))

x<-as.data.frame(rbind(Firewood,Crab_Snail, Home_Construction, Fish_Production,Medicine,Honey,Fish_Trap,Other))

x<-setDT(x, keep.rownames = TRUE)[]
names(x)[1:2]<-c("Use","Num")
labs<-paste(format((x$Num/sum(x$Num))*100,digits=0), "%", sep="")
x$labs<-labs
#plot
df <- x %>% 
  mutate(end = 2 * pi * cumsum(Num)/sum(Num),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
collss<-c("Firewood"="#7fc97f","Crab_Snail"="#beaed4","Home_Construction"="#fdc086", "Fish_Production"="#ffff99",
          "Medicine"="#386cb0","Honey"="#f0027f","Fish_Trap"="#bf5b17","Other"="#666666")
ggplot(df) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = Use)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = labs,
                hjust = hjust, vjust = vjust), size=6) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.4),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)+theme_void()+ 
  scale_fill_manual(values =collss, name="Mangrove Use") + blank_theme +
  theme(axis.text.x=element_blank(),text=element_text(size=30),plot.title = element_text(lineheight=.8, face="bold", size=25) ,legend.key.size = unit(1.5, "cm"))+
  ggtitle("Mangrove Use in 2014 & 2018")



#use Yn 2018
x<-dat_long[dat_long$Year==2018,]
x<-select(x,Mang_USE_YN)
x<-x%>%group_by(Mang_USE_YN)%>% summarise(perc=n()/nrow(x)*100)
bp<- ggplot(x, aes(x="", y=perc, fill=Mang_USE_YN))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie + scale_fill_brewer(palette="Accent", name="mang_use", labels=c("No","Yes")) + blank_theme +
  
  theme(axis.text.x=element_blank())

#changes in livlihoods

#2014 first
x<-dat_long[dat_long$Year==2014,]
x<-x%>%select(Income_source)%>%na.omit()%>%group_by(Income_source)%>%count()
labs<-paste(format((x$n/sum(x$n))*100,digits=1), "%", sep="")
x$labs<-labs

#Plot
x<-as.data.table(x)
df <- x %>% 
  mutate(end = 2 * pi * cumsum(n)/sum(n),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
collss<-c("Cash_crops"="#882E72","Firewood_charcoal"="#B178A6","Fish_sale_external"="#D6C1DE", "Fish_sale_local"="#1965B0",
          "Formal_work"="#5289C7","Fruit"="#7BAFDE","Informal_formal_business"="#4EB265","Informal_work"="#90C987","Monthly_pension"="#CAE0AB",
          "Other"="#F7EE55","Remittances"="#F6C141",
          "Sell_drinks"="#F1932D", "Subsist_crops"="#E8601C","Transportation"="#DC050C")
ggplot(df) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = Income_source)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = labs,
                hjust = hjust, vjust = vjust), size=6) +
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-2, 2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)+theme_void()+ 
  scale_fill_manual(values =collss, name="Income Source") + blank_theme +
  theme(axis.text.x=element_blank(),text=element_text(size=30),plot.title = element_text(lineheight=.8, face="bold", size=25) ,legend.key.size = unit(1.5, "cm"))+
  ggtitle("Primary Income Source in 2014")



#Make Horizontal Stacked bars for Religion, Education, Language, literacy, gender, food source, income source
dat_G
dat_PL
dat_IS
dat_FS
dat_L
dat_EA
dat_R

#Gender
dat_G%<>%mutate(cat="Category")
gp<-ggplot(dat_G, aes( x=cat,y=perc, fill=Gender))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width = 0.1)+coord_flip()+
  geom_text(aes(label=paste0(format(perc*100, digits=1),"%"," ",Gender)),position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Gender of Household Head")+
  scale_fill_manual(values=c("Female"="#b2df8a", "Male"="#a6cee3") )+
  theme_bw()+theme(legend.position="none",panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_text(size=20, color="black"),
                   axis.ticks = element_blank(),plot.title = element_text(lineheight=.2, face="bold",hjust = 0.5, vjust=-150, size=30),
                   text=element_text(size=20),panel.grid.minor = element_blank(), axis.text.x=element_text(vjust=155),
                   axis.title.x = element_text(vjust=145))

#portugese literacy
dat_PL%<>%mutate(cat="Category")
dat_PL$Literacy<-ifelse(dat_PL$Literacy==TRUE, "Literate", "Illiterate")
plp<-ggplot(dat_PL, aes( x=cat,y=perc, fill=Literacy))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width = 0.1)+coord_flip()+
  geom_text(aes(label=paste0(format(perc*100, digits=1),"%"," ",Literacy)),position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Portugese Literacy")+
  scale_fill_manual(values=c("Literate"="#7fc97f", "Illiterate"="#fdc086") )+
  theme_bw()+theme(legend.position="none",panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_text(size=20, color="black"),
                   axis.ticks = element_blank(),plot.title = element_text(lineheight=.2, face="bold",hjust = 0.5, vjust=-150, size=30),
                   text=element_text(size=20),panel.grid.minor = element_blank(), axis.text.x=element_text(vjust=155),
                   axis.title.x = element_text(vjust=145))

#Income Source
dat_IS%<>%mutate(cat="Category")
#combine all of the low % categories into "other"
dat_IS[15,2]=(sum(dat_IS[dat_IS$perc<=0.02,]$perc))+dat_IS[dat_IS$Income_source=="Other",]$perc
dat_IS<-dat_IS[dat_IS$perc> 0.02,]
isp<-ggplot(dat_IS, aes( x=cat,y=perc, fill=Income_source))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width = 0.1)+coord_flip()+
  geom_text(aes(label=paste0(format(perc*100, digits=1),"%"," ",
                             ifelse(dat_IS$Income_source=="Subsistence crops"|dat_IS$Income_source=="Other"|dat_IS$Income_source=="Local fish sale",Income_source,""))),
            position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Income Source")+
  scale_fill_brewer(name= "Income source",palette="Set3" )+
  theme_bw()+theme(#legend.position="none",
    panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_text(size=20, color="black"),
                   axis.ticks = element_blank(),plot.title = element_text(lineheight=.2, face="bold",hjust = 0.5, vjust=-150, size=30),
                   text=element_text(size=20),panel.grid.minor = element_blank(), axis.text.x=element_text(vjust=155),
                   axis.title.x = element_text(vjust=145))
#food source
dat_FS%<>%mutate(cat="Category")
#combine all of the low % categories into "other"
dat_FS[15,2]=(sum(dat_FS[dat_FS$perc<=0.02,]$perc))+dat_FS[dat_FS$Food_source=="Other",]$perc
dat_FS<-dat_FS[dat_FS$perc> 0.02,]
fsp<-ggplot(dat_FS, aes( x=cat,y=perc, fill=Food_source))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width = 0.1)+coord_flip()+
  geom_text(aes(label=paste0(format(perc*100, digits=1),"%"," ", ifelse(dat_FS$Food_source=="Subsistence crops"|dat_FS$Food_source=="Cash crops",Food_source,""))),
            position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Food Source")+
  scale_fill_brewer(name="Food source",palette="Set3" )+
  theme_bw()+theme(#legend.position="none",
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.ticks = element_blank(),plot.title = element_text(lineheight=.2, face="bold",hjust = 0.5, vjust=-150, size=30),
    text=element_text(size=20),panel.grid.minor = element_blank(), axis.text.x=element_text(vjust=155),
    axis.title.x = element_text(vjust=145))

#Language
dat_L%<>%mutate(cat="Category")
pl<-ggplot(dat_L, aes( x=cat,y=perc, fill=Language))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width =0.1)+coord_flip()+
  geom_text(aes(label=paste0(ifelse(dat_L$Language=="Macua"|dat_L$Language=="Koti",format(perc*100, digits=1),""),
                             ifelse(dat_L$Language=="Macua"|dat_L$Language=="Koti","%","")," ",ifelse(dat_L$Language=="Macua"|dat_L$Language=="Koti",Language,""))),
            position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Language")+
  scale_fill_manual(values = c("#f1b6da","#b8e186","#d01c8b","#4dac26") )+
  theme_bw()+theme(#legend.position="none",
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_text(size=20, color="black"),
                   axis.ticks = element_blank(),plot.title = element_text(lineheight=.2, face="bold",hjust = 0.5, vjust=-150, size=30),
                   text=element_text(size=20),panel.grid.minor = element_blank(), axis.text.x=element_text(vjust=155),
                   axis.title.x = element_text(vjust=145))

#education
dat_EA%<>%mutate(cat="Category")
pea<-ggplot(dat_EA, aes( x=cat,y=perc, fill=factor(Education_HH,levels=c("Secondary or higher", "Incomplete secondary","Primary","Incomplete primary","None","Illiterate"))))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width = 0.1)+coord_flip()+
  geom_text(aes(label=paste0(ifelse(dat_EA$Education_HH=="None"|dat_EA$Education_HH=="Incomplete primary"|dat_EA$Education_HH=="Incomplete secondary"|
                                      dat_EA$Education_HH=="Primary",format(perc*100, digits=1),""),
                             ifelse(dat_EA$Education_HH=="None"|dat_EA$Education_HH=="Incomplete primary"|
                                      dat_EA$Education_HH=="Incomplete secondary"|dat_EA$Education_HH=="Primary","%","")," ",
                             ifelse(dat_EA$Education_HH=="None"|dat_EA$Education_HH=="Incomplete primary"| dat_EA$Education_HH=="Primary",Education_HH,""))),
            position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Education of Household Head")+
  scale_fill_manual(name="Education",values = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f") )+
  theme_bw()+theme(#legend.position="none",
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.ticks = element_blank(),plot.title = element_text(lineheight=.2, face="bold",hjust = 0.5, vjust=-150, size=30),
    text=element_text(size=20),panel.grid.minor = element_blank(), axis.text.x=element_text(vjust=155),
    axis.title.x = element_text(vjust=145))

#Religion
dat_R%<>%mutate(cat="Category")
pr<-ggplot(dat_R, aes( x=cat,y=perc, fill=Religion))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width = 0.1)+coord_flip()+
  geom_text(aes(label=paste0(ifelse(dat_R$Religion=="Christian"|dat_R$Religion=="Muslim",format(perc*100, digits=1),""),
                             ifelse(dat_R$Religion=="Christian"|dat_R$Religion=="Muslim","%","")," ",
                            ifelse(dat_R$Religion=="Christian"|dat_R$Religion=="Muslim",Religion,""))),
            position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Religion")+
  scale_fill_manual(values = c("#fdb462","#80b1d3","#fb8072","#b3de69","#bebada","#8dd3c7") )+
  theme_bw()+theme(#legend.position="none",
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.ticks = element_blank(),plot.title = element_text(lineheight=.2, face="bold",hjust = 0.5, vjust=-150, size=30),
    text=element_text(size=20),panel.grid.minor = element_blank(), axis.text.x=element_text(vjust=155),
    axis.title.x = element_text(vjust=145))

#combine plots
library(gtable)
g1<-ggplotGrob(gp)
g2<-ggplotGrob(plp)
g3<-ggplotGrob(isp)
g4<-ggplotGrob(fsp)
g5<-ggplotGrob(pl)
g6<-ggplotGrob(pea)
g7<-ggplotGrob(pr)
g <- rbind(g1,g2, g3,g4,g5,g6,g7,size = "first")
x<-grid::grid.draw(rbind(ggplotGrob(gp), ggplotGrob(plp), size = "last"))
y<-grid::grid.draw(rbind(ggplotGrob(isp), ggplotGrob(fsp), size = "last"))

grid::grid.draw(rbind(x,y,size = "last"))
gridExtra::grid.arrange(gp,plp,isp,fsp,pl,pea,pr,nrow=7)

ggpubr::ggarrange(gp,plp,isp ,
          ncol = 1, nrow = 2)




#Look at dominant language predictor
#here are the dominant languages for each community
#1. Nauluco - Macua

#2. Namiepe - Macua

#3. PuliEzica - Koti

#4. Namame - Macua and second most widely spoken language is Koti

#5. Macogone - Macua and second most widely spoken language is Koti

#6. Mingolene - Macua [Locally called by Marovone, for very local characteristics]

#7. Corane / M'pive Beach - Macua [Locally called by Marovone, for very local characteristics]

#8. Manene - Macua [Locally called by Marovone, for very local characteristics]


unique(dat_2018$Community)
unique(dat_2018$Language)
dat_2018$Dom_lang<-as.logical(ifelse(dat_2018$Community == "Namiepe" & dat_2018$Language == "Macua",TRUE,
                              ifelse(dat_2018$Community == "Nauluco" & dat_2018$Language == "Macua",TRUE,
                              ifelse(dat_2018$Community == "Pulizica" & dat_2018$Language == "Koti",TRUE,
                              ifelse(dat_2018$Community == "Namame" & dat_2018$Language == "Macua",TRUE,
                              ifelse(dat_2018$Community == "Macogone" & dat_2018$Language == "Macua",TRUE,
                              ifelse(dat_2018$Community == "Mingolene" & dat_2018$Language == "Macua",TRUE,
                              ifelse(dat_2018$Community == "Corane" & dat_2018$Language == "Macua",TRUE,
                              ifelse(dat_2018$Community == "Manene" & dat_2018$Language == "Macua",TRUE,FALSE)))))))))
                              
                              

dat_2018$WDD<-dat_long[dat_long$Year==2018,]$Weighted_Dietary_Diversity                           
dat_2018$TAV<-dat_long[dat_long$Year==2018,]$TotalAssetValue
dat_2018$PAV<-dat_long[dat_long$Year==2018,]$ProductiveAssetValue

fit<-lmer(WDD~Gender+(Gender|Dom_lang),data=dat_2018)
summary(fit)

library(scatterplot3d)
s3d <- scatterplot3d(as.numeric(dat_2018$Dom_lang),as.factor(dat_2018$Gender),dat_2018$WDD, type = "p",highlight.3d = TRUE, pch = 20)

s3d$plane3d(fit,draw_polygon = TRUE, draw_lines = TRUE)


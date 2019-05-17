library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
dat_long<-read_csv("~/CARE-WWF/Data/Harmonized_longitudinal_data_4_23_2019.csv")
dat_2018<-read_csv("~/CARE-WWF/Data/Cleaned_2018_only_data.csv")
dat_long<-dat_long%>%subset(Community %in% c(unique(dat_2018$Community)) )


#2018 data descriptives --dataset it lives in
#income source --long data
unique(dat_long[dat_long$Year==2018,]$Income_source)
x<-dat_long[dat_long$Year==2018,]
x<-x%>%select(Income_source)
dat_IS<-x%>%group_by(Income_source)%>% summarise(perc=n()/nrow(x))
#food source  --long data
unique(dat_long[dat_long$Year==2018,]$Food_source)
x<-dat_long[dat_long$Year==2018,]
x<-x%>%select(Food_source)
dat_FS<-x%>%group_by(Food_source)%>% summarise(perc=n()/nrow(x))
#gender --both
unique(dat_long[dat_long$Year==2018,]$Gender)
x<-dat_long[dat_long$Year==2018,]
x<-x%>%select(Gender)
dat_G<-x%>%group_by(Gender)%>% summarise(perc=n()/nrow(x))
#Portugese literacy -- 2018
unique(dat_2018$Literacy)
x<-dat_2018
x<-x%>%select(Literacy)
x<-na.omit(x)
dat_PL<-x%>%group_by(Literacy)%>% summarise(perc=n()/nrow(x))
#language spoken at home -- 2018
unique(dat_2018$Language)
x<-dat_2018
x<-x%>%select(Language)
x<-na.omit(x)
dat_L<-x%>%group_by(Language)%>% summarise(perc=n()/nrow(x))
##educational attainment --long
unique(dat_long[dat_long$Year==2018,]$Education_HH)
x<-dat_long[dat_long$Year==2018,]
x<-x%>%select(Education_HH)
x<-na.omit(x)
dat_EA<-x%>%group_by(Education_HH)%>% summarise(perc=n()/nrow(x))
#Religion --2018
unique(dat_2018$Religion)
x<-dat_2018
x<-x%>%select(Religion)
x<-na.omit(x)
dat_R<-x%>%group_by(Religion)%>% summarise(perc=n()/nrow(x))

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
  scale_fill_manual(values=c("Literate"="#b2df8a", "Illiterate"="#33a02c") )+
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
                             ifelse(dat_IS$Income_source=="Subsist_crops"|dat_IS$Income_source=="Other"|dat_IS$Income_source=="Fish_sale_local",Income_source,""))),
            position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Income Source")+
  scale_fill_brewer(palette="Set3" )+
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
  geom_text(aes(label=paste0(format(perc*100, digits=1),"%"," ", ifelse(dat_FS$Food_source=="Subsist_crops"|dat_FS$Food_source=="Cash_crops",Food_source,""))),
            position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Food Source")+
  scale_fill_brewer(palette="Set3" )+
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
pea<-ggplot(dat_EA, aes( x=cat,y=perc, fill=factor(Education_HH,levels=c("Complete_secondary_or_higher", "Incomplete_secondary","Complete_primary","Incomplete_primary","None","Illiterate"))))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width = 0.1)+coord_flip()+
  geom_text(aes(label=paste0(ifelse(dat_EA$Education_HH=="None"|dat_EA$Education_HH=="Incomplete_primary"|dat_EA$Education_HH=="Incomplete_secondary"|dat_EA$Education_HH=="Complete_primary",format(perc*100, digits=1),""),
                             ifelse(dat_EA$Education_HH=="None"|dat_EA$Education_HH=="Incomplete_primary"|dat_EA$Education_HH=="Incomplete_secondary"|dat_EA$Education_HH=="Complete_primary","%","")," ",
                             ifelse(dat_EA$Education_HH=="None"|dat_EA$Education_HH=="Incomplete_primary",Education_HH,""))),
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

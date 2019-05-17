library(dplyr)
library(readr)
dat2014_liveli <- read_csv("~/CARE-WWF/Data/2014Livelihood_Data.csv")
dat2018 <- read_csv("~/CARE-WWF/Data/2018Data.csv")


dat2014<-dat2014_liveli%>% dplyr::select("2.7","2.8")
dat2018<-dat2018%>%select(36:39)
dat2014$Change<-as.logical(ifelse(dat2014$`2.7` == "n/a",NA,
                                  ifelse(dat2014$`2.7` ==1,TRUE,FALSE)))
dat2018$Change<-as.logical(ifelse(dat2018[1] == 1,TRUE,FALSE))
names(dat2018)[2:4]<-c("Reason_a","Reason_b","Reason_c")
dat2018<-dat2018[-1]
dat2018$Reason_a<-as.character(ifelse(dat2018$Reason_a == 1 | dat2018$Reason_a == 2,"Resource_scarcity_competition",
                               ifelse(dat2018$Reason_a == 3 | dat2018$Reason_a == 4| dat2018$Reason_a == 5| dat2018$Reason_a == 7,"Economic_challenges_opportunity",
                               ifelse(dat2018$Reason_a == 6 | dat2018$Reason_a == 8| dat2018$Reason_a == 9,"Climate_idiosyn_shocks" ,
                               ifelse(dat2018$Reason_a == 11 | dat2018$Reason_a == 13,"Access_inputs_labor",
                               ifelse(dat2018$Reason_a == 14 | dat2018$Reason_a == 15| dat2018$Reason_a == 16| dat2018$Reason_a == 88,"Other",NA  ))))))

dat2018$Reason_b<-as.character(ifelse(dat2018$Reason_b == 1 | dat2018$Reason_b == 2,"Resource_scarcity_competition",
                               ifelse(dat2018$Reason_b == 3 | dat2018$Reason_b == 4| dat2018$Reason_b == 5| dat2018$Reason_b == 7,"Economic_challenges_opportunity",
                               ifelse(dat2018$Reason_b == 6 | dat2018$Reason_b == 8| dat2018$Reason_b == 9,"Climate_idiosyn_shocks" ,
                              ifelse(dat2018$Reason_b == 11 | dat2018$Reason_b == 13,"Access_inputs_labor",
                              ifelse(dat2018$Reason_b == 14 | dat2018$Reason_b == 15| dat2018$Reason_b == 16| dat2018$Reason_b == 88,"Other",NA  ))))))

dat2018$Reason_c<-as.character(ifelse(dat2018$Reason_c == 1 | dat2018$Reason_c == 2,"Resource_scarcity_competition",
                               ifelse(dat2018$Reason_c == 3 | dat2018$Reason_c == 4| dat2018$Reason_c == 5| dat2018$Reason_c == 7,"Economic_challenges_opportunity",
                                ifelse(dat2018$Reason_c == 6 | dat2018$Reason_c == 8| dat2018$Reason_c == 9,"Climate_idiosyn_shocks" ,
                                ifelse(dat2018$Reason_c == 11 | dat2018$Reason_c == 13,"Access_inputs_labor",
                                ifelse(dat2018$Reason_c == 14 | dat2018$Reason_c == 15| dat2018$Reason_c == 16| dat2018$Reason_c == 88,"Other",NA  ))))))

changea<-as.data.frame(dat2018%>%select(Reason_a)%>%group_by(Reason_a)%>%count())
changeb<-as.data.frame(dat2018%>%select(Reason_b)%>%group_by(Reason_b)%>%count())
changec<-as.data.frame(dat2018%>%select(Reason_c)%>%group_by(Reason_c)%>%count())
names(changea)[1]<-"Why"
names(changeb)[1]<-"Why"
names(changec)[1]<-"Why"
reasonall<-rbind(changea,changeb,changec)
reasonall<-as.data.frame(reasonall%>%group_by(Why)%>%summarise(total=sum(n)))
#remove na's
reasonall<-reasonall[-1,]
reasonall<-reasonall%>%mutate(perc= (total/sum(total)))
reasonall$Why<-revalue(reasonall$Why,c("Access_inputs_labor" ="Access to inputs & labor","Climate_idiosyn_shocks"="Climate & idiosyncratic shocks",
                                                   "Economic_challenges_opportunity"="Economic challenges & opportunities",
                                       "Resource_scarcity_competition"="Resource scarcity & competition"))
reasonall%<>%mutate(cat="Category")
rp<-ggplot(reasonall, aes( x=cat,y=perc, fill=Why))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_col( width =0.1)+coord_flip()+
  #geom_text(aes(label=paste0(ifelse(reasonall$Why=="Access to inputs & labor"|reasonall$Why=="Climate & idiosyncratic shocks"|
                                    #  reasonall$Why=="Other"|reasonall$Why=="Economic_challenges_opportunity",format(perc*100, digits=1),""),
                           #"%","")," ",ifelse(reasonall$Why=="Other",Why,""))),
           #position = position_stack(vjust=0.5), size=6,fontface = "bold")+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("% of population")+
  ggtitle("Reason for Change")+
  scale_fill_manual(values = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854") )+
  theme_bw()+theme(#legend.position="none",
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.ticks = element_blank(),plot.title = element_text(lineheight=.2, face="bold",hjust = 0.5, vjust=-150, size=30),
    text=element_text(size=20),panel.grid.minor = element_blank(), axis.text.x=element_text(vjust=155),
    axis.title.x = element_text(vjust=145))

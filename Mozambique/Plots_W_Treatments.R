library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(lme4)
library(rstanarm)
library(ggthemes)
dat_long<-read_csv("~/CARE-WWF/Data/Harmonized_longitudinal_data_3_28_2019.csv")
dat_2018<-read_csv("~/CARE-WWF/Data/Cleaned_2018_only_data.csv")
long_relevant<-dat_long%>%subset(Community %in% c(unique(dat_2018$Community)) )

#comment next line if you want to compare the all communities.
dat_long<-long_relevant

#Key plots:
#1. change in assets over time (development int vs not)
#2. borrowing affect on assets
#3. FFS on food security
#4.Womens economic decision making
#5. Affect of NTZ on household

#1.
x<-long_relevant %>% 
  dplyr::select(Year, Gender,Devel_Intervention_gen, Assets)
  x<-na.omit(x)
  x<-x %>%group_by(Year, Devel_Intervention_gen)%>%
  summarise(Avg_assets=mean(Assets))
  x$Devel_Intervention_gen<-as.logical(x$Devel_Intervention_gen)

  ggplot(data= x, aes(x=as.integer(Year), y=Avg_assets, by=Devel_Intervention_gen))+
    geom_point(aes(color=Devel_Intervention_gen), size=3)+ylim(1,10)+
    geom_smooth(aes(color=Devel_Intervention_gen))
    
#2a lolipop
  y<-long_relevant %>% 
    dplyr::select(Year, Gender,Borrow, Assets)
  y<-na.omit(y)
  y<-y %>%group_by( Borrow, Gender)%>%
    summarise(Avg_assets=mean(Assets))
  y$Borrow<-as.logical(y$Borrow) 
 
  ym<-y[y$Gender=="Male",]
  yf<-y[y$Gender=="Female",]
  ggplot()+
    geom_point(data=ym,aes( x=Borrow, y=Avg_assets),color="#1f78b4", size=25,position = position_nudge(- 0.08))+
    geom_segment(data=ym,aes(y = 2, 
                             x = Borrow, 
                             yend = Avg_assets, 
                             xend = Borrow), 
                 size=2, color="#1f78b4",position = position_nudge(- 0.08))+
    geom_point(data=yf,aes( x=Borrow, y=Avg_assets),color="#b2df8a", size=25,position = position_nudge( 0.08))+
    geom_segment(data=yf,aes(y = 2, 
                             x = Borrow, 
                             yend = Avg_assets, 
                             xend = Borrow), 
                 size=2, color="#b2df8a",position = position_nudge( 0.08))+
    ylim(2,8)
#2b boxplots 
  y<-dat_long %>% 
    dplyr::select(Year, Gender,Borrow, Assets)
  y<-na.omit(y)
  ggplot(data=y, aes(x=as.logical(Borrow), y=as.integer(Assets), color=Gender))+
    geom_boxplot()
#3a.
  ggplot(data=dat_2018, aes(x=as.logical(FFS), y=as.logical(Food_prov)))+
    geom_jitter(aes(color=as.logical(Food_prov)),size=4)+
    scale_color_manual(values=c( "#1f78b4","#b2df8a"))+theme_classic()
  
#3b.
  
  
  z<-dat_2018%>%group_by(FFS, Gender)%>%summarise(perc_prov = (1.00-mean(Food_prov)))
  
  zm<-z[z$Gender=="Male",]
  zf<-z[z$Gender=="Female",]
  ggplot()+
    geom_point(data=zm,aes( x=as.logical(FFS), y=perc_prov),color="#1f78b4", size=40,position = position_nudge(- 0.08))+
    geom_segment(data=zm,aes(y = 0, 
                             x = as.logical(FFS), 
                             yend = perc_prov, 
                             xend = as.logical(FFS)), 
                 size=8, color="#1f78b4",position = position_nudge(- 0.08))+
    geom_point(data=zf,aes( x=as.logical(FFS), y=perc_prov),color="#b2df8a", size=40,position = position_nudge( 0.08))+
    geom_segment(data=zf,aes(y = 0, 
                             x = as.logical(FFS), 
                             yend = perc_prov, 
                             xend = as.logical(FFS)), 
                 size=8, color="#b2df8a",position = position_nudge( 0.08))+
    ylim(0,1)
  
  
#5a
#ntz on days ate seafood
f<-dat_long%>%dplyr::select(Fish_NTZ,Days_seafood, Year)
 f<-na.omit(f) 
 f$Days_seafood<-as.integer(f$Days_seafood)
 f<-f%>%group_by(Fish_NTZ,Year)%>%summarise(avg_days=mean(Days_seafood)) 
 
 fr<-f[f$Fish_NTZ==1,]
 fnr<-f[f$Fish_NTZ==0,]
 ggplot()+
   geom_point(data=fr,aes( x=as.character(Year), y=avg_days),color="#1f78b4", size=40,position = position_nudge(- 0.08))+
   geom_segment(data=fr,aes(y = 2, 
                            x = as.character(Year), 
                            yend = avg_days, 
                            xend = as.character(Year)), 
                size=8, color="#1f78b4",position = position_nudge(- 0.08))+
   geom_point(data=fnr,aes( x=as.character(Year), y=avg_days),color="#b2df8a", size=40,position = position_nudge( 0.08))+
   geom_segment(data=fnr,aes(y = 2, 
                            x =as.character(Year), 
                            yend = avg_days, 
                            xend = as.character(Year)), 
                size=8, color="#b2df8a",position = position_nudge( 0.08))+
   ylim(2,6)
 
 
 #5b perceived effect
 e<-dat_long%>%dplyr::select(Fish_sanct_impct,Year)
 e<-na.omit(e) 
 e<-e%>%group_by(Fish_sanct_impct,Year)%>%summarise(rspnse=n()) 
 e$rspnse_pct<-ifelse(e$Year==2014,(e$rspnse/237)*100,(e$rspnse/138)*100 )
  
 eb<-e[e$Fish_sanct_impct=="benefited",]
 en<-e[e$Fish_sanct_impct=="none",]
 eh<-e[e$Fish_sanct_impct=="harmed",]
 ggplot()+
   geom_point(data=eb,aes( x=as.character(Year), y=rspnse_pct),color="#b2df8a", size=30,position = position_nudge(- 0.2))+
   geom_segment(data=eb,aes(y = 0, 
                            x = as.character(Year), 
                            yend = rspnse_pct, 
                            xend = as.character(Year)), 
                size=8, color="#b2df8a",position = position_nudge(- 0.2))+
   geom_point(data=en,aes( x=as.character(Year), y=rspnse_pct),color="#a6cee3", size=30)+
   geom_segment(data=en,aes(y = 0, 
                             x =as.character(Year), 
                             yend = rspnse_pct, 
                             xend = as.character(Year)), 
                size=8, color="#a6cee3")+
   geom_point(data=eh,aes( x=as.character(Year), y=rspnse_pct),color="#1f78b4", size=30,position = position_nudge( 0.2))+
   geom_segment(data=eh,aes(y = 0, 
                            x =as.character(Year), 
                            yend = rspnse_pct, 
                            xend = as.character(Year)), 
                size=8, color="#1f78b4",position = position_nudge(0.2))+
   ylim(0,100)
 
 #5c fish specific indicators
 q<-dat_2018%>%dplyr::select(Fish_change_size, Fish_NTZ)
 q<-na.omit(q) 
 q<-q%>%group_by(Fish_change_size,Fish_NTZ)%>%summarise(rspnse=n()) 
 q$rspnse_pct<-ifelse(q$Fish_NTZ==1,(q$rspnse/85)*100,(q$rspnse/72)*100 )
 
 qm<-q[q$Fish_sanct_impct=="More",]
 qs<-q[q$Fish_sanct_impct=="Same",]
 qm<-q[q$Fish_sanct_impct=="Less",]
 
 
 
 #Boxplots showing same info
 
 #affect of farmer field school.....not sure if this is better than the lolipops, it's a bit contrived 
 #because there are only 3 communities that didn't have ffs
 s<-dat_2018%>%group_by(FFS, Gender, Community)%>%summarise(perc_prov = (1.00-mean(Food_prov)))
 
 ggplot(data=s, aes(x=as.logical(FFS), y=perc_prov, fill=Gender))+geom_boxplot()
 ggplot(data=dat_2018, aes(x=as.logical(FFS), y=Months_adequate_fp, fill=Gender))+geom_boxplot()
 
 #days seafood
 f<-dat_long%>%dplyr::select(Fish_NTZ,Days_seafood, Year, Community)
 f<-na.omit(f) 
 f$Days_seafood<-as.integer(f$Days_seafood)
 f<-f%>%group_by(Fish_NTZ, Year, Community)%>%summarise(mean_days = (mean(Days_seafood)))
 ggplot(data=f, aes(x=as.character(Year), y=mean_days, fill=as.logical(Fish_NTZ)))+geom_boxplot()
 
 
 #borrow from where? pie chart (all years)
 borrowa<-as.data.frame(dat_long%>%select(Credit_from_A)%>%group_by(Credit_from_A)%>%count())
 borrowb<-as.data.frame(dat_long%>%select(Credit_from_B)%>%group_by(Credit_from_B)%>%count())
 borrowc<-as.data.frame(dat_long%>%select(Credit_from_C)%>%group_by(Credit_from_C)%>%count())
 names(borrowa)[1]<-"From"
 names(borrowb)[1]<-"From"
 names(borrowc)[1]<-"From"
 borrowall<-rbind(borrowa,borrowb,borrowc)
 borrowall<-as.data.frame(borrowall%>%group_by(From)%>%summarise(total=sum(n)))
 #remove na's
 borrowall<-borrowall[-6,]
 borrowall<-borrowall%>%mutate(perc= (total/sum(total))*100)
 
 bp<- ggplot(borrowall, aes(x="", y=total, fill=From))+
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
 
 pie + scale_fill_brewer("Blues") + blank_theme +
   theme(axis.text.x=element_blank())
 
 #borrow from where? pie chart (2008)
 borrowa<-as.data.frame(dat_long%>% filter(Year == 2008)%>%select(Credit_from_A)%>%group_by(Credit_from_A)%>%count())
 borrowb<-as.data.frame(dat_long%>% filter(Year == 2008)%>%select(Credit_from_B)%>%group_by(Credit_from_B)%>%count())
 borrowc<-as.data.frame(dat_long%>% filter(Year == 2008)%>%select(Credit_from_C)%>%group_by(Credit_from_C)%>%count())
 names(borrowa)[1]<-"From"
 names(borrowb)[1]<-"From"
 names(borrowc)[1]<-"From"
 borrowall<-rbind(borrowa,borrowb,borrowc)
 borrowall<-as.data.frame(borrowall%>%group_by(From)%>%summarise(total=sum(n)))
 #remove na's
 borrowall<-borrowall[-6,]
 bp<- ggplot(borrowall, aes(x="", y=total, fill=From))+
   geom_bar(width = 1, stat = "identity")
 pie <- bp + coord_polar("y", start=0)
 pie + scale_fill_brewer("Blues") + blank_theme +
   theme(axis.text.x=element_blank())
 
 #borrow from where? pie chart (2018)
 borrowa<-as.data.frame(dat_long%>% filter(Year == 2018)%>%select(Credit_from_A)%>%group_by(Credit_from_A)%>%count())
 borrowb<-as.data.frame(dat_long%>% filter(Year == 2018)%>%select(Credit_from_B)%>%group_by(Credit_from_B)%>%count())
 borrowc<-as.data.frame(dat_long%>% filter(Year == 2018)%>%select(Credit_from_C)%>%group_by(Credit_from_C)%>%count())
 names(borrowa)[1]<-"From"
 names(borrowb)[1]<-"From"
 names(borrowc)[1]<-"From"
 borrowall<-rbind(borrowa,borrowb,borrowc)
 borrowall<-as.data.frame(borrowall%>%group_by(From)%>%summarise(total=sum(n)))
 #remove na's
 borrowall<-borrowall[-6,]
 bp<- ggplot(borrowall, aes(x="", y=total, fill=From))+
   geom_bar(width = 1, stat = "identity")
 pie <- bp + coord_polar("y", start=0)
 pie + scale_fill_brewer("Blues") + blank_theme +
   theme(axis.text.x=element_blank())

#Informal credit borrowing spent where?
 inform<-dat_long%>% filter(Credit_from_A =="Informal_credit"|
                              Credit_from_B =="Informal_credit"|
                              Credit_from_C =="Informal_credit" )%>% 
   select(Credit_reason)%>%group_by(Credit_reason)%>%count()
  inform<-na.omit(inform)
  
  bp<- ggplot(inform, aes(x="", y=n, fill=Credit_reason))+
    geom_bar(width = 1, stat = "identity")
  pie <- bp + coord_polar("y", start=0)
  pie + scale_fill_brewer(palette="Accent") + blank_theme +
    theme(axis.text.x=element_blank())
  
 #household asset & diet boxplots by community association
 x<-dat_long%>%filter(Association_Alliance==0 | Association_Alliance==1)
 bp<-ggplot(x, aes(x=as.character(Year), y=Food_Groups, fill=as.logical(Association_Alliance))) +
   geom_boxplot()
 
 bp+scale_fill_manual(values=c("#1f78b4","#b2df8a"))+
   theme_classic()
   theme(legend.position="none")
   
   
   #now assets
   x<-dat_long%>%filter(Association_Alliance==0 | Association_Alliance==1)
   bp<-ggplot(x, aes(x=as.character(Year), y=Assets, fill=as.logical(Association_Alliance))) +
     geom_boxplot()+theme_few()+scale_fill_manual(values=c("#1f78b4","#b2df8a"))
    
   
  
   #try something different
   x<-x%>%filter(Gender=="Male" | Gender=="Female")
   ggplot(x, aes(x=as.character(Year), y=Food_Groups)) +
     geom_boxplot(aes(fill=Gender))+
     theme_bw()+
     theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
     #stat_summary(fun.y=median, geom="smooth", aes(group=Gender, color=Gender),lwd=2, linetype="dashed")+
     facet_grid(. ~ as.logical(Association_Alliance))+scale_fill_manual(values=c("#1f78b4","#b2df8a"))+
     scale_color_manual(values=c("#1f78b4","#b2df8a"))+
     theme_few()

   #Womens economic decision making #these next two plots need to go into the folder
   x<-na.omit(dat_2018%>%select(WEDM,Association_Alliance))
   xy<-x[x$Association_Alliance==1,]
   xn<-x[x$Association_Alliance==0,]
   
   ggplot() + 
     geom_bar(data=xy,aes(x=WEDM, fill=as.logical(Association_Alliance),
                          y = (..count..)/sum(..count..)),position=position_nudge(-0.15),width = 0.3)+
     geom_bar(data=xn,aes(x=WEDM, fill=as.logical(Association_Alliance),
                          y = (..count..)/sum(..count..)),position=position_nudge(0.15),width=0.3)+
     scale_y_continuous(labels=scales::percent)+
     theme_few()+scale_fill_manual(values=c("#e9a3c9","#4d9221"),labels=c("No (n=160)","Yes (n=65)"),
                                   name=("Member of Community Association\nLikely Reached by Alliance")) +
     xlab("Change in Women's Economic Decision Making From 2013")+
     ylab("Percent of Respondants")+
     ggtitle("2018 Change in Women's Economic Decision Making from 2013\nCorrelation w/ Membership in a Community Group Likely Reached by Alliance")+
     theme(text=element_text(size=20),#legend.position="none",
           axis.text = element_text(size=20, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
           legend.key.size = unit(2, "cm"))
  
   
  #bar plot, Pretty good, complete. Share in folder after flight 
x<-na.omit(dat_2018%>%select(WEDM,Devel_Intervention_gen))
  xy<-x[x$Devel_Intervention_gen==1,]
  xn<-x[x$Devel_Intervention_gen==0,]
   
   ggplot() + 
     geom_bar(data=xy,aes(x=WEDM, fill=as.logical(Devel_Intervention_gen),
                          y = (..count..)/sum(..count..)),position=position_nudge(-0.15),width = 0.3)+
     geom_bar(data=xn,aes(x=WEDM, fill=as.logical(Devel_Intervention_gen),
                          y = (..count..)/sum(..count..)),position=position_nudge(0.15),width=0.3)+
            scale_y_continuous(labels=scales::percent)+
     theme_few()+scale_fill_manual(values=c("#e9a3c9","#4d9221"),labels=c("No (n=11)","Yes (n=214)"),
                                   name=("Community Development\nIntervention")) +
     xlab("Change in Women's Economic Decision Making From 2013")+
     ylab("Percent of Respondants")+
     ggtitle("2018 Change in Women's Economic Decision\nMaking from 2013 Correlation w/ Development Interventions")+
     theme(text=element_text(size=20),#legend.position="none",
           axis.text = element_text(size=20, color="black"),plot.title = element_text(lineheight=.8, face="bold"),
           legend.key.size = unit(2, "cm"))
   
  
   #Mangrove use pie chart
   
   
   
   
   
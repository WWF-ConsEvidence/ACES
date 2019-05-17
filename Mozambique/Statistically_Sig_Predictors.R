library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(lme4)
library(MASS)
library(rstanarm)
dat_long<-read_csv("~/CARE-WWF/Data/Harmonized_longitudinal_data_3_28_2019.csv")
dat_2018<-read_csv("~/CARE-WWF/Data/Cleaned_2018_only_data.csv")
long_relevant<-dat_long%>%subset(Community %in% c(unique(dat_2018$Community)) )
dat_2018<-cbind(dat_2018, dat_long[dat_long$Year==2018,105])
dat_2018<-cbind(dat_2018, dat_long[dat_long$Year==2018,19])
dat_2018$WEDM<-factor(dat_2018$WEDM, levels=c("Decreased","Same", "Increased"))
dat_2018<-cbind(dat_2018, dat_long[dat_long$Year==2018,100])
dat_2018$Fish_change_quantity<-factor(dat_2018$Fish_change_quantity, levels=c("Less","Same", "More"))
dat_2018$Fish_change_species<-factor(dat_2018$Fish_change_species, levels=c("Less","Same", "More"))
dat_2018$Fish_change_size<-factor(dat_2018$Fish_change_size, levels=c("Less","Same", "More"))

#comment next line if you want to compare the all communities.
dat_long<-long_relevant


#Things we want to predict
#1. Adequate food provisioning y/n
#2. Months of adequate HHFP
#3. Dietary diversity (num food groups)
#4. Number of assets
#5. Change in WEDM
#6. Size/quantity/num species of fish


#1a Predictors at the community level

#development intervention
fit<-glm(Food_prov~as.logical(Devel_Intervention_gen),
           family="binomial",data=dat_2018)

summary(fit)

#conservation intervention
fit<-glm(Food_prov~as.logical(Cons_Intervention_gen),
         family="binomial",data=dat_2018)

summary(fit)

#farmer field school*
fit<-glm(Food_prov~as.logical(FFS),
         family="binomial",data=dat_2018)

summary(fit)

#disaggregate by gender (random intercept) **
fit<-glmer(Food_prov~as.logical(FFS)+ (1|Gender),
         family="binomial",data=dat_2018, control = glmerControl(optimizer ="Nelder_Mead"))

summary(fit)

#disaggregate by gender (random intercept & slope) 
fit<-glmer(Food_prov~as.logical(FFS)+ (FFS|Gender),
           family="binomial",data=dat_2018, control = glmerControl(optimizer ="Nelder_Mead"))

summary(fit)

#VSLA
fit<-glm(Food_prov~as.logical(VSLA),
         family="binomial",data=dat_2018)

summary(fit)

#NTZ
fit<-glm(Food_prov~as.logical(Fish_NTZ),
         family="binomial",data=dat_2018)

summary(fit)

#Bowwowing*
fit<-glm(Food_prov~as.logical(Borrow),
         family="binomial",data=dat_2018)

summary(fit)
#disaggregate by gender (random intercept) **
fit<-glmer(Food_prov~as.logical(Borrow)+(1|Gender),
         family="binomial",data=dat_2018)

summary(fit)

#disaggregate by gender (random intercept & slope) 
fit<-glmer(Food_prov~as.logical(Borrow)+(as.logical(Borrow)|Gender),
           family="binomial",data=dat_2018)

summary(fit)
#1b Predictors at the household level

#FFS Participation
fit<-glm(Food_prov~FFS_Participation,
         family="binomial",data=dat_2018)

summary(fit)

#Apply FFS
fit<-glm(Food_prov~as.logical(Apply_FFS),
         family="binomial",data=dat_2018)

summary(fit)

#2a Months adequate fp community level

#development intervention
fit<-glm(Months_adequate_fp~as.logical(Devel_Intervention_gen),family="poisson",
         data=dat_2018)

summary(fit)

#cons intervention
fit<-glm(Months_adequate_fp~as.logical(Cons_Intervention_gen),family="poisson",
         data=dat_2018)

summary(fit)
#Borrow
fit<-glm(Months_adequate_fp~as.logical(Borrow),family="poisson",
         data=dat_2018)

summary(fit)

#FFS
fit<-glm(Months_adequate_fp~as.logical(FFS),family="poisson",
         data=dat_2018)

summary(fit)

#VSLA
fit<-glm(Months_adequate_fp~as.logical(VSLA),family="poisson",
         data=dat_2018)

summary(fit)

#fish ntz
fit<-glm(Months_adequate_fp~as.logical(Fish_NTZ),family="poisson",
         data=dat_2018)

summary(fit)

#2b household level predictors
#FFS Participation
fit<-glm(Months_adequate_fp~FFS_Participation,
         family="poisson",data=dat_2018)

summary(fit)

#Apply FFS
fit<-glm(Months_adequate_fp~as.logical(Apply_FFS),
         family="poisson",data=dat_2018)

summary(fit)

#3a Dietary diversity community predictor

#development intervention ~*(.)
fit<-glm(Food_Groups~as.logical(Devel_Intervention_gen),family="poisson",
         data=dat_long)

summary(fit)

#disaggregate by gender (random intercept) ~*(.)
fit<-glmer(Food_Groups~as.logical(Devel_Intervention_gen)+(1|Gender),family="poisson",
         data=dat_long)

summary(fit)

#disaggregate by gender (random intercept & slope) 
fit<-glmer(Food_Groups~as.logical(Devel_Intervention_gen)+
             (as.logical(Devel_Intervention_gen)|Gender),family="poisson",
           data=dat_long)

summary(fit)

#cons intervention * (negative!)
fit<-glm(Food_Groups~as.logical(Cons_Intervention_gen),family="poisson",
         data=dat_long)

summary(fit)
#disaggregate by gender (random intercept) *
fit<-glmer(Food_Groups~as.logical(Cons_Intervention_gen)+(1|Gender),family="poisson",     
         data=dat_long)

summary(fit)
#disaggregate by gender (random intercept & slope) ~*(.)
fit<-glmer(Food_Groups~as.logical(Cons_Intervention_gen)+
             (as.logical(Cons_Intervention_gen)|Gender),family="poisson",
           control = glmerControl(optimizer ="Nelder_Mead"),
           data=dat_long)

summary(fit)

#FFS (*)
fit<-glm(Food_Groups~as.logical(FFS),family="poisson",
           data=dat_long)

summary(fit)
#disaggregate by gender (random intercept) *
fit<-glmer(Food_Groups~as.logical(FFS)+(1|Gender),family="poisson",
         data=dat_long)

summary(fit)

#disaggregate by gender (random intercept & slope)
fit<-glmer(Food_Groups~as.logical(FFS)+(as.logical(FFS)|Gender),family="poisson",
           data=dat_long)

summary(fit)
#VSLA (*)
fit<-glm(Food_Groups~as.logical(VSLA),family="poisson",
         data=dat_long)

summary(fit)

#disaggregate by gender (random intercept)*
fit<-glmer(Food_Groups~as.logical(VSLA)+(1|Gender),family="poisson",
         data=dat_long)

summary(fit)
#disaggregate by gender (random intercept & slope)~*(.)
fit<-glmer(Food_Groups~as.logical(VSLA)+(as.logical(VSLA)|Gender),family="poisson",
           data=dat_long)

summary(fit)
#fish ntz (* negative!)
fit<-glm(Food_Groups~as.logical(Fish_NTZ),family="poisson",
         data=dat_long)

summary(fit)
#disaggregate by gender (random intercept)*
fit<-glmer(Food_Groups~as.logical(Fish_NTZ)+(1|Gender),family="poisson",
         data=dat_long)

summary(fit)

#disaggregate by gender (random intercept & slope)
fit<-glmer(Food_Groups~as.logical(Fish_NTZ)+
             (as.logical(Fish_NTZ)|Gender),family="poisson",
           data=dat_long)

summary(fit)
#num days ate fish (***)
fit<-glm(as.integer(Days_seafood)~as.logical(Fish_NTZ),family="poisson",
         data=dat_long)
summary(fit)

#disaggregate by gender (random intercept)**
fit<-glmer(as.integer(Days_seafood)~as.logical(Fish_NTZ)+(1|Gender),family="poisson",
         data=dat_long)
summary(fit)

#disaggregate by gender (random intercept & slope)*
fit<-glmer(as.integer(Days_seafood)~as.logical(Fish_NTZ)+
             (as.logical(Fish_NTZ)|Gender),family="poisson",
           data=dat_long)
summary(fit)
#3b household level predictors
#FFS Participation
fit<-glm(Food_Groups~FFS_Participation,
         family="poisson",data=dat_long)

summary(fit)

#Apply FFS
fit<-glm(Food_Groups~as.logical(Apply_FFS),
         family="poisson",data=dat_2018)

summary(fit)

#4a number of assets community level predictor (**)
#development intervention
fit<-glm(Assets~as.logical(Devel_Intervention_gen),
         family="poisson",data=dat_long)

summary(fit)
#disaggregate by gender (random intercept)**
fit<-glmer(Assets~as.logical(Devel_Intervention_gen)+(1|Gender),
         family="poisson",data=dat_long)

summary(fit)
#disaggregate by gender (random intercept & slope)
fit<-glmer(Assets~as.logical(Devel_Intervention_gen)+
             (as.logical(Devel_Intervention_gen)|Gender),
           family="poisson",data=dat_long)

summary(fit)
#conservation intervention (~*(.))
fit<-glm(Assets~as.logical(Cons_Intervention_gen),
         family="poisson",data=dat_long)

summary(fit)

#disaggregate by gender (random intercept)
fit<-glmer(Assets~as.logical(Cons_Intervention_gen)+(1|Gender),
         family="poisson",data=dat_long)

summary(fit)

#disaggregate by gender (random intercept & slope)
fit<-glmer(Assets~as.logical(Cons_Intervention_gen)+
             (as.logical(Cons_Intervention_gen)|Gender),
           family="poisson",data=dat_long)

summary(fit)
#VSLA (~*(.))
fit<-glm(Assets~as.logical(VSLA),
         family="poisson",data=dat_long)

summary(fit)

#disaggregate by gender (random intercept)(~*(.))
fit<-glmer(Assets~as.logical(VSLA)+(1|Gender),
         family="poisson",data=dat_long)

summary(fit)

#disaggregate by gender (random intercept & slope)
fit<-glmer(Assets~as.logical(VSLA)+(as.logical(VSLA)|Gender),
           family="poisson",data=dat_long)

summary(fit)
#4b number of assets househld level

#borrow (***)
fit<-glm(Assets~as.logical(Borrow),
         family="poisson",data=dat_long)

summary(fit)

#disaggregate by gender (random intercept)***
fit<-glmer(Assets~as.logical(Borrow)+(1|Gender),
         family="poisson",data=dat_long)

summary(fit)

#disaggregate by gender (random intercept & slope)* <-best
fit<-glmer(Assets~as.logical(Borrow)+(as.logical(Borrow)|Gender),
           family="poisson",data=dat_long)

summary(fit)
#5a Change in women's economic decision making community level

#development intervention
fit<-glm(as.integer(WEDM)~as.logical(Devel_Intervention_gen),
         family="poisson",data=dat_2018)
summary(fit)

#VSLA
fit<-glm(as.integer(WEDM)~as.logical(VSLA),
         family="poisson",data=dat_2018)
summary(fit)
#5b household level
#membership in a community group
fit<-glm(as.integer(WEDM)~Community_Group,
         family="poisson",data=dat_2018)
summary(fit)

#borrow
#VSLA
fit<-glm(as.integer(WEDM)~as.logical(Borrow),
         family="poisson",data=dat_2018)
summary(fit)

#6a fish change community level 

#conservation intervention gen
##quanitity
fit<-glm(as.integer(Fish_change_quantity)~as.logical(Cons_Intervention_gen),
         family="poisson",data=dat_2018)
summary(fit)
##num species
fit<-glm(as.integer(Fish_change_species)~as.logical(Cons_Intervention_gen),
         family="poisson",data=dat_2018)
summary(fit)
##fish size
fit<-glm(as.integer(Fish_change_size)~as.logical(Cons_Intervention_gen),
         family="poisson",data=dat_2018)
summary(fit)

#Fish no take zone 
##quanitity
fit<-glm(as.integer(Fish_change_quantity)~as.logical(Fish_NTZ),
         family="poisson",data=dat_2018)
summary(fit)
##num species
fit<-glm(as.integer(Fish_change_species)~as.logical(Fish_NTZ),
         family="poisson",data=dat_2018)
summary(fit)
##fish size
fit<-glm(as.integer(Fish_change_size)~as.logical(Fish_NTZ),
         family="poisson",data=dat_2018)
summary(fit)

#Look at community group (any) as predictor
fit<-glm(Food_Groups~Community_Group, family="poisson",data=dat_long)
fit<-glm(Food_Groups~Community_Group+Gender, family="poisson",data=dat_long)
fit<-glmer(Food_Groups~Community_Group+(1|Gender), family="poisson",data=dat_long)

#Look at community group (alliance affected) as predictor
fit<-glm(Food_Groups~Association_Alliance, family="poisson",data=dat_long)
fit<-glmer(Food_Groups~Association_Alliance+(1|Gender), family="poisson",data=dat_long)
fit<-glmer(Food_Groups~Association_Alliance+(Association_Alliance|Gender), family="poisson",data=dat_long)
#assets as outcome
fit<-glmer(Assets~Association_Alliance+(1|Gender), family="poisson",data=dat_long)
fit<-glmer(Assets~Association_Alliance+(Association_Alliance|Gender), family="poisson",data=dat_long)



#t.test to show difference in fish consumption for communities with/without fish ntzs
x<-dat_long[dat_long$Fish_NTZ==0 & dat_long$Year==2014,]
y<-dat_long[dat_long$Fish_NTZ==1 & dat_long$Year==2014,]
x<-as.integer(as.vector(x$Days_seafood))
y<-as.integer(as.vector(y$Days_seafood))
t.test(x,y)




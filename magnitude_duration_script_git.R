setwd("/Users/andrewfelton/Desktop/year")
mad_dur<-read.csv("2018/Mag:Duration/Wilcox_Meta_Data.csv")
#head(mad_dur)
mad_dur_2<-mad_dur[-c(155,156),] #remove bad ones
library(dplyr)

#summary(mad_dur)
#Subset to ANPP
anpp<-subset(mad_dur_2,Data.type=="ANPP")
#find mis-labeled sites...
ppt_increase<-subset(anpp,Treatment=="increase") #appears accurate
ppt_decrease<-subset(anpp,Treatment=="decrease") #108 109 mislabeled

#get rid of combined years
anpp.3 <- anpp%>% dplyr::filter(!Notes %in% c("years combined","combined years"))

# ANPP Duration code -----------------------------------------------------------

#############increases
duration <- list()
duration_increase<-list()

#increase year 1
ANPP_increase_1<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("increase") & year =="1")%>%
  tbl_df()

ANPP_increase_1$lrr<-log((ANPP_increase_1$u.trt)/(ANPP_increase_1$u.amb))
mean(ANPP_increase_1$lrr)

#add to clean list
duration[["ANPP_increase_1"]] <-ANPP_increase_1
duration_increase[["ANPP_increase_1"]] <-ANPP_increase_1

#increase year 2
ANPP_increase_2<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("increase") & year =="2")%>%
  tbl_df()


ANPP_increase_2$lrr<-log((ANPP_increase_2$u.trt)/(ANPP_increase_2$u.amb))
mean(ANPP_increase_2$lrr)

#add to clean list
duration[["ANPP_increase_2"]] <-ANPP_increase_2
duration_increase[["ANPP_increase_2"]] <-ANPP_increase_2

#increase year 3
ANPP_increase_3<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("increase") & year =="3")%>%
  tbl_df()

ANPP_increase_3$lrr<-log((ANPP_increase_3$u.trt)/(ANPP_increase_3$u.amb))
mean(ANPP_increase_3$lrr)

#add to clean list
duration[["ANPP_increase_3"]] <-ANPP_increase_3
duration_increase[["ANPP_increase_3"]] <-ANPP_increase_3

#increase year 4
ANPP_increase_4<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("increase") & year =="4")%>%
  tbl_df()

ANPP_increase_4$lrr<-log((ANPP_increase_4$u.trt)/(ANPP_increase_4$u.amb))
mean(ANPP_increase_4$lrr)

#add to clean list
duration[["ANPP_increase_4"]] <-ANPP_increase_4
duration_increase[["ANPP_increase_4"]] <-ANPP_increase_4

#increase year 5
ANPP_increase_5<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("increase") & year =="5")%>%
  tbl_df()

ANPP_increase_5$lrr<-log((ANPP_increase_5$u.trt)/(ANPP_increase_5$u.amb))
mean(ANPP_increase_5$lrr)

#add to clean list
duration[["ANPP_increase_5"]] <-ANPP_increase_5
duration_increase[["ANPP_increase_5"]] <-ANPP_increase_5

#increase year 6
ANPP_increase_6<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("increase") & year =="6")%>%
  tbl_df()

ANPP_increase_6$lrr<-log((ANPP_increase_6$u.trt)/(ANPP_increase_6$u.amb))
mean(ANPP_increase_6$lrr)

#add to clean list
duration[["ANPP_increase_6"]] <-ANPP_increase_6
duration_increase[["ANPP_increase_6"]] <-ANPP_increase_6

#increase year 7
ANPP_increase_7<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("increase") & year =="7")%>%
  tbl_df()

ANPP_increase_7$lrr<-log((ANPP_increase_7$u.trt)/(ANPP_increase_7$u.amb))
mean(ANPP_increase_7$lrr)

#add to clean list
duration[["ANPP_increase_7"]] <-ANPP_increase_7
duration_increase[["ANPP_increase_7"]] <-ANPP_increase_7

#increase year 8
ANPP_increase_8<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("increase") & year =="8")%>%
  tbl_df()

ANPP_increase_8$lrr<-log((ANPP_increase_8$u.trt)/(ANPP_increase_8$u.amb))
mean(ANPP_increase_8$lrr)

#add to clean list
duration[["ANPP_increase_8"]] <-ANPP_increase_8
duration_increase[["ANPP_increase_8"]] <-ANPP_increase_8

#increase year 23 )exclude from analysis)
#ANPP_increase_23<-anpp %>%
#dplyr::filter(Treatment %in% c("increase") & year =="23")%>%
#tbl_df()

#ANPP_increase_23$lrr<-log((ANPP_increase_23$u.trt)/(ANPP_increase_23$u.amb))
#mean(ANPP_increase_23$lrr)

#add to clean list
#duration[["ANPP_increase_23"]] <-ANPP_increase_23


# Decreases ---------------------------------------------------------------

#make list to add them to

duration_decrease <- list()

#decrease year 1
ANPP_decrease_1<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("decrease") & year =="1")%>%
  tbl_df()

ANPP_decrease_1$lrr<-log((ANPP_decrease_1$u.trt)/(ANPP_decrease_1$u.amb))
mean(ANPP_decrease_1$lrr)

#add to clean list
duration[["ANPP_decrease_1"]] <-ANPP_decrease_1
duration_decrease[["ANPP_decrease_1"]] <-ANPP_decrease_1

#decrease year 2

ANPP_decrease_2<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("decrease") & year =="2")%>%
  tbl_df()

ANPP_decrease_2$lrr<-log((ANPP_decrease_2$u.trt)/(ANPP_decrease_2$u.amb))
mean(ANPP_decrease_2$lrr)

duration[["ANPP_decrease_2"]] <-ANPP_decrease_2
duration_decrease[["ANPP_decrease_2"]] <-ANPP_decrease_2

#decrease year 3
ANPP_decrease_3<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("decrease") & year =="3")%>%
  tbl_df()

ANPP_decrease_3$lrr<-log((ANPP_decrease_3$u.trt)/(ANPP_decrease_3$u.amb))
mean(ANPP_decrease_3$lrr)

duration[["ANPP_decrease_3"]] <-ANPP_decrease_3
duration_decrease[["ANPP_decrease_3"]] <-ANPP_decrease_3

#decrease year 4
ANPP_decrease_4<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("decrease") & year =="4")%>%
  tbl_df()

ANPP_decrease_4$lrr<-log((ANPP_decrease_4$u.trt)/(ANPP_decrease_4$u.amb))
mean(ANPP_decrease_4$lrr)

duration[["ANPP_decrease_4"]] <-ANPP_decrease_4
duration_decrease[["ANPP_decrease_4"]] <-ANPP_decrease_4

#increase year 5
ANPP_decrease_5<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("decrease") & year =="5")%>%
  tbl_df()

ANPP_decrease_5$lrr<-log((ANPP_decrease_5$u.trt)/(ANPP_decrease_5$u.amb))
mean(ANPP_decrease_5$lrr)

duration[["ANPP_decrease_5"]] <-ANPP_decrease_5
duration_decrease[["ANPP_decrease_5"]] <-ANPP_decrease_5

#decreases year 6
ANPP_decrease_6<-anpp.3 %>%
  dplyr::filter(Treatment %in% c("decrease") & year =="6")%>%
  tbl_df()

ANPP_decrease_6$lrr<-log((ANPP_decrease_6$u.trt)/(ANPP_decrease_6$u.amb))
mean(ANPP_decrease_6$lrr)

duration[["ANPP_decrease_6"]] <-ANPP_decrease_6
duration_decrease[["ANPP_decrease_6"]] <-ANPP_decrease_6

#make to table
library(data.table)
#single list
duration_2<-rbindlist(duration)
tbl_df(duration_2)

#just a single relationship with all points, find outliers first
library(car)
duration_2_lm<-outlierTest(lm(lrr~ppt.add.rm,data=duration_2))
duration_3<-duration_2[-c(147,180),]

plot(lrr~ppt.add.rm,data=duration_3)


#combine separate lists

#decrease
duration_decrease_2<-rbindlist(duration_decrease)
tbl_df(duration_decrease_2)
duration_decrease_2$year<-duration_decrease_2$year*-1 #make year -1 for precip reduciton years
duration_decrease_2$dPPT<-duration_decrease_2$dPPT*-1 #make year -1 for precip reduciton years

#increase
duration_increase_2<-rbindlist(duration_increase)
tbl_df(duration_increase_2)

#merge
library(dplyr)


duration_decrease_3 <- duration_decrease_2[,c("Study_site","Treatment","year","MAP","MAT","ppt.add.rm","dPPT","lrr")]
duration_increase_3 <- duration_increase_2[,c("Study_site","Treatment","year","MAP","MAT","ppt.add.rm","dPPT","lrr")]

increase_decrease_merge  <- merge(duration_decrease_3, duration_increase_3[,c("Treatment","year")], all.x = T)
plot(lrr~year,increase_decrease_merge)

full<-rbind(duration_decrease_3,duration_increase_3)
head(full)

#nlme approach
library(car)
library(nlme)
library(MASS)

duration.lme<-lme(lrr~dPPT*year*MAP + MAT,random=~1|Study_site,full,na.action = na.exclude) #included MAP as a random effect
anova(duration.lme) #no evidence for year as a main or an interactive effect. ppt is significant
Anova(duration.lme,type = "II")
summary(duration.lme)

#model selection

#first drop
full.1<-lm(lrr~dPPT + year + MAP + MAT,full,na.action = na.exclude) 
AIC(full.1) #70.24

#no MAT
full.no_MAT<-lm(lrr~dPPT + year + MAP,full,na.action = na.exclude) 
AIC(full.no_MAT) #67.11
#no MAP
full.no_MAP<-lm(lrr~dPPT + year + MAT,full,na.action = na.exclude) 
AIC(full.no_MAP) #68.8
#no year
full.no_year<-lm(lrr~dPPT + MAP + MAT,full,na.action = na.exclude) 
AIC(full.no_year) #68.41
#no ppt
full.no_ppt<-lm(lrr~year + MAP + MAT,full,na.action = na.exclude) 
AIC(full.no_ppt) #162.53
#pretty clear precipitation is driving these responses, and that every other factor is a moodifier

#after dropping MAT
#no MAP
second_no_MAP<-lm(lrr~dPPT + year,full,na.action = na.exclude) 
AIC(second_no_MAP) #65.44

#no year
second_no_year<-lm(lrr~dPPT + MAP,full,na.action = na.exclude) 
AIC(second_no_year) #65.26

#just ppt
second_ppt<-lm(lrr~dPPT,full.no.extreme,na.action = na.exclude) 
AIC(second_ppt)

plot(lrr~dPPT,data=full)

#remove the most extreme point
outlierTest(lm(lrr~dPPT,data=full))

full.no.outliers.1<-full[-c(67,25,24),]

plot(lrr~dPPT,data=full.no.outliers.1)

#linear model
linear_dur_1<-lm(lrr~dPPT,data=full.no.outliers.1)
AIC(linear_dur_1)
summary(linear_dur_1)

#nonlinear model
nonlinear_dur_1<-lm(lrr~dPPT+I(year^2),full.no.outliers.1)
AIC(nonlinear_dur_1)
summary(nonlinear_dur_1)
#no evidence against a linear relationship

#only precipitation added or removed is significant, no evidence for year as a modifying factor
#Potentially consistent with the saturation due to flat-lining at precip additions
# in addition, a negative interaction of LRR with MAP that is significant

#Regression approach
#check for outliers
library(car)
outlierTest(lm(lrr~year,data=full))

#remove outliers
full.no.outliers<-full[-c(25),]
plot(lrr~year,full.no.outliers)

#AIC comparison

#linear model
linear_dur<-lm(lrr~year,data=full.no.outliers)
AIC(linear_dur)
summary(linear_dur)

#nonlinear model
nonlinear_dur<-lm(lrr~year+I(year^2),full.no.outliers)
AIC(nonlinear_dur)
#nonlinear model performs better

######single year extremes

# Single year -------------------------------------------------------------
ANPP_single<-anpp%>%
  dplyr::filter(Data.type %in% c("ANPP") & year =="1")%>%
  tbl_df()

#adding the log RR
ANPP_single$lrr<-log((ANPP_single$u.trt)/(ANPP_single$u.amb))

#modifying the percent column
ANPP_single$dPPT<-ANPP_single$dPPT*100

decrease<-subset(ANPP_single,Treatment=="decrease")
decrease$dPPT<-decrease$dPPT*-1

increase<-subset(ANPP_single,Treatment=="increase")

increase_2 <- increase[,c("Study_site","Treatment","year","MAP","MAT","dPPT","lrr")]
decrease_2 <- decrease[,c("Study_site","Treatment","year","MAP","MAT","dPPT","lrr")]
single_full<-rbind(increase_2,decrease_2 )
plot(lrr~dPPT,single_full)




# percentage basis --------------------------------------------------------------


anpp_single_ambient<-ANPP_single %>%
  dplyr::filter(!is.na(ppt.amb)) 

anpp_single_ambient$per_anpp<-(((anpp_single_ambient$u.trt - anpp_single_ambient$u.amb)/(anpp_single_ambient$u.amb))*100)
anpp_single_ambient$per_precip<-(((anpp_single_ambient$ppt.amb + anpp_single_ambient$ppt.add.rm)/(anpp_single_ambient$ppt.amb))*100)
plot(per_anpp~per_precip,data=anpp_single_ambient)

head(ANPP_single)
summary(ANPP_single)
plot(lrr~ppt.add.rm,data=ANPP_single)

# assess outliers for lrr single-year -------------------------------------------------

outlierTest(lm(lrr~dPPT,data=single_full))
single_no_outliers_lrr<-single_full[-c(1,65,66),]
plot(lrr~dPPT,single_no_outliers_lrr)

#AIC comparison
#linear
linear_mag<-lm(lrr~dPPT,data=single_no_outliers_lrr)
AIC(linear_mag) #4.81
summary(linear_mag) #r-square 0.48

#nonlinear
nonlinear_mag<-lm(lrr~dPPT+I(dPPT^2),data=single_no_outliers_lrr)
AIC(nonlinear_mag) #2.23
summary(nonlinear_mag) #r-square = 0.5
# nonlinear model performs slightly better


#errorbars


# nlme single year ------------------------------------------------
library(nlme)
single_all = lme(lrr~ppt.add.rm*MAP,random=~1|Study_site,data=single_no_outliers_lrr,na.action = na.exclude)
summary(single_all)
single_all

single_gs = lme(lrr~ppt.add.rm*MAP,random=~1|Study_site,data=single_no_outliers_gs_lrr_2,na.action = na.exclude)
summary(single_gs)SINGLE YEA
single_all

# graphing code -----------------------------------------------------------

library(ggplot2)

#prepping changes

anpp$dPPT<-anpp$dPPT*100
mad_dur_ggplot<-mad_dur[-c(3,155,156,220),] #remove the most extreme valeus along with incorrect data
anpp_ggplot<-subset(mad_dur_ggplot,Data.type=="ANPP")
anpp_ggplot$dPPT<-anpp_ggplot$dPPT*100
full.no.outliers.1$dPPT<-full.no.outliers.1$dPPT*100
dodge<-position_dodge(width=0.4)
(anpp$dPPT)
#ggplot
ggplot(sgs_experiment_final_2,aes(GSP.diff,anpp.effect,na.rm=TRUE)) +
  #scale_color_manual(values=c('increase'='blue','decrease'='red'),name="") +
  #geom_bar() +
  #geom_point(pch=1,size=4) +
  #stat_summary(fun.y="mean",geom="point") +
  #scale_y_continuous(expand = c(0,0)) +
  #stat_summary(fun.y="count",geom="bar")
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  #geom_point() +
  #geom_smooth(method="lm",se=TRUE,linetype="dashed") +
  #geom_hline(yintercept = 713.97,color="black",size=.5) +
  #stat_smooth(method = "lm", formula = y ~ poly(x, 2), linetype="dashed",color="black",size = 1.5,se=FALSE) + 
  #ylab(bquote('ANPP ('*g/m^2*')')) +
  #stat_summary(fun.y="mean",geom="point",size=6,pch=19) +
  #geom_boxjitter(outlier.color = NA, jitter.shape = 21, jitter.color = NA, 
  #jitter.height = 0.05, jitter.width = 0.075, errorbar.draw = TRUE) +
  #geom_point(size=4,pch=21,color="black") +
  stat_summary(geom="point",fun.y="identity",size=5,color="black",aes(fill=as.factor(manipulation)),shape=21,show.legend =FALSE) +
  scale_fill_manual(values=c('Increase'='blue','Decrease'='red'),name="") +
  #geom_smooth(method="lm",se=FALSE,linetype="dashed",color="black",aes(fill=Treatment),show.legend=FALSE) +
  #geom_smooth(method="lm",se=FALSE,color="black",linetype="dashed",size=1.5) +
  #stat_smooth(method = "lm", formula = y ~ poly(x, 2), linetype="dashed",size = 1,se=FALSE,color="black") + #geom_smooth(method="lm",se=FALSE,color="black") +
  #xlab("Duration of study") +
  #xlab("% Precipiaton deviation from control") +
  xlab("% Precipiaton deviation from median") +
  ylab("ANPP effect size") +
  #ylab("Precipitation treatment") +
  geom_smooth(method="lm",se=TRUE,color="black",size=0.5) +
  #stat_summary(fun.y = "mean",geom="point") +
  #geom_hline(yintercept = 20,color="black",size=1) +
  theme(
    axis.text = element_text(color='black',size=20),
    axis.title.x = element_text(color='black',size=22),
    axis.title.y = element_text(color='black',size=24),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=20),
    legend.text = element_text(size=22),
    #legend.position = c(.7,.7),
    legend.position = c(.7,.25),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

de<-subset(single_no_outliers,Treatment=="decrease")
#potts burned has a "decrease" treatment, put the precipitation is an increase from the control...

#observational record
sgs_obs<-read.csv(file.choose(),header = TRUE)
head(sgs_obs)
plot(ANPP~GSP..June.August.,data=sgs_obs)
median(sgs_obs$GSP..June.August.) #ANPP at median = 142.37
(143.002 + 141.732)/2 #precip for median
(35.34 + 37.37)/2 #36.36

lrr_sgs <- function(x) {
  
  
  lrr <- log(x/36.36)
  
  return(lrr)
}

lrr_sgs_2<-aggregate(ANPP ~ GSP..June.August. + Year,lrr_sgs,data=sgs_obs)

perc_precip_sgs<- function(x) {
  
  
  perc_sgs <- ((x-142.37)/142.37)*100
  
  return(perc_sgs )
}

precip_change_sgs<-aggregate(GSP..June.August. ~ Year,perc_precip_sgs,data=sgs_obs)

merge_sgs<-merge(precip_change_sgs,lrr_sgs_2,by=c("Year"))

plot(ANPP~GSP..June.August..x,data=merge_sgs)

#konza
konza_obs<-read.csv(file.choose(),header = TRUE)
konza<-subset(konza_obs,Variability=="Ambient")
konza_exp<-read.csv(file.choose(),header = TRUE)
konza_exp_2<-subset(konza_exp,Variability.2=="Even.2016")
konza_obs<-subset(konza_exp,Variability=="Ambient")

plot(ANPP~GSP,data=konza_obs)
median(konza_obs$GSP) #ANPP at median = 410.25
(412.5 + 408)/2
(379.25 + 349.69)/2
(143.002 + 141.732)/2 #precip for median
(35.34 + 37.37)/2 #36.36

lrr_konza <- function(x) {
  
  
  lrr <- log(x/364.47)
  
  return(lrr)
}

lrr_konza_2<-aggregate(ANPP ~ GSP + Year + Variability,lrr_konza,data=konza_obs)

perc_precip_konza<- function(x) {
  
  
  perc_konza <- ((x-410.25)/410.25)*100
  
  return(perc_konza)
}

precip_change_konza<-aggregate(GSP ~ Year + Variability,perc_precip_konza,data=konza_obs)
merge_konza_obs<-merge(precip_change_konza,lrr_konza_2,by=c("Year","Variability"))
write.csv(merge_konza_obs,file="konza_obs_mag_dur.csv")
konza_obs_mag_dur<-read.csv(file.choose(),header=TRUE)

plot(ANPP~GSP.x,data=konza_obs_mag_dur)
plot(ANPP.effect~GSP.diff,konza_obs_mag_dur)
konza_lm<-lm(ANPP~GSP.x,data=konza_obs_mag_dur)
poly.konza.obs<-lm(ANPP~GSP.diff+I(GSP.x^2),konza_obs_mag_dur)
AIC(konza_lm,poly.konza.obs) #linear model only slightly better
summary(konza_lm)
library(car)
outlierTest(konza_lm)

#experiment

#median anpp
median<-subset(konza_exp_2,GSP=="422.875")
mean(median$ANPP)

lrr_konza_exp <- function(x) {
  
  
  lrr <- log(x/393.64)
  
  return(lrr)
}


effect_konza_exp<-aggregate(ANPP ~ GSP + Year + Plot + Variability,lrr_konza_exp,data=konza_exp_2)

perc_precip_konza_exp<- function(x) {
  
  
  perc_konza_exp <- ((x-422.88)/422.88)*100
  
  return(perc_konza_exp)
}

precip_change_konza_exp<-aggregate(GSP ~ Year + Plot + Variability, perc_precip_konza_exp,data=konza_exp_2)

merge_konza_exp<-merge(precip_change_konza_exp,effect_konza_exp,by=c("Year","Plot","Variability"))

plot(ANPP~GSP.x,data=konza_experiment_final_2)
konza_exp_lm<-lm(ANPP.effect~GSP.diff,data=konza_experiment_final_2)
summary(konza_exp_lm)
outlierTest(konza__exp_lm) #no outliers
poly.konza.exp<-lm(ANPP.effect~GSP.diff +I(GSP.diff^2),konza_experiment_final_2)
AIC(konza_exp_lm,poly.konza.exp) #linear model slightly preferable

write.csv(merge_konza_exp,file = "konza_ex_mag_dur.csv")
konza_experiment_final<-read.csv(file.choose(),header=TRUE)

library(dplyr)
konza_experiment_final_2 <- konza_experiment_final %>%
  dplyr:: group_by(Year,Plot,data,manipulation, ANPP.effect,GSP.diff) %>%
  dplyr:: filter(!manipulation %in% c("Median"))

#shortgrasss steppe
#experimental data
sgs_experiment<-read.csv(file.choose(),header=TRUE)
plot(x.100~mm,data=sgs_experiment)
anpp.ag.experiment<-aggregate(x.100 ~ Plot + mm + percentile + vwc.20cm + subset + Block,mean,data=sgs_experiment)
plot(x.100~mm,data=anpp.ag.experiment)

#test/remove outliers
lm_sgs_experiment<-lm(x.100~mm,data=anpp.ag.experiment)
outlierTest(lm_sgs_experiment) #remove one observation
anpp.ag.experiment.2<-anpp.ag.experiment[-4,] 

#median anpp
median_sgs<-subset(anpp.ag.experiment.2,mm=="141.86")
mean(median_sgs$x.100)

lrr_sgs_exp <- function(x) {
  
  
  lrr_sgs <- log(x/71.51)
  
  return(lrr_sgs)
}


effect_sgs_exp<-aggregate(x.100 ~ mm + Plot,lrr_sgs_exp,data=anpp.ag.experiment.2)

perc_precip_sgs_exp<- function(x) {
  
  
  perc_sgs_exp <- ((x-141.86)/141.86)*100
  
  return(perc_sgs_exp)
}

precip_change_sgs_exp<-aggregate(mm ~ Plot, perc_precip_sgs_exp,data=anpp.ag.experiment.2)

merge_sgs_exp<-merge(precip_change_sgs_exp,effect_sgs_exp,by=c("Plot"))

write.csv(merge_sgs_exp,file = "sgs_ex_mag_dur.csv")
sgs_experiment_final<-read.csv(file.choose(),header=TRUE)

sgs_experiment_final_2 <- sgs_experiment_final %>%
  dplyr:: group_by(Plot,manipulation, anpp.effect,GSP.diff) %>%
  dplyr:: filter(!manipulation %in% c("Median"))

plot(anpp.effect~GSP.diff,data=sgs_experiment_final_2 )
sgs_exp_lm<-lm(anpp.effect~GSP.diff,data=sgs_experiment_final_2)
summary(sgs_exp_lm)
outlierTest(sgs_exp_lm) #no outliers
poly.sgs.exp<-lm(anpp.effect~GSP.diff +I(GSP.diff^2),sgs_experiment_final_2)
AIC(sgs_exp_lm,poly.sgs.exp) #linear model slightly preferable

#observational
sgs_obs<-read.csv(file.choose(),header=TRUE)
head(sgs_obs)  
swale<-subset(sgs_obs,site_name=="SWALE")
head(swale)
swale_summed<-aggregate(weight~plot + transect + site_name + Year,sum,data=swale)  
head(swale_summed)  
swale_averaged<-aggregate(weight~Year + site_name,mean,data=swale_summed)
wd
##stopped

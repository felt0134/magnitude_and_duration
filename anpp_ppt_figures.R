library(ggplot2)
library(lattice)
library(grid)
library(dplyr)

anpp<-'/Volumes/GoogleDrive/My Drive/Other projects/Magnitude and Duration/Datasets'
konza_lter<-read.csv(file.path(anpp, "KNZ_1D_ANPP_PPT_Summary.csv"))
head(konza_lter)
konza_lter_upland<-subset(konza_lter,Topographic.Position =='Upland')
plot(PUE~Year,data=konza_lter_upland)
plot(GSP..mm.~Year,data=konza_lter_upland)
summary(konza_lter_upland)
sd(konza_lter_upland$GSP..mm.)
#1rd quartile for GSP is: 476

#inset dataframe for plotting
konza_lter_upland$gsp_perc <- ((konza_lter_upland$GSP..mm. - mean(konza_lter_upland$GSP..mm.))/mean(konza_lter_upland$GSP..mm.)*100)
konza_lter_upland$anpp_perc <- ((konza_lter_upland$Total.ANPP..g.m2. - mean(konza_lter_upland$Total.ANPP..g.m2.))/mean(konza_lter_upland$Total.ANPP..g.m2.)*100)
konza_lter_upland$se_perc <- (konza_lter_upland$Total.ANPP.st.error/konza_lter_upland$Total.ANPP..g.m2.)*100

drought_konza<-subset(konza_lter_upland,Year==c('1988','1989'))

Year<-c('1988','1988','1989','1989')
Variable<-c('Precipitation','ANPP','Precipitation','ANPP')
Mean<-c(-45.2,-23.93,-37.11,-48.07)
SE<-c(0,10,0,6.042)
konza.plotting<-data.frame(Year,Variable,Mean,SE)
SE<-c('')

inset_konza<-ggplot(konza.plotting,aes(as.factor(Year),Mean,fill=Variable)) +
  geom_errorbar(aes(ymin=Mean-SE , ymax=Mean+SE),position=position_dodge(width=1), size=1,width=.2) +
  #geom_point()
  #geom_bar(aes(fill = Variable), position = "dodge", stat="identity")
  stat_summary(fun.y='mean',geom='bar',position=position_dodge(width=1),pch=21,size=0.5,color='black') +
  geom_hline(yintercept = 0,color='black',size=1.25) +
  scale_fill_manual(name='',values=c(ANPP ="red", Precipitation ="blue"),
                      labels=c('ANPP'='ANPP','Precipitation'= 'Precipitation')) +
  xlab('') +
  #ylab(bquote('PUE ('*g/m^2/mm*')')) +
  ylab('% reduction from mean') +
  ggtitle('') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('top'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

konza_experiment<-read.csv(file.path(anpp, "MaD.ANPP.VWC.2016.csv"))
head(konza_experiment)
konza_experiment_2016<-subset(konza_experiment,Year=='2016') 
main_konza<-ggplot(konza_experiment_2016,aes(mm,ANPP)) +
  geom_point(size=5,pch=1,fill='white',color='black',alpha=0.75) +
  stat_smooth(method='lm',se=F,color='black',size=1.5) +
  xlim(175,1050) +
  xlab('') +
  ylab('') +
  ggtitle('') +
  theme(
    axis.text.x = element_text(color='black',size=18), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

vp <- viewport(width = 0.30, height = 0.40, x = 0.32,y=0.84)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(main_konza)
  print(inset_konza, vp = vp)
}
full()


#shortgrass steppe
#calculate standard error for sgs sumamrized anpp...
raw_anpp<-read.csv(file.path(anpp, "sgs_ANPP_LTER_for_analysis.csv"))
summary(raw_anpp)
ridge_anpp<-subset(raw_anpp,site_name=='RIDGE')
ridge_anpp_no_sd <- ridge_anpp%>% dplyr::filter(!species %in% c("OSD"))
ridge_anpp_summed<-aggregate(weight~plot + transect + Year,sum,data=ridge_anpp_no_sd)
ridge_anpp_summed$anpp<-ridge_anpp_summed$weight*4
ridge_anpp_transect_means<-aggregate(anpp ~ Year,mean,data=ridge_anpp_summed)
ridge_anpp_transect_ser<-aggregate(anpp ~ Year,ser,data=ridge_anpp_summed)
plot(anpp~Year,data=ridge_anpp_transect_means)
ridge_anpp_ser<-aggregate(anpp~Year,ser,data=ridge_anpp_no_sd)

#se equation
ser<-function(x) {
  sqrt(var(x)/length(x))
}

#full anpp
sgs_lter<-read.csv(file.path(anpp, "sgs_pue_lter.csv"))
plot(GSP..May.August.~Year,data=sgs_lter)
plot(GSP..June.August.~Year,data=sgs_lter)
summary(sgs_lter)
sgs_lter_mean_ser<-merge(sgs_lter,ridge_anpp_ser,by='Year')
drought_sgs<-subset(sgs_lter_mean_ser,Year==c('1988'))
drought_sgs_2<-subset(sgs_lter_mean_ser,Year==c('1989'))
drought_sgs_3<-rbind(drought_sgs_2,drought_sgs)

#prep for insetting
sgs_lter_mean_ser$gsp_perc <- ((sgs_lter_mean_ser$GSP..May.August. - mean(sgs_lter_mean_ser$GSP..May.August.))/mean(sgs_lter_mean_ser$GSP..May.August.)*100)
sgs_lter_mean_ser$anpp_perc <- ((sgs_lter_mean_ser$ANPP - mean(sgs_lter_mean_ser$ANPP))/mean(sgs_lter_mean_ser$ANPP)*100)
sgs_lter_mean_ser$se_perc <- (sgs_lter_mean_ser$anpp/sgs_lter_mean_ser$ANPP)*100

drought_sgs<-subset(sgs_lter_mean_ser,Year==c('1988'))
drought_sgs_2<-subset(sgs_lter_mean_ser,Year==c('1989'))
drought_sgs_3<-rbind(drought_sgs_2,drought_sgs)

Year<-c('1988','1988','1989','1989')
Variable<-c('Precipitation','ANPP','Precipitation','ANPP')
Mean<-c(-37.82,3.53,-17.77,-19.047)
SE<-c(0,4.015,0,4.3)
sgs.plotting<-data.frame(Year,Variable,Mean,SE)

inset_sgs<-ggplot(sgs.plotting,aes(as.factor(Year),Mean,fill=Variable)) +
  geom_errorbar(aes(ymin=Mean-SE , ymax=Mean+SE),position=position_dodge(width=1), size=1,width=.2) +
  #geom_point()
  #geom_bar(aes(fill = Variable), position = "dodge", stat="identity")
  stat_summary(fun.y='mean',geom='bar',position=position_dodge(width=1),pch=21,size=0.5,color='black') +
  geom_hline(yintercept = 0,color='black',size=1.25) +
  scale_fill_manual(name='',values=c(ANPP ="red", Precipitation ="blue"),
                    labels=c('ANPP'='ANPP','Precipitation'= 'Precipitation')) +
  xlab('') +
  #ylab(bquote('PUE ('*g/m^2/mm*')')) +
  ylab('% reduction from mean') +
  ggtitle('') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('top'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

sgs_experiment<-read.csv(file.path(anpp, "mad_sgs_ANPP.csv"))
head(sgs_experiment)
#remove known outlier
sgs_experiment_2<-sgs_experiment[-3,]

sgs_experiment_averaged<-aggregate(x.100~Plot + mm,mean,data=sgs_experiment_2)

main_sgs<-ggplot(sgs_experiment_averaged,aes(mm,x.100)) +
  geom_point(size=5,pch=1,fill='white',color='black',alpha=0.75) +
  stat_smooth(method='lm',se=F,color='black',size=1.5) +
  xlab('') +
  ylab(bquote('ANPP ('*g/m^2*')')) +
  ggtitle('') +
  theme(
    axis.text.x = element_text(color='black',size=18), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

vp.sgs <- viewport(width = 0.30, height = 0.40, x = 0.32,y=0.82)

#executing the inset, you create a function the utlizes all the previous code
full.sgs <- function() {
  print(main_sgs)
  print(inset_sgs, vp = vp.sgs)
}
full.sgs()



library(ggplot2)
library(lattice)
library(grid)

anpp<-'G:/My Drive/Other projects/Magnitude and Duration/Datasets'
konza_lter<-read.csv(file.path(anpp, "KNZ_1D_ANPP_PPT_Summary.csv"))
head(konza_lter)
konza_lter_upland<-subset(konza_lter,Topographic.Position =='Upland')
plot(PUE~Year,data=konza_lter_upland)
plot(GSP..mm.~Year,data=konza_lter_upland)
summary(konza_lter_upland)
sd(konza_lter_upland$GSP..mm.)

drought_konza<-subset(konza_lter_upland,Year==c('1988','1989'))


inset_konza<-ggplot(drought_konza,aes(as.factor(Year),Total.ANPP..g.m2.)) +
  stat_summary(fun.y='mean',geom='bar',size=1,fill='white',color='black',alpha=0.75) +
  #geom_errorbar(ymin = Total.ANPP..g.m2.-Total.ANPP.st.error ,ymax=Total.ANPP..g.m2.+Total.ANPP.st.error)
  xlab('') +
  ylab(bquote('PUE ('*g/m^2/mm*')')) +
  ggtitle('Multi-year drought') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title = element_text(color='black',size=14),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
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
  xlab('') +
  ylab(bquote('ANPP ('*g/m^2*')')) +
  ggtitle('') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
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

vp <- viewport(width = 0.3, height = 0.35, x = 0.8,y=0.31)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(main_konza)
  print(inset_konza, vp = vp)
}
full()


#shortgrass steppe
sgs_lter<-read.csv(file.path(anpp, "sgs_pue_lter.csv"))
summary(sgs_lter)

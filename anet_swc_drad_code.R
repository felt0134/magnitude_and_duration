drad<-'C:/Users/A02296270/Desktop/Side_projects/DOE/doe_greenhouse_experiment_data'
drad_phys<-read.csv(file.path(drad, "DRAD_GAS_EXCHANGE.csv"))
drad_gwc<-read.csv(file.path(drad, "Drad_GWC_Final_2.csv"))

head(drad_phys)
head(drad_gwc)
plot(A~Ci,data=drad_phys)
library(dplyr)
nozero<-drad_phys %>% dplyr::filter(E>0.01)
plot(A~E,data=nozero)
drad_phys$Pot<-drad_phys$Plant
drad_phys$Treatment <-drad_phys$treatment

drad_phys_gwc<-merge(drad_phys_2,drad_gwc,by.y=c('Date','Plant','Treatment'))
head(drad_phys_gwc)
plot(A~newgwc,data=drad_phys_gwc)
andro<-subset(drad_phys_gwc,Species=='A.gerardii')
plot(A~E,data=andro)
drad_phys_2<-drad_phys[-c(1,2,3,4,5,6,7,9,10,11)]
head(drad_phys_2)
head(drad_gwc)
summary(drad_phys)
andro.phys<-subset(drad_phys,Species=='A.gerardii')
bogr.phys<-subset(drad_phys,Species=='B.gracilis')
head(bogr.phys)
plot(A~gsw,data=bogr.phys)
andro_2<-
andro.gwc<-subset(drad_gwc,Species=='A.gerardii')
head(andro.gwc)
andro.gwc.2<-andro.gwc[-c(1,3,4,5,7)]
head(andro.gwc.2)
head(gwc_phys)
andro.phys.2<-andro.phys[-c(1,2,3,4,5,6,7,9,10,11)]
head(andro.phys.2)
merged.andro<-merge(andro.gwc.2,andro.phys.2,by=c('Date','Pot','Treatment'))
head(merged.andro)
merged.andro<-cbind(andro.gwc.2,andro.phys.2)                   
andro.phys.mean~
  
  
  #net photosynthesis, evaptranspiration
  pasm<-subset(nozero,Species=='P.smithii')
  ggplot(pasm,aes(E,A)) +
  geom_point(pch=21) +
    geom_smooth(method = "lm", color="black",formula=y ~ poly(x, 2, raw=TRUE),se = FALSE,size=1,show.legend = F) +
    xlab(bquote('Transpiration ('*'mmol' ~mm~ m^-2~s^-1*')')) +
    ylab(bquote('Net photosynthesis ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
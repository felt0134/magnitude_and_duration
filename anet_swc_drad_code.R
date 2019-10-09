drad<-'C:/Users/A02296270/Desktop/Side_projects/DOE/doe_greenhouse_experiment_data'
drad_phys<-read.csv(file.path(drad, "DRAD_GAS_EXCHANGE.csv"))
drad_gwc<-read.csv(file.path(drad, "Drad_GWC_Final_2.csv"))

head(drad_phys)
head(drad_gwc)

drad_phys$Pot<-drad_phys$Plant
drad_phys$Treatment <-drad_phys$treatment

drad_phys_gwc<-merge(drad_phys_2,drad_gwc,by.y=c('Date','Plant','Treatment'))
head(drad_phys_gwc)
plot(A~newgwc,data=drad_phys_gwc)
andro<-subset(drad_phys_gwc,Species=='A.gerardii')
plot(A~newgwc,data=merged.andro)
drad_phys_2<-drad_phys[-c(1,2,3,4,5,6,7,9,10,11)]
head(drad_phys_2)
head(drad_gwc)

andro.phys<-subset(drad_phys,Species=='A.gerardii')
head(andro.phys)
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
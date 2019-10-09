#california example

library(dplyr)
library(spdep)
library(splitstackshape)
library(raster)
library(gstat)
library(tidyr)
library(ggplot2)
library(sp)

#load initial dataframe
# ********need to update this dataframe to include the new soil moisture data Bob updated******

test_wd<-"G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Processing NPP Data/NPP Data processing"
rangeland_npp_covariates<-readRDS(file.path(test_wd, "npp_climate_rangelands_final_updated_SWC.rds")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(rangeland_npp_covariates)
summary(rangeland_npp_covariates)
head(rangeland_npp_covariates)

#california_annuals
california_annuals_1 <-subset(rangeland_npp_covariates,region=="california_annuals")
summary(california_annuals_1)

npp.perc

#2011
california_annuals_2011<-subset(california_annuals_1,year=='2011')
hist(california_annuals_2011$mm)
california_annuals_2011_2<-california_annuals_2011[c(1,2,6)]
head(california_annuals_2011_2)
california_raster_2011<-rasterFromXYZ(california_annuals_2011_2)
plot(california_raster_2011)

#2012
california_annuals_2012<-subset(california_annuals_1,year=='2012')
summary(california_annuals_2012)
hist(california_annuals_2012$mm)
california_annuals_2012_2<-california_annuals_2012[c(1,2,6)]
head(california_annuals_2012_2)
california_raster_2012<-rasterFromXYZ(california_annuals_2012_2)
plot(california_raster_2012)

#2013
california_annuals_2013<-subset(california_annuals_1,year=='2013')
summary(california_annuals_2013)
hist(california_annuals_2013$mm)
california_annuals_2013_2<-california_annuals_2013[c(1,2,6)]
head(california_annuals_2013_2)
california_raster_2013<-rasterFromXYZ(california_annuals_2013_2)
plot(california_raster_2013)

#2014
california_annuals_2014<-subset(california_annuals_1,year=='2014')
summary(california_annuals_2014)
hist(california_annuals_2014$mm)
california_annuals_2014_2<-california_annuals_2014[c(1,2,6)]
head(california_annuals_2014_2)
california_raster_2014<-rasterFromXYZ(california_annuals_2014_2)
plot(california_raster_2014)

#2014
california_annuals_2015<-subset(california_annuals_1,year=='2015')
summary(california_annuals_2015)
hist(california_annuals_2015$mm)
california_annuals_2015_2<-california_annuals_2015[c(1,2,6)]
head(california_annuals_2015_2)
california_raster_2015<-rasterFromXYZ(california_annuals_2015_2)
plot(california_raster_2015)

#plot
break_npp_cali<-quantile(california_annuals_1$npp,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
break_npp_cali<-break_npp_cali$npp
break_npp_cali
plot(us)

#2011
spplot(california_raster_2011,#scales = list(draw = TRUE),
       at=break_npp_cali,
       #par.settings = list(axis.line = list(col = 'transparent')),
       asp=0.01,
       col.regions = 
         rev(terrain.colors(length(break_npp_cali)-1)),
       main="") +
  latticeExtra::layer(sp.polygons(dat.poly, lwd = 1))

#2012
spplot(california_raster_2012,#scales = list(draw = TRUE),
       at=break_npp_cali,
       #par.settings = list(axis.line = list(col = 'transparent')),
       asp=0.01,
       col.regions = 
         rev(terrain.colors(length(break_npp_cali)-1)),
       main="2012") +
  
#2013
  spplot(california_raster_2013,#scales = list(draw = TRUE),
         at=break_npp_cali,
         #par.settings = list(axis.line = list(col = 'transparent')),
         asp=0.01,
         col.regions = 
           rev(terrain.colors(length(break_npp_cali)-1)),
         main="2013")
  latticeExtra::layer(sp.polygons(dat.poly, lwd = 1))
  
  #2014
  spplot(california_raster_2014,#scales = list(draw = TRUE),
         at=break_npp_cali,
         #par.settings = list(axis.line = list(col = 'transparent')),
         asp=0.01,
         col.regions = 
           rev(terrain.colors(length(break_npp_cali)-1)),
         main="2014")
  latticeExtra::layer(sp.polygons(dat.poly, lwd = 1))
  
  #2015
  spplot(california_raster_2015,#scales = list(draw = TRUE),
         at=break_npp_cali,
         #par.settings = list(axis.line = list(col = 'transparent')),
         asp=0.01,
         col.regions = 
           rev(terrain.colors(length(break_npp_cali)-1)),
         main="")
  latticeExtra::layer(sp.polygons(dat.poly, lwd = 1))
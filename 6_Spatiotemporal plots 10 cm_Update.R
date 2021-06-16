# File: Spatiotemporal Predictions.R
# Author: Guillermo Martin 
# Template Created: Fri May 14 18:02:40 2021
# ---------------------------------------------------------------------------
# Description:
# Plot of spatial temporal predictions in 10cm bins
# ---------------------------------------------------------------------------

rm(list=ls())

library(mgcv)
library(ggplot2)
library(sp)
library(raster)
library(lubridate)
library(metR)
library(maptools)
#library(data.table)
#require(RCurl)
#require(grid)
#require(maptools)

#Load model
load(file.path("C:/Users/ggonzales/Dropbox/GAM_cod_MS/Data_Clean_Model",
               "MODELS1.3.2.RData"))

#Load prediction dataset
load(file.path("C:/Users/ggonzales/Dropbox/GAM_cod_MS/Data_Clean_Model",
               "prediction.data.Rdata"))

# Shapefiles:
UK<-readRDS(file.path("C:/Users/ggonzales/Dropbox/GAM_cod_MS/",
                       "Data_Clean_Model/geoData","GADM_2.8_GBR_adm1.rds"))
UKmap<-fortify(UK)

NOR<-readRDS(file.path("C:/Users/ggonzales/Dropbox/GAM_cod_MS/",
                  "Data_Clean_Model/geoData","GADM_2.8_NOR_adm0.rds"))
NORmap<-fortify(NOR)


# Layers of predictions ---------------------------------------------------
month.i<-c(2,5,8,11)
lengt.i<-seq(2,50,by=2)

preds_Both <- vector("list", length(month.i)) #For the storage of model outputs


# Year 2015
data.pred.sub<-data.pred2[data.pred2$Year2=='5' & data.pred2$month==2,]

# Loop through layers of predictions for both models ----------------------

for (m in 1:length(month.i)) {
  for (l in 1:length(lengt.i)) {
    
    dat<-data.frame(depth.geo.bin=data.pred.sub$depth.geo.bin,
                    ShootLon=data.pred.sub$ShootLon,
                    ShootLat=data.pred.sub$ShootLat,
                    Year2='5',
                    month=month.i[m],
                    lngtfactor.int=lengt.i[l],
                    gear="GOV",
                    Trawlmin=60,
                    vessel2=110)
    
    # Model with both data Sources                
    preds_Both[[m]][[l]]<- predict(gam1.3.2, newdata = dat, 
                                   type = "response", se = TRUE,
                                   exclude="s(vessel2)")
    

  }
}



# Extract model predictions -----------------------------------------------
d.l<-dim(data.pred.sub)[1] #Data length


ini<-data.frame(fit.Both=rep(NA,length.out=d.l),
                se.Both=rep(NA,length.out=d.l))

for (m in 1:length(month.i)) {
  for(l in 1:length(lengt.i)){
    
    #Both data sources predictions
    ini$fit.Both <- preds_Both[[m]][[l]][[1]]
    ini$se.Both <- preds_Both[[m]][[l]][[2]]
    
    ini$ShootLon<-dat$ShootLon
    ini$ShootLat<-dat$ShootLat
    ini$Month<-month.i[m]
    ini$LengthClass<-lengt.i[l]
    ini$Data<-"Both"
    
    if (m==1  &&  l == 1) {
      out=ini
    } else{
      out=rbind(ini,out)
    }
  }
}

breaks.L<-c(-Inf,10,20,30,40,Inf)
labels.L<-c("0-10","10-20","20-30","30-40","40-50")

out$LengthGroup<-cut(out$LengthClass,
                     breaks=breaks.L,
                     labels=labels.L)

# Calculate the mean of Length Groups for each month.
# Change fit or se based if in you want the mean prediction or the standard error
out_summary<-aggregate(fit.Both~Month+ShootLon+ShootLat+LengthGroup,
                       data=out,FUN=mean)

#Add month Abb
out_summary$Month.f<-month(out_summary$Month, label = TRUE, abbr = TRUE)

#Log of predictions
# Change fit or se based if in you want the mean prediction or the standard error
out_summary$log.Count<-log(out_summary$fit.Both)


plot.final<-ggplot()+
  geom_tile(data=out_summary,aes(x=ShootLon,y=ShootLat,fill=log.Count))+
  geom_polygon(data = NOR,aes(x=long,y=lat,group=group),fill='grey', colour="black")+
  geom_polygon(data = UK,aes(x=long,y=lat,group=group),fill='grey', colour="black")+
  facet_grid(Month.f~LengthGroup)+
  scale_fill_distiller(palette = "Spectral",direction =-1)+
  xlab('Longitude (°)')+
  ylab('Latitude (°)')+
  labs(fill= "log CPUE" ,colour = "Depth (m)") + 
  coord_map(xlim=c(-5, 7), ylim=c(54.5,61.5)) +
  scale_x_continuous(breaks = seq(-5,7,by=2.5)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

  

# Including bathymetry on the plot ----------------------------------------
#Loading depth grid
depth.grid <- readAsciiGrid(file.path("C:/Users/ggonzales/Dropbox/GAM_cod_MS/",
                            "Data_Clean_Model/geoData",
                            "bath_asc.txt"))
proj4string(depth.grid) <- proj4string(UK)
depth.ras <- raster(depth.grid)
depth.pt <- rasterToPoints(depth.ras) 
depth.df <- data.frame(depth.pt)
colnames(depth.df) <- c("ShootLon","ShootLat","depth")
#Just data under 0
depth.df<-depth.df[depth.df$depth<0,]

### leaving just north sea data  for depth grid
lon.min=-5
lon.max=7
lat.min=54.5
lat.max=61.5

depth.df <- subset(depth.df, ShootLon >= lon.min)
depth.df <- subset(depth.df, ShootLon <=lon.max)
depth.df<- subset(depth.df, ShootLat>=lat.min)
depth.df <- subset(depth.df, ShootLat<=lat.max)

#Create depth ranges
breaks.depth<-c(-Inf,seq(-400,0,by=50))
labels.depth<-c(">400","350-400","300-350","250-300","200-250","150-200",
                "100-150","50-100","0-50")

depth.df$depth.range<-cut(depth.df$depth,breaks=breaks.depth,labels=labels.depth)
depth.df$depth.range<-as.factor(depth.df$depth.range)


#Order the data frame by the depth.range for plotting
depth.df$depth.range = factor(depth.df$depth.range, levels=c("0-50","50-100","100-150","150-200","200-250","250-300","300-350","350-400",">400"))

  
plot.final.2<-plot.final + 
  geom_contour(data=subset(depth.df,depth.df$depth <= -40 & depth.df$depth >= -310),
               aes(x=ShootLon,y=ShootLat,z=depth,colour = ..level..),
               linetype="solid",size=.5,
               breaks = c(-50,-100,-200))+
  labs(fill= "log CPUE" ,colour = "Depth (m)") + 
  coord_map(xlim=c(-5, 7), ylim=c(54.5,61.5)) +
  scale_x_continuous(breaks = seq(-5,7,by=2.5)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())  
plot.final.2



# Plot the SE of the predictions for 10, 20, 30, 40  and 50cm --------------
#Add month Abb
out$Month.f<-month(out$Month, label = TRUE, abbr = TRUE)

#To add location of fishing points
data.pregam$Month.f<-month(data.pregam$month, label = TRUE, abbr = TRUE)
data.pregam$LengthClass<-data.pregam$lngtfactor.int

se.plot<-
  ggplot()+
  geom_tile(data=subset(out,LengthClass %in% c(10,20,30,40,50)),
            aes(x=ShootLon,y=ShootLat,fill=log(se.Both)))+
  #geom_polygon(data = NOR,aes(x=long,y=lat,group=group),fill='grey', colour="black")+
  #geom_polygon(data = UK,aes(x=long,y=lat,group=group),fill='grey', colour="black")+
  facet_grid(Month.f~LengthClass)+
  scale_fill_distiller(palette = "Spectral",direction =-1)+
  xlab('Longitude (°)')+
  ylab('Latitude (°)')+
  labs(fill= "log SE" ,colour = "Depth (m)") + 
  coord_map(xlim=c(-5, 7), ylim=c(54.5,61.5)) +
  scale_x_continuous(breaks = seq(-5,7,by=2.5)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
se.plot


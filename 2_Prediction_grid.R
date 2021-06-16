# File: Prediction_grid.R
# Author: Guillermo Martin 
# Template Created: Wed May 26 16:14:26 2021
# ---------------------------------------------------------------------------
# Description:
# Constructing database for predictions
# ---------------------------------------------------------------------------
library(sp)
library(raster)

load("C:/Users/ggonzales/Dropbox/GAM_cod_MS/Data_Clean_Model/prediction.data.Rdata")

ind.y=sort(unique(data.sub.bin$Year2))
data.pred<-c()
un=unique(data.sub.bin$ICES.corr)
le.pred=length(un)

lon.pred<-NULL
lat.pred<-NULL
for(g in 1:length(un)){
  id<-un[g]
  lon.pred[g]=ICES.polygon.win2@polygons[[id]]@labpt[1]
  lat.pred[g]=ICES.polygon.win2@polygons[[id]]@labpt[2]
}

s=0
for(i in 1:length(ind.y)){
  for(j in 1:length(month.id)){
  pred.ICES.corr=rep(un,1)
  pred.source=rep("prediction",le.pred)
  pred.Year=rep(NA,le.pred)
  pred.Trawlmin=rep(60,le.pred)  # predictions per two hour of trawling
  pred.Count=rep(NA,le.pred)
  pred.depth.geo=NA
  pred.ICES.areas=NA
  pred.lngtfactor.int=NA

for(f in 1:length(un)){ 
  ind.a=which(data.sub.bin$ICES.corr==un[f])
  pred.ICES.areas[f]=data.sub.bin$ICES.areas[ind.a[1]]
  ind.d=match(pred.ICES.areas[f],ID.depth)
  pred.depth.geo[f]=mean(subset(dep[[ind.d]],dep[[ind.d]]<0))
  }
 
pred.gear=rep(NA,le.pred)
pred.month=rep(j,le.pred)
pred.Year2=rep(i,le.pred)
pred.ShootLon=lon.pred
pred.ShootLat=lat.pred
pred.vessel=rep(NA,le.pred)
pred.vessel2=rep(NA,le.pred)
pred.YearMonth=rep(j+s,le.pred)

data.pred_it=data.frame(ShootLon=pred.ShootLon, ShootLat=pred.ShootLat, Count=pred.Count, Trawlmin=pred.Trawlmin, gear=pred.gear, month=pred.month, Year=pred.Year,Year2=pred.Year2, source=pred.source,ICES.areas=pred.ICES.areas,ICES.corr=pred.ICES.corr,vessel=pred.vessel,YearMonth=pred.YearMonth, depth.geo=pred.depth.geo, vessel2=pred.vessel2,lngtfactor.int=pred.lngtfactor.int)

data.pred=rbind(data.pred,data.pred_it)
  }
s=12*i
}

data.pred$depth.geo.bin<-NA
data.pred$CPUE<-NA
data.pred$lngtfactor<-NA
data.pred$J.day<-NA
data.pred$Depth<-NA
data.pred$day<-NA
data.pred$Haul.Number<-NA


####### binding positive, NULL observations, and predictions
data.sub.bin.p=rbind(data.sub.bin,data.pred)
data.sub.bin.pred<-data.sub.bin.p

############ binning depth.geo data in each 10 m (delta) for performing rw2
min.dg=min(data.sub.bin.pred$depth.geo,na.rm=T)-0.1  #to be sure to include the boundaries
max.dg=max(data.sub.bin.pred$depth.geo,na.rm=T)+0.1
delta=10
sq=seq(min.dg,max.dg,delta)
data.sub.bin.pred$depth.geo.bin=data.sub.bin.pred$depth.geo
for(i in 1:(length(sq)-1)){
  ind.dg=which(data.sub.bin.pred$depth.geo>=sq[i]&data.sub.bin.pred$depth.geo<sq[i+1])
  data.sub.bin.pred$depth.geo.bin[ind.dg]=round((sq[i]+sq[i+1])/2)
}
############ also adding a max lim for fishing of 200 m, 
############ acording to the observe depth in loogbooks (depth2)
ind.dep=which(data.sub.bin.pred$depth.geo.bin< -200)
data.sub.bin.pred$depth.geo.bin[ind.dep]=-200
#########################
data.sub.bin.pred$depth.geo.bin=round(data.sub.bin.pred$depth.geo.bin) # help precision

################### selecting and cutting up layers for prediction

################### getting rid of all layers first
ind.pred=which(data.sub.bin.pred$source=="prediction")

data.pred2<-data.sub.bin.pred[ind.pred,]#My prediction data
data.sub.bin.pred=data.sub.bin.pred[-ind.pred,]
##################


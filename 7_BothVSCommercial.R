# File: BothVSCommercial.R
# Author: Guillermo Martin 
# Template Created: Fri May 07 09:18:39 2021
# ---------------------------------------------------------------------------
# Description:
# Comparison of predictions of the model with and without scientific data
# ---------------------------------------------------------------------------
rm(list = ls())

library(ggplot2)
library(mgcv)
library(sp)
library(lubridate)

#All data
load(file.path("C:/Users/ggonzales/Dropbox/Backup/University of Aberdeen/",
               "University_of_Aberdeen/MSc projects/Data/",
               "Preparing data aggregated size classes/",
               "Data up to 50cm with all ceros",
               "MODELS1.3.2.RData"))
#No GOV data
load(file.path("C:/Users/ggonzales/Dropbox/Backup/University of Aberdeen/",
               "University_of_Aberdeen/MSc projects/Data/",
               "Preparing data aggregated size classes/",
               "Data up to 50cm with all ceros",
               "MODELS1.3.2_NoGOV.RData"))

#Prediction dataset
load(file.path("C:/Users/ggonzales/Dropbox/Backup/University of Aberdeen/",
               "University_of_Aberdeen/MSc projects/Data/",
               "Preparing data aggregated size classes/",
               "Data up to 50cm with all ceros",
               "prediction.data.Rdata"))

ls()


str(data.pred2)


# Function to predict the two models --------------------------------------
pred.model<-function(dat,source) {
  if(source=="Both"){
    model<-pregam1.3.2
    
  }
  if(source=="Only Commercial"){
    model<-pregam1.3.2_noGOV
  }
  
  preds.gam<-predict.gam(model, 
                         newdata = dat, 
                         type = "response",
                         se.fit = TRUE,
                         exclude="s(vessel2)")
  
  return(list(preds.gam$fit, preds.gam$se.fit))
}



# Layers of predictions ---------------------------------------------------
models.i<-c("pregam1.3.2","pregam1.3.2_noGOV")
month.i<-c(2,5,8,11)
lengt.i<-seq(10,50,by=10)
gear.i<-"MTN" #Other commercial gear could be chosen

preds_Both <- vector("list", length(month.i)) #For the storage of model outputs
preds_Com <- vector("list", length(month.i)) #For the storage of model outputs

# Year 2015
data.pred.sub<-data.pred2[data.pred2$Year2=='5' & data.pred2$month==2,]


# Old section to subset only to commercial grids --------------------------
# Subset only grids were the commercial data is recorded....

# Polygon to delimit commercial hauls:
#hull<-chull(data.pregam_noGOV[, c("ShootLon",
#                                  "ShootLat")])
#coords <- data.pregam_noGOV[c(hull, hull[1]), ]
#sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords[,c("ShootLon" ,
#                                                                "ShootLat")])), 
#                                         ID=1)))

# Overlay hull with commercial data
#data.pred.sub$ID<-over(SpatialPoints(data.frame(SI_LONG=data.pred.sub$ShootLon,
#                                                SI_LATI=data.pred.sub$ShootLat)),sp_poly)
#data.pred.sub.C<-subset(data.pred.sub,ID==1)

#plot to check
#plot(data.pred.sub.C[, c("ShootLon",
#                       "ShootLat")])
#lines(sp,col="green")
#points(data.pregam_noGOV[, c("ShootLon",
#                             "ShootLat")],add=T,col="red")





# Loop through layers of predictions for both models ----------------------

for (m in 1:length(month.i)) {
  for (l in 1:length(lengt.i)) {
    
    
    dat<-data.frame(depth.geo.bin=data.pred.sub$depth.geo.bin,
                    ShootLon=data.pred.sub$ShootLon,
                    ShootLat=data.pred.sub$ShootLat,
                    Year2='5',
                    month=month.i[m],
                    lngtfactor.int=lengt.i[l],
                    gear=gear.i,
                    Trawlmin=60,
                    vessel2=110)
    
    # Model with both data Sources                
    preds_Both[[m]][[l]]<- pred.model(dat,source = "Both")
    
    # Model with only commercial data
    preds_Com[[m]][[l]]<- pred.model(dat,source = "Only Commercial")
    
    
  }
}

  


# Extract predicted values ------------------------------------------------
#dim(data.pred.sub.C)[1]*length(month.i)*length(lengt.i)  ## 1357 cell in the spatial grid!

# Matrices sizes for data extraction
m.l<-length(month.i) # Month length
l.l<-length(lengt.i) # Length classes length 
d.l<-dim(data.pred.sub)[1] #Data length


ini<-data.frame(fit.Both=rep(NA,length.out=d.l),
                se.Both=rep(NA,length.out=d.l),
                fit.Com=rep(NA,length.out=d.l),
                se.Com=rep(NA,length.out=d.l))

for (m in 1:length(month.i)) {
  for(l in 1:length(lengt.i)){
    
    #Both data sources predictions
    ini$fit.Both <- preds_Both[[m]][[l]][[1]]
    ini$se.Both <- preds_Both[[m]][[l]][[2]]
    
    ini$fit.Com <- preds_Com[[m]][[l]][[1]]
    ini$se.Com <- preds_Com[[m]][[l]][[2]]
     
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


fits.all<-out
str(fits.all)

#Month in Labels
fits.all$Month.f<-month(fits.all$Month, label = TRUE, abbr = TRUE)

x11()
ggplot(fits.all,aes(x=fit.Both,y=fit.Com,
                    alpha=se.Com)) + 
  geom_point()+
  #geom_smooth(method="lm")+
  scale_y_continuous(limits = c(0,2))+
  scale_x_continuous(limits = c(0,2))+
  scale_alpha(range = c(.1,0.001))+
  facet_grid(Month.f~LengthClass)+
  labs(y="Predictions Only Commercial",
       x="Predictions Both",
       alpha="se(Only_Commercial)")+
  theme_bw()+
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title=element_text(size=16),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))

# Plot comper the SE of the predictions from the model with and withouth
# Scientific data
plot(fits.all$se.Com~fits.all$se.Both,ylim=c(0,40),xlim=c(0,40),asp=1)


# OLD ---------------------------------------------------------------------
# To check that the loop above is making the correct plots for Month/LengthClass
#data.pred.sub<-data.pred2[data.pred2$Year2=='5' & data.pred2$month==2,]

dat.1<-data.frame(depth.geo.bin=data.pred.sub$depth.geo.bin,
                ShootLon=data.pred.sub$ShootLon,
                ShootLat=data.pred.sub$ShootLat,
                Year2='5',
                month=2,
                lngtfactor.int=30,
                gear="MTN",
                Trawlmin=60,
                vessel2=110)

preds.Both<-predict.gam(pregam1.3.2, 
                       newdata = dat.1, 
                       type = "response",
                       se.fit = TRUE,
                       exclude="s(vessel2)")

dat.1<-data.frame(depth.geo.bin=data.pred.sub$depth.geo.bin,
                  ShootLon=data.pred.sub$ShootLon,
                  ShootLat=data.pred.sub$ShootLat,
                  Year2='5',
                  month=2,
                  lngtfactor.int=30,
                  gear="MTN",
                  Trawlmin=60,
                  vessel2=110)

preds.Com<-predict.gam(pregam1.3.2_noGOV, 
                        newdata = dat.1, 
                        type = "response",
                        se.fit = TRUE,
                        exclude="s(vessel2)")

df<-data.frame(fit_both=preds.Both$fit,
               fit_Com=preds.Com$fit)

x11()
plot(df$fit_Com~df$fit_both,asp=1,xlim=range(df$fit_both),
     ylim=range(df$fit_both))



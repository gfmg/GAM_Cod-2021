# File: GAM 1.3.2.R
# Author: Guillermo Martin 
# Template Created: Wed Jun 16 15:20:39 2021
# ---------------------------------------------------------------------------
# Description:
# Implementing GAM model with vessel random effect
# ---------------------------------------------------------------------------

library(mgcv)
library(ggplot2)

#Year 2 as a factor

setwd("")

load("./data.pregam.RData")

rm(list=setdiff(ls(), "data.pregam"))
gc()


# Changing columns accordingly: 
data.pregam$vessel2<-factor(data.pregam$vessel2)
data.pregam$month<-as.numeric(data.pregam$month)
data.pregam$lngtfactor.int<-as.numeric(data.pregam$lngtfactor.int)
data.pregam$Year2<-factor(data.pregam$Year2) #Year has to be a factor to see differences accros years.

# Data without scientific surveys
data.pregam_noGOV<-subset(data.pregam,!gear %in% 'GOV')
data.pregam_noGOV$gear<-as.factor(data.pregam_noGOV$gear)

ctrl <- gam.control(trace = TRUE)

# Model with all data
# Caution, very time consuming
gam1.3.2<-gam(Count ~ 
                ti(ShootLon,ShootLat,month,lngtfactor.int,bs=c("ts","ts","cc","ts"),k=-1)+
                ti(month,lngtfactor.int,depth.geo.bin,bs=c("cc","tp","tp"),k=-1)+
                ti(lngtfactor.int,depth.geo.bin,bs=c("tp","tp"),k=-1)+
                ti(month,lngtfactor.int,bs=c("cc","tp"),k=-1)+
                s(lngtfactor.int,by=gear,bs="tp",k=-1)+
                ti(depth.geo.bin,bs="tp",k=-1)+
                ti(month,bs="cc",k=-1)+
                s(vessel2,bs="re")+
                gear+Year2+offset(log(Trawlmin)),
              data=data.pregam,family=nb(),method='REML',
              control = ctrl)

# Model with only commercial data
gam1.3.2_noGOV<-gam(Count ~ 
                      ti(ShootLon,ShootLat,month,lngtfactor.int,bs=c("ts","ts","cc","ts"),k=-1)+
                      ti(month,lngtfactor.int,depth.geo.bin,bs=c("cc","tp","tp"),k=-1)+
                      ti(lngtfactor.int,depth.geo.bin,bs=c("tp","tp"),k=-1)+
                      ti(month,lngtfactor.int,bs=c("cc","tp"),k=-1)+
                      s(lngtfactor.int,by=gear,bs="tp",k=-1)+
                      ti(depth.geo.bin,bs="tp",k=-1)+
                      ti(month,bs="cc",k=-1)+
                      s(vessel2,bs="re")+
                      gear+Year2+offset(log(Trawlmin)),
                    data=data.pregam_noGOV,family=nb(),method='REML',
                    control = ctrl)


# Load model outputs if wanted
# load("./MODELS1.3.2.RData")

gam.check(gam1.3.2)
summary(gam1.3.2)

#Extract residuals
res.gam1.3.2.pearson<-resid(gam1.3.2,type='pearson')
res.gam1.3.2.deviance<-resid(gam1.3.2,type='deviance')
fitted.gam1.3.2<-fitted(gam1.3.2)


#Residuals vs covariates

fitvsresid<-ggplot()+
geom_point(aes(y=res.gam1.3.2.deviance,x=log(fitted.gam1.3.2)))+
ylab('Deviance residuals')+
xlab('Linear predictor')+
theme(axis.title.x=element_text(margin=margin(10,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title=element_text(size=12,face="bold"))  
fitvsresid


vessel.resid<-ggplot()+
geom_boxplot(data=data.pregam,aes(x=as.factor(vessel2),
                                  y=res.gam1.3.2.deviance))+
ylab('')+
xlab('Vessel')+
theme(axis.title.x=element_text(margin=margin(10,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
      axis.title=element_text(size=12,face="bold"),
      axis.ticks = element_blank(),
      axis.text.x = element_blank()) 
vessel.resid

year.resid<-ggplot()+
geom_boxplot(data=data.pregam,aes(x=as.factor(Year),y=res.gam1.3.2.deviance))+
ylab('')+
xlab('Year')+
theme(axis.title.x=element_text(margin=margin(10,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title=element_text(size=12,face="bold"))
year.resid   

length.resid<-ggplot()+
geom_boxplot(data=data.pregam,aes(x=lngtfactor.int,y=res.gam1.3.2.deviance,group=lngtfactor.int),alpha=0.02)+
ylab('Deviance residuals')+
xlab('Length (cm)')+
theme(axis.title.x=element_text(margin=margin(10,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title=element_text(size=12,face="bold"))
length.resid

month.resid<-ggplot()+
geom_boxplot(data=data.pregam,aes(x=as.factor(month),y=res.gam1.3.2.deviance),alpha=0.02)+
ylab('')+
xlab('Month')+
theme(axis.title.x=element_text(margin=margin(10,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title=element_text(size=12,face="bold"))
month.resid

gear.resid<-ggplot()+
geom_boxplot(data=data.pregam,aes(x=as.factor(gear),y=res.gam1.3.2.deviance),alpha=0.02)+
ylab('Deviance residuals')+
xlab('Gear')+
theme(axis.title.x=element_text(margin=margin(10,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title=element_text(size=12,face="bold"))
gear.resid


depth.resid<-ggplot()+
geom_point(data=data.pregam,aes(x=depth.geo.bin,y=res.gam1.3.2.deviance),alpha=0.02)+
ylab('Deviance residuals')+
xlab('Depth (m)')+
theme(axis.title.x=element_text(margin=margin(10,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title=element_text(size=12,face="bold"))
depth.resid



#All residual plots together
library(grid)
library(gridExtra)

lay=rbind(c(1,1,2,2),
    c(3,3,4,4),
    c(5,5,6,6))
   
    
grid.arrange(fitvsresid,year.resid ,
             length.resid,month.resid,
             depth.resid,vessel.resid,
             layout_matrix = lay)


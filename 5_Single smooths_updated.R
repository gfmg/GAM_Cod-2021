# File: Single_smooths_Updated.R
# Author: Guillermo Martin 
# Template Created: Fri May 14 17:29:46 2021
# ---------------------------------------------------------------------------
# Description:
# Plotting marginal effects for Depth, Month and Year
# ---------------------------------------------------------------------------

library(mgcv)
library(ggplot2)

#Making plot of gear selectivity

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


 load(file.path("C:/Users/ggonzales/Dropbox/Backup/University of Aberdeen/",
                "University_of_Aberdeen/MSc projects/Data/",
                "Preparing data aggregated size classes/",
                "Data up to 50cm with all ceros",
                "prediction.data.Rdata"))


# Depth smooth ------------------------------------------------------------
#Extract depth smooth for all data and data without GOV
#Model with all data
 gam_All<-data.frame(x=rep(NA,100),
                     fit=rep(NA,100),
                     Depthupr=rep(NA,100),
                     Depthlwr=rep(NA,100),
                     model=rep(NA,100))
 
P.smooths<-plot(pregam1.3.2,scale=0,seWithMean = TRUE,shade=T,select=15)

gam_All$x<-P.smooths[[15]]$x
gam_All$fit<-P.smooths[[15]]$fit[,1]
gam_All$Depthupr<-P.smooths[[15]]$fit[,1] + P.smooths[[15]]$se
gam_All$Depthlwr<-P.smooths[[15]]$fit[,1] - P.smooths[[15]]$se
gam_All$model<-"All"

depth.df<-gam_All

depth.plot.gam<-
  ggplot(data=depth.df,aes(x=x, y=fit))+
  geom_line()+
  geom_ribbon(aes(ymin=Depthlwr,ymax=Depthupr,x=x),alpha=0.4)+
  labs(x="Depth",y="Marginal effect Depth",fill="Data Source",
       colour="Data Source")+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title=element_text(size=16),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")
depth.plot.gam

depth.plot.gam<- depth.plot.gam +
  geom_rug(data=data.pregam,aes(x=depth.geo,y=0),sides="b",
           inherit.aes = F,position = "jitter",colour="black")



# Month smooth ------------------------------------------------------------
#Model with all data
gam_All<-data.frame(x=rep(NA,100),
                    fit=rep(NA,100),
                    Monthupr=rep(NA,100),
                    Monthlwr=rep(NA,100),
                    model=rep(NA,100))

P.smooths<-plot(pregam1.3.2,scale=0,seWithMean = TRUE,select=16)

gam_All$x<-P.smooths[[16]]$x
gam_All$fit<-P.smooths[[16]]$fit[,1]
gam_All$Monthupr<-P.smooths[[16]]$fit[,1] + (P.smooths[[16]]$se)
gam_All$Monthlwr<-P.smooths[[16]]$fit[,1] - (P.smooths[[16]]$se)
gam_All$model<-"All"

month.df<-gam_All

month.plot.gam<-
  ggplot(data=month.df,aes(x=x, y=fit))+
  geom_line()+
  geom_ribbon(aes(ymin=Monthlwr,ymax=Monthupr,x=x),alpha=0.4)+
  labs(x="Month",y="Marginal effect Month",fill="Data Source",
       colour="Data Source")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title=element_text(size=16),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")
month.plot.gam




# Year differences --------------------------------------------------------
#All data
Y.smooth<-plot(pregam1.3.2,scale=0,all.terms=T,select=19)

###### 2141 cells
data.pred.subY3M2<-data.pred2[data.pred2$Year2==3 & data.pred2$month==2,]
d.l<-dim(data.pred.subY3M2)[1]

year.i<-seq(1,5,by=1)
month.i<-seq(1,12,by=1)

preds_Both <- vector("list", length(month.i)) 

for (y in 1:length(year.i)) {
  for (m in 1:length(month.i)) {
    
    dat<-data.frame(depth.geo.bin=mean(data.pred.subY3M2$depth.geo.bin),
                    ShootLon=mean(data.pred.subY3M2$ShootLon),
                    ShootLat=mean(data.pred.subY3M2$ShootLat),
                    Year2=year.i[y],
                    month=month.i[m],
                    lngtfactor.int=median(data.pregam$lngtfactor.int),
                    gear='GOV',Trawlmin=mean(data.pregam$Trawlmin),
                    vessel2=87)
    
    # Model with both data Sources                
    preds_Both[[y]][[m]]<- predict(pregam1.3.2, newdata = dat, 
                                   type = "link", se = TRUE,
                                   exclude=c("s(vessel2)"))
  }
}


#Extract predictions
ini<-data.frame(fit.Both=rep(NA,length.out=1),
                se.Both=rep(NA,length.out=1))

for (y in 1:length(year.i)) {
  for(m in 1:length(month.i)){
    
    #Both data sources predictions
    ini$fit.Both <- preds_Both[[y]][[m]][[1]]
    ini$se.Both <- preds_Both[[y]][[m]][[2]]
    
    ini$Year2<-year.i[y]
    ini$Month<-month.i[m]
    ini$Data<-"Both"
    
    if (y==1  &&  m == 1) {
      out=ini
    } else{
      out=rbind(ini,out)
    }
  }
}

year.df<-out

# Renaming Year levels
year.df$Year2<-factor(year.df$Year2)
levels(year.df$Year2)[levels(year.df$Year2)=="1"] <- '2011'
levels(year.df$Year2)[levels(year.df$Year2)=="2"] <- "2012"
levels(year.df$Year2)[levels(year.df$Year2)=="3"] <- "2013"
levels(year.df$Year2)[levels(year.df$Year2)=="4"] <- "2014"
levels(year.df$Year2)[levels(year.df$Year2)=="5"] <- "2015"

year.plot.final<-ggplot(data=year.df,aes(x=as.factor(Year2),y=fit.Both))+
  geom_boxplot()+
  ylab("Marginal effect Year")+
  xlab("Year")+
  labs(x="Year",y="Marginal effect Year",
       fill="Data Source",colour="Data Source")+
  scale_fill_grey(start = .5,end=.1)+
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title=element_text(size=16),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = c(0.8, 0.1),
        legend.text = element_text(size=14),
        legend.title = element_text(size=15))
year.plot.final



# Combining all plots together --------------------------------------------
library(grid)
library(gridExtra)

lay <- rbind(c(1,1,2,2),
             c(3,3,2,2))

grid.arrange(month.plot.gam,year.plot.final,depth.plot.gam,layout_matrix = lay)

# File: F5_Depth_Length.R
# Author: Guillermo Martin 
# Template Created: Wed Jun 30 12:18:54 2021
# ---------------------------------------------------------------------------
# Description:
# Figure 5 of the manuscript
# ---------------------------------------------------------------------------
library(ggplot2)
library(reshape)
library(metR)
library(colorRamps)


#Raw data
load(file.path(".",
               "data.pregam.RData"))

#All data
load(file.path(".",
               "MODELS1.3.2.RData"))



#Standardize for each length separately

# Predicting for each  months and Lengths class and standardizing fitted values 
# for each length class across depths separately

months<-seq(1,12,by=1)
df.STD.fit<-NULL
df.STD.fit<-data.frame(df.STD.fit)
for (m in 1:length(months)) {
  df.m<-NULL
  data.frame(df.m)
  for(i in seq(2,50,l=100)){
    dat<-data.frame(lngtfactor.int= i, depth.geo.bin= seq(-7, -197, l= 100),
                    ShootLon=mean(data.pregam$ShootLon),
                    ShootLat=mean(data.pregam$ShootLat),Year2=2,
                    month=m,gear='GOV',
                    Trawlmin=60,vessel2=110)
    
    dat.pred<- predict(gam1.3.2, newdata = dat, type = "response", 
                       se = TRUE,exclude="s(vessel2)")
    
    dat.pred$std.fit<-((dat.pred$fit-min(dat.pred$fit))/(max(dat.pred$fit)-min((dat.pred$fit)))) 
    
    df.m=rbind(df.m,dat.pred$std.fit)
    
  } 
  df.m<-data.frame(df.m)
  df.m.std.fit<-stack(df.m)
  df.m.std.fit<- df.m.std.fit[,-2]
  
  if (m==1) {
    df.STD.fit=df.m.std.fit
  }
  if (m>1) {
    df.STD.fit=c(df.STD.fit,df.m.std.fit)}
}  


# Create dataframe to include standarized data
for (m in 1:length(months)){
  marg.3d<-data.frame(expand.grid(lngtfactor.int= seq(0,50, l=100), 
                                  depth.geo.bin=
                                    seq(-7, -197, l= 100)),
                      ShootLon=mean(data.pregam$ShootLon),
                      ShootLat=mean(data.pregam$ShootLat),
                      Year2=2,month=m,gear='GOV',Trawlmin=60,vessel2=110)
  if (m==1) {
    marg.3d.f=marg.3d
  }
  if (m>1) {
    marg.3d.f=rbind(marg.3d.f,marg.3d)}
}



marg.3d.paper<-cbind(marg.3d.f,df.STD.fit)
names(marg.3d.paper)[names(marg.3d.paper) == 'df.STD.fit'] <- 'std.fit'
marg.3d.paper$Month_NEW<-month.abb[marg.3d.paper$month]


marg.3d.paper$Month_NEW <- factor(marg.3d.paper$Month_NEW, 
                                  levels = c("Jan", "Feb", "Mar","Apr",
                                             "May","Jun","Jul","Aug",
                                             "Sep","Oct","Nov","Dec"))


Plot_rug<-data.pregam[data.pregam$Count > 0,c("month",
                                               "depth.geo.bin",
                                               "lngtfactor.int",
                                               "Count")]
Plot_rug<-untable(Plot_rug,num=Plot_rug$Count)


Plot_rug$Month_NEW<-month.abb[Plot_rug$month]

Plot_rug$Month_NEW <- factor(Plot_rug$Month_NEW, 
                             levels = c("Jan", "Feb", "Mar","Apr",
                                        "May","Jun","Jul","Aug",
                                        "Sep","Oct","Nov","Dec"))



x11()
contour1<-ggplot(marg.3d.paper, 
                 aes(x=lngtfactor.int,y=depth.geo.bin,z=std.fit,alpha=std.fit))+
  geom_contour_fill(size = 40,bins=500,binwidth=100)+
  geom_rug(data=Plot_rug, aes(x=lngtfactor.int,y=depth.geo.bin)
           ,inherit.aes = F,position = "jitter",alpha=1/10,color="green") +
  scale_fill_gradientn(colours = matlab.like(500),breaks=c(0,0.25,0.5,0.75,1),
                       limits = c(0,1))+
  labs(y= "Depth (m)", x = "Length (cm)", 
       fill="Standardised\nmarginal\neffect")+
  facet_wrap(.~Month_NEW)+
  scale_x_continuous(limits=c(min(marg.3d.paper$lngtfactor.int),
                              max(marg.3d.paper$lngtfactor.int)),expand=c(0,0)) +
  scale_y_continuous(limits=c(min(marg.3d.paper$depth.geo.bin),
                              max(marg.3d.paper$depth.geo.bin)),expand=c(0,0))+
  theme_bw()+
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=rel(1.4), face="bold",
                                margin = margin(t = 25, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size=rel(1.4), face="bold",
                                margin = margin(t = 0, r = 25, b = 0, l = 0)), 
    axis.text = element_text(color="black", size=rel(1.3)),
    axis.text.y  = element_text(hjust=1),
    legend.title=element_text(size=rel(1.4),face="bold"),
    strip.text = element_text(size=rel(1.4))) 
contour1

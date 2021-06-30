# File: Godness of fit figure.R
# Author: Guillermo Martin 
# Template Created: Fri May 14 17:47:17 2021
# ---------------------------------------------------------------------------
# Description:
# Figure plotting observed vs predicted values
# ---------------------------------------------------------------------------
rm(list = ls())

library(ggplot2)

#All data
load(file.path(".",
               "MODELS1.3.2.RData"))


#Extract residuals and fitted values
res.pregam1.3.2pearson<-resid(pregam1.3.2,type='pearson')
res.pregam1.3.2deviance<-resid(pregam1.3.2,type='deviance')
fitted.pregam1.3.2<-fitted(pregam1.3.2)
#

############Observed vs predicted values + 1
df<-data.frame(Obs=log((data.pregam$Count+1)))
df$Pred<- log((fitted.pregam1.3.2+1))

lm_eqn <- function(df){
  m <- lm(Pred ~ Obs, df);
  eq <- substitute(#italic(y) == a + b %.% italic(x)*","
    ~~italic(R)^2~"="~r2, 
    list(#a = format(coef(m)[1], digits = 2), 
      #b = format(coef(m)[2], digits = 2), 
      r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
log((data.pregam$Count+1))


Obsvspred<-ggplot()+
  geom_point(aes(x=df$Pred,
                 y=df$Obs,alpha=0.01))+
  scale_y_continuous(limit=c(0,8))+    
  labs(x = "log(Predicted + 1)",y="log(Observed + 1)")+
  geom_text(aes(x =1, y =6),size=15, label = lm_eqn(df), parse = TRUE)+
  geom_abline(intercept = 0, slope = 1,colour='black',linetype="dashed")+
  theme_bw()+
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title=element_text(size=14))
Obsvspred

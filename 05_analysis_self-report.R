#Code aim: to examine the corr for ADHD and IQ measures, and to produce a single ADHD factor
#Contributors: Nitzan Shahar

# initiate ----------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(magrittr)
library(tidyr)
library(tidyverse)
library(ggplot2)
maindir<-'myfolder/03_data/02_aggregated_data/'


# adhd scores and pca -----------------------------------------------------

#housekeeping
load(paste(maindir,'asrs.Rdata',sep=""))
load(paste(maindir,'wurs.Rdata',sep=""))
load(paste(maindir,'bis.Rdata',sep=""))
df<-plyr::join_all(list(asrs,wurs,bis), by='prolific_id', type='left')
df[,-1]<-data.frame(na.omit(df[,-1]))
df[,-1]<-data.frame(scale(df[,-1]))
summary(df)
df%<>% select(prolific_id,asrs6,asrs.partb,wurs,attention, motor, nonplanning)

#corr matrix
round(cor(df[,-1]),2)

#pca
library(psych)
library(nFactors)
fit.sr <- principal(df[,-1],nfactors = 1,rotate = 'promax',scores=TRUE)
scree(df[,-1],factors=TRUE,pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 
parallel<-fa.parallel(df[,-1], fm="ml", fa="pc", main = "Parallel Analysis Scree Plots",
                      n.iter=20,error.bars=T,ylabel=NULL,show.legend=TRUE,sim=TRUE)
print(fit.sr$loadings,cutoff = 0.5)
adhd=data.frame(prolific_id=df$prolific_id,adhd_factor=fit.sr$scores[,1])
save(adhd,file='myfolder/03_data/02_aggregated_data/adhd_factor.Rdata')

# adhd scores cor with iq -----------------------------------------------------
rm(list=ls())
maindir<-'myfolder/03_data/02_aggregated_data/'
load(paste(maindir,'adhd_factor.Rdata',sep=""))
load(paste(maindir,'icar.Rdata',sep=""))

df<-na.omit(merge(icar,adhd,by=('prolific_id')))

cor.test(df$iq,df$adhd_factor)
ggplot(df,aes(x=iq,y=adhd_factor))+geom_point(color='navy')+geom_smooth(method='lm')


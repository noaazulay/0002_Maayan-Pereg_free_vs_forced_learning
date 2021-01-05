rm(list=ls())
library(dplyr)
library(magrittr)
library(tidyr)
maindir<-'myfolder/03_data/02_aggregated_data/'
load(paste(maindir,'icar.Rdata',sep=""))
load(paste(maindir,'asrs.Rdata',sep=""))
load(paste(maindir,'wurs.Rdata',sep=""))
load(paste(maindir,'bis.Rdata',sep=""))
load(paste(maindir,'oci.Rdata',sep=""))
load(paste(maindir,'tab.Rdata',sep=""))

####teacher-student model-agnostic scores ---------------------------------------------
detach(package:plyr)
#pObey (tendency to take teacher advice across all other factors)
names(tab)
tab2<-
tab%>%
  filter(reveal==1)%>%
  group_by(prolific_id,session)%>%
  summarise(p.obey=mean(obey))%>%
  spread(session,p.obey)%>%
  na.omit()


cor(tab2$`1`,tab2$`2`)
plot(tab2$`1`,tab2$`2`)
m  <- glmer(obey ~ 1 + (1 | prolific_id), data = tab, family = binomial(link = "logit"))


m  <- glmer(stay.card ~ 1 + rw_1back*obey_1back+(1+ rw_1back*obey_1back | prolific_id), data = tab[tab$reveal_1back==1 & tab$reveal==0 ,], family = binomial(link = "logit"))

summary(m)

library(lme4)
i=tab$reveal_1back==1&tab$reveal==1& tab$reoffer==1
m  <- glmer(obey ~ 1 + rw_1back +session+(1+ rw_1back | prolific_id), data = tab[i==T,], family = binomial(link = "logit"))
summary(m)
m1 <- glmer(obey ~ 1 + rw_1back +(1+ rw_1back | prolific_id), data = tab[tab$session==1&i==T,], family = binomial(link = "logit"))
m2 <- glmer(obey ~ 1 + rw_1back +(1+ rw_1back | prolific_id), data = tab[tab$session==2&i==T,], family = binomial(link = "logit"))
df1<-data.frame(prolific_id=rownames(ranef(m1)$prolific_id),x1=ranef(m1)$prolific_id)
df2<-data.frame(prolific_id=rownames(ranef(m2)$prolific_id),x2=ranef(m2)$prolific_id)
df<-merge(df1,df2,by=c('prolific_id'))
plot(df$X.Intercept..x,df$X.Intercept..y)
cor(df$X.Intercept..x,df$X.Intercept..y)

summary(m1)

m1 <- glmer(stay.card ~ 1 + rw_1back*obey_1back + (1+rw_1back*obey_1back | prolific_id), data = tab[tab$reveal_1back==1,], family = binomial(link = "logit"),nAGQ=0)
tab2<-data.frame(prolific_id=rownames(ranef(m)$prolific_id),ob1=ranef(m)$prolific_id[,'rw_1back:obey_1back'])
hist(tab2$ob1)
####merge sets---------------------------------------------
library(plyr)
df<-join_all(list(icar,asrs,wurs,bis,oci,tab2), by='prolific_id', type='left')
df<-na.omit(df[,-1])
df<-data.frame(df)
df<-data.frame(scale(df))
summary(df)
df%<>% select(iq,asrs6,wurs,bis,oci,ob1)
library("Hmisc")
df%>% with(rcorr(as.matrix(.)))
rcorr(as.matrix(df))
with(df,plot(iq,p.obey))

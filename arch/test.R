library(data.table)
library(dplyr)

####
asrs$asrs<-rowMeans(sapply(asrs[,1:20],function(v) {as.numeric(v)}))
bis$bis<-rowMeans(sapply(bis[,1:30],function(v) {as.numeric(v)}))
wurs$wurs<-rowMeans(sapply(wurs[,1:25],function(v) {as.numeric(v)}))

sr<-merge(asrs[,c('prolific_id','asrs')],bis[,c('prolific_id','bis')],by=('prolific_id'))
sr<-merge(sr,wurs[,c('prolific_id','wurs')],by=('prolific_id'))
cor(sr[,-1])


###
tab<-na.omit(tab)
tab$pv_rw<-shift(tab$rw, n=1, fill=1, type=c("lag"), give.names=FALSE)
tab$stay <-tab$ch==shift(tab$ch, n=1, fill=1, type=c("lag"), give.names=FALSE)
tab$ob   <-0
tab$ob[tab$reveal_teacher_decision==1 & tab$obey==F]<-1
tab$ob[tab$reveal_teacher_decision==1 & tab$obey==T]<-2
tab$ob<-factor(tab$ob)
tab$ob_pv<-shift(tab$ob, n=1, fill=1, type=c("lag"), give.names=FALSE)
tab$reoffer<-0
tab$reoffer[shift(tab$ch, n=1, fill=1, type=c("lag"), give.names=FALSE)==tab$frcA |
            shift(tab$ch, n=1, fill=1, type=c("lag"), give.names=FALSE)==tab$frcB ]<-1

df<-
tab%>%
  group_by(subj,ob,pv_rw)%>%
  summarise(pStay=mean(stay))
cor(sr)

library(lme4)
m1 <- glmer(stay ~ ob_pv*pv_rw + (ob_pv*pv_rw | prolific_id), data = tab[tab$reoffer==1&tab$ob==0,], family = binomial(link = "logit"))
library(car)
Anova(m1)
df2<-data.frame(prolific_id=rownames(ranef(m1)$prolific_id),ob1=ranef(m1)$prolific_id[,'ob_pv1:pv_rw'],ob2=ranef(m1)$prolific_id[,'ob_pv2:pv_rw'])
df2<-merge(df2,sr,by=('prolific_id'))
cor(df2[,-1])
df2$imp<-rowMeans(cbind(df2$asrs,df2$bis,df2$wurs))
m2<-lm(scale(imp)~scale(ob1)+scale(ob2),data=df2)
Anova(m2)

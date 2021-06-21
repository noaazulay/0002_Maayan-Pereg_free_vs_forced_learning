rm(list=ls())
library(dplyr)
#library(plyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(ggeffects)

source('myfolder/01_functions/logit2prob.R')

maindir<-'myfolder/03_data/02_aggregated_data/'
load(paste(maindir,'tab.Rdata',sep=""))
load(paste(maindir,'adhd_factor.Rdata',sep=""))
load(paste(maindir,'icar.Rdata',sep=""))
load(paste(maindir,'crt.Rdata',sep=""))

load(paste(maindir,'subjects_that_completed_session1_and_2.Rdata',sep=""))

#keep only these how had both sessions
tab%<>%filter(prolific_id%in%subj.list)

tab<-
  tab%>%
  mutate(running_trial=ifelse(block==2,trial+129,ifelse(block==3,trial+259,trial)))

tab<-
  tab%>%
  mutate(trial_scale=trial/130, block_scale=block/3)


m <- glmer(acc.player ~ trial_scale*block_scale + (trial_scale*block_scale| prolific_id), data = tab, family = binomial(link = "logit"),nAGQ=0)

summary(m)
coef(m)

hist(ranef(m)$prolific_id[,"trial_scale"])

sum(ranef(m)$prolific_id[,"trial_scale"]>0)


df <- data.frame(ranef(m)$prolific_id)
df <- tibble::rownames_to_column(df, "prolific_id")

tab$trial_effect<-NA
for (ind in unique(tab$prolific_id)) {
  if (sum(df$prolific_id == ind)>0){
    tab[tab$prolific_id == ind, "trial_effect"] = df$trial_scale[df$prolific_id == ind]
  }
}

tab$trial_effect_bool <- (tab$trial_effect>0)*1

# Variability for pObey (tendency to take teacher advice across all other factors)
df<-
  tab%>%
  filter(reveal==1)%>%
  group_by(session,block,running_trial)%>%
  summarise(p.obey=mean(obey))%>%
  spread(session,p.obey)%>%
  na.omit()%>%as.data.frame()

ggplot(df,aes(x=running_trial,y=session1))+geom_point(color=df$block)
ggplot(df,aes(x=running_trial,y=session2))+geom_point(color=df$block)

# Variability for acc 
df<-
  tab%>%
  filter()%>%
  group_by(session,block,running_trial)%>%
  summarise(acc=mean(acc.player))%>%
  spread(session,acc)%>%
  na.omit()%>%as.data.frame()

ggplot(df,aes(x=running_trial,y=session1))+geom_point(color=df$block)
ggplot(df,aes(x=running_trial,y=session2))+geom_point(color=df$block)

# Variability for choice 

ggplot(data=tab[tab$session=="session1",], aes(x=running_trial, y=ch))+
  geom_line() +
  facet_wrap(~prolific_id)


ggplot(tab[tab$session=="session1",], aes(x = running_trial, y = ch)) +
  geom_line() +
  labs(
    title = "",
    x = "Trial",
    y = "Choice")+
  ylim(0,3)




#test-retest by trial effect

df<-
  tab%>%
  filter(reveal==1)%>%
  group_by(prolific_id,trial_effect_bool,session)%>%
  summarise(p.obey=mean(obey))%>%
  #spread(session)%>%
  na.omit()%>%as.data.frame()

df <- pivot_wider(df,names_from = session, values_from = p.obey)

#for all subjets:
ggplot(df,aes(x=session1,y=session2))+geom_point(color='navy')+geom_smooth(method='lm')
r=cor.test(df$session1,df$session2)
print(paste('test-retest reliability for pObey is',round(r$estimate,2),'(p=',round(r$p.value,3),')'))

#by trial_effect group:
r.bad <- cor.test(df[df$trial_effect_bool == 0,]$session1,df[df$trial_effect_bool == 0,]$session2)
r.good <- cor.test(df[df$trial_effect_bool == 1,]$session1,df[df$trial_effect_bool == 1,]$session2)

print(paste('test-retest reliability for pObey is',round(r.good$estimate,2),'(p=',round(r.good$p.value,3),') for good subjects and',round(r.bad$estimate,2),'(p=',round(r.bad$p.value,3),') for bad subjects'))


ggplot(data = df, aes(x = session1,  y = session2, color = as.factor(trial_effect_bool))) +
  geom_point() +
  geom_smooth(method = "lm", se = T)


# p.obey test-retest reliability - needs more work, this model is wrong

#for first 25 trials:

df<-
  tab%>%
  filter(reveal==1, trial<65)%>%
  group_by(prolific_id,session)%>%
  summarise(p.obey=mean(obey))%>%
  #spread(session)%>%
  na.omit()%>%as.data.frame()

df <- pivot_wider(df,names_from = session, values_from = p.obey)

ggplot(df,aes(x=session1,y=session2))+geom_point(color='navy')+geom_smooth(method='lm')
r=cor.test(df$session1,df$session2)

print(paste('test-retest reliability for pObey in first 25 trials is',round(r$estimate,2),'(p=',round(r$p.value,3),')'))


#test-retest by trial effect



m <- glmer(obey ~ session + (session| prolific_id), data = tab%>%filter(reveal==1), family = binomial(link = "logit"),nAGQ=1)

summary(m)
ggpredict(m, c("session"),digits=0) %>% plot()

## complete model

#view stay.card as function of reward in N, free/obey in N, and free/inst in N=1 (where inst is for prev choice)

data = tab%>%filter(reoffer==1,cond_1back!="disobey",acc.teacher==1,acc.teacher_1back==1,reoffer.choice==1,trial>1)

m <- glmer(stay.card ~ rw_1back*cond_1back*reveal + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)
Anova(m)

ggpredict(m, c("rw_1back","cond_1back","reveal"),digits=0) %>% plot()

write.csv(data,"complete_dat.csv")  # for jasp graph

## complete model

#view stay.card as function of reward in N, free/obey in N, and free/inst in N=1 (where inst is for prev choice)

data = tab%>%filter(reoffer==1,cond_1back!="disobey",acc.teacher==1,acc.teacher_1back==1,reoffer.choice==1,trial>1)

m <- glmer(stay.card ~ rw_1back*cond_1back*reveal*trial_scale + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)
Anova(m)

ggpredict(m, c("rw_1back","cond_1back","reveal"),digits=0) %>% plot()

write.csv(data,"complete_dat_trial.csv")  # for jasp graph

# model continue: focus on reveal=1
data = tab%>%filter(reoffer==1,cond_1back!="disobey",acc.teacher==1,acc.teacher_1back==1,reoffer.choice==1,trial>1, reveal==1)

m <- glmer(stay.card ~ rw_1back*cond_1back*trial_scale + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)
Anova(m)

ggpredict(m, c("rw_1back","cond_1back","reveal"),digits=0) %>% plot()

## learning model

data = tab%>%filter(reoffer==1,reveal_1back==1,obey_1back==1,acc.teacher==1)

data %<>%
  
  mutate(cond_obey= ifelse(reveal==0,"free",ifelse(reoffer.choice==1,"reoffer_prev_ch","reoffer_new_ch"))
         
  )


m <- glmer(stay.card ~ rw_1back*cond_obey*trial_scale + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)
Anova(m)

ggpredict(m, c("rw_1back", "cond_obey"),digits=0) %>% plot()

write.csv(data,"learning_dat_trial.csv")  # for jasp graph

## decision model
data = tab%>%filter(reoffer==1,reveal_1back==0,acc.teacher==1)
data %<>%
  
  mutate(cond_obey= ifelse(reveal==0,"free",ifelse(reoffer.choice==1,"reoffer_prev_ch","reoffer_new_ch"))
         
  )

m <- glmer(stay.card ~ rw_1back*cond_obey*trial_scale + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)
summary(m)
Anova(m)

ggpredict(m, c("rw_1back", "cond_obey"),digits=0) %>% plot()

write.csv(data,"decision_data_trial.csv") # for jasp graph


# stay obey model

data = tab%>%filter(reveal==1,!reoffer,reveal_1back==1)

data<-
  tab%>%
  filter(reveal==1,!reoffer,reveal_1back==1)%>%
  group_by(prolific_id,rw_1back,obey_1back)%>%
  summarise(stay.obey=mean(stay.obey))%>%
  #spread(session,p.obey)%>%
  na.omit()%>%as.data.frame()

m <- glmer(stay.obey ~ rw_1back*obey_1back + (rw_1back*obey_1back | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)
ggpredict(m, c("rw_1back", "obey_1back"),digits=0) %>% plot()

#with Trial
m <- glmer(stay.obey ~ rw_1back*obey_1back*trial_scale + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)
Anova(m)
ggpredict(m, c("obey_1back", "trial_scale"),digits=0) %>% plot()



#model

m   <- glmer(stay.card ~ 1+rw_1back+rw_1back:reveal_1back + (1+rw_1back+rw_1back:reveal_1back | prolific_id), data = tab%>%filter(!reveal,reoffer), family = binomial(link = "logit"),nAGQ=0)
Anova(m)
df<-data.frame(prolific_id=rownames(coef(m)$`prolific_id`),pStay.free=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]),
               pStay.reveal=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]+coef(m)$`prolific_id`[,3]))

ggplot(df,aes(x=prolific_id,y=pStay.reveal-pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')

df$learning <- NA
for (ind in 1:nrow(df)) {
  df[ind,4] <- df[ind,3]-df[ind,2]
  }


df$learning <- df[,3]
df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)
Anova(lm(learning~adhd_factor+iq+tau,data=df))
ggplot(df,aes(x=adhd_factor,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=learning))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=tau,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')



#control trial
m   <- glmer(stay.card ~ 1+rw_1back+rw_1back:reveal_1back+trial_scale + (1+rw_1back+rw_1back:reveal_1back | prolific_id), data = tab%>%filter(!reveal,reoffer), family = binomial(link = "logit"),nAGQ=0)
Anova(m)
df<-data.frame(prolific_id=rownames(coef(m)$`prolific_id`),pStay.free=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]),
               pStay.reveal=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]+coef(m)$`prolific_id`[,4]))

ggplot(df,aes(x=prolific_id,y=pStay.reveal-pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')


#control trial for instructed trials
m   <- glmer(stay.card ~ 1+rw_1back+rw_1back:reveal_1back+trial_scale + (1+rw_1back+rw_1back:reveal_1back | prolific_id), data = tab%>%filter(acc.teacher==1,reveal==1,reoffer==1), family = binomial(link = "logit"),nAGQ=0)
Anova(m)
df<-data.frame(prolific_id=rownames(coef(m)$`prolific_id`),pStay.free=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]),
               pStay.reveal=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]+coef(m)$`prolific_id`[,4]))

ggplot(df,aes(x=prolific_id,y=pStay.reveal-pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')

df$learning <- NA
for (ind in 1:nrow(df)) {
  df[ind,4] <- df[ind,3]-df[ind,2]
}


df$learning <- df[,3]
df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)
Anova(lm(learning~adhd_factor+iq+tau,data=df))
ggplot(df,aes(x=adhd_factor,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=learning))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=tau,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')

#free-free inst-inst ind diff

m   <- glmer(stay.card ~ 1+rw_1back*trial_scale + (rw_1back | prolific_id), data = tab%>%filter(reveal_1back==0,reveal==0,reoffer==1), family = binomial(link = "logit"),nAGQ=0)
Anova(m)

df<-data.frame(prolific_id=rownames(coef(m)$`prolific_id`),pStay.free=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]))

ggplot(df,aes(x=prolific_id,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')

m2   <- glmer(stay.card ~ 1+rw_1back*trial_scale + (rw_1back | prolific_id), data = tab%>%filter(acc.teacher==1, acc.teacher_1back==1,reveal_1back==1,reveal==1,reoffer==1), family = binomial(link = "logit"),nAGQ=0)
Anova(m2)

df2<-data.frame(prolific_id=rownames(coef(m2)$`prolific_id`),pStay.inst=logit2prob(coef(m2)$`prolific_id`[,1]+coef(m2)$`prolific_id`[,2]))

ggplot(df2,aes(x=prolific_id,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')

df<-merge(df,df2,by='prolific_id')
df$learning.from.inst <- NA
for (ind in 1:nrow(df)) {
  df[ind,4] <- df[ind,3]-df[ind,2]
}

ggplot(df,aes(x=prolific_id,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')

df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)

library(Hmisc)

rcorr(as.matrix(df[-1]))


Anova(lm(pStay.inst~adhd_factor+iq+tau,data=df))
Anova(lm(pStay.free~adhd_factor+iq+tau,data=df))

ggplot(df,aes(x=adhd_factor,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=adhd_factor,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=adhd_factor,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')

ggplot(df,aes(x=iq,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')

ggplot(df,aes(x=iq,y=learning))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=tau,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')


######control for teacher accuracy

# 1) teacher wrong now and before:

m   <- glmer(stay.card ~ 1+rw_1back*trial_scale + (rw_1back | prolific_id), data = tab%>%filter(reveal_1back==0,reveal==0,reoffer==1), family = binomial(link = "logit"),nAGQ=0)
Anova(m)

df<-data.frame(prolific_id=rownames(coef(m)$`prolific_id`),pStay.free=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]))

ggplot(df,aes(x=prolific_id,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')

m2   <- glmer(stay.card ~ 1+rw_1back*trial_scale + (rw_1back | prolific_id), data = tab%>%filter(acc.teacher==0,acc.teacher_1back==0,reveal_1back==1,reveal==1,reoffer==1), family = binomial(link = "logit"),nAGQ=0)
Anova(m2)

df2<-data.frame(prolific_id=rownames(coef(m2)$`prolific_id`),pStay.inst=logit2prob(coef(m2)$`prolific_id`[,1]+coef(m2)$`prolific_id`[,2]))

ggplot(df2,aes(x=prolific_id,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')

df<-merge(df,df2,by='prolific_id')
df$learning.from.inst <- NA
for (ind in 1:nrow(df)) {
  df[ind,4] <- df[ind,3]-df[ind,2]
}

ggplot(df,aes(x=prolific_id,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')

df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)

library(Hmisc)

rcorr(as.matrix(df[-1]))

ggplot(df,aes(x=adhd_factor,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=adhd_factor,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=adhd_factor,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')

ggplot(df,aes(x=iq,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')


# 2) teacher wrong before (not controlling now):

m   <- glmer(stay.card ~ 1+rw_1back*trial_scale + (rw_1back | prolific_id), data = tab%>%filter(reveal_1back==0,reveal==0,reoffer==1), family = binomial(link = "logit"),nAGQ=0)
Anova(m)

df<-data.frame(prolific_id=rownames(coef(m)$`prolific_id`),pStay.free=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]))

ggplot(df,aes(x=prolific_id,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')

m2   <- glmer(stay.card ~ 1+rw_1back*trial_scale + (rw_1back | prolific_id), data = tab%>%filter(acc.teacher_1back==0,reveal_1back==1,reveal==1,reoffer==1), family = binomial(link = "logit"),nAGQ=0)
Anova(m2)

df2<-data.frame(prolific_id=rownames(coef(m2)$`prolific_id`),pStay.inst=logit2prob(coef(m2)$`prolific_id`[,1]+coef(m2)$`prolific_id`[,2]))

ggplot(df2,aes(x=prolific_id,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')

df<-merge(df,df2,by='prolific_id')
df$learning.from.inst <- NA
for (ind in 1:nrow(df)) {
  df[ind,4] <- df[ind,3]-df[ind,2]
}

ggplot(df,aes(x=prolific_id,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')

df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)

library(Hmisc)

rcorr(as.matrix(df[-1]))

ggplot(df,aes(x=adhd_factor,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=adhd_factor,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=adhd_factor,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')

ggplot(df,aes(x=iq,y=pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pStay.inst))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=learning.from.inst))+geom_point(color='navy')+geom_smooth(method='lm')


#inst-inst ind diff
# prev rw effect block progress
m <- glmer(stay.card ~ rw_1back*trial_scale + (1 | prolific_id), 
           data = tab, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 0)
ggpredict(m, c("rw_1back", "trial_scale"),digits=0) %>% plot()



# cor of pObey 
library(lme4)
source('myfolder/01_functions/logit2prob.R')

#for all trials: 
m   <- glmer(obey ~ 1 +(1 | prolific_id), data = tab%>%filter(reveal==1), family = binomial(link = "logit"),nAGQ=1)
df  <-coef(m)$`prolific_id`
df  <-data.frame(prolific_id=rownames(df),pObey=logit2prob(df[,1]))
colnames(df[2]) <- "pObey"
df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)
Anova(lm(pObey~adhd_factor+iq,data=df))
ggplot(df,aes(x=adhd_factor,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=tau,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')


Anova(lm(tau~adhd_factor,data=df))
ggplot(df,aes(x=adhd_factor,y=tau))+geom_point(color='navy')+geom_smooth(method='lm')

#teacher acc=0

m   <- glmer(obey ~ 1 +(1 | prolific_id), data = tab%>%filter(reveal==1,acc.teacher==0), family = binomial(link = "logit"),nAGQ=1)
df  <-coef(m)$`prolific_id`
df  <-data.frame(prolific_id=rownames(df),pObey_inaccurate_teacher=logit2prob(df[,1]))
colnames(df[2]) <- "pObey_inaccurate_teacher"
df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)
Anova(lm(pObey_inaccurate_teacher~adhd_factor+iq,data=df))
ggplot(df,aes(x=adhd_factor,y=pObey_inaccurate_teacher))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pObey_inaccurate_teacher))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=tau,y=pObey_inaccurate_teacher))+geom_point(color='navy')+geom_smooth(method='lm')


#teacher acc=1

m   <- glmer(obey ~ 1 +(1 | prolific_id), data = tab%>%filter(reveal==1,acc.teacher==1), family = binomial(link = "logit"),nAGQ=1)
df  <-coef(m)$`prolific_id`
df  <-data.frame(prolific_id=rownames(df),pObey_accurate_teacher=logit2prob(df[,1]))
colnames(df[2]) <- "pObey_accurate_teacher"
df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)
Anova(lm(pObey_accurate_teacher~adhd_factor+iq,data=df))
ggplot(df,aes(x=adhd_factor,y=pObey_accurate_teacher))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pObey_accurate_teacher))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=tau,y=pObey_accurate_teacher))+geom_point(color='navy')+geom_smooth(method='lm')

##
#first 25 trials per block:
m   <- glmer(obey ~ 1 +(1 | prolific_id), data = tab%>%filter(reveal==1,trial<26), family = binomial(link = "logit"),nAGQ=1)
df  <-coef(m)$`prolific_id`
df  <-data.frame(prolific_id=rownames(df),pObey=logit2prob(df[,1]))
colnames(df[2]) <- "pObey"
df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')
df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)
Anova(lm(pObey~adhd_factor+iq,data=df))
ggplot(df,aes(x=adhd_factor,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')

#control for trial in model:
m   <- glmer(obey ~ trial_scale +(trial_scale | prolific_id), data = tab%>%filter(reveal==1), family = binomial(link = "logit"),nAGQ=1)
summary(m)
df  <-coef(m)$`prolific_id`
df  <-data.frame(prolific_id=rownames(df),pObey=logit2prob(df[,1]))
colnames(df[2]) <- "pObey"
df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')
df[,-1]<-scale(df[,-1])
round(cor(df[,-1]),2)
Anova(lm(pObey~adhd_factor+iq,data=df))
ggplot(df,aes(x=adhd_factor,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')

m   <- glmer(obey ~ 1 +acc.teacher+(1+acc.teacher | prolific_id), data = tab%>%filter(reveal==1), family = binomial(link = "logit"),nAGQ=1)

# trial effect corr

df<-merge(tab%>%filter(running_trial==1,session=="session1"),adhd,by='prolific_id')

m   <- glmer(trial_effect_bool ~ adhd_factor+iq+(1 | prolific_id), data = df, family = binomial(link = "logit"),nAGQ=1)

summary(m)
Anova(m)

r1=cor.test(df$trial_effect,df$adhd_factor)
r2=cor.test(df$trial_effect,df$iq)


ggplot(df,aes(x=adhd_factor,y=trial_effect))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=trial_effect))+geom_point(color='navy')+geom_smooth(method='lm')

m   <- glmer(obey ~ 1 +acc.teacher+(1+acc.teacher | prolific_id), data = tab%>%filter(reveal==1), family = binomial(link = "logit"),nAGQ=1)

#trying to separate acc from obedience
source('myfolder/01_functions/logit2prob.R')

icar$z.iq <- scale(icar$iq)

tab$z.iq<-NA
for (ind in unique(tab$prolific_id)) {
  if (sum(icar$prolific_id == ind)>0){
    tab[tab$prolific_id == ind, "z.iq"] = icar$z.iq[icar$prolific_id == ind]
  }
}

m <- glmer(obey~1+teacher_choice*z.iq+(1|prolific_id),data = tab%>%filter(reveal==1),family = binomial(link = "logit"),nAGQ=1)
summary(m)

df  <-coef(m)$`prolific_id`
df2  <-ranef(m)$`prolific_id`

df$prob <- NA
for (i in 1:nrow(df)) {
  df$prob[i] <- df[i,1]+df2[i,1]
}

df  <-data.frame(prolific_id=rownames(df),pObey=logit2prob(df$prob))
df<-merge(df,icar,by='prolific_id')
round(cor(df[,-1]),2)
Anova(lm(pObey~z.iq,data=df))
ggplot(df,aes(x=iq,y=pObey))+geom_point(color='navy')+geom_smooth(method='lm')


#for tzlil:
#m1 -stay card
data = tab%>%filter(reoffer==1,trial>1)

m <- glmer(stay.card ~ rw_1back*trial_scale + (rw_1back | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

Anova(m)

df<-data.frame(prolific_id=rownames(coef(m)$`prolific_id`),pStay.rw=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]))

df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
rcorr(as.matrix(df[-1]))

Anova(lm(pStay.rw~adhd_factor+iq+tau,data=df))

ggplot(df,aes(x=adhd_factor,y=pStay.rw))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pStay.rw))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=tau,y=pStay.rw))+geom_point(color='navy')+geom_smooth(method='lm')

#m2-stay.obey
data = tab%>%filter(reveal==1,!reoffer,reveal_1back==1)

m <- glmer(stay.obey ~ rw_1back*trial_scale + (rw_1back | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)


Anova(m)


df<-data.frame(prolific_id=rownames(coef(m)$`prolific_id`),pStay.obey.rw=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]))

df<-merge(df,adhd,by='prolific_id')
df<-merge(df,icar,by='prolific_id')

cut_crt <- subset(merged_crt, select = c("prolific_id","tau") )
df <- merge(df,cut_crt,by="prolific_id")

#df[,-1]<-scale(df[,-1])
rcorr(as.matrix(df[-1]))
Anova(lm(pStay.obey.rw~adhd_factor+iq+tau,data=df))

ggplot(df,aes(x=adhd_factor,y=pStay.obey.rw))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pStay.obey.rw))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=tau,y=pStay.obey.rw))+geom_point(color='navy')+geom_smooth(method='lm')


# 2-back

tab%<>%
  mutate(reveal_2back       =lag(reveal,2)*1,
         stay.obey_2back    =(obey==lag(obey,2))*1,
         reoffer_2back      =(lag(ch,2)==frcA | lag(ch,2)==frcB),
         acc.teacher_2back  =lag(acc.teacher,2)*1,
         rw_2back           =lag(rw_1back,1)*1,
         cond_2back         =relevel(lag(cond,2),'free'),
         stay.card_2back    =(ch==lag(ch,2))*1,
         stay.cond          =(cond==lag(cond,1))*1,
         stay.cond_2back    =(cond==lag(cond,2))*1,
         
         
)
  

data = tab%>%filter(reoffer==1,cond_2back!="disobey",cond!="disobey",stay.cond==1,acc.teacher==1,acc.teacher_2back==1,reoffer_2back==1,trial>1)

m <- glmer(stay.card_2back ~ rw_2back*cond_2back*reveal*trial_scale + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)
Anova(m)

ggpredict(m, c("rw_2back","cond_2back","reveal"),digits=0) %>% plot()

ggpredict(m, c("reveal","cond_2back"),digits=0) %>% plot()

write.csv(data,"complete_dat_trial.csv")  # for jasp graph







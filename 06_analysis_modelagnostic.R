#Code aim:main analysis for the teacher-student paradigm wit previous-trial regression and corr with adhd and IQ
#Contributors: Nitzan Shahar

# initiate ----------------------------------------------------------------
rm(list=ls())
library(dplyr)
#library(plyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
maindir<-'myfolder/03_data/02_aggregated_data/'
load(paste(maindir,'tab.Rdata',sep=""))
load(paste(maindir,'adhd_factor.Rdata',sep=""))
load(paste(maindir,'icar.Rdata',sep=""))
load(paste(maindir,'subjects_that_completed_session1_and_2.Rdata',sep=""))


#keep only these how had both sessions
tab%<>%filter(prolific_id%in%subj.list)


# sanity checks 
tab%>%group_by(session,block)%>%summarise(mean(rw),mean(acc.player),mean(acc.teacher))


## control reduction in player.acc


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

#### pObey(intercept) ---------------------------------------------

# Variability for pObey (tendency to take teacher advice across all other factors)
df<-
  tab%>%
  filter(reveal==1)%>%
  group_by(prolific_id,session)%>%
  summarise(p.obey=mean(obey))%>%
  spread(session,p.obey)%>%
  na.omit()%>%as.data.frame()

plot(df$session1)
plot(df$session2)

ggplot(df,aes(x=trial,y=session1))+geom_point(color=df$block)
ggplot(df,aes(x=trial,y=session2))+geom_point(color=df$block)

# pObey across experiment 
df<-
  tab%>%
  filter(reveal==1,acc.teacher==1)%>%
  group_by(session,block,trial)%>%
  summarise(p.obey=mean(obey))%>%
  spread(session,p.obey)%>%
  na.omit()%>%as.data.frame()

df<-
  df%>%
  mutate(running_trial=ifelse(block==2,trial+129,ifelse(block==3,trial+259,trial)))


plot(df$session1)
plot(df$session2)

ggplot(df,aes(x=trial,y=session1))+geom_point(color=df$block)
ggplot(df,aes(x=trial,y=session2))+geom_point(color=df$block)

ggplot(df,aes(x=running_trial,y=session1))+geom_point(color=df$block)
ggplot(df,aes(x=running_trial,y=session2))+geom_point(color=df$block)


# change in reward across trials (given that they stop obeying)
df<-
  tab%>%
  filter(reveal==1,acc.teacher==1)%>%
  group_by(session,block,trial)%>%
  summarise(reward=mean(rw))%>%
  spread(session,reward)%>%
  na.omit()%>%as.data.frame()

df<-
  df%>%
  mutate(running_trial=ifelse(block==2,trial+129,ifelse(block==3,trial+259,trial)))


ggplot(df,aes(x=running_trial,y=session1))+geom_point(color=df$block)
ggplot(df,aes(x=running_trial,y=session2))+geom_point(color=df$block)

#testing what happens with reward as function of obey.. should be worthwhile following the teacher

df<-
  tab%>%
  filter(reveal==1,acc.teacher==1,obey==1)%>%
  group_by(session,block,trial)%>%
  summarise(reward=mean(rw))%>%
  spread(session,reward)%>%
  na.omit()%>%as.data.frame()

df<-
  df%>%
  mutate(running_trial=ifelse(block==2,trial+129,ifelse(block==3,trial+259,trial)))


ggplot(df,aes(x=running_trial,y=session1))+geom_point(color=df$block)
ggplot(df,aes(x=running_trial,y=session2))+geom_point(color=df$block)

##RT
df<-
  tab%>%
  filter(reveal==1,acc.teacher==1,obey==1)%>%
  group_by(session,block,trial)%>%
  summarise(RT=mean(rt))%>%
  spread(session,RT)%>%
  na.omit()%>%as.data.frame()

df<-
  df%>%
  mutate(running_trial=ifelse(block==2,trial+129,ifelse(block==3,trial+259,trial)))


ggplot(df,aes(x=running_trial,y=session1))+geom_point(color=df$block)
ggplot(df,aes(x=running_trial,y=session2))+geom_point(color=df$block)

####acc.player

df<-
  tab%>%
  filter(reveal==1,acc.teacher==1)%>%
  group_by(session,block,trial,acc.player)%>%
  summarise(acc=mean(acc.player))%>%
  spread(session,reward)%>%
  na.omit()%>%as.data.frame()

df<-
  tab%>%
  filter()%>%
  group_by(session,block,trial,acc.player)%>%
  summarise(acc=mean(acc.player))%>%
  spread(session,acc)%>%
  na.omit()%>%as.data.frame()

df<-
  df%>%
  mutate(running_trial=ifelse(block==2,trial+129,ifelse(block==3,trial+259,trial)))


ggplot(df,aes(x=acc.player,y=session1))+geom_point()
ggplot(df,aes(x=acc.player,y=session2))+geom_point()


tab<-
  tab%>%
  mutate(running_trial=ifelse(block==2,trial+129,ifelse(block==3,trial+259,trial)))

tab<-
  tab%>%
  mutate(trial_scale=trial/130, block_scale=block/3)

m <- glmer(acc.player ~ trial_scale*block_scale*session + (trial_scale*block_scale*session| prolific_id), data = tab, family = binomial(link = "logit"),nAGQ=0)

summary(m)
coef(m)

# test-retest for pObey (tendency to take teacher advice across all other factors)
df<-
tab%>%
  filter(reveal==1)%>%
  group_by(prolific_id,session)%>%
  summarise(p.obey=mean(obey))%>%
  spread(session,p.obey)%>%
  na.omit()%>%as.data.frame()

ggplot(df,aes(x=session1,y=session2))+geom_point(color='navy')+geom_smooth(method='lm')
r=cor.test(df$session1,df$session2)
print(paste('test-retest reliability for pObey is',round(r$estimate,2),'(p=',round(r$p.value,3),')'))

# test-retest for pObey on first 75 trials 
df<-
  tab%>%
  filter(reveal==1,trial<75)%>%
  group_by(prolific_id,session)%>%
  summarise(p.obey=mean(obey))%>%
  spread(session,p.obey)%>%
  na.omit()%>%as.data.frame()

ggplot(df,aes(x=session1,y=session2))+geom_point(color='navy')+geom_smooth(method='lm')
r=cor.test(df$session1,df$session2)
print(paste('test-retest reliability for pObey is',round(r$estimate,2),'(p=',round(r$p.value,3),')'))

# test-retest for reward 
df<-
  tab%>%
  group_by(prolific_id,session)%>%
  summarise(rw=mean(rw))%>%
  spread(session,rw)%>%
  na.omit()%>%as.data.frame()

ggplot(df,aes(x=session1,y=session2))+geom_point(color='navy')+geom_smooth(method='lm')
r=cor.test(df$session1,df$session2)
print(paste('test-retest reliability for pObey is',round(r$estimate,2),'(p=',round(r$p.value,3),')'))


# test-retest for accuracy 
df<-
  tab%>%
  group_by(prolific_id,session)%>%
  summarise(acc.player=mean(acc.player))%>%
  spread(session,acc.player)%>%
  na.omit()%>%as.data.frame()

ggplot(df,aes(x=session1,y=session2))+geom_point(color='navy')+geom_smooth(method='lm')
r=cor.test(df$session1,df$session2)
print(paste('test-retest reliability for pObey is',round(r$estimate,2),'(p=',round(r$p.value,3),')'))


library(lme4)
source('myfolder/01_functions/logit2prob.R')
m   <- glmer(obey ~ 1 + (1 | session:prolific_id), data = tab%>%filter(reveal==1), family = binomial(link = "logit"),nAGQ=1)
df  <-coef(m)$`session:prolific_id`
df  <-data.frame(session=factor(substr(rownames(df),1,8)),subject=substr(rownames(df),10,33),pObey=logit2prob(df[,1]))
df%<>%pivot_wider(names_from = session, values_from = pObey)
ggplot(df,aes(x=session1,y=session2))+geom_point(color='navy')+geom_smooth(method='lm')
r=cor.test(df$session1,df$session2)
print(paste('test-retest reliability for pObey is',round(r$estimate,2),'(p=',round(r$p.value,3),')'))


# add accuracy as slope for obey model

source('myfolder/01_functions/logit2prob.R')
m   <- glmer(obey ~ acc.player + (1+acc.player | session:prolific_id), data = tab%>%filter(reveal==1), family = binomial(link = "logit"),nAGQ=1)
df  <-coef(m)$`session:prolific_id`
df  <-data.frame(session=factor(substr(rownames(df),1,8)),subject=substr(rownames(df),10,33),pObey=logit2prob(df[,1]),acc.player=logit2prob(df[,2]))
df%<>%pivot_wider(names_from = session, values_from = c(pObey,acc.player))
ggplot(df,aes(x=pObey_session1,y=pObey_session2))+geom_point(color='navy')+geom_smooth(method='lm')
r=cor.test(df$pObey_session1,df$pObey_session2)
print(paste('test-retest reliability for pObey is',round(r$estimate,2),'(p=',round(r$p.value,3),')'))



# cor of pObey 
library(lme4)
source('myfolder/01_functions/logit2prob.R')
m   <- glmer(obey ~ 1 +(1 | prolific_id), data = tab%>%filter(reveal==1), family = binomial(link = "logit"),nAGQ=1)
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





####  learning as a function of reward and reveal in the previous trial ---------------------------------------------
names(tab)
library(dplyr)

library(lme4)
library(car)
library(afex)
library(emmeans)


source('myfolder/01_functions/logit2prob.R')

#model1 -stay.obey ~ 1back_rw (instructed)

data = tab%>%filter(reveal==1,!reoffer,reveal_1back==1)

data<-
  tab%>%
  filter(reveal==1,!reoffer,reveal_1back==1)%>%
  group_by(prolific_id,rw_1back)%>%
  summarise(stay.obey=mean(stay.obey))%>%
  #spread(session,p.obey)%>%
  na.omit()%>%as.data.frame()

m <- glmer(stay.obey ~ rw_1back + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)

library(effects)

plot(effect('rw_1back', m))


library("gplots")

plotmeans(stay.obey ~ rw_1back, data = data, frame = FALSE,
          xlab = "prev reward", ylab = "p. stay obey") 



#model2 -obey ~ 1back_rw (instructed) +learning type

data = tab%>%filter(reveal==1,!reoffer,reveal_1back==1)

data<-
  tab%>%
  filter(reveal==1,!reoffer,reveal_1back==1)%>%
  group_by(prolific_id,rw_1back,obey_1back)%>%
  summarise(stay.obey=mean(stay.obey))%>%
  #spread(session,p.obey)%>%
  na.omit()%>%as.data.frame()

m <- glmer(stay.obey ~ rw_1back+obey_1back+rw_1back:obey_1back + (rw_1back+obey_1back+rw_1back:obey_1back | prolific_id), 
            data = data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 1)
summary(m)

Anova(m)


plot(effect('rw_1back:obey_1back', m,xlevels=2))

ggplot(data,aes(y=stay.obey,x=rw_1back,color=as.factor(obey_1back)))+
  stat_summary(fun='mean',geom='line') +
  scale_x_continuous(breaks=c(0,1))

#model3 -stay.card ~ 1back_rw (instructed) + trial type

data = tab%>%filter(reoffer==1,reveal_1back==1,obey_1back==1)
two.way <- aov(stay.card ~ rw_1back + cond + rw_1back*cond, data = data)

summary(two.way)

ggplot(data,aes(y=stay.card,x=rw_1back,color=cond))+
  stat_summary(fun='mean',geom='line') +
  scale_x_continuous(breaks=c(0,1))

#model3b -stay.card ~ 1back_rw (instructed) + trial type


data = tab%>%filter(reoffer==1,reveal_1back==1,obey_1back==1,acc.teacher==1)

data %<>%
  
  mutate(cond_obey= ifelse(reveal==0,"free",ifelse(reoffer.choice==1,"reoffer_prev_ch","reoffer_new_ch"))
         
  )

write.csv(data,"learning_dat.csv")

m <- glmer(stay.card ~ rw_1back+cond_obey+rw_1back:cond_obey + (rw_1back+cond_obey+rw_1back:cond_obey | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

m2 <- glmer(stay.card ~ rw_1back+cond_obey+rw_1back:cond_obey + (1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)
summary(m)

Anova(m)


plot(effect('rw_1back:cond_obey', m,xlevels=3))


library(magrittr)
library(ggeffects)
library(sjmisc)
library(lme4)
library(splines)

ggpredict(m, c("rw_1back", "cond_obey"),digits=0) %>% plot()

#ggplot(data,aes(y=stay.card,x=rw_1back,color=cond_obey))+
#  stat_summary(fun='mean',geom='line') +
#  scale_x_continuous(breaks=c(0,1))

#model4 -stay.card ~ 1back_rw (uninstructed) + trial type

data = tab%>%filter(reoffer==1,reveal_1back==0)

fit <- aov_ez(id = "prolific_id", dv = "stay.card",
              within = c("rw_1back", "cond"),
              ## When working with large within-subject datasets, this will
              ## speed up the fitting considerably!
              # include_aov = FALSE,
              data = data)
fit

joint_tests(fit, by = "cond")

ggplot(data,aes(y=stay.card,x=rw_1back,color=cond))+
  stat_summary(fun='mean',geom='line') +
  scale_x_continuous(breaks=c(0,1))


#model4b -stay.card ~ 1back_rw (uninstructed) + trial type

data = tab%>%filter(reoffer==1,reveal_1back==0,acc.teacher==1)
data %<>%
  
  mutate(cond_obey= ifelse(reveal==0,"free",ifelse(reoffer.choice==1,"reoffer_prev_ch","reoffer_new_ch"))
        
)
  
write.csv(data,"decision_dat.csv")
m <- glmer(stay.card ~ rw_1back+cond_obey+rw_1back:cond_obey + (rw_1back+cond_obey+rw_1back:cond_obey | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)
summary(m)

Anova(m)

ggpredict(m, c("rw_1back", "cond_obey"),digits=0) %>% plot()


plot(effect('rw_1back:cond_obey', m,xlevels=3))

ggplot(data,aes(y=stay.card,x=rw_1back,color=cond_obey))+
  stat_summary(fun='mean',geom='line') +
  scale_x_continuous(breaks=c(0,1))

#model 3-4 c - combine models to compare free and instruted learning


data1 = tab%>%filter(reoffer==1,reveal_1back==0) #free

data1 %<>%
  
  mutate(cond_obey= ifelse(reveal==0,"free",ifelse(reoffer.choice==1,"reoffer_prev_ch","reoffer_new_ch"))
         
  )

data2 = tab%>%filter(reoffer==1,reveal_1back==1,obey_1back==1) #inst

data2 %<>%
  
  mutate(cond_obey= ifelse(reveal==0,"free",ifelse(reoffer.choice==1,"reoffer_prev_ch","reoffer_new_ch"))
         
  )


data <- rbind(data1,data2)

m <- glmer(stay.card ~ cond_obey+reveal_1back+rw_1back+ rw_1back:cond_obey + rw_1back:reveal_1back+ reveal_1back:rw_1back:cond_obey+(1 | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)
summary(m)

Anova(m)


plot(effect('cond_obey:reveal_1back:rw_1back', m,xlevels=2))


### stay.card in free trial as function if there were (accurate) instructions before
data = tab%>%
  filter(reoffer==1,reveal==0) %>%
  mutate(prev_acc= ifelse(reveal_1back==0,1,acc.teacher_1back))

data = data%>%filter(prev_acc==1)


m <- glmer(stay.card ~ rw_1back+reveal_1back+rw_1back:reveal_1back + (rw_1back+reveal_1back+rw_1back:reveal_1back | prolific_id), 
           data = data, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)
summary(m)

Anova(m)

plot(effect('rw_1back:reveal_1back', m,xlevels=3))

ggpredict(m, c("rw_1back", "reveal_1back"),digits=0) %>% plot()


ggplot(data,aes(y=stay.card,x=rw_1back,color=cond))+
  stat_summary(fun='mean',geom='line') +
  scale_x_continuous(breaks=c(0,1))


#model5 -try to deal with the IQ confound -look at pobey for trials in which teacher was wrong/correct

df<- #check number of trials, require dplyr
  tab%>%
  filter(reveal==1)%>%
  group_by(prolific_id,acc.teacher)%>%
  summarise(count.obey=count(obey))%>%
  #spread(acc.teacher,count.obey)%>%
  na.omit()%>%as.data.frame()

df<-
  tab%>%
  filter(reveal==1)%>%
  group_by(prolific_id,acc.teacher)%>%
  summarise(p.obey=mean(obey))%>%
  #spread(acc.teacher,p.obey)%>%
  na.omit()%>%as.data.frame()

ggplot(df,aes(x=acc.teacher,y=p.obey))+geom_point(color='navy')+geom_smooth(method='lm')

df<-
  tab%>%
  filter(reveal==1)%>%
  group_by(prolific_id,acc.teacher)%>%
  summarise(p.obey=mean(obey))%>%
  spread(acc.teacher,p.obey)%>%
  na.omit()%>%as.data.frame()

df<-merge(df,icar,by='prolific_id')

colnames(df) <- c("prolific_id", "pObey_teacher.acc0","pObey_teacher.acc1","iq")


cor(df$pObey_teacher.acc0,df$iq) #teacher.acc0
cor(df$pObey_teacher.acc1,df$iq) #teacher.acc1

ggplot(df,aes(x=iq,y=pObey_teacher.acc0))+geom_point(color='navy')+geom_smooth(method='lm')
ggplot(df,aes(x=iq,y=pObey_teacher.acc1))+geom_point(color='navy')+geom_smooth(method='lm')







data = tab%>%filter(reveal==1)
one.way <- aov(obey ~ acc.teacher, data = data)

summary(one.way)

ggplot(data,aes(y=obey,x=acc.teacher))+
  stat_summary(fun='mean',geom='line') +
  scale_x_continuous(breaks=c(0,1))




#model
m   <- glmer(stay.card ~ 1+rw_1back+rw_1back:reveal_1back + (1+rw_1back+rw_1back:reveal_1back | prolific_id), data = tab%>%filter(!reveal,reoffer), family = binomial(link = "logit"),nAGQ=0)
Anova(m)
df<-data.frame(prolific_id=df[1],pStay.free=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]),
               pStay.reveal=logit2prob(coef(m)$`prolific_id`[,1]+coef(m)$`prolific_id`[,2]+coef(m)$`prolific_id`[,3]))

ggplot(df,aes(x=prolific_id,y=pStay.reveal-pStay.free))+geom_point(color='navy')+geom_smooth(method='lm')


###

m1 <- glmer(stay.card ~ rw_1back*reveal_1back + (1*rw_1back*reveal_1back | prolific_id), data = tab%>%filter(!reveal,reoffer), family = binomial(link = "logit"),nAGQ=0)
Anova(m1)


##### pobey and reward #####

df<-
  tab%>%
  filter(reveal==1)%>%
  group_by(prolific_id)%>%
  summarise(p.obey=mean(obey), reward=mean(rw))%>%
  na.omit()%>%as.data.frame()

cor.test(df$p.obey, df$reward, 
         method = "pearson")

ggplot(df,aes(x=reward,y=p.obey))+geom_point(color='navy')+geom_smooth(method='lm')


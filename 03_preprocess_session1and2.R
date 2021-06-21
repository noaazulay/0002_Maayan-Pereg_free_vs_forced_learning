#Code aim: to clean the behavioral data for the teacher-student paradigm, omit subjects with very suspicious data and add required columns 
#Contributors: Nitzan Shahar

# initiate ----------------------------------------------------------------
rm(list=ls())
library(dplyr)
library(magrittr)
maindir<-'myfolder/03_data/02_aggregated_data/'

#####teacher-student paradigm-------------------------------------------------------------------
load(paste(maindir,'tab_raw.Rdata',sep=""))

#arrange columns
names(tab)

tab%<>%
  mutate(session        =factor(Session,labels = c('session1','session2')),
         block          =blk,
         trial          =trl,
         reveal         =reveal_teacher_decision,
         obey           =obey*1,
         rw_1back       =lag(rw,1)*1,
         obey_1back     =lag(obey,1)*1,
         reveal_1back   =lag(reveal_teacher_decision,1)*1,
         stay.card      =(ch==lag(ch))*1,
         reoffer.choice =ifelse(reveal_teacher_decision==1,(teacher_choice==lag(ch))*1,1),
         stay.obey      =(obey==lag(obey,1))*1,
         stay.key       =(obey==lag(obey,1))*1,
         reoffer        =(lag(ch)==frcA | lag(ch)==frcB),
         prob.ch        =prob1,
         prob.unch      =prob2,
         prob_teacher.ch=ifelse(teacher_choice==ch,prob1,prob2),
         prob_teacher.unch=ifelse(teacher_choice!=ch,prob1,prob2),
         beta_teacher   =beta_teacher,
         cond           =factor(ifelse(reveal_teacher_decision==1,obey,2),labels=c('disobey','obey','free')),
         abort          =(rt<200 | rt>4000 | trl==0)
         
  )%>%
  select(subj,prolific_id,session,block,trial,reveal,ch,obey,key,rt,rw,frcA,frcB,teacher_choice,
         rw_1back,obey_1back,reveal_1back,stay.card,reoffer.choice,stay.obey,stay.key,reoffer,prob.ch,prob.unch,
         beta_teacher,prob_teacher.ch,prob_teacher.unch,cond,abort)%>%
  mutate(acc.player         =(prob.ch>prob.unch)*1,
         acc.teacher        =(prob_teacher.ch>prob_teacher.unch)*1,
         acc.teacher_1back  =lag(acc.teacher,1)*1,
         cond=relevel(cond, 'free'),
         cond_1back=relevel(lag(cond,1),'free'))
  

tab<-as.data.frame(tab)

#check proporation of ommited trials
mean(tab$abort)

#check implausible stickiness in card or key 
tab%>%
  group_by(prolific_id)%>%
  summarise(p.stay=mean(stay.card))%>%
  with(plot(p.stay,ylim=c(0,0.5)))

tab%>%
  group_by(prolific_id)%>%
  summarise(p.stay=mean(stay.key))%>%
  with(plot(p.stay,ylim=c(0.25,0.75)))

print(paste(cat('Summary of trials and subjects ommission in the teacher-student paradigm:\n'),
            cat('1. No subjects were ommited based on key and card stickiness\n'),
            cat('2. ~8% of the total amount of trials were ommited due to quick or slow rt, and first trial')))

#filter the data
tab%<>%filter(abort==0)
tab<-na.omit(tab)


#add iq to each participant 
load(paste(maindir,'icar.Rdata',sep=""))
tab$iq<-NA
for (ind in unique(tab$prolific_id)) {
  if (sum(icar$prolific_id == ind)>0){
  tab[tab$prolific_id == ind, "iq"] = icar$iq[icar$prolific_id == ind]
  }
}

#save
save(tab,file='myfolder/03_data/02_aggregated_data/tab.Rdata')

#Code aim: to count how many subject are included in each dataset and how many completed all three sessions
#Contributors: Nitzan Shahar

# initiate ----------------------------------------------------------------

rm(list=ls())
library(dplyr)
maindir<-'myfolder/03_data/02_aggregated_data/'




# main analysis -----------------------------------------------------------

#sessions 1 and 2 (teacher-student paradigm)
load(paste(maindir,'tab_raw.Rdata',sep=""))

subjects.tab.session1<-tab%>%filter(Session==1)%>%summarise(unique(prolific_id))%>%unlist() 
subjects.tab.session2<-tab%>%filter(Session==2)%>%summarise(unique(prolific_id))%>%unlist() 
print(paste('In the teacher-student paradigm: Session 1 had',length(subjects.tab.session1),'subjects, ',
            'Session 2 had',length(subjects.tab.session2),'subjects, ',
            'and',sum(subjects.tab.session1%in%subjects.tab.session2),'subjects completed both sessions'))

subj.list<-subjects.tab.session1[subjects.tab.session1%in%subjects.tab.session2]
save(subj.list,file=paste(maindir,'subjects_that_completed_session1_and_2.Rdata',sep=""))

#sessions 3 (IQ, self-report, choice-RT task)
load(paste(maindir,'icar_raw.Rdata',sep=""))
load(paste(maindir,'asrs_raw.Rdata',sep=""))
load(paste(maindir,'wurs_raw.Rdata',sep=""))
load(paste(maindir,'bis_raw.Rdata',sep=""))
load(paste(maindir,'oci_raw.Rdata',sep=""))
load(paste(maindir,'crt_raw.Rdata',sep=""))

subjects.iq<-icar%>%summarise(unique(prolific_id))%>%unlist()
subjects.asrs<-asrs%>%summarise(unique(prolific_id))%>%unlist() 
subjects.wurs<-wurs%>%summarise(unique(prolific_id))%>%unlist()
subjects.bis<-bis%>%summarise(unique(prolific_id))%>%unlist() 
subjects.oci<-oci%>%summarise(unique(prolific_id))%>%unlist() 
subjects.choiceRT<-crt%>%summarise(unique(prolific_id))%>%unlist() 

print(paste(cat('In Session 3:\n'),
            cat('IQ dataset included',length(subjects.iq),'subjects, \n'),
            cat('ASRS dataset included',length(subjects.asrs),'subjects, \n'),
            cat('WURS dataset included',length(subjects.wurs),'subjects, \n'),
            cat('BIS dataset included',length(subjects.bis),'subjects, \n'),
            cat('OCI-R dataset included',length(subjects.oci),'subjects, \n'),
            cat('ChoiceRT dataset included',length(subjects.choiceRT),'subjects, \n')
            ))

print(paste(cat('In Session 3:\n'),
            cat(length((subjects.asrs%in%subjects.wurs+subjects.asrs%in%subjects.bis+subjects.wurs%in%subjects.bis)==3),'subjects had all three adhd self-reports (asrs,wurs,bis)\n'),
            cat(paste(sum(subjects.tab.session1%in%subjects.iq),'subjects had session1 and IQ datasets\n')),
            cat(paste(sum(subjects.tab.session2%in%subjects.iq),'subjects had session1, session 2 and IQ datasets\n')),
            cat(paste(sum(subjects.tab.session1%in%subjects.asrs),'subjects had session1, and all self-report datasets\n')),
            cat(paste(sum(subjects.tab.session2%in%subjects.asrs),'subjects had session1, session 2 and all self-report datasets\n'))
))




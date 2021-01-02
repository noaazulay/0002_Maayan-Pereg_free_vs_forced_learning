rm(list=ls())
library(dplyr)
library(magrittr)
maindir<-'myfolder/03_data/02_aggregated_data/'




#####OCI-------------------------------------------------------------------
load(paste(maindir,'oci_raw.Rdata',sep=""))
oci$oci<-apply(oci[,1:18],1,sum)
oci%<>%
  mutate(hoard=rowMeans(cbind(item_1,item_7,item_13)),
         check=rowMeans(cbind(item_2,item_8,item_14)),
         order=rowMeans(cbind(item_3,item_9,item_15)),
         count=rowMeans(cbind(item_4,item_10,item_16)),
         clean=rowMeans(cbind(item_5,item_11,item_17)),
         obsess=rowMeans(cbind(item_6,item_12,item_18)))%>%
  select(prolific_id,hoard,check,order,count,clean,obsess,oci)
head(oci)
hist(oci$oci)
save(oci,file='myfolder/03_data/02_aggregated_data/oci.Rdata')


#####BIS-------------------------------------------------------------------
load(paste(maindir,'bis_raw.Rdata',sep=""))

bis<-bis%<>%
     mutate(item_9=5-item_9,item_20=5-item_20,item_30=5-item_30, item_1=5-item_1,
            item_7=5-item_7,item_8=5-item_8,item_10=5-item_10,item_12=5-item_12,
            item_13=5-item_13,item_15=5-item_15,item_29=5-item_29)


bis$bis<-apply(bis[,1:30],1,sum)
bis%<>%
  mutate(attention  =rowMeans(cbind(item_6, item_5, item_9, item_11, item_20, item_24, item_26, item_28)),
         motor      =rowMeans(cbind(item_2, item_3, item_4, item_16, item_17, item_19, item_21, item_22, item_23, item_25, item_30)),
         nonplanning=rowMeans(cbind(item_1, item_7, item_8, item_10, item_12, item_13, item_14, item_15, item_18, item_27, item_29)))%>%
  select(prolific_id,attention,motor,nonplanning,bis)
head(bis)
hist(bis$bis)
save(bis,file='myfolder/03_data/02_aggregated_data/bis.Rdata')


#####WURS-------------------------------------------------------------------
load(paste(maindir,'wurs_raw.Rdata',sep=""))
wurs$wurs<-apply(wurs[,1:25],1,sum)
wurs%<>%select(prolific_id,wurs)
save(wurs,file='myfolder/03_data/02_aggregated_data/wurs.Rdata')


#####ASRS-------------------------------------------------------------------
load(paste(maindir,'asrs_raw.Rdata',sep=""))
asrs$asrs6     <-apply(asrs[,1:6],1,sum)
asrs$asrs.partb<-apply(asrs[,7:20],1,sum)
asrs%<>% select(prolific_id,asrs6,asrs.partb)
save(asrs,file='myfolder/03_data/02_aggregated_data/asrs.Rdata')


#####ICAR-------------------------------------------------------------------
load(paste(maindir,'icar_raw.Rdata',sep=""))
icar$iq<-rowSums(icar[,grepl(c('acc'), colnames(icar))])
icar%<>%select(prolific_id,iq)
head(icar)
hist(icar$iq)
save(icar,file='myfolder/03_data/02_aggregated_data/icar.Rdata')


#####choice-reaction time task-------------------------------------------------------------------
load(paste(maindir,'crt_raw.Rdata',sep=""))


#####teacher-student paradigm-------------------------------------------------------------------
load(paste(maindir,'tab_raw.Rdata',sep=""))

#arrange columns
names(tab)
tab%<>%
  mutate(session     =Session,
         reveal      =reveal_teacher_decision,
         obey        =obey*1,
         rw_1back    =lag(rw,1)*1,
         obey_1back  =lag(obey,1)*1,
         reveal_1back=lag(reveal_teacher_decision,1)*1,
         stay.card   =(ch==lag(ch))*1,
         stay.obey   =(obey==lag(obey,1))*1,
         stay.key    =(obey==lag(obey,1))*1,
         reoffer     =(lag(ch)==frcA | lag(ch)==frcB),
         abort       =(rt<200 | rt>4000 | trl==0)
  )%>%
  select(subj,prolific_id,session,blk,trl,reveal,ch,obey,key,
         rt,rw,rw_1back,obey_1back,reveal_1back,stay.card,stay.obey,stay.key,abort)

tab<-as.data.frame(tab)

#check proporation of ommited trials
mean(tab$abort)

#check to many stays
detach(package:plyr)
tab%>%
  group_by(prolific_id)%>%
  summarise(p.stay=mean(stay.card))%>%
  with(plot(p.stay,ylim=c(0,1)))

tab%>%
  group_by(prolific_id)%>%
  summarise(p.stay=mean(stay.key))%>%
  with(plot(p.stay,ylim=c(0,1)))

# no subjects were ommited based on key and card stickinees.
# ~8% of the total amount of trials were ommited due to quick or slow rt, and first trial
tab%<>%filter(abort==0)
tab<-na.omit(tab)
save(tab,file='myfolder/03_data/02_aggregated_data/tab.Rdata')






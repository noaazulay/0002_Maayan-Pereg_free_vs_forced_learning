#Code aim: to convert the raw data of self-report and IQ measures to clean data-files with the correct sub-scales. 
#Contributors: Nitzan Shahar

# initiate ----------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(magrittr)
maindir<-'myfolder/03_data/02_aggregated_data/'



#####choice-reaction time task-------------------------------------------------
load(paste(maindir,'crt_raw.Rdata',sep=""))

crt <- crt%>%
  filter(block=='test',trial_num>1)

library(retimes)
crt_exgaus <- data.frame(matrix(NA,ncol=3))

for (ind in unique(crt$prolific_id)) {
  crt_exgaus[ind,] <-  mexgauss(crt$rt[crt$prolific_id==ind & crt$acc==1], n = length(crt$rt[crt$prolific_id==ind & crt$acc==1]))
}

crt_exgaus <- na.omit(crt_exgaus)
crt_exgaus  <-data.frame(prolific_id=rownames(crt_exgaus),mu=crt_exgaus[,1],sigma=crt_exgaus[,2],tau=crt_exgaus[,3])


check <- crt%>%filter(block=='test',trial_num>1)%>%group_by(prolific_id)%>%summarise(mean(acc),mean(rt))

crt_rt<-
  crt%>%
  filter(block=='test',trial_num>1,rt>150,acc==1)%>%
  group_by(prolific_id)%>%
  summarise(mean_rt=mean(rt), sd_rt=sd(rt))%>%
  na.omit()%>%as.data.frame()

crt_acc<-
  crt%>%
  filter(block=='test',trial_num>1,rt>150)%>%
  group_by(prolific_id)%>%
  summarise(mean_acc=mean(acc),mean_pe=1-mean(acc))%>%
  na.omit()%>%as.data.frame()


merged_crt <- merge(crt_acc, crt_rt,by="prolific_id") %>%
  merge(crt_exgaus,by="prolific_id")



head(merged_crt)
hist(merged_crt$mean_rt)
save(merged_crt,file='myfolder/03_data/02_aggregated_data/crt.Rdata')


#####ICAR---------------------------------------------------------
load(paste(maindir,'icar_raw.Rdata',sep=""))
icar$iq<-rowSums(icar[,grepl(c('acc'), colnames(icar))])
icar%<>%select(prolific_id,iq)
head(icar)
hist(icar$iq)
save(icar,file='myfolder/03_data/02_aggregated_data/icar.Rdata')


#####ASRS-------------------------------------------------------------------
load(paste(maindir,'asrs_raw.Rdata',sep=""))
asrs$asrs6     <-apply(asrs[,1:6],1,sum)
asrs$asrs.partb<-apply(asrs[,7:20],1,sum)
asrs%<>% select(prolific_id,asrs6,asrs.partb)
save(asrs,file='myfolder/03_data/02_aggregated_data/asrs.Rdata')


#####WURS-------------------------------------------------------------------
load(paste(maindir,'wurs_raw.Rdata',sep=""))
wurs$wurs<-apply(wurs[,1:25],1,sum)
wurs%<>%select(prolific_id,wurs)
save(wurs,file='myfolder/03_data/02_aggregated_data/wurs.Rdata')




#####BIS-------------------------------------------------------------------
load(paste(maindir,'bis_raw.Rdata',sep=""))


bis$bis<-apply(bis[,1:30],1,sum)
bis%<>%
  mutate(attention  =rowMeans(cbind(item_6, item_5, item_9, item_11, item_20, item_24, item_26, item_28)),
         motor      =rowMeans(cbind(item_2, item_3, item_4, item_16, item_17, item_19, item_21, item_22, item_23, item_25, item_30)),
         nonplanning=rowMeans(cbind(item_1, item_7, item_8, item_10, item_12, item_13, item_14, item_15, item_18, item_27, item_29)))%>%
  select(prolific_id,attention,motor,nonplanning,bis)
head(bis)
hist(bis$bis)
save(bis,file='myfolder/03_data/02_aggregated_data/bis.Rdata')




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


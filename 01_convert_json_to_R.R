#Code aim: to extract the raw json files for session 1, 2 and 3 including: the teacher-student paradigm (i.e., tsp), self-report, IQ and choice-RT task
#Contributors: Nitzan Shahar, Maayan Pereg 


# initiate ----------------------------------------------------------------

rm(list=ls())
library("rjson")
library(dplyr)
source('myfolder/01_functions/convert_json_teacher_student_paradigm.R')





#### teacher-student task ----------------------------------------------------

#session 1
mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/session_1',sep="")
subfolder <-dir(mainfolder)
tab       <-data.frame()
print(length(subfolder))

for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  tab<-con_task_json('teacher_test_session1',tab,curnfolder,files,i) 
}  


#session 2
mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/session_2',sep="")
subfolder <-dir(mainfolder)
print(length(subfolder))
for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  tab<-con_task_json('teacher_test_session2',tab,curnfolder,files,i) 
}  

#check for duplicates
tab%>%group_by(prolific_id,Session)%>%summarise(length(trl))%>%View()


save(tab,file='myfolder/03_data/02_aggregated_data/tab_raw.Rdata')







####self-report ----------------------------------------------------
source('myfolder/01_functions/convert_json_self_reports.R')
mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/session_3',sep="")
subfolder=dir(mainfolder)

asrs<-wurs<-bis<-oci <-data.frame()
for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  
  wurs <-con_sr_json('wurs',wurs,curnfolder,files,i)
  asrs <-con_sr_json('asrs',asrs,curnfolder,files,i)
  bis  <-con_sr_json('bis',bis,curnfolder,files,i)
  oci  <-con_sr_json('oci',oci,curnfolder,files,i) 
  
}  
wurs[,1:25]<-sapply(wurs[,1:25],function(v) {as.numeric(v)})
asrs[,1:20]<-sapply(asrs[,1:20],function(v) {as.numeric(v)})
bis[,1:30] <-sapply(bis[,1:30],function(v) {as.numeric(v)})
oci[,1:18] <-sapply(oci[,1:18],function(v) {as.numeric(v)})

#check for duplicates
wurs[duplicated(wurs$prolific_id),]
asrs[duplicated(asrs$prolific_id),]
bis[duplicated(bis$prolific_id),]
oci[duplicated(oci$prolific_id),]

#save
save(wurs,file='myfolder/03_data/02_aggregated_data/wurs_raw.Rdata')
save(asrs,file='myfolder/03_data/02_aggregated_data/asrs_raw.Rdata')
save(bis,file='myfolder/03_data/02_aggregated_data/bis_raw.Rdata')
save(oci,file='myfolder/03_data/02_aggregated_data/oci_raw.Rdata')







#### icar ----------------------------------------------------
source('myfolder/01_functions/convert_json_icar.R')
mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/session_3',sep="")
subfolder=dir(mainfolder)
icar<-data.frame()

for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  icar <-con_icar_json('icar',icar,curnfolder,files,i,icar_cr)
  
} 
icar$prolific_id[duplicated(icar$prolific_id)]

save(icar,file='myfolder/03_data/02_aggregated_data/icar_raw.Rdata')





#####choice reaction ----------------------------------------------------
source('myfolder/01_functions/convert_json_choice_reaction_task.R')
mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/session_3',sep="")
subfolder=dir(mainfolder)
crt<-data.frame()

for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  crt <-con_choice_json('choice',crt,curnfolder,files,i)
  
}

#check for duplicates
names(crt)
crt%>%group_by(prolific_id)%>%summarise(length(subj))%>%View()

save(crt,file='myfolder/03_data/02_aggregated_data/crt_raw.Rdata')






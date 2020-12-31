rm(list=ls())
library("rjson")
source('myfolder/01_functions/convert_json_teacher_student_paradigm.R')

##########################  teacher-student task  ##########################
#here we extract the raw json files for the teacher-student paradigm (i.e., tsp) and put them in one long-format
#file named tab_raw

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

save(tab,file='myfolder/03_data/02_aggregated_data/tab_raw.Rdata')




########################## self-report ############################
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

save(wurs,file='myfolder/03_data/02_aggregated_data/wurs_raw.Rdata')
save(asrs,file='myfolder/03_data/02_aggregated_data/asrs_raw.Rdata')
save(bis,file='myfolder/03_data/02_aggregated_data/bis_raw.Rdata')
save(oci,file='myfolder/03_data/02_aggregated_data/oci_raw.Rdata')



####################### icar #######################
source('myfolder/01_functions/convert_json_icar.R')
mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/session_3',sep="")
icar<-data.frame()

for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  icar <-con_icar_json('icar',icar,curnfolder,files,i,icar_cr)
  
} 

save(icar,file='myfolder/03_data/02_aggregated_data/icar_raw.Rdata')

###################### choice reaction ######################
source('myfolder/01_functions/convert_json_choice_reaction_task.R')
mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/session_3',sep="")
crt<-data.frame()

for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  crt <-con_choice_json('choice',crt,curnfolder,files,i)
  
}


save(crt,file='myfolder/03_data/02_aggregated_data/crt_raw.Rdata')





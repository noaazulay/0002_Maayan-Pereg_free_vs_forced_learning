rm(list=ls())
maindir<-'myfolder/03_data/02_aggregated_data/'
load(paste(maindir,'icar_raw.Rdata',sep=""))
load(paste(maindir,'crt_raw.Rdata',sep=""))
load(paste(maindir,'wurs_raw.Rdata',sep=""))
load(paste(maindir,'asrs_raw.Rdata',sep=""))
load(paste(maindir,'bis_raw.Rdata',sep=""))
load(paste(maindir,'oci_raw.Rdata',sep=""))
load(paste(maindir,'tab_raw.Rdata',sep=""))

####import for prolifc payment decision
write.csv(oci$prolific_id,file='oci_prolificID.csv')
write.csv(icar$prolific_id[(icar$prolific_id %in% oci$prolific_id)==F],file='subjects_that_did_icar_but_not_oci_prolificID.csv')
head(tab)

library(dplyr)
head(tab)
bonus<-
tab%>%
  group_by(prolific_id,Session)%>%
  summarise(bonus=min(bonus))
write.csv(bonus,file='maayan_task_prolificID.csv')

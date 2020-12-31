########################################################################
################ convert json to Rdata  ################################
#########################################################################

#the following function covert files with a single entry per estimate per subject (e.g., self report)
#if you are expecting one row per subject in you data.frame - this is the right function
#note that here specifically prolificID is taked from the IQ test result names 'icar' as part of Pereg study

con_sr_json<-function(sr_name,datafile,curnfolder,files,subnum) {
  
  temp<-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('icar',files)],sep="")))
  prolific_id<-temp[,grepl(c('prolificId'), colnames(temp))]
  
  if (sum(grepl(sr_name,files))>0) {
    x <- as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl(sr_name,files)],sep="")))
    x <- x[,grepl('value',colnames(x))]
    colnames(x)<-sapply(1:ncol(x), function(cl) paste('item_',as.numeric(substr(gsub("([0-9]+).*$", "\\1", colnames(x)[cl]), start = 6,stop=1000000L))+1,sep=""))
    x$prolific_id<-rep(prolific_id,dim(x)[1])
    x$subj<-rep(subnum,dim(x)[1])
    return(rbind(datafile,x))}
  else{
    return(datafile)
  }
}

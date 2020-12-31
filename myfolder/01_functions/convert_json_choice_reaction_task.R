########################################################################
################ convert json to Rdata  ################################
####################################################################
#this converts choice reaction time task from jason to data.frame
#note that here prolific id is taken from the icar's iq test file as part of Pereg's study

con_choice_json<-function(task_name,datafile,curnfolder,files,subnum) {

if (sum(grepl(task_name,files))>0) {
    #x - will hold the raw converted json 
    x <- fromJSON(fromJSON(file=paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
    
    #index according to screen_id so x will hold only 'stim' screen with the relevant columns
    i<-sapply(1:length(x), function(i) {(x[[i]]$trial_id=='stim')}) 
    temp<-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('icar',files)],sep=""))) #this is just to get prolific_id
    prolific_id<-temp[,29]    
    
    key        =unlist(lapply(x[i],function(xx) {xx[names(xx)=='key_press']}))
    corr_key   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='corr_resp']}))
    
    df<-
      data.frame(
        subj       =rep(subnum,sum(i)),
        prolific_id=rep(prolific_id,sum(i)),
        block      =unlist(lapply(x[i],function(xx) {xx[names(xx)=='exp_stage']})),
        stim       =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stim']})),
        rt         =unlist(lapply(x[i],function(xx) {xx[names(xx)=='rt']})),
        key        =unlist(lapply(x[i],function(xx) {xx[names(xx)=='key_press']})),
        corr_key   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='corr_resp']})),
        trial_num  =unlist(lapply(x[i],function(xx) {xx[names(xx)=='trial_number']})),
        acc        =(corr_key==key)*1
      )
    
  return(rbind(datafile,df))
}
  else{
    return(datafile)
  }
}








#the following function covert files with a multiple entries per estimate per subject (e.g., task data)
#if you are excpecting multiple rows per subject in you data.frame - this is the right function
con_task_json<-function(task_name,datafile,curnfolder,files,subnum) {
  x <- fromJSON(fromJSON(file=paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
  i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]])})
  x<-x[i]
  i<-sapply(1:length(x), function(i) {exists('FB_probs',x[[i]])})
  df1<-as.data.frame(do.call(rbind,x[i]))
  df2<-as.data.frame(do.call(rbind,x[!i]))
  df1<-data.frame(rw=unlist(df1['feedback']),trl=unlist(df1['trial_num']),trl_cont=unlist(df1['running_trial']),obey=unlist(df1['obey']),
                  prob1=unlist(do.call(rbind,df1['FB_probs'][,1])[,1]),
                  prob2=unlist(do.call(rbind,df1['FB_probs'][,1])[,2]),
                  blk=unlist(df1$block_number))
  
  df2<-data.frame(rt=unlist(df2['rt']),trl=unlist(df2['trial_num']),trl_cont=unlist(df2['running_trial']),key=unlist(df2['key_press']),mini_block=unlist(df2['mini_block_number']),reveal_teacher_decision=unlist(df2['reveal_decision']),teacher_inst=unlist(df2['teacher_inst']),teacher_choice=unlist(df2['instruction_either']),
                  frcA=unlist(do.call(rbind,df2['stim_order'][,1])[,1]),frcB=unlist(do.call(rbind,df2['stim_order'][,1])[,2]),
                  ch=unlist(df2['stim_selected']),cond=unlist(df2['current_condition']),
                  blk=unlist(df2$block_number),
                  Session = unlist(df2$session),
                  ID = unlist(df2$ID),
                  deck.images1 = unlist(do.call(rbind,df2['deck_images'][,1])[,1]))
  library(data.table)
  df<-merge(df1,df2,by=c('blk','trl'),all=T)
  
  prolific_id<-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('starter',files)],sep="")))
  colnames(prolific_id)<-c('prolific_id')
  df$prolific_id<-rep(prolific_id$prolific_id,dim(df)[1])
  df$subj<-rep(subnum,dim(df)[1])
  
  
  x <- fromJSON(fromJSON(file = paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
  i<-sapply(1:length(x), function(i) {x[[i]]$trial_id=='end_game'})
  x<-x[i]
  df$bonus<-rep(x[[1]]$final_bonus,dim(df)[1])
  return(rbind(datafile,df))
}

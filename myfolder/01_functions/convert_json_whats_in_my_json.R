########################################################################
################ convert json to Rdata  ################################
####################################################################

#the following function covert files with a single entry per estimate per subject (e.g., self report)
#if you are excpecting one row per subject in you data.frame - this is the right function

con_sr_json<-function(sr_name,datafile,curnfolder,files,subnum) {
  
  prolific_id<-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('starter',files)],sep="")))
  colnames(prolific_id)<-c('prolific_id')
  
  if (sum(grepl(sr_name,files))>0) {
    x <- as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl(sr_name,files)],sep="")))
    x <- x[,grepl('value',colnames(x))]
    colnames(x)<-sapply(1:ncol(x), function(cl) paste('item_',as.numeric(substr(gsub("([0-9]+).*$", "\\1", colnames(x)[cl]), start = 6,stop=1000000L))+1,sep=""))
    x$prolific_id<-rep(prolific_id$prolific_id,dim(x)[1])
    x$subj<-rep(subnum,dim(x)[1])
    return(rbind(datafile,x))}
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
  
  prolific_id<-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('teacher_starter',files)],sep="")))
  colnames(prolific_id)<-c('prolific_id')
  df$prolific_id<-rep(prolific_id$prolific_id,dim(df)[1])
  df$subj<-rep(subnum,dim(df)[1])
  
  
  x <- fromJSON(fromJSON(file = paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
  i<-sapply(1:length(x), function(i) {x[[i]]$trial_id=='end_game'})
  x<-x[i]
  df$bonus<-rep(x[[1]]$final_bonus,dim(df)[1])
  return(rbind(datafile,df))
}


########################################################################
################ convert json to Rdata  ################################
#########################################################################

#the following function covert files with a single entry per estimate per subject (e.g., self report)
#if you are excpecting one row per subject in you data.frame - this is the right function

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

con_icar_json<-function(icar_name,datafile,curnfolder,files,subnum,icar_cr) {
  if (sum(grepl(icar_name,files))>0) {
    library(stringr)
    temp       <-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('icar',files)],sep="")))
    
    #take prolific id, item, response and item order from temp
    prolific_id<-temp[,grepl(c('prolificId'), colnames(temp))]
    item        <-temp[,grepl(c('name'), colnames(temp))==1]
    item        <- as.integer(sub("(?i).*?\\ICAR_\\s*(\\d+).*", "\\1", item))
    resp        <-temp[,grepl(c('value'), colnames(temp))==1]
    
    #keep a vector telling which item was given at the 1st position, 2nd position, etc.
    item_order<-item
    names(item_order)<-sapply(1:16, function(i) {paste('position',i,sep="")})
    
    #create a vector of acc
    acc<-vector()
    for (i in 1:16){
      cr=icar_cr[icar_cr[,3]==item[i],2]
      acc[i]=(resp[i]==cr)*1  
    }
    
    #order according to item
    names(resp)<-paste('item_',item,'_resp',sep="")
    names(acc)<-paste('item_',item,'_acc',sep="")
    resp<-resp[ ,order(names(resp))]
    acc <-acc [order(names(acc))]
    
    x<-as.data.frame(c(acc,resp,item_order))
    x<-data.frame(prolific_id=prolific_id,x)
    
    
    return(rbind(datafile,x))
  }
  else
  {
    return(datafile)
  }
}
#the following function covert files with a multiple entries per estimate per subject (e.g., task data)
#if you are excpecting multiple rows per subject in you data.frame - this is the right function

#task_name - is the name of the task included as part of the name of the json file. e.g., 'test' or 'practice'
#datafile  - is the data.frame where the converted data is stored

whats_in_my_json<-function(task_name,mainfolder,subj) {
  subfolder=dir(mainfolder)
  print(subfolder[subj])
  print(subj)
  
  #files - is the list of all files for a specific participant
  files     <-dir(curnfolder<-paste(mainfolder,'/',subfolder[subj],sep="", row.names=NULL))             
  
  #x - will hold the raw converted json 
  x         <- fromJSON(fromJSON(file=paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
  
  #screens_id lists all the available screens_id in the json
  screens_id<-unique(sapply(1:length(x), function(i) {(x[[i]]$trial_id)}))
  
  #add the names of the vars in each screen
  xx<-lapply(1:length(screens_id), function(scrn) {
    i<-sapply(1:length(x), function(i) {(x[[i]]$trial_id==screens_id[[scrn]])})
    colnames(as.data.frame(do.call(rbind,x[i])))
  })
  
  #add the name of the screens to xx
  names(xx)<-screens_id
  
  return(xx)
}


con_choice_json<-function(task_name,mainfolder) {
  subfolder=dir(mainfolder)
  df<-df.sng<-data.frame()
  # loop over all subjects
  for (subj in 1:length(subfolder)){
    print(subfolder[subj])
    print(subj)
    
    #files - is the list of all files for a specific participant
    files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[subj],sep="", row.names=NULL))             
    
    #x - will hold the raw converted json 
    x <- fromJSON(fromJSON(file=paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
    
    #index according to screen_id so x will hold only 'stim' screen with the relevant columns
    i<-sapply(1:length(x), function(i) {(x[[i]]$trial_id=='stim')}) 
    temp<-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('icar',files)],sep=""))) #this is just to get prolific_id
    prolific_id<-temp[,29]    
    
    key        =unlist(lapply(x[i],function(xx) {xx[names(xx)=='key_press']}))
    corr_key   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='corr_resp']}))
    
    x<-
      data.frame(
        subj       =rep(subj,sum(i)),
        prolific_id=rep(prolific_id,sum(i)),
        block      =unlist(lapply(x[i],function(xx) {xx[names(xx)=='exp_stage']})),
        stim       =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stim']})),
        rt         =unlist(lapply(x[i],function(xx) {xx[names(xx)=='rt']})),
        key        =unlist(lapply(x[i],function(xx) {xx[names(xx)=='key_press']})),
        corr_key   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='corr_resp']})),
        trial_num  =unlist(lapply(x[i],function(xx) {xx[names(xx)=='trial_number']})),
        acc        =(corr_key==key)*1
      )
    
    
    df<-rbind(x,df)
    rm(x)
    
  }
  
  return(df)
}

#aim: create data that can passed in to stan. Variables will be passed as subject x trial matrices.
#     These matrices are padded with Inf value to have exactly the same number of trials despite missing values
#contributors: Nitzan Shahar, Shira Niv
#inputs:
#Ntrials_per_subject - is a vector 1 x Nsubj with each cell including the number of available trials per subject
#subjects_list - is a vector with the subjects id
#var_toinclude - is a vector of strings with the names of data columns that should be included in the stan data
#Nparms - is the number of parameters the model is going to have (to create the mu and sigma population parameters)

make_mystandata<-function(data, Ntrials_per_subject, subjects_list,var_toinclude,Nparams){
  
  #find the largest number of available data per subject
  max_trials_per_subject=max(Ntrials_per_subject)
  
  #loop over the variables that needs to be included
  mydata<-lapply(var_toinclude,function(myvar) { 
    
  #for each variable, loop over all subjects to create a padded matrix
              t(sapply(subjects_list,function(subject) 
      
                      { #create vector for a specific variable and subject
                        current_var=df[df$subject==subject,myvar]
                        # data padding with Inf according to the max number of trials across subjects
                        c(current_var,rep(9999,max_trials_per_subject-sum(df$subject==subject)))})) 
    
  }
  )
  #add variables names
  names(mydata)=var_toinclude
  
  #add additional variables
  mydata=append(list(Nsubjects=length(subjects_list), 
                     Ntrials=max_trials_per_subject,  
                     Ntrials_per_subject=Ntrials_per_subject),
                     mydata)
}

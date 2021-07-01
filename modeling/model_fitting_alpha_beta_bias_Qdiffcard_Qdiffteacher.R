#aim: Parameter recovery for the teacher-student paradigm with reliability estimates
#contributor: Nitzan Shahar, Maayan Pereg

rm(list=ls())
library('rstan') # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 
library(MASS)
library(dplyr)



################################################################################################################
# loading data  -----------------------------------------------------------
################################################################################################################
load(paste('modeling/data/tab.Rdata',sep=""))
tab$ch=tab$ch+1
tab$frcA=tab$frcA+1
tab$frcB=tab$frcB+1

tab$raffle_student_ch=(tab$ch==tab$frcB)*1+1
tab$raffle_teacher_ch=(tab$teacher_choice==tab$frcB)*1+1
tab=tab[1:10000,]
head(tab[,c(
  'ch',
  'rw',
  'reveal',
  'obey',
  'frcA',
  'frcB',
  'raffle_student_ch',
  'raffle_teacher_ch')],50)



source('modeling/stan_models/make_mystandata.R')
data_for_stan<-make_mystandata(tab, 
                               subject_column     =tab$prolific_id,
                               var_toinclude      =c(
                                 'ch',
                                 'rw',
                                 'reveal',
                                 'obey',
                                 'frcA',
                                 'frcB',
                                 'raffle_student_ch',
                                 'raffle_teacher_ch'),
                               var_tobenamed      =c(
                                 'student_ch',
                                 'reward',
                                 'reveal',
                                 'follow',
                                 'offer1',
                                 'offer2',
                                 'raffle_student_ch',
                                 'raffle_teacher_ch'))

################################################################################################################
# fit model using stan --------------------------------------------
################################################################################################################


#fit stan model 
start_time <- Sys.time()
rl_fit<- stan(file = "modeling/stan_models/stan_alpha_beta_bias_Qcarddiff.stan", 
              data=data_for_stan, 
              iter=100,                          #number of warmup=0.5*iter
              chains=1,
              cores =1, 
              pars=c('mu'), #define which parameters to save so that the final file won't be larger then necessary
              save_warmup=F)

end_time <- Sys.time()

#save results
saveRDS(rl_fit,file='modeling/results/Parameter_recovery_alpha_beta_bias_iqcorr.rds')
trueparams=list(Nsubjects,mu,tau,cov_param,sigma,auxiliary_parameters)

save(trueparams,file='modeling/results/Parameter_recovery_alpha_beta_bias_iqcorr_trueparams.Rdata')


# examine mcmc ----------------------------------------------------------------------------
library("bayesplot")
library(ggplot2)
#rl_fit<-readRDS('modeling/results/Parameter_recovery_alpha_beta_bias_iqcorr.rds')
#plot posteriors
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(rl_fit,
           pars = c("mu[5]"),
           prob = 0.8) + plot_title



#plot mcmc chains
color_scheme_set("blue")
mcmc_trace(rl_fit, pars = c("mu[1]", "mu[2]","mu[3]","mu[4]"), n_warmup=0,
           facet_args = list(ncol = 1, strip.position = "left"))


traceplot(rl_fit, c("mu[1]", "mu[2]","mu[3]"), inc_warmup = TRUE, nrow = 3)


################################################################################################################
# compare recovered parameters to true parameters  --------------------------------------------
################################################################################################################

#individual parameters
plot(
  summary(rl_fit , pars=c("alpha"))$summary[,1], 
  inv.logit(auxiliary_parameters[,1]))


plot(
  summary(rl_fit , pars=c("beta"))$summary[,1], 
  exp(auxiliary_parameters[,2]))


plot(
  summary(rl_fit , pars=c("bias_intercept"))$summary[,1], 
  (auxiliary_parameters[,3]))


plot(
  summary(rl_fit , pars=c("bias_slope1"))$summary[,1], 
  (auxiliary_parameters[,4]))

plot(
  summary(rl_fit , pars=c("bias_slope2"))$summary[,1], 
  (auxiliary_parameters[,5]))











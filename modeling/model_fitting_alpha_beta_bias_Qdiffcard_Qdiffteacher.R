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
tab$teacher_choice=tab$teacher_choice+1

tab$raffle_student_ch=(tab$ch==tab$frcB)*1+1
tab$raffle_teacher_ch=(tab$teacher_choice==tab$frcB)*1+1
names(tab)

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
rl_fit<- stan(file = "modeling/stan_models/stan_alpha_beta_bias.stan", 
              data=data_for_stan, 
              iter=2000,                          #number of warmup=0.5*iter
              chains=4,
              cores =4, 
              #pars=c('mu'), #define which parameters to save so that the final file won't be larger then necessary
              save_warmup=F)

end_time <- Sys.time()

#save results
saveRDS(rl_fit,file='modeling/results/model_fit_alpha_beta_bias.rds')


# examine mcmc ----------------------------------------------------------------------------
library("bayesplot")
library(ggplot2)
#rl_fit<-readRDS('modeling/results/Parameter_recovery_alpha_beta_bias_iqcorr.rds')
#plot posteriors
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(rl_fit,
           pars = c("mu[2]"),
           prob = 0.95) + plot_title
inv.logit(-2)


#plot mcmc chains
color_scheme_set("blue")
mcmc_trace(rl_fit, pars = c("mu[1]", "mu[2]","mu[3]"), n_warmup=0,
           facet_args = list(ncol = 1, strip.position = "left"))


traceplot(rl_fit, c("mu[1]", "mu[2]","mu[3]"), inc_warmup = TRUE, nrow = 3)


hist(summary(rl_fit , pars=c("bias"))$summary[,1])

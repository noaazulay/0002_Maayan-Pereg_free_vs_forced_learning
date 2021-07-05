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
my_models = c("modeling/stan_models/stan_alpha_beta_bias.stan",
              "modeling/stan_models/stan_alpha_beta_bias_Qcarddiff.stan",
              "modeling/stan_models/stan_alpha_beta_bias_Qcarddiff_Qteacherdiff.stan")

rl_fit<- stan(file = my_models[3], 
              data=data_for_stan, 
              iter=2000,                          #number of warmup=0.5*iter
              chains=4,
              cores =4, 
              #pars=c('mu'), #define which parameters to save so that the final file won't be larger then necessary
              save_warmup=F)

end_time <- Sys.time()

#save results
#saveRDS(rl_fit,file='modeling/results/model_fit_alpha_beta_bias_Qdiffcard_Qdiffteacher.rds')


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


traceplot(rl_fit, c("mu[1]", "mu[2]","mu[3]","mu[4]"), inc_warmup = TRUE, nrow = 3)

hist(summary(rl_fit , pars=c("alpha_card"))$summary[,1])
hist(summary(rl_fit , pars=c("alpha_teacher"))$summary[,1])

hist(summary(rl_fit , pars=c("bias_intercept"))$summary[,1])
hist(summary(rl_fit , pars=c("bias_slope1"))$summary[,1])
hist(summary(rl_fit , pars=c("bias_slope2"))$summary[,1])

alpha_card=summary(rl_fit , pars=c("alpha_card"))$summary[,1]
alpha_teacher=summary(rl_fit , pars=c("alpha_teacher"))$summary[,1]
beta=summary(rl_fit , pars=c("beta"))$summary[,1]
beta_intercept=summary(rl_fit , pars=c("bias_intercept"))$summary[,1]
beta_slope1=summary(rl_fit , pars=c("bias_slope1"))$summary[,1]
beta_slope2=summary(rl_fit , pars=c("bias_slope2"))$summary[,1]

load(paste('modeling/data/IQ_ADHD.Rdata',sep=""))
head(IQ_ADHD)
iq=IQ_ADHD$iq
subj=unique(tab$prolific_id)
m<-data.frame(prolific_id=subj,alpha_card,alpha_teacher,beta,beta_intercept,beta_slope1,beta_slope2)
m2<-merge(IQ_ADHD,m,by=c('prolific_id'))
plot(m2$iq,as.numeric(m2$beta_slope1))
cor.test(m2$iq,as.numeric(m2$beta_slope1))

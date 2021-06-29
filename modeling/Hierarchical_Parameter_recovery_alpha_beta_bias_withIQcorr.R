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
# generate population and subject level parameters -----------------------------------------------------------
################################################################################################################

Nsubjects = 10

#population location parameters
mu=c(
alpha              =logit(0.4),
bias               =0,
beta               =log(3),
iq_slope           =0.5
)
Nparam=length(mu)

#population scale parameters
tau          =c(.25,.05,.05,.1) #var vector
cov_param    =0
sigma        = diag(tau)
sigma[!diag(nrow=Nparam)]=cov_param

# sample aux parameters
auxiliary_parameters = mvrnorm(n = Nsubjects, mu = mu, Sigma = sigma)
cor(auxiliary_parameters[,3],auxiliary_parameters[,4])



################################################################################################################
# run a simulation study -----------------------------------------------------------
################################################################################################################

source('modeling/sim_functions/simme_tsp_alpha_beta_bias_withIQcorr.R')

#main configuration variables for the simulation
Nalt    =4         #number of alternatives
Noffer  =2         #how many cards from the deck are presented
Nblocks =3         #number of blocks
Ntrials =130       #number of trials

rndwlk_card=cbind(as.matrix(read.csv('modeling/sim_functions/rndwalk_4cards_130trials.csv',header=F)),
                  as.matrix(read.csv('modeling/sim_functions/rndwalk_4cards_130trials.csv',header=F)),
                  as.matrix(read.csv('modeling/sim_functions/rndwalk_4cards_130trials.csv',header=F))) #duplicate for three blocks

rndwlk_teacher=as.vector(as.matrix(read.csv('modeling/sim_functions/rndwalk_3teachers_130trials.csv',header=F))) #duplicate for three blocks
                   
                  
cfg=list(      Nblocks=Nblocks,
               Ntrials=Ntrials,
               Nalt   =Nalt,
               Noffer =Noffer,
               rndwlk_card   =rndwlk_card,
               rndwlk_teacher=rndwlk_teacher,
               teacher.rate=0.6)
#simulated iq scores
iq=rnorm(Nsubjects)


# simulating N agents 
df<-lapply(1:Nsubjects,function(s)   {cfg$subject=s
                                  sim.block(par=auxiliary_parameters[s,],cfg,iq[s])})
df<-do.call(rbind,df)
df%>%group_by(subject)%>%summarise(reward=mean(reward))%>%plot()    


# add abort column to simulate missing trials due to missing values or trial omission
max_precent_of_aborted_trials=0.1
df$abort<-0
for (subject in seq(1:Nsubjects)){
    
    index_abort           =sample(which(df$subject==subject),runif(1,min=0,max=max_precent_of_aborted_trials)*Ntrials*Nblocks)  #index of rows to abort
  
    df$abort[index_abort]=1
}



#count and omit aborted trials
df%>%group_by(subject)%>%summarise(mean(abort))
df<-df[df$abort==0,]
df%>%group_by(subject)%>%summarise(mean(abort))




################################################################################################################
# parameter recovery with stan --------------------------------------------
################################################################################################################
source('modeling/stan_models/make_mystandata.R')
data_for_stan<-make_mystandata(data, 
                               subjects_list      =unique(df$subject),
                               Ntrials_per_subject=as.numeric(df%>%group_by(subject)%>%summarise(trials=length(trial))%>%select(trials)%>%unlist()),
                               var_toinclude      =c(
                                 'student_ch',
                                 'reward',
                                 'reveal',
                                 'follow',
                                 'offer1',
                                 'offer2',
                                 'raffle_student_ch',
                                 'raffle_teacher_ch'))

data_for_stan=append(data_for_stan,list(iq=iq))

#fit stan model 
start_time <- Sys.time()
rl_fit<- stan(file = "modeling/stan_models/stan_alpha_beta_bias_withIQcorr.stan", 
              data=data_for_stan, 
              iter=1000,                          #number of warmup=0.5*iter
              chains=1,
              cores =1, 
              #pars=c('alpha','bias','beta','mu'), #define which parameters to save so that the final file won't be larger then necessary
              save_warmup=F)

end_time <- Sys.time()

# examine mcmc ----------------------------------------------------------------------------
library("bayesplot")
library(ggplot2)
#plot posteriors
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(rl_fit,
           pars = c("mu[4]"),
           prob = 0.8) + plot_title


mcmc_areas(rl_fit,
           pars = c("iq_slope_corr"),
           prob = 0.8) + plot_title

#plot mcmc chains
color_scheme_set("blue")
mcmc_trace(rl_fit, pars = c("mu[1]", "mu[2]","mu[3]"), n_warmup=500,
           facet_args = list(ncol = 1, strip.position = "left"))


traceplot(rl_fit, c("mu[1]", "mu[2]","mu[3]"), inc_warmup = TRUE, nrow = 3)

posterior=as.array(rl_fit)
mcmc_trace(rl_fit, pars = c("bias"))
print(rl_fit)
#rl_fit<-readRDS('fit.rds')
mcmc_trace(rl_fit, pars = c("auxiliary_parameters[1,1]"))
mcmc_areas(posterior,pars='bias[1]')

################################################################################################################
# compare recovered parameters to true parameters  --------------------------------------------
################################################################################################################
        
#individual parameters
plot(
  summary(rl_fit , pars=c("alpha"))$summary[,1], 
  inv.logit(auxiliary_parameters[,1]))

plot(
  summary(rl_fit , pars=c("bias"))$summary[,1], 
  (auxiliary_parameters[,2]))


plot(
summary(rl_fit , pars=c("beta"))$summary[,1], 
exp(auxiliary_parameters[,3]))








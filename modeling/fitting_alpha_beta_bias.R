#aim: Parameter recovery for the teacher-student paradigm with reliability estimates
#contributor: Nitzan Shahar, Maayan Pereg

rm(list=ls())
library('rstan') # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 
library(MASS)
library(dplyr)


# load data -----------------------------------------------------------
load('modeling/data/tab.Rdata')

Nsubj =50

#population location parameters
mu=c(
alpha              =logit(0.4),
bias               =0,
beta               =log(3)
)
Nparam=length(mu)

#population scale parameters
tau          =c(.25,.05,.05) #var vector
cov_param    =0
sigma        = diag(tau)
sigma[!diag(nrow=Nparam)]=cov_param

# sample aux parameters
auxiliary_parameters = mvrnorm(n = Nsubj, mu = mu, Sigma = sigma)

hist(inv.logit(auxiliary_parameters[,1]))
hist(auxiliary_parameters[,2])
hist(exp(auxiliary_parameters[,3]))

# run a simulation study -----------------------------------------------------------

source('modeling/sim_functions/simme_tsp_alpha_beta_bias.R')

#main configuration variables for the simulation
Nalt  =4         #number of alternatives
Noffer=2         #how many cards from the deck are presented
Nblock=3         #number of blocks
Ntrl  =130       #number of trials

rndwlk_card=cbind(as.matrix(read.csv('modeling/sim_functions/rndwalk_4cards_130trials.csv',header=F)),
                  as.matrix(read.csv('modeling/sim_functions/rndwalk_4cards_130trials.csv',header=F)),
                  as.matrix(read.csv('modeling/sim_functions/rndwalk_4cards_130trials.csv',header=F))) #duplicate for three blocks


rndwlk_teacher=as.vector(as.matrix(read.csv('modeling/sim_functions/rndwalk_3teachers_130trials.csv',header=F))) #duplicate for three blocks
                   
                  
cfg=list(      Nblock=Nblock,
               Ntrl  =Ntrl,
               Nalt  =Nalt,
               Noffer=Noffer,
               rndwlk_card   =rndwlk_card,
               rndwlk_teacher=rndwlk_teacher,
               teacher.rate=0.6)

# simulating N agents 
df<-lapply(1:Nsubj,function(s)   {cfg$subject=s
                                  sim.block(par=auxiliary_parameters[s,],cfg)})

df<-do.call(rbind,df)

df%>%group_by(subject)%>%summarise(reward=mean(reward))%>%plot()    

# parameter recovery with stan --------------------------------------------

#prepare action and reward matrices (subject x trial)
student_ch       =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'student_ch']}))
reward           =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'reward']}))
reveal           =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'reveal']}))
follow           =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'follow']}))
offer1           =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'offer1']}))
offer2           =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'offer2']}))
raffle_student_ch=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'raffle_student_ch']}))
raffle_teacher_ch=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'raffle_teacher_ch']}))

#prepare data
model_data <- list(Nsubj = Nsubj,
                   Ntrials = Ntrl,
                   Narms = Nalt,
                   student_ch = student_ch,
                   reward = reward,
                   reveal = reveal,
                   follow = follow,
                   offer1 = offer1,
                   offer2 = offer2,
                   raffle_student_ch=raffle_student_ch,
                   raffle_teacher_ch=raffle_teacher_ch,
                   Nparam = 3
                   )
        
#fit stan model 
start_time <- Sys.time()
rl_fit<- stan(file = "modeling/stan_models/stan_alpha_beta_bias.stan", data=model_data, iter=2000,chains=4,cores =4) #iter - number of MCMC samples 
end_time <- Sys.time()

# examine mcmc ----------------------------------------------------------------------------
library("bayesplot")
plot(rl_fit)
posterior=as.array(rl_fit)
mcmc_trace(rl_fit, pars = c("bias"))
print(rl_fit)
#rl_fit<-readRDS('fit.rds')
mcmc_trace(rl_fit, pars = c("auxiliary_parameters[1,1]"))
mcmc_areas(posterior,pars='bias[1]')

# compare recovered parameters to true parameters  --------------------------------------------

        
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








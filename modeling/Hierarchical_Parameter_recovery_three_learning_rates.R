#aim: Parameter recovery for the teacher-student paradigm with reliability estimates
#contributor: Nitzan Shahar, Maayan Pereg

rm(list=ls())
library('rstan') # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 
library(MASS)
library(dplyr)


# generate population and subject level parameters -----------------------------------------------------------

Nsubj =10

#population location parameters
mu=c(
alpha_follow       =logit(0.4),
alpha_oppose       =logit(0.3),
alpha_free         =logit(0.5),
beta               =log(3)
)
Nparam=length(mu)

#population scale parameters
tau          =c(.25,.25,.25,.05) #var vector
cov_param    =0
sigma        = diag(tau)
sigma[!diag(nrow=Nparam)]=cov_param

# sample aux parameters
auxiliary_parameters = mvrnorm(n = Nsubj, mu = mu, Sigma = sigma)

hist(inv.logit(auxiliary_parameters[,1]))
hist(inv.logit(auxiliary_parameters[,2]))
hist(inv.logit(auxiliary_parameters[,3]))
hist(exp(auxiliary_parameters[,4]))

# run a simulation study -----------------------------------------------------------

source('modeling/sim_functions/simme_tsp_three_learning_rate.R')

#main configuration variables for the simulation
Nalt  =4         #number of alternatives
Noffer=2         #how many cards from the deck are presented
Ntrl  =300       #number of trials

cfg=list(      Ntrl  =Ntrl,
               Nalt  =Nalt,
               Noffer=Noffer,
               rndwlk_frac   =as.matrix(read.csv('modeling/sim_functions/rndwlk_4frc_1000trials.csv',header=F)),
               rndwlk_teacher=rep(5,Ntrl),
               teacher.rate=0.6)

# simulating N agents 

df<-lapply(1:Nsubj,function(s)   {cfg$subject=s
                                  sim.block(par=auxiliary_parameters[s,],cfg)})

df<-do.call(rbind,df)

df%>%group_by(trial)%>%summarise(Q1=mean(Qcard3))%>%plot()    
df%>%group_by(subject)%>%summarise(reward=mean(reward))%>%plot()    
df%>%group_by(trial)%>%summarise(follow=mean(student.follow))%>%plot()    

# parameter recovery with stan --------------------------------------------

#prepare action and reward matrices (subject x trial)
student_ch=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'student_ch']}))
reward    =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'reward']}))
reveal    =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'reveal']}))
follow    =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'follow']}))
offer1    =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'offer1']}))
offer2    =t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'offer2']}))
raffle_ch=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'raffle_ch']}))

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
                   raffle_ch=raffle_ch,
                   Nparam = 4
                   )
        
#fit stan model 
start_time <- Sys.time()
rl_fit<- stan(file = "modeling/stan_models/stan_three_learning_rates.stan", data=model_data, iter=100,chains=1,cores =1) #iter - number of MCMC samples 
end_time <- Sys.time()

print(rl_fit)
#rl_fit<-readRDS('fit.rds')


# compare recovered parameters to true parameters  --------------------------------------------

        
#individual parameters
plot(
  alpha_free_recovered=summary(rl_fit , pars=c("alpha_follow"))$summary[,1], 
  inv.logit(auxiliary_parameters[,1]))

plot(
  alpha_free_recovered=summary(rl_fit , pars=c("alpha_oppose"))$summary[,1], 
  inv.logit(auxiliary_parameters[,2]))


plot(
alpha_free_recovered=summary(rl_fit , pars=c("alpha_free"))$summary[,1], 
inv.logit(auxiliary_parameters[,3]))




#population level (hyperparameter)
summary(rl_fit , pars=c("mu"))$summary[,1]


alpha_aux_mu_recovered   = (summary(rl_fit , pars=c("mu[1]"))$summary[,1])
beta_aux_mu_recovered    = summary(rl_fit , pars=c("mu[2]"))$summary[,1]
sigma_recovered          = matrix(summary(rl_fit , pars=c("sigma_matrix"))$summary[,1],2,2)
omega_recovered          = cov2cor(sigma_matrix_recovered)

tau_recovered            =summary(rl_fit , pars=c("tau"))$summary[,1]
L_Omega_recovered        =matrix(summary(rl_fit , pars=c("L_Omega"))$summary[,1],2,2)
omega=diag(tau_recovered)*L_Omega_recovered*t(L_Omega_recovered)
omega*diag(tau_recovered)
cov2cor(omega*diag(tau_recovered))




#compare recovered to true population parameters
#location parameters
cat(paste('true alpha population parm is',     alpha_mu,' sample mean is',    mean(true.parms[,1]),'and recovered is',          inv.logit(alpha_aux_mu_recovered)),
    paste('true beta population parm is',      beta_mu,' sample mean is',     mean(true.parms[,2]),'and recovered is',          exp(beta_aux_mu_recovered)),
    paste('true alpha aux population parm is', alpha_aux_mu,' sample mean is',mean(auxiliary_parameters[,1]),'and recovered is',alpha_aux_mu_recovered),
    paste('true beta aux population parm is',  beta_aux_mu,' sample mean is', mean(auxiliary_parameters[,2]),'and recovered is',beta_aux_mu_recovered),
    sep = '\n')

#scale parameters
cat(paste('true alpha aux var parm is',        alpha_aux_var,' sample mean is',  var(auxiliary_parameters[,1]),'and recovered is',alpha_aux_var_recovered),
    paste('true beta aux var parm is',         beta_aux_var,' sample mean is',   var(auxiliary_parameters[,2]),'and recovered is',beta_aux_var_recovered),
    paste('true corr between aux parms is',    corr_alpha_beta,' sample mean is',cor(auxiliary_parameters[,1],auxiliary_parameters[,2]),
      'and recovered is',cov_alpha_beta_recovered/(sqrt(alpha_aux_var_recovered)*sqrt(beta_aux_var_recovered))),
    sep = '\n')


#individual level parameters (subjects parameters)
alpha_individual_recovered=summary(rl_fit , pars=c("alpha"))$summary[,1] 
beta_individual_recovered=summary(rl_fit , pars=c("beta"))$summary[,1]
plot(true.parms[,1],(alpha_individual_recovered))
plot(true.parms[,2],(beta_individual_recovered))
cor(true.parms,cbind(alpha_individual_recovered,beta_individual_recovered))

#additional
summary(rl_fit, pars=c("L_Omega"))$summary
sigma_matrix_recovered=summary(rl_fit, pars=c("sigma_matrix"))$summary[,1]


cor(true.parms[,1],true.parms[,2])
cor(alpha_recovered,beta_recovered)
var(true.parms[,1])
var(alpha_recovered)
var(true.parms[,2])
var(beta_recovered)



library("shinystan")
launch_shinystan(rl_fit)


#print table to file
df.tbl %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))        






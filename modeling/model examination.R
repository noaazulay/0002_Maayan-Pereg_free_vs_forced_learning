rm(list=ls())


my_models = c("modeling/results/model_fit_alpha_beta_bias.rds",
              "modeling/results/model_fit_alpha_beta_bias_Qcarddiff.rds",
              "modeling/results/model_fit_alpha_beta_bias_Qcarddiff_Qteacherdiff.rds")

rl_fit<-readRDS(my_models[2])

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

# subject's parameters ----------------------------------------------------------------------------
load(paste('modeling/data/tab.Rdata',sep=""))
subjects_list=unique(tab$prolific_id)

#alpha_beta_bias_Qdiff
alpha=summary(rl_fit , pars=c("alpha"))$summary[,1]
beta=summary(rl_fit , pars=c("beta"))$summary[,1]
bias=summary(rl_fit , pars=c("bias"))$summary[,1]
indv_params<-data.frame(prolific_id=subjects_list,alpha,beta,bias)
save(indv_params,file='modeling/results/indv_params_alpha_beta_bias.Rdata')


#alpha_beta_bias_Qdiff
alpha=summary(rl_fit , pars=c("alpha"))$summary[,1]
beta=summary(rl_fit , pars=c("beta"))$summary[,1]
bias_intercept=summary(rl_fit , pars=c("bias_intercept"))$summary[,1]
bias_slope1=summary(rl_fit , pars=c("bias_slope1"))$summary[,1]
indv_params<-data.frame(prolific_id=subjects_list,alpha,beta,bias_intercept,bias_slope1)
save(indv_params,file='modeling/results/indv_params_alpha_beta_bias_Qdiffcard.Rdata')

#alpha_beta_bias_Qdiff
alpha=summary(rl_fit , pars=c("alpha"))$summary[,1]
beta=summary(rl_fit , pars=c("beta"))$summary[,1]
bias_intercept=summary(rl_fit , pars=c("bias_intercept"))$summary[,1]
bias_slope1=summary(rl_fit , pars=c("bias_slope1"))$summary[,1]
bias_slope2=summary(rl_fit , pars=c("bias_slope2"))$summary[,1]

indv_params<-data.frame(prolific_id=subjects_list,alpha_card,alpha_teacher,beta,beta_intercept,beta_slope1,beta_slope2)
save(indv_params,file='modeling/results/indv_params_alpha_beta_bias_Qdiffcard.Rdata')



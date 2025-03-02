rm(list=ls())
my_models = c("modeling/results/model_fit_alpha_beta_bias.rds",
              "modeling/results/model_fit_alpha_beta_bias_Qcarddiff.rds",
              "modeling/results/model_fit_alpha_beta_bias_Qcarddiff_Qteacherdiff.rds")

model_alpha_beta_bias<-readRDS(my_models[1])
model_alpha_beta_bias_Qcarddiff<-readRDS(my_models[2])
model_alpha_beta_bias_Qcarddiff_teacher<-readRDS(my_models[3])


library(loo)
loo1_loglike<-extract_log_lik(model_alpha_beta_bias, parameter_name = "log_lik", merge_chains = TRUE) 
loo1_loglike_vector<-as.vector(loo1_loglike)
loo1_r_eff <- relative_eff(exp(loo1_loglike_vector), cores = 4)
loo_1 <- loo(log_lik_1, r_eff = r_eff, cores = 2)


loo_1<-loo(model_alpha_beta_bias,
    pars = "log_lik",
    save_psis = FALSE,
    cores = 4,
    moment_match = FALSE,
    k_threshold = 0.7)

loo_2<-loo(model_alpha_beta_bias_Qcarddiff,
          pars = "log_lik",
          save_psis = FALSE,
          cores = 4,
          moment_match = FALSE,
          k_threshold = 0.7)


loo_3<-loo(model_alpha_beta_bias_Qcarddiff,
          pars = "log_lik",
          save_psis = FALSE,
          cores = 4,
          moment_match = FALSE,
          k_threshold = 0.7)

comp<-loo_compare(loo_1, loo_2)


# model comparison BF -----------------------------------------------------

library(bridgesampling)
lml_1 <- bridge_sampler(model_alpha_beta_bias, silent = TRUE)
lml_2 <- bridge_sampler(model_alpha_beta_bias_Qcarddiff, silent = TRUE)
BF_att <- bridgesampling::bf(lml_1, lml_null)
BF_att

#PSIS-LOO
(loo_pos <- loo(fit_1))
(loo_null <- loo(fit_null))
loo_compare(loo_pos, loo_null)

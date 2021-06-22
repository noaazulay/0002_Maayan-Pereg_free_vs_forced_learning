data {
  int<lower = 1> Nsubj; //number of subjects
  int<lower = 1> Ntrials; //number of trials
  int<lower = 2> Narms; //number of alternatives 
  int<lower = 1> Nparam; //number of parameters
  int<lower = 0> a1[Nsubj,Ntrials]; //index of which arm was pulled
  int<lower = 0, upper = 1> reward[Nsubj,Ntrials]; //outcome of bandit arm pull
  int<lower = 0, upper = 1> reveal[Nsubj,Ntrials]; //did the teacher give instructions
  int<lower = 0, upper = 1> follow[Nsubj,Ntrials]; //did the teacher give instructions
  int<lower = 1, upper = 4> offer1[Nsubj,Ntrials]; //did the teacher give instructions
  int<lower = 1, upper = 4> offer2[Nsubj,Ntrials]; //did the teacher give instructions
  int<lower = 1, upper = 4> teacher_ch[Nsubj,Ntrials]; //did the teacher give instructions


}

parameters {
  vector[Nparam] auxiliary_parameters[Nsubj]; 

  //hyper parameters 
  vector[Nparam] mu;
  vector<lower=0>[Nparam] tau; //vector of random effects variance
  cholesky_factor_corr[Nparam] L_Omega;

}

transformed parameters {
      real alpha[Nsubj];
      real alpha_follow[Nsubj];
      real alpha_oppose[Nsubj];
      real alpha_free[Nsubj];
      real alpha_teacher_as_bandit[Nsubj];
      real beta[Nsubj];
      real w_teacher1[Nsubj];
      real w_teacher2[Nsubj];
      matrix [Nsubj,Ntrials] log_lik;
      vector<lower=0, upper=1>[2]     Qnet;
      vector<lower=0, upper=1>[Narms] Qcard;
      vector<lower=0, upper=1>[2]     Qteacher;
      real PE;

      
      //setting the scale matrix for hyperparameters
      matrix[Nparam,Nparam] sigma_matrix;
      sigma_matrix = diag_pre_multiply(tau, (L_Omega*L_Omega')); //L_Omega*L_omega' give us Omega (the corr matrix). 
      sigma_matrix = diag_post_multiply(sigma_matrix, tau);     // diag(tau)*omega*diag(tau) gives us sigma_matirx (the cov matrix)


  for (subj in 1:Nsubj){
        alpha_follow[subj]              = inv_logit(auxiliary_parameters[subj][1]);
        alpha_oppose[subj]              = inv_logit(auxiliary_parameters[subj][2]); 
        alpha_free[subj]                = inv_logit(auxiliary_parameters[subj][3]); 
        alpha_teacher_as_bandit[subj]   = inv_logit(auxiliary_parameters[subj][4]); 
        beta[subj]                      = exp(auxiliary_parameters[subj][5]);
       

        for (a in 1:2)     Qnet[a]     = 0;
        for (a in 1:Narms) Qcard[a]    = 0;
        for (a in 1:2)     Qteacher[a] = 0;
        

        for (trial in 1:Ntrials){
            
            Qnet[1]= Qcard[offer1[subj,trial]];
            Qnet[2]= Qcard[offer2[subj,trial]];
            
            if(teacher_ch[subj,trial]==offer1[subj,trial]) {
            Qnet[1]+=w_teacher1[subj] +w_teacher2[subj]*Qteacher[1];
            Qnet[2]+=0                +w_teacher2[subj]*Qteacher[2];
            };
            
           if(teacher_ch[subj,trial]==offer1[subj,trial]) {
            Qnet[2]+=w_teacher1[subj] +w_teacher2[subj]*Qteacher[1];
            Qnet[1]+=0                +w_teacher2[subj]*Qteacher[2];
            };
            
            
            if (reveal[subj,trial]==0) alpha[subj]=alpha_free[subj];
            if (reveal[subj,trial]==1 && follow[subj,trial]==1) alpha[subj]=alpha_follow[subj];
            if (reveal[subj,trial]==1 && follow[subj,trial]==0) alpha[subj]=alpha_oppose[subj];

            log_lik[subj,trial]=log_softmax(Qcard*beta[subj])[a1[subj,trial]];
            //card update
            PE= reward[subj,trial] - Qcard[a1[subj,trial]];
            Qcard[a1[subj,trial]] += alpha[subj] * PE;
            
            //teacher update
            PE= reward[subj,trial] - Qteacher[follow[subj,trial]+1];
            Qcard[follow[subj,trial]+1] += alpha_teacher_as_bandit[subj] * PE;
        } 
  }

}
model {
  
  // population level priors (hyper-parameters)
  mu ~ normal(0, 5);
  tau ~ cauchy(0, 1);             //tau is the hyperparameters variance vector
  L_Omega ~ lkj_corr_cholesky(2); //L_omega is the lower triangle of the correlations. Setting the lkj prior to 2 means the off-diagonals are priored to be near zero


  // indvidual level priors (subject parameters)
  auxiliary_parameters ~ multi_normal(mu, sigma_matrix);

  target += sum(log_lik);

}


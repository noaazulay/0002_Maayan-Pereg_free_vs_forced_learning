data {
  
  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;           //number of subjects
  int<lower = 1> Ntrials;             //number of trials without missing data, typically the maximum of trials per subject
  int<lower = 1> Ntrials_per_subject[Nsubjects];  //number of trials left for each subject after data omission

  
  //Behavioral data:
  //each variable being a subject x trial matrix
  //note that sunce all subjects need to have the same number of trials,
  //and since some have missing data, this cases should be transfered to the end of the matrix and padded with Inf using the
  //make_mystandata function
  int<lower = 0> offer1[Nsubjects,Ntrials];            //what card was offered on the first position (e.g., left)
  int<lower = 0> offer2[Nsubjects,Ntrials];            //what card was offered on the second position (e.g., right)
  int<lower = 0> student_ch[Nsubjects,Ntrials];        //index of which arm was pulled coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> raffle_student_ch[Nsubjects,Ntrials]; //which arm did the student select (coded 1 or 2)
  int<lower = 0> raffle_teacher_ch[Nsubjects,Ntrials]; //which arm did the teacher select (coded 1 or 2)
  int<lower = 0> reveal[Nsubjects,Ntrials];            //whether the teacher decided to reavel its choice
  int<lower = 0> follow[Nsubjects,Ntrials];            //whether the student followed the teacher advice

}
transformed data{
    int<lower = 1> Nparameters; //number of parameters
    int<lower = 2> Narms;       //number of overall alternatives
    int<lower = 2> Nraffle;     //number of alternatives offered each trial
    
    Nparameters=4;
    Narms = 4;
    Nraffle=2;
}

parameters {

  //population level parameters 
  vector[Nparameters] mu;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] tau;          //vector of random effects variance for each model parameter
  cholesky_factor_corr[Nparameters] L_Omega; //lower triangle of a correlation matrix to be used for the random effect of the model parameters
  
  //subject level parameters
  vector[Nparameters] auxiliary_parameters[Nsubjects]; 


}

transformed parameters {
//declare parameter and variables

      //hyperparameters
      matrix[Nparameters,Nparameters] sigma_matrix;
      
      //individuals parameters
      real alpha[Nsubjects];
      real beta[Nsubjects];
      real bias[Nsubjects];
      real bias_intercept[Nsubjects];
      real bias_slope1[Nsubjects];
      
      //additional variabels
      matrix [Nsubjects,Ntrials] log_lik;
      vector [Nraffle] Qnet;
      vector<lower=0, upper=1>[Narms] Qcard;
      real PEcard;
      
      //Scale matrix for individual level parameters
      //specifically we are intrested in getting a sigma matrix we is the covariance matrix that is used to sample
      //the model parameters from a multivariate normal in the "model" block for stan
      //here, we take tau (variance vector) and L_Omega (Lower triangle of correlation matrix)
      //and convert them to the sigma_matrix (covariance matrix)
      sigma_matrix = diag_pre_multiply(tau, (L_Omega*L_Omega')); //L_Omega*L_omega' give us Omega (the corr matrix). 
      sigma_matrix = diag_post_multiply(sigma_matrix, tau);     // diag(tau)*omega*diag(tau) gives us sigma_matirx (the cov matrix)

      
      //preallocate loglike
      log_lik=rep_matrix(0,Nsubjects,Ntrials);
      
      
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  for (subject in 1:Nsubjects){
        //assiging subject level model parameters
        //use inv_logit for parameters that should be between 0 and 1
        //use exp for parameters that should be positive
        //leave without transformation for the reast of model parameters
        alpha[subject]              = inv_logit(auxiliary_parameters[subject][1]);
        beta[subject]               = exp(auxiliary_parameters[subject][2]);
        bias_intercept[subject]     = auxiliary_parameters[subject][3];
        bias_slope1[subject]        = auxiliary_parameters[subject][4]; 

        
        //pre-assignment of Qvalues
        for (a in 1:Nraffle) Qnet[a]     = 0;
        for (a in 1:Narms)   Qcard[a]    = 0;


        //trial by trial loop
        
        for (trial in 1:Ntrials_per_subject[subject]){
            
            //calculate integrated Qvalues based on student and teacher internal values            
            Qnet[1]= Qcard[offer1[subject,trial]];
            Qnet[2]= Qcard[offer2[subject,trial]];
            
            bias[subject]=bias_intercept[subject]+bias_slope1[subject]*(fabs(Qnet[1]-Qnet[2]))'
            
            Qnet[raffle_teacher_ch[subject,trial]]=Qnet[raffle_teacher_ch[subject,trial]]+bias[subject];

            //liklihood function (softmax)
            log_lik[subject,trial]=log_softmax(Qnet*beta[subject])[raffle_student_ch[subject,trial]];

            //Qvalues update
            PEcard   = reward[subject,trial] - Qcard[student_ch[subject,trial]];
            Qcard[student_ch[subject,trial]] += alpha[subject] * PEcard;
      } 
  }
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
model {
  
  // population level priors (hyper-parameters)
  mu  ~ normal(0, 5);             // mu is a vector 1xNparameters with the population mean (i.e., location) for each model parameter
  tau ~ cauchy(0, 1);             //tau is the hyperparameters variance vector
  L_Omega ~ lkj_corr_cholesky(2); //L_omega is the lower triangle of the correlations. Setting the lkj prior to 2 means the off-diagonals are priored to be near zero

  // indvidual level priors (subject parameters)
  auxiliary_parameters ~ multi_normal(mu, sigma_matrix);

  target += sum(log_lik);

}


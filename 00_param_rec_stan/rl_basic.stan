data {
  int<lower = 0> nTrials; //number of trials
  int<lower = 2> nArms; //number of alternatives 
  int<lower = 0> a1[nTrials]; //index of which arm was pulled
  int<lower = 0> reward[nTrials]; //outcome of bandit arm pull
  int<lower = 0> reveal[nTrials]; //teacher instructed?
  int<lower = 0> obey[nTrials]; //student obeyed? 
  int<lower = 0> teacherChoice[nTrials]; //teacher's card selection

  
}

parameters {
  real alpha; //learning rate for card 
  real BiasInst; //bias towards instruction
  real alphaObey; //learning rate for teacher 
  real w; //weight between card and instructions 
  real beta; //softmax parameter - inverse temperature
}

transformed parameters {
  vector<lower=0, upper=1>[nArms] Qcard[nTrials];  // value function for each arm
  vector<lower=0, upper=1>[2] Qob[nTrials];  // value function for following the teacher
  vector<lower=0, upper=1>[nArms] Qnet[nTrials];  // value function for the decision
  vector<lower=0, upper=1>[nArms] Bonusob[1];  //bonus obey (serves in Qnet), starts with zero every trial

  
  real delta[nTrials];  // prediction error
  
  for (trial in 1:nTrials) {
    
    //set initial Q and delta for each trial
    if (trial == 1) {
      
      //if first trial, initialize Q values as specified
      for (a in 1:nArms) {
        Qcard[1, a] = 0;
        
        Qnet[1, a] = 0;
        
      }
      
      for (b in 1:2) {
      
      Qob[1, b] = 0;
      
      }
      
    } else {
      
      //otherwise, carry forward Q from last trial to serve as initial value
      for (a in 1:nArms) {
        Qcard[trial, a] = Qcard[trial - 1, a];
        
        Qnet[trial, a] = Qnet[trial - 1, a];
        
        Qob[trial, obey] = Qob[trial - 1, obey];
        
      }
      
    }
    
    //calculate prediction error and update Q (based on specified beta)
    
    
    Qob[trial,obey] = Qob[trial-1,obey] +(alphaObey*(reward[trial]-Qob[trial-1,obey]))*reveal[trial];
    
    
    delta[trial] = reward[trial] - Qcard[trial, a1[trial]];
    
    
    
    //update Q value based on prediction error (delta) and learning rate (alpha)
    
    Bonusob[teacherChoice[trial]] = (Qob[trial,obey] + BiasInst)*reveal[trial];
    
    Qnet[trial, a1] = Qcard[trial - 1, a1] + w *  Bonusob[teacherChoice[trial]];
    
    Qcard[trial, a1[trial]] = Qcard[trial, a1[trial]] + inv_logit(alpha) * delta[trial];
  }
}

model {
  // priors
  beta ~ normal(0, 1); 
  alpha ~ normal(0, 1);
  
  for (trial in 1:nTrials) {
    //returns the probability of having made the choice you made, given your beta and your Q's
    target += log_softmax(Qnet[trial] *exp(beta))[a1[trial]];
    
  }
}

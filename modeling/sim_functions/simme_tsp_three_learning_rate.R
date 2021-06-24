

#sim_1 helper function
sim.block = function(par,cfg){
  Ntrl          = cfg$Ntrl
  Nalt          = cfg$Nalt
  Noffer        = cfg$Noffer           # how many cards from the deck are presented
  rndwlk_frac   = cfg$rndwlk_frac     
  rndwlk_teacher= cfg$rndwlk_teacher
  teacher.rate  = cfg$teacher.rate 

  alpha_free        = inv.logit(par['alpha_free'])
  alpha_follow      = inv.logit(par['alpha_follow'])
  alpha_oppose      = inv.logit(par['alpha_oppose'])
  beta              = exp(par['beta'])

  Qcard         = rep(0, Nalt)
  Qteacher      = c(0,0)       #follow or oppose
  df            =data.frame()
  for (t in 1:Ntrl){

    #computer decides which cards to offer
    raffle      = sample(1:Nalt,Noffer,prob=rep(1/Nalt,Nalt))
    cards.expval= rndwlk_frac[raffle,t]
    teacher.beta= rndwlk_teacher[t]
 
    #teacher card and revel choices
    teacher_ch      = sample(raffle,1,prob=exp(teacher.beta*cards.expval) / sum(exp(teacher.beta*cards.expval)))
    reveal          = sample(c(0,1),1,prob=c((1-teacher.rate),teacher.rate))


    #student integrated action values
    Qnet= c(0,0)
    Qnet= Qcard[raffle]

    #student choice
    student_ch    = sample(raffle, size=1, replace=TRUE,prob=exp(beta*Qnet)/sum(exp(beta*Qnet)))
    follow=(student_ch==teacher_ch)*1  
    
    #outcome
    reward = sample(0:1, size=1, replace=TRUE,prob=c((1-rndwlk_frac[student_ch,t]),rndwlk_frac[student_ch,t]))
    
    #Prediction errors
    PEcard             = reward-Qcard[student_ch]

    #save data
    df<-rbind(df,data.frame(
      subject        =cfg$subject,
      trial          =t,
      offer1         =raffle[1],
      offer2         =raffle[2],
      ev1            =rndwlk_frac[1,t],
      ev2            =rndwlk_frac[2,t],
      ev3            =rndwlk_frac[3,t],
      ev4            =rndwlk_frac[4,t],
      teacher_ch     =teacher_ch,
      reveal         =reveal,
      student_ch     =student_ch,
      raffle_ch      =((student_ch==raffle[2])*1+1), #did the student take A or B from the two offers to be used in the softmax from Qnet
      follow         =follow,
      reward         =reward,
      PEcard         =PEcard,
      Qcard1         =Qcard[1],
      Qcard2         =Qcard[2],
      Qcard3         =Qcard[3],
      Qcard4         =Qcard[4])
    )
    
    #Card update student state-action values and reliability scores
    if (reveal==0)             {alpha=alpha_free}
    if (reveal==1 & follow==1) {alpha=alpha_follow}
    if (reveal==1 & follow==0) {alpha=alpha_oppose}
    
    Qcard[student_ch]  = Qcard[student_ch]+alpha*PEcard
}
  
  return (df)
}





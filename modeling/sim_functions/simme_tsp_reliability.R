

#sim_1 helper function
sim.block = function(par,cfg){
  Ntrl          = cfg$Ntrl
  Nalt          = cfg$Nalt
  Noffer        = cfg$Noffer # how many cards from the deck are presented
  rndwlk_frac   = cfg$rndwlk_frac
  rndwlk_teacher= cfg$rndwlk_teacher
  teacher.rate  = cfg$teacher.rate 

  alpha.card             = inv.logit(par['alpha.card'])
  alpha.teacher_as_bandit= inv.logit(par['alpha.teacher_as_bandit'])
  alpha.reliability      = inv.logit(par['alpha.reliability'])
  beta                   = exp(par['beta'])
  inst.bias              = par['inst.bias']
  b1                     = par['b1']
  b2                     = exp(par['b2'])

  Qcard         = rep(0, Nalt)
  Rcard         = 0
  Qteacher      = c(0,0) #follow or oppose
  Rteacher      = 1
  df            =data.frame()
  for (t in 1:Ntrl){

    #computer decides which cards to offer
    raffle      = sample(1:Nalt,Noffer,prob=rep(1/Nalt,Nalt))
    cards.expval= rndwlk_frac[raffle,t]
    teacher.beta= rndwlk_teacher[t]
 
    #teacher card and revel choices
    teacher.ch      = sample(raffle,1,prob=exp(teacher.beta*cards.expval) / sum(exp(teacher.beta*cards.expval)))
    teacher.reveal  = sample(c(0,1),1,prob=c((1-teacher.rate),teacher.rate))


    #student integrated action values
    p   =b1+b2*(Rcard*(1-Rteacher))
    Qnet=c(0,0)
    Qnet                     = p*Qcard[raffle]
    Qnet[teacher.ch!=raffle] = Qnet[teacher.ch!=raffle]+(1-p)*Qteacher[1]
    Qnet[teacher.ch==raffle] = Qnet[teacher.ch==raffle]+(1-p)*Qteacher[2]+(1-Rcard)*(1-Rteacher)*inst.bias
    
    #student choice
    student.ch    = sample(raffle, size=1, replace=TRUE,prob=exp(beta*Qnet)/sum(exp(beta*Qnet)))
    student.follow=(student.ch==teacher.ch)*1  
    
    #outcome
    reward = sample(0:1, size=1, replace=TRUE,prob=c((1-rndwlk_frac[student.ch,t]),rndwlk_frac[student.ch,t]))
    
    #Prediction erros
    PEcard             = reward-Qcard[student.ch]
    PEteacher          = reward-Qteacher[student.follow+1]
    
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
      teacher.ch     =teacher.ch,
      teacher.reveal =teacher.reveal,
      student.ch     =student.ch,
      student.follow =student.follow,
      reward         =reward,
      PEcard         =PEcard,
      PEteacher      =PEteacher,
      Qcard1         =Qcard[1],
      Qcard2         =Qcard[2],
      Qcard3         =Qcard[3],
      Qcard4         =Qcard[4],
      Qteacher.opp   =Qteacher[1],
      Qteacher.follow=Qteacher[2],
      Rcard          =as.numeric(Rcard),
      Rteacher       =as.numeric(Rteacher))
    )
    
    #Card update student state-action values and reliability scores
    Qcard[student.ch]  = Qcard[student.ch]+alpha.card*PEcard
    Rcard              = Rcard+alpha.reliability*((1-abs(PEcard))-Rcard)
    
    
    #teacher update student state-action values and reliability scores
    alpha.teacher      = alpha.teacher_as_bandit*teacher.reveal
    Qteacher[student.follow+1] = Qteacher[student.follow+1] +alpha.teacher*PEteacher
    Rteacher                   = Rteacher+alpha.reliability*((1-abs(PEteacher)-Rteacher))*teacher.reveal
    
  }
  
  return (df)
}





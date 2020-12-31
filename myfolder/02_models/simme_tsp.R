

#sim_1 helper function
sim.block = function(par,cfg){
  Nalt          = cfg$Nalt
  Ntrl          = cfg$Ntrl
  rndwlk_frac   = cfg$rndwlk_frac
  rndwlk_teacher= cfg$rndwlk_teacher
  teacher.rate  = cfg$teacher.rate #initial instructions rate
  shown_frac    = cfg$shown_frac # how many cards from the deck are presented
  beta.teacher.exp = cfg$beta.teacher.exp
  alpha.teacher = cfg$alpha.teacher
  
  alpha.free    = par$alpha.free
  alpha.inst    = par$alpha.inst
  alpha.opp     = par$alpha.opp
  alpha.obey    = par$alpha.obey
  beta          = par$beta
  BiasInst      = par$BiasInst
  pInst         = c(teacher.rate, 1-teacher.rate)
  
  Qfrc          = rep(0, Nalt)
  Qob           = c(0,0)
  Qinst           = c(0,0)
  BonusOb       = rep(0,3)
  arms          = c(1:Nalt)
  raffle        = data.frame(matrix(NA, nrow = Ntrl, ncol = shown_frac))
  cards.reveal.exp = matrix(NA, nrow = Ntrl, ncol = shown_frac)
  instructions.rate = rep(teacher.rate, Ntrl)
  w             = 0.5 #for pearce Hall
  associability = rep(1, Nalt)
  
  
  student.ch  =  instructions.rate = student.ob = student.obey = teacher.ch = teacher.advice = reward = beta.teacher = teacher.acc  = teacher.reveal = rep(NA, Ntrl)
  
  for (t in 1:Ntrl){
    instructions.rate[t] = pInst[1]
    
    #teacher choice (0-no advice, 1 to Nalt - which advice)
    expval                 <- rndwlk_frac[1:Nalt,t]
    beta.teacher           <- rndwlk_teacher[t]
    raffle[t,1:shown_frac] = sample(1:Nalt,shown_frac,prob=rep(1/Nalt,Nalt))
    cards.reveal           = c()
    
    for (x in seq(raffle[t,1:shown_frac])) {
      cards.reveal[x]        = c(raffle[t,1:shown_frac][1,x])}
    cards.reveal.exp[t,]    = cards.reveal
    
    expval.reveal          = expval[cards.reveal]
    
    #in teacher.ch the 1 sample should be changed to a variable selecting 1/2 instructions:
    teacher.ch[t]          = sample(cards.reveal,1,prob=exp(beta.teacher*expval.reveal) / sum(exp(beta.teacher*expval.reveal)))
    teacher.reveal[t]      = sample (c(0,1),1,prob=c((1-instructions.rate[t]),instructions.rate[t]))
    BonusOb                = rep(0, Nalt)
    
    seq                    = cards.reveal[cards.reveal!=intersect(teacher.ch[t],cards.reveal)]
    for (i in seq){BonusOb[i] = (Qob[1])*teacher.reveal[t]}
    
    #seq                    = arms[arms!=match(teacher.ch[t],arms)]
    #BonusOb[!teacher.ch[t]]= (Qob[1])*teacher.reveal[t]
    
    BonusOb[teacher.ch[t]] = (Qob[2] + BiasInst)*teacher.reveal[t]
    Qnet <- Qfrc+BonusOb
    
    
    #student choice
    p             <- exp(beta*Qnet)/sum(exp(beta*Qnet))
    student.ch[t] = sample(cards.reveal.exp[t,], size=1, replace=TRUE,prob=p[cards.reveal.exp[t,]])
    #student.ob[t] = (student.ch[t]==teacher.ch[t])*1
    student.ob[t] = ifelse(teacher.reveal[t]==0,1,(student.ch[t]==teacher.ch[t])*1) #following the original code, here obey=0 only if the agent didn't follow an actual instruction
    
    
    #outcome
    reward[t] = sample(0:1, size=1, replace=TRUE,prob=c((1-expval[student.ch[t]]),expval[student.ch[t]]))
    
    #update Qval
    alpha = (alpha.free)*(teacher.reveal[t]==0)+(alpha.inst)*(student.ob[t]==1)*(teacher.reveal[t]!=0)+(alpha.opp)*(student.ob[t]==0)*(teacher.reveal[t]!=0)
    Qfrc[student.ch[t]] = Qfrc[student.ch[t]]+alpha*(reward[t]-Qfrc[student.ch[t]])
    Qob[student.ob[t]+1] = Qob[student.ob[t]+1] +(alpha.obey*(reward[t]-Qob[student.ob[t]+1]))*teacher.reveal[t]
    
    
    
  }
  
  return (data.frame(teacher.ch,teacher.reveal,student.ch,student.ob,reward,Ntrl,cards.reveal.exp,teacher.rate,instructions.rate))
}
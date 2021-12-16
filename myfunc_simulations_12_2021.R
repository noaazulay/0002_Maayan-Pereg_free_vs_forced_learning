### I would add a code goal
### which file in the github runs the code?
sim.block = function(par,cfg,sub){ ## additive model (### Is it simblock or simdata?)
  Nblk          =cfg$Nblk ###Nblock
  Subject       = sub
  Nalt          = cfg$Nalt
  Ntrl          = cfg$Ntrl ###Ntrial
  rndwlk_teacher= cfg$rndwlk_teacher
  teacher.rate  = cfg$teacher.rate #initial instructions rate
  shown_frac    = cfg$shown_frac # how many cards from the deck are presented
  alpha    = par$alpha
  
  
  beta          = par$beta
  BiasInst      = par$BiasInst
  pInst         = c(teacher.rate, 1-teacher.rate)
  
  
  BonusOb       = rep(0,2)
  arms          = c(1:Nalt)
  raffle        = data.frame(matrix(NA, nrow = Ntrl, ncol = shown_frac))
  cards.reveal.exp = matrix(NA, nrow = Ntrl, ncol = shown_frac)
  instructions.rate = rep(teacher.rate, Ntrl)
  
  midresults     = data.frame(matrix(NA, nrow = Ntrl, ncol = 16))
  
  
  
  
  for (b in 1:Nblk){
    trial = block = student.ch  =  teacher.unch = teacher.acc = student.unch = student.acc = BiasWeight = Qdiff = realdiff = instructions.rate = student.ob = student.obey = teacher.ch = teacher.advice = reward = beta.teacher  = teacher.reveal = rep(NA, Ntrl)
    
    Qfrc          = rep(0, Nalt)
    
    for (t in 1:Ntrl){
      block[t] <- b
      
      trial[t] = t
      instructions.rate[t] = pInst[1]
      
      #teacher choice (0-no advice, 1 to Nalt - which advice)
      expval                 <- rndwlk_frac[1:Nalt,t] ###where is rndwlk_frac defined?
      beta.teacher           <- rndwlk_teacher[b,t]
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
      for (i in seq){BonusOb[i] = 0}
      
      
      Qdiff[t] <- abs(Qfrc[cards.reveal[1]]-Qfrc[cards.reveal[2]])
      
      Qdiff[t] <- ifelse(Qdiff[t]==0,0.0001,Qdiff[t])
      BonusOb[teacher.ch[t]] = (BiasInst)*teacher.reveal[t]
      
      realdiff[t] <- abs(expval[cards.reveal[1]]-expval[cards.reveal[2]])
      
      Qnet <- Qfrc +BonusOb #Qfrc+(BonusOb[teacher.ch[t]])*BiasWeight[t])  # 
      
      
      #student choice
      p             <- exp(beta*Qnet)/sum(exp(beta*Qnet))
      student.ch[t] = sample(cards.reveal.exp[t,], size=1, replace=TRUE,prob=p[cards.reveal.exp[t,]]) 
      student.ob[t] = ifelse(teacher.reveal[t]==0,1,(student.ch[t]==teacher.ch[t])*1) #following the original code, here obey=0 only if the agent didn't follow an actual instruction
      
      student.unch[t]  = cards.reveal[cards.reveal!=intersect(student.ch[t],cards.reveal)]
      teacher.unch[t]  = cards.reveal[cards.reveal!=intersect(teacher.ch[t],cards.reveal)]
      
      
      student.acc[t] <- (expval[student.ch[t]] > expval[student.unch[t]])*1
      teacher.acc[t] <- (expval[teacher.ch[t]] > expval[teacher.unch[t]])*1
      
      #outcome
      reward[t] = sample(0:1, size=1, replace=TRUE,prob=c((1-expval[student.ch[t]]),expval[student.ch[t]]))
      
      #update Qval 
      Qfrc[student.ch[t]] = Qfrc[student.ch[t]]+alpha*(reward[t]-Qfrc[student.ch[t]])
      
    }
    midresults=data.frame(Subject,block,trial,teacher.ch,teacher.reveal,student.ch,student.ob,reward,Ntrl,cards.reveal.exp,teacher.rate,instructions.rate,realdiff,Qdiff,student.acc,teacher.acc)
    
    assign(paste0("block_", b), midresults)
    
  }
  return (do.call("rbind", list(block_1,block_2,block_3,block_4,block_5,block_6)))
}



sim.block3 = function(par,cfg,sub){ #Qteacher
  Nblk          =cfg$Nblk
  Subject       = sub
  Nalt          = cfg$Nalt
  Ntrl          = cfg$Ntrl
  rndwlk_teacher= cfg$rndwlk_teacher
  teacher.rate  = cfg$teacher.rate #initial instructions rate
  shown_frac    = cfg$shown_frac # how many cards from the deck are presented
  
  alpha    = par$alpha
  
   beta          = par$beta
  BiasInst      = par$BiasInst
  pInst         = c(teacher.rate, 1-teacher.rate)
  
  
  QTeacher           = c(0,0)
  #Qinst           = c(0,0)
  BonusOb       = rep(0,2)
  arms          = c(1:Nalt)
  raffle        = data.frame(matrix(NA, nrow = Ntrl, ncol = shown_frac))
  cards.reveal.exp = matrix(NA, nrow = Ntrl, ncol = shown_frac)
  instructions.rate = rep(teacher.rate, Ntrl)
  midresults     = data.frame(matrix(NA, nrow = Ntrl, ncol = 16))
  
  
  
  
  for (b in 1:Nblk){
    trial = block = Conflict = student.ch  =  teacher.unch = teacher.acc = student.unch = student.acc = BiasWeight = Qdiff = realdiff = instructions.rate = student.ob = student.obey = teacher.ch = teacher.advice = reward = beta.teacher  = teacher.reveal = rep(NA, Ntrl)
    
    Qfrc          = rep(0, Nalt)
    
    for (t in 1:Ntrl){
      block[t] <- b
      
      trial[t] = t
      instructions.rate[t] = pInst[1]
      
      #teacher choice (0-no advice, 1 to Nalt - which advice)
      expval                 <- rndwlk_frac[1:Nalt,t]
      beta.teacher           <- rndwlk_teacher[b,t]
      raffle[t,1:shown_frac] = sample(1:Nalt,shown_frac,prob=rep(1/Nalt,Nalt))
      cards.reveal           = c()
      
      for (x in seq(raffle[t,1:shown_frac])) {
        cards.reveal[x]        = c(raffle[t,1:shown_frac][1,x])}
      cards.reveal.exp[t,]    = cards.reveal
      
      expval.reveal          = expval[cards.reveal]
      
      #in teacher.ch the 1 sample should be changed to a variable selecting 1/2 instructions:
      teacher.ch[t]          = sample(cards.reveal,1,prob=exp(beta.teacher*expval.reveal) / sum(exp(beta.teacher*expval.reveal)))
      teacher.unch[t]        = cards.reveal[cards.reveal!=intersect(teacher.ch[t],cards.reveal)]
      
      teacher.reveal[t]      = sample (c(0,1),1,prob=c((1-instructions.rate[t]),instructions.rate[t]))
      BonusOb                = rep(0, Nalt)
      
      seq                    = cards.reveal[cards.reveal!=intersect(teacher.ch[t],cards.reveal)]
      for (i in seq){BonusOb[i] = 0}#(Qob[1])*teacher.reveal[t]}
      
      Qdiff[t] <- abs(Qfrc[cards.reveal[1]]-Qfrc[cards.reveal[2]])
      
      Qdiff[t] <- ifelse(Qdiff[t]==0,0.0001,Qdiff[t])

      realdiff[t] <- abs(expval[cards.reveal[1]]-expval[cards.reveal[2]])
      
      
      Qnet <- Qfrc + QTeacher[2]

      
      #student choice
      p             <- exp(beta*Qnet)/sum(exp(beta*Qnet))
      student.ch[t] = sample(cards.reveal.exp[t,], size=1, replace=TRUE,prob=p[cards.reveal.exp[t,]]) 
      student.ob[t] = ifelse(teacher.reveal[t]==0,1,(student.ch[t]==teacher.ch[t])*1) #following the original code, here obey=0 only if the agent didn't follow an actual instruction
      
      student.unch[t]  = cards.reveal[cards.reveal!=intersect(student.ch[t],cards.reveal)]
      
      
      student.acc[t] <- (expval[student.ch[t]] > expval[student.unch[t]])*1
      teacher.acc[t] <- (expval[teacher.ch[t]] > expval[teacher.unch[t]])*1
      
      #outcome
      reward[t] = sample(0:1, size=1, replace=TRUE,prob=c((1-expval[student.ch[t]]),expval[student.ch[t]]))
      
      #update Qval 
      #alpha = (alpha.free)*(teacher.reveal[t]==0)+(alpha.inst)*(student.ob[t]==1)*(teacher.reveal[t]!=0)+(alpha.opp)*(student.ob[t]==0)*(teacher.reveal[t]!=0)
      Qfrc[student.ch[t]] = Qfrc[student.ch[t]]+alpha*(reward[t]-Qfrc[student.ch[t]])
      QTeacher[student.obey[t]+1] = QTeacher[student.obey[t]+1] +(alpha*(reward[t]-QTeacher[student.obey[t]+1]))*teacher.reveal[t]
      
      
      
    }
    midresults=data.frame(Subject,block,trial,teacher.ch,teacher.reveal,student.ch,student.ob,reward,Ntrl,cards.reveal.exp,teacher.rate,instructions.rate,realdiff,Qdiff,student.acc,teacher.acc,QTeacher)
    
    assign(paste0("block_", b), midresults)
    
  }
  return (do.call("rbind", list(block_1,block_2,block_3,block_4,block_5,block_6)))
}


sim.block4 = function(par,cfg,sub){ #decreasing bias with task ease
  Nblk          =cfg$Nblk
  Subject       = sub
  Nalt          = cfg$Nalt
  Ntrl          = cfg$Ntrl
  rndwlk_teacher= cfg$rndwlk_teacher
  teacher.rate  = cfg$teacher.rate #initial instructions rate
  shown_frac    = cfg$shown_frac # how many cards from the deck are presented
  alpha         = par$alpha
  b1            = par$b1
  
  beta          = par$beta
  BiasInst      = par$BiasInst
  pInst         = c(teacher.rate, 1-teacher.rate)
  
  BonusOb       = rep(0,2)
  arms          = c(1:Nalt)
  raffle        = data.frame(matrix(NA, nrow = Ntrl, ncol = shown_frac))
  cards.reveal.exp = matrix(NA, nrow = Ntrl, ncol = shown_frac)
  instructions.rate = rep(teacher.rate, Ntrl)
  
  midresults     = data.frame(matrix(NA, nrow = Ntrl, ncol = 16))
  
  
  
  
  for (b in 1:Nblk){
    trial = block = Conflict = student.ch  =  teacher.unch = teacher.acc = student.unch = student.acc = BiasWeight = Qdiff = realdiff = instructions.rate = student.ob = student.obey = teacher.ch = teacher.advice = reward = beta.teacher  = teacher.reveal = rep(NA, Ntrl)
    
    Qfrc          = rep(0, Nalt)
    
    for (t in 1:Ntrl){
      block[t] <- b
      
      trial[t] = t
      instructions.rate[t] = pInst[1]
      
      #teacher choice (0-no advice, 1 to Nalt - which advice)
      expval                 <- rndwlk_frac[1:Nalt,t]
      beta.teacher           <- rndwlk_teacher[b,t]
      raffle[t,1:shown_frac] = sample(1:Nalt,shown_frac,prob=rep(1/Nalt,Nalt))
      cards.reveal           = c()
      
      for (x in seq(raffle[t,1:shown_frac])) {
        cards.reveal[x]        = c(raffle[t,1:shown_frac][1,x])}
      cards.reveal.exp[t,]    = cards.reveal
      
      expval.reveal          = expval[cards.reveal]
      
      #in teacher.ch the 1 sample should be changed to a variable selecting 1/2 instructions:
      teacher.ch[t]          = sample(cards.reveal,1,prob=exp(beta.teacher*expval.reveal) / sum(exp(beta.teacher*expval.reveal)))
      teacher.unch[t]        = cards.reveal[cards.reveal!=intersect(teacher.ch[t],cards.reveal)]
      
      teacher.reveal[t]      = sample (c(0,1),1,prob=c((1-instructions.rate[t]),instructions.rate[t]))
      BonusOb                = rep(0, Nalt)
      
      seq                    = cards.reveal[cards.reveal!=intersect(teacher.ch[t],cards.reveal)]
      for (i in seq){BonusOb[i] = 0}#(Qob[1])*teacher.reveal[t]}
      
      Qdiff[t] <- abs(Qfrc[cards.reveal[1]]-Qfrc[cards.reveal[2]])
      
      Qdiff[t] <- ifelse(Qdiff[t]==0,0.0001,Qdiff[t])
      BonusOb[teacher.ch[t]] = (BiasInst+b1*Qdiff[t])*teacher.reveal[t]
      
      Qnet <- Qfrc + BonusOb  
      
      realdiff[t] <- abs(expval[cards.reveal[1]]-expval[cards.reveal[2]])
      
      
      
      #student choice
      p             <- exp(beta*Qnet)/sum(exp(beta*Qnet))
      student.ch[t] = sample(cards.reveal.exp[t,], size=1, replace=TRUE,prob=p[cards.reveal.exp[t,]]) 
      student.ob[t] = ifelse(teacher.reveal[t]==0,1,(student.ch[t]==teacher.ch[t])*1) #following the original code, here obey=0 only if the agent didn't follow an actual instruction
      
      student.unch[t]  = cards.reveal[cards.reveal!=intersect(student.ch[t],cards.reveal)]
      
      
      student.acc[t] <- (expval[student.ch[t]] > expval[student.unch[t]])*1
      teacher.acc[t] <- (expval[teacher.ch[t]] > expval[teacher.unch[t]])*1
      
      #outcome
      reward[t] = sample(0:1, size=1, replace=TRUE,prob=c((1-expval[student.ch[t]]),expval[student.ch[t]]))
      
      #update Qval 
      Qfrc[student.ch[t]] = Qfrc[student.ch[t]]+alpha*(reward[t]-Qfrc[student.ch[t]])
      
    }
    midresults=data.frame(Subject,block,trial,teacher.ch,teacher.reveal,student.ch,student.ob,reward,Ntrl,cards.reveal.exp,teacher.rate,instructions.rate,realdiff,Qdiff,student.acc,teacher.acc)
    
    assign(paste0("block_", b), midresults)
    
  }
  return (do.call("rbind", list(block_1,block_2,block_3,block_4,block_5,block_6)))
}


m1  <- glmer(stay.card ~ rw_1back*cond_1back+ (1+rw_1back*cond_1back | prolific_id), data = tab%>%filter(!reveal,reoffer), family = binomial(link = "logit"),nAGQ=0)
m2  <- glmer(stay.card ~ rw_1back*reveal_1back+ (1+rw_1back*reveal_1back | prolific_id), data = tab%>%filter(!reveal,reoffer), family = binomial(link = "logit"),nAGQ=0)
Anova(m1)
Anova(m2)
df<-merge(adhd,icar,by='prolific_id')
df<-merge(df,data.frame(prolific_id=rownames(ranef(m2)$prolific_id),interaction=ranef(m2)$prolific_id[,'rw_1back:reveal_1back']),by='prolific_id')
round(cor(df[,-c(1)]),2)
summary(lm(interaction~iq,data=df))



m   <- glmer(obey ~ 1 +rw_1back*obey_1back+(1+rw_1back*obey_1back | prolific_id), data = tab%>%filter(reveal==1,reveal_1back==1,!reoffer), family = binomial(link = "logit"),nAGQ=0)

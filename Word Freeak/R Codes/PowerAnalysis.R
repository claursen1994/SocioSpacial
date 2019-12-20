#############################################
# Power Analyses 

library(simr)
# Run After Pilot R
###########################################
#The Basic Models and their power

#LandStart
summary(LM1<- lmer(landStart~ Age + (1|item), data= RS))
simtreat4LM1=powerSim(LM1,nsim=40)
simtreat4LM1
#extsimtreat4LM1=extend(LM1,along=sub,n=20)
#powerCurve(simtreat4LM1=powerSim(LM1,nsim=30))

#LaunchSite
summary(LM2<- lmer(launchSite~ Age + (1|item), data= RS))
simtreat4LM2=powerSim(LM2,nsim=50)
simtreat4LM2

#Undersweep probability
summary(GLM0<- glmer(undersweep_prob~ Age + (1|item), data= RS, family= binomial))

simtreat4GLM0=powerSim(GLM0,nsim=50)
simtreat4GLM0
powerCurve(simtreat4GLM0)

#Under Sweep Prob but Age*Landstart
summary(GLM1<- glmer(undersweep_prob~ Age * 
                       landStart + (1|item)+ (1|sub), data= RS, family= binomial))

simtreat4GLM1=powerSim(GLM1,along= sub, nsim=30)
simtreat4GLM1

#Different launchSite model
summary(LM3.1<- lmer(launchSite~ Age + (1|item)+ (1|sub), data= RS))
simtreat4LM3.1=powerSim(LM3.1,nsim=30)
simtreat4LM3.1

# Fixation Duration
summary(LM5<- lmer(fix_dur~ Age + (1|item)+ (1|sub), data= raw_fix))
tetst=lmer(fix_dur~ Age + (1|item)+ (Age|sub), data= Inter_line)
simtreat4LM5=powerSim(LM5,nsim=5)
simtreat4LM5

# Fixation Duration of fixation prior to return sweep
summary(LM6<- lmer(prev_fix_dur~ Age + (1|item)+ (1|sub), data= RS))
simtreat4LM6=powerSim(LM6,nsim=20)
simtreat4LM6

# UnderSweep Prob but with prev fix as predictor 
summary(GLMI<- glmer(undersweep_prob~ prev_fix_dur + (1|item)+ (1|sub)+ (1|Age), data= RS, family= binomial))
simtreat4GLMI=powerSim(GLMI,nsim=10)
simtreat4GLMI                      


# Difference of Fixation durations 
summary(lmm1<- lmer(fix_dur~ Age + (1|item)+ (1|sub), data= All_fix))
FixEff=effect("Age",lmm1)
plot(FixEff)
summary(LM6<- lmer(prev_fix_dur~ Age + (1|item)+ (1|sub), data= RS))


FixPsim=powerSim(lmm1, nsim=10)
FixPsim

#Calculate effect size
r2.corr.mer(lmm1)
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}
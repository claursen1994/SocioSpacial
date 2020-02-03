######################################################
# This script loads in the data we already have, we don't need to create it again 
# Library
rm(list= ls())

if('devtools' %in% rownames(installed.packages())==FALSE){
  install.packages('devtools')
  library(devtools)
}else{
  library(devtools)
}
install_github('martin-vasilev/EMreading')

install.packages("reshape")
install.packages("tm")
install.packages("arm")
install.packages("MASS")
install.packages("lattice")
install.packages("lme4")
install.packages("effects")
install.packages("quanteda")
install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("digest")
install.packages("EMreading")
install.packages("simr")
install.packages("jtools")
install.packages("ggplot2")
install.packages("readxl")
#Library#
library("tm")
library("arm")
library("MASS")
library("lattice")
library("lme4")
library("effects")
library("quanteda")
library("readr")
library("tidyr")
library("dplyr")
library("tidyverse")
library("digest")
library("EMreading")
library("simr")
library("jtools")



# Load in Data for Skips, raw_fix, and RS
Skips=read.csv("Skips.csv")
Skips$X=NULL
raw_fix=read.csv("raw_fix_data.csv")
raw_fix$X=NULL
PilotAges <- read_excel("preproc/PilotAges.xlsx")
RS=read.csv("RS.csv")
RS$X=NULL

################################ UNDERSWEEP PROBABILITY##############################################

##################################### Make models
library("lme4")
contrasts(RS$Age)<- c(1, -1)

summary(GLM0<- glmer(undersweep_prob~ Age + (1|item) +(1|sub), data= RS, family= binomial))

#Effect of Age on undersweep Probability
library("effects")
ef0=effect("Age", GLM0)
summary(ef0)
plot(ef0)

######################################## SKIPS ####################################################


############################################ Make Model
# center by means 

Skips$Length=center(Skips$Length)
Skips$Zipf=center(Skips$Zipf)


#Run Model
summary(GLM1<- glmer(skip_1st~ Age +(1|item)+ (1|sub), data= Skips, family= binomial))
ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)

########################### Simulate



model2=extend(GLM1,along="sub", n=80)
powerSim(GLM1, test=fixed("Age"),nsim=10)


PC2=powerCurve(model2, along = "sub", breaks = c(48,56,72,80), test = fixed("Age"),nsim = 20,
               sim = model2, seed=10)
plot(PC2)
#USPSIM


##################################### Regressions #######################################
# Return to skipped words
#Delete previous model to free up space
rm(model2)
#contrast treat first pass skips for exploratory analysis only but we may as well do it here  


Skips$skip_1st<-as.factor(Skips$skip_1st)
contrasts(Skips$skip_1st)=contr.treatment(2)

summary(GLM2<- glmer(regress~ Age +(1|item)+ (1|sub), data= raw_fix, family= binomial))
RegEF=effect("Age", GLM2)
summary(RegEF)
plot(RegEF)

####################### Simulate 

powerSim(GLM2, test= fixed("Age"),nsim=10)

model3=extend(GLM2,along="sub", n=80)



PC3=powerCurve(model3, along = "sub", breaks = c(48,56,80),test = fixed("Age"),nsim=20,
               sim = model3, seed=10)
plot(PC3)
chk<-lastResult()
chk$errors
#Yes

##################################### Launch Site #######################################

summary(LaunchLM <- lmer(launchSite~Age+(1|item)+(1|sub),data=RS))
LaunchEF=effect("Age",LaunchLM)
summary(LaunchEF)
plot(LaunchEF)
#sim
fixef(LaunchLM)["Age1"]<-0.8


model4=extend(LaunchLM,along="sub", n=80)


PC4=powerCurve(model4, along = "sub", breaks = c(16,24,32,40,48,56,64,72,80),test = fixed("Age"),nsim=1000,
               sim = model4, seed=10)
plot(PC4)
chk<-lastResult()
chk$errors
rm(model4)

#Yes

#################################### Landing position ###################################
#install.packages("lmerTest")
#library("lmerTest")
summary(LandLM <- lmer(landStart~Age+launchSite+(1|item)+(1|sub),data=RS))
LandEF=effect("Age",LandLM)
summary(LandEF)
plot(LandEF)

#Simulation 
fixef(LandLM)["Age1"]<-1.5
fixef(LandLM)["launchSite"]<-0.08


model5=extend(LandLM,along="sub", n=80)

PC5=powerCurve(model5, along = "sub", breaks = c(16,24,32,40,48,56,64,72,80),test = fixed("Age"),nsim=1000,
               sim = model5, seed=10)

plot(PC5)
chk<-lastResult()
chk$errors
rm(ef7)




################################# Fixation Duration 
##################### Check for Age effects within fixation groups ###########################
#fix type and age
contrasts(RS$Age)<- c(1, -1)
raw_fix$Fix_type<- as.factor(raw_fix$Fix_type)
raw_fix$Fix_type<- factor(raw_fix$Fix_type, levels= c('intra-line', 'accurate', 'undersweep', 'line-final'))
contrasts(raw_fix$Fix_type)

#####################################################################################################################
# Remember Simr can't yet look at interactions  in the same run yet, so we need to look at them seperately
#####################################################################################################################

# General Model
summary(allfixtypelm<- lmer(log(fix_dur)~  Age * Fix_type+ (1|item)+ (1|sub), data= raw_fix))

allfixtypelm
# Split by fix_type

UnderFix<-split(raw_fix, raw_fix$Fix_type)
IntraFix<-UnderFix$`intra-line`
AccurateFix<-UnderFix$accurate
LineFinFix<-UnderFix$`line-final`
UnderFix<-UnderFix$undersweep


#Undersweep fix dur model 
summary(UnderFixlm<- lmer(log(fix_dur)~  Age  + (1|item)+ (1|sub), data= UnderFix))

model7=extend(UnderFixlm,along="sub", n=80)

PC7=powerCurve(model7, along = "sub", breaks = c(16,72),test = fixed("Age"),nsim=500,
               sim = model7, seed=10)

plot(PC7)
chk<-lastResult()
chk$errors
chk$warnings

#intra line model
summary(IntraFixlm<- lmer(log(fix_dur)~  Age + (1|item)+ (1|sub), data= IntraFix))

model8=extend(IntraFixlm,along="sub", n=80)

PC8=powerCurve(model8, along = "sub", breaks = c(56,72),test = fixed("Age"),nsim=500,
               sim = model8, seed=10)

plot(PC8)
chk<-lastResult()
chk$errors
chk$warnings

# Line final fixation model
summary(LineFinlm<- lmer(log(fix_dur)~  Age + (1|item)+ (1|sub), data= LineFinFix))

model9=extend(LineFinlm,along="sub", n=80)

PC9=powerCurve(model9, along = "sub", breaks = c(56,72),test = fixed("Age"),nsim=500,
               sim = model9, seed=10)

plot(PC9)
chk<-lastResult()
chk$errors
chk$warnings

#Accurate model
summary(AccurateFixlm<- lmer(log(fix_dur)~  Age + (1|item)+ (1|sub), data= AccurateFix))

model10=extend(AccurateFixlm,along="sub", n=80)

PC10=powerCurve(model10, along = "sub", breaks = c(56,72, 80),test = fixed("Age"),nsim=500,
                sim = model10, seed=10)

plot(PC10)
chk<-lastResult()
chk$errors
chk$warnings

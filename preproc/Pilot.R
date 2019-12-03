

rm(list= ls())

load("preproc/raw_fix.Rda")

# Add some extra stuff to data frame:
new<- NULL
nsubs<- unique(raw_fix$sub)

for(i in 1:length(nsubs)){ # for each subject..
  n<- subset(raw_fix, sub== nsubs[i])
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){ # for each item..
    m<- subset(n, item== nitems[j])
    m$prevX<- NA
    m$prevY<- NA
    m$prevChar<- NA
    m$prev_max_char_line<- NA
    m$prev_fix_dur<- NA
    m$nextX<- NA
    
    for(k in 1:nrow(m)){ # for each fixation
      if(k>1){
        m$prevX[k]<- m$xPos[k-1]
        m$prevY[k]<- m$yPos[k-1]
        m$prevChar[k]<- m$char_line[k-1]
        m$prev_max_char_line[k]<- m$max_char_line[k-1]
        m$prev_fix_dur[k]<- m$fix_dur[k-1]
      }
      
      if(k< nrow(m)){ # next sacc
        m$nextX[k]<- m$xPos[k+1]
      }
      
    } # end of k
    
    new<- rbind(new, m)
    
  } # end of j
  cat(i); cat(' ')
} # end of i

raw_fix<- new; rm(new)


# remove blinks:
raw_fix<- subset(raw_fix, blink==0 & prev_blink==0 & after_blink==0)


# remove outliers:
out<- which(raw_fix$fix_dur<80 | raw_fix$fix_dur>1000)
raw_fix<- raw_fix[-out,]


# map age group:
old<- c(2,5,8,9,11)

raw_fix$Age<- ifelse(is.element(raw_fix$sub, old), "Old", "Young")


RS<- subset(raw_fix, Rtn_sweep==1)

RS$launchSite<- RS$prev_max_char_line- RS$prevChar
RS$landStart<- RS$char_line
RS$undersweep_prob<- ifelse(RS$Rtn_sweep_type=="undersweep", 1, 0)


library(reshape)
Des<- melt(RS, id=c('sub', 'item', 'Age'), 
           measure=c("landStart", "undersweep_prob", "launchSite", "sacc_dur") , na.rm=TRUE)

m<- cast(Des, Age ~ variable
         , function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

RS$Age<- as.factor(RS$Age)
contrasts(RS$Age)<- c(1, -1)

library(lme4)

summary(LM1<- lmer(landStart~ Age + (1|item), data= RS))
summary(LM2<- lmer(launchSite~ Age + (1|item), data= RS))
summary(GLM0<- glmer(undersweep_prob~ Age + (1|item), data= RS, family= binomial))
######################################################################################
######################################################################################

# Making sense# 
# Landing Position and Age Interaction
contrasts(RS$Age)<- c(1, -1)
summary(GLM1<- glmer(undersweep_prob~ Age * 
                       landStart + (1|item)+ (1|sub), data= RS, family= binomial))


ef1=effect("Age:landStart", GLM1)
summary(ef1)

######################################################################################
# Launch Position 
#USP

summary(LM3<- lmer(undersweep_prob~ launchSite + (1|item)+ (1|sub), data= RS))
effect("launchSite",LM3)
#Age

summary(LM3.1<- lmer(launchSite~ Age + (1|item)+ (1|sub), data= RS))
anova(LM3.1)

effect("Age",LM3.1)
#summary(LM4<- lmer(undersweep_prob~ Age + (1|item)+ (1|sub), data= RS))
#anova(LM4)

LaunchPosVioLin=ggplot(data = RS, aes(x = Age, y = launchSite, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()

LaunchPosGG=ggplot(RS, aes(launchSite, undersweep_prob, colour = Age, fill = Age)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 

####################################################################################
## Fixation duration 
## General
##############################

summary(LM5<- lmer(fix_dur~ Age + (1|item)+ (1|sub), data= raw_fix))
anova(LM5)

effect("Age",LM5)

#Violin
Fix_durViolin=ggplot(data = raw_fix, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
  # + geom_jitter()

##################################################################################
# Fixation Duration Prior to performing a return sweep

summary(LM6<- lmer(prev_fix_dur~ Age + (1|item)+ (1|sub), data= RS))
anova(LM6)

effect("Age",LM6)

#Violin
prev_fix_durViolin=ggplot(data = RS, aes(x = Age, y = prev_fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()


# Regression
prev_fix_durGG=ggplot(RS, aes(prev_fix_dur, undersweep_prob, colour = Age, fill = Age)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(prev_fix_durGG)

#### Mildy interesting lets see what a model would look like...
summary(GLMI<- glmer(undersweep_prob~ prev_fix_dur + (1|item)+ (1|sub)+ (1|Age), data= RS, family= binomial))
anova(GLMI)

effect("prev_fix_dur",GLMI)


###################################################################################################
# Landing Position by age 



summary(LndPo<- lmer(landStart~ Age + (1|item), data= RS))


land_startViolin=ggplot(data = RS, aes(x = Age, y = landStart, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()

effect("Age",LndPo)
##################################################################################################
#Power
library(simr)
###########################################
#The Basic Models

#LandStart
summary(LM1<- lmer(landStart~ Age + (1|item), data= RS))
simtreat4LM1=powerSim(LM1,nsim=30)
simtreat4LM1
#extsimtreat4LM1=extend(LM1,along=sub,n=20)
#powerCurve(simtreat4LM1=powerSim(LM1,nsim=30))

#LaunchSite
summary(LM2<- lmer(launchSite~ Age + (1|item), data= RS))
simtreat4LM2=powerSim(LM2,nsim=30)
simtreat4LM2

#Undersweep probability
summary(GLM0<- glmer(undersweep_prob~ Age + (1|item), data= RS, family= binomial))

simtreat4GLM0=powerSim(GLM0,nsim=30)
simtreat4GLM0
powerCurve(simtreat4GLM0)

#Under Sweep Prob but Age*Landstart
summary(GLM1<- glmer(undersweep_prob~ Age * 
                       landStart + (1|item)+ (1|sub), data= RS, family= binomial))

simtreat4GLM1=powerSim(GLM1,nsim=30)
simtreat4GLM1

#Different launchSite model
summary(LM3.1<- lmer(launchSite~ Age + (1|item)+ (1|sub), data= RS))
simtreat4LM3.1=powerSim(LM3.1,nsim=30)
simtreat4LM3.1

# Fixation Duration
summary(LM5<- lmer(fix_dur~ Age + (1|item)+ (1|sub), data= raw_fix))

simtreat4LM5=powerSim(LM5,nsim=10)
simtreat4LM5

# Fixation Duration of fixation prior to return sweep
summary(LM6<- lmer(prev_fix_dur~ Age + (1|item)+ (1|sub), data= RS))
simtreat4LM6=powerSim(LM6,nsim=15)
simtreat4LM6

# UnderSweep Prob but with prev fix as predictor 
summary(GLMI<- glmer(undersweep_prob~ prev_fix_dur + (1|item)+ (1|sub)+ (1|Age), data= RS, family= binomial))
simtreat4GLMI=powerSim(GLMI,nsim=10)
simtreat4GLMI                      
                      




#######################################################################################

# Hunch zone, perhaps nonsensical investigations. 

#Saccade duration differences 
NS1=melt(raw_fix,id=c('sub', 'item', 'Age'), 
         measure=c("sacc_dur"), na.rm=TRUE)
NS1m<- cast(NS1, Age ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))
NS1=ggplot(data = raw_fix, aes(x = Age, y = sacc_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()

NS2=lmer(sacc_dur~ Age + (1|item)+ (1|sub), data= RS)
simNS2=powerSim(NS2,nsim=20)                   
simNS2



# Number of fixations 

NS3=melt(raw_fix,id=c('sub', 'item', 'Age'), 
                         measure=c("fix_num"), na.rm=TRUE)
         NS3<- cast(NS3, Age ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))
         
         ggplot(data = raw_fix, aes(x = Age, y = fix_num, fill = Age))+
           geom_bar(stat = "summary", fun.y = "mean", position = "dodge")+
           geom_violin()
         
##############################################
# Making a proportional violin to show no effects

RS$Rtn_sweep_type <- as.factor(RS$Rtn_sweep_type)
RS$Age <- as.factor(RS$Age)

chrisPlotDat = RS %>%
  group_by(sub, Age, Rtn_sweep_type) %>%
  summarise(count = n())

prop = c()

for(i in 1:nrow(chrisPlotDat)){
  
  tmpDat = chrisPlotDat[i,]
  partID = tmpDat$sub
  
  allSweeps = sum(chrisPlotDat$count[chrisPlotDat$sub == partID])
  
  propTMP = tmpDat$count/allSweeps
  
  prop = rbind(prop, propTMP)
  
  
}

chrisPlotDat$prop = prop

ggplot(data = chrisPlotDat, aes(x = Age, y = prop, fill = Rtn_sweep_type))+
  geom_bar(stat = "summary", fun.y = "median", position = "dodge")+
  geom_violin()

chrisModel = lmer(prop ~ Age * Rtn_sweep_type + (1|sub), data = chrisPlotDat)
summary(chrisModel)


       

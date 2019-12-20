
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

write.csv(raw_fix,"raw_fix.csv")
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
#########################################################################################
#Check and Mark 2nd pass in Return Sweeps. 

##############################################################
RS$remove<-  0
newDatas<- NULL
nsubs<- unique(RS$sub)

for( i in 1:length(nsubs)){
  n<- subset(RS, sub== nsubs[i])
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    nlines<- unique(m$line)
    
    for(k in 1:length(nlines)){
      o<- subset(m, line== nlines[k])
      if(nrow(o)>1){
        o$remove[2:nrow(o)]=1
        cat(sprintf('Second pass RS: subject %i, item %i, cond %i, line %i ', o$sub[1],
                o$item[1], o$cond[1], o$line[1]))
      }
      
      newDatas<- rbind(newDatas, o)
    }
  
  }
}

RS<- subset(RS, remove==0)
RS$remove<- NULL
rm(newDatas)

#########################
# Seperate Regressions

Regressions=split(raw_fix,raw_fix$regress)
Regressions=Regressions$`1`

########################
# Line initial fixations 
# These are fix_dur in RS and Line 1 first fixations

Lineinit= RS
LI=subset(raw_fix,raw_fix$line==1)
LI=subset(LI,LI$fix_num==2)
old<- c(2,5,8,9,11)
LI$Age=NULL
LI$remove=NULL
LI$Age<- ifelse(is.element(LI$sub, old), "Old", "Young")
LI$launchSite<- LI$prev_max_char_line- LI$prevChar
LI$landStart<- LI$char_line
LI$undersweep_prob<- ifelse(LI$Rtn_sweep_type=="undersweep", 1, 0)
Lineinit=rbind(Lineinit,LI)
##############################
# Line init following accurate or undersweep return sweep

Acc_RS_line_init=subset(RS,RS$Rtn_sweep_type=="accurate")
Und_RS_line_init=subset(RS,RS$Rtn_sweep_type=="undersweep")

#######################
# Line final 

Line_final=NULL
Line_final$sub=RS$sub
Line_final$item=RS$item
Line_final$seq=RS$seq
Line_final$fix_num=RS$fix_num-1
#Line_final$xpos=RS$prevX
#Line_final$ypos=RS$prevY
#Line_final$line=RS$line-1
Line_final=as.data.frame(Line_final)
Line_final=merge(raw_fix,Line_final)
# This doesn't give us the amount we should have...
# We are missing Line final fixations on the last line




# Add columns so that these match later on for duplicate removal
Line_final$Age=NULL
Line_final$remove=NULL
Inter_line$remove=NULL
Line_final$Age<- ifelse(is.element(Line_final$sub, old), "Old", "Young")
Line_final$launchSite<- Line_final$prev_max_char_line- Line_final$prevChar
Line_final$landStart<- Line_final$char_line
Line_final$undersweep_prob<- ifelse(Line_final$Rtn_sweep_type=="undersweep", 1, 0)

# Add line final fixations on the last line ?

## Inter-line fixations
# Make the columns match so they can be bound
raw_fix2=raw_fix
raw_fix2$Age=NULL
raw_fix2$remove=NULL
raw_fix2$Age<- ifelse(is.element(raw_fix2$sub, old), "Old", "Young")
raw_fix2$launchSite<- raw_fix2$prev_max_char_line- raw_fix2$prevChar
raw_fix2$landStart<- raw_fix2$char_line
raw_fix2$undersweep_prob<- ifelse(raw_fix2$Rtn_sweep_type=="undersweep", 1, 0)

##rbind the columns and then remove all ones that are the same in line Final 
Inter_line=rbind(raw_fix2,Line_final) 
Inter_line=Inter_line[!duplicated(Inter_line,fromLast = FALSE)&!duplicated(Inter_line,fromLast = TRUE),]
# Bind in line initial and remove the duplicates
Inter_line=rbind(Inter_line,Lineinit)
Inter_line=Inter_line[!duplicated(Inter_line,fromLast = FALSE)&!duplicated(Inter_line,fromLast = TRUE),]

#Correct the spelling error
Intra_line=Inter_line

  
  ######################################
# Mark second pass fixations Line initial, Line Final and Inter Line fixations?
######################################


######################################
# Mark second pass fixations Line initial, Line Final and Inter Line fixations?
######################################

########################################
#Mark Fix Groups

Und_RS_line_init$Fix_type=c("Undersweep_init")
Acc_RS_line_init$Fix_type=c("Accurate_init")
Line_final$Fix_type=c("Line_Final")
Intra_line$Fix_type=c("Intra_line")

#Merge back into full df for some reason 
All_fix=rbind(Und_RS_line_init,Acc_RS_line_init)
All_fix2=rbind(Line_final,Intra_line)
All_fix=rbind(All_fix,All_fix2)

All_fix$Fix_type=as.factor(All_fix$Fix_type)

# Make new RS data frame
NRS=subset(All_fix, All_fix$Rtn_sweep==1)
NRS2=subset(NRS,NRS$Fix_type=="Accurate_init")
NRS=subset(NRS,NRS$Fix_type=="Undersweep_init")
NRS=rbind(NRS,NRS2)
########################################
#Check for stuff within fixation groups

#Line initial general
ggplot(data = Lineinit, aes(x = Age, y = fix_dur, fill = Age))+

  #Line initial general
  ggplot(data = Lineinit, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

#Line initial undersweep
ggplot(data = Und_RS_line_init, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

#Line initial accurate
ggplot(data = Acc_RS_line_init, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

#Line final

ggplot(data = Line_final, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

#Intra line
ggplot(data = Intra_line, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

#Intra Line saccade lengths 
ggplot(data = Intra_line, aes(x = Age, y = sacc_len, fill = Age))+

geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()



#Line initial undersweep
ggplot(data = Und_RS_line_init, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

#Line initial accurate
ggplot(data = Acc_RS_line_init, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

#Line final

ggplot(data = Line_final, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()


#Intra line
ggplot(data = Intra_line, aes(x = Age, y = fix_dur, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

#Intra Line saccade lengths 
ggplot(data = Intra_line, aes(x = Age, y = sacc_len, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()


############################################################################################
## Nothing to see here...only means
#Intra
Intra_means=melt(Intra_line, id=c('sub', 'item', 'Age'), 
                 measure=c("fix_dur"), na.rm=TRUE)
Intra_means<- cast(Intra_means, Age ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

#Line Init Undersweep
Und_line_init_means=melt(Und_RS_line_init, id=c('sub', 'item', 'Age'), 
                         measure=c("fix_dur"), na.rm=TRUE)
Und_line_init_means<- cast(Und_line_init_means, Age ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))
#Line init accurate
acc_line_init_means=melt(Acc_RS_line_init, id=c('sub', 'item', 'Age'), 
                         measure=c("fix_dur"), na.rm=TRUE)
acc_line_init_means<- cast(acc_line_init_means, Age ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

#Line final 
Line_final_means=melt(Line_final, id=c('sub', 'item', 'Age'), 
                      measure=c("fix_dur"), na.rm=TRUE)
Line_final_means<- cast(Line_final_means, Age ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))


## Intra line Saccade Length means??
Sacc_len_means=melt(Intra_line, id=c('sub', 'item', 'Age'), 
                    measure=c("sacc_len"), na.rm=TRUE)
Sacc_len_means<- cast(Sacc_len_means, Age ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))


######################################################################################
######################################################################################

# Making sense# 
# Landing Position and Age Interaction
contrasts(RS$Age)<- c(1, -1)
summary(GLM1<- glmer(undersweep_prob~ Age * 
                        + (1|item)+ (1|sub), data= NRS, family= binomial))


ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)
#All Fix differences in fixation durations. 
contrasts(RS$Age)<- c(1, -1)
summary(allfixtypelm<- lmer(fix_dur~ Age * Fix_type + (1|item)+ (1|sub), data= All_fix))


ef2=effect("Age:Fix_type", allfixtypelm)
summary(ef2)
plot(ef2)

#NRS difference in return sweep fixation durations
summary(RSfixTypelm<- lmer(fix_dur~ Age * Fix_type + (1|item)+ (1|sub), data= NRS))


ef3=effect("Age:Fix_type", RSfixTypelm)
summary(ef3)
plot(ef3)

# Sacc_len based on fixation durations 
summary(Poop<-lmer(sacc_len~ Age + (1|item)+ (1|sub) , data= All_fix))
ef4=effect("Age",Poop)
plot(ef4)

#Age and Saccade Length
summary(GLM666<- glmer(undersweep_prob~ Age + 
                      sacc_len + (1|item)+ (1|sub), data= NRS, family= binomial))


ef5=effect("Age*sacc_len", GLM666)
summary(ef5)
plot(ef5)

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

##########################################
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

###########################################################
# Fix_dur line initial vs line final
#GG
fix_durGG=ggplot(RS, aes(fix_dur, undersweep_prob, colour = Age, fill = Age)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(fix_durGG)

#Violin
fix_durViolin=ggplot(data = RS, aes(x = Age, y = fix_dur, fill = Rtn_sweep_type))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# Model


summary(GLMP<- glmer(undersweep_prob~ Age + fix_dur + (1|item), data= RS, family= binomial))

summary(LMF<- lmer(fix_dur~ Age + (1|item), data= RS))
anova(LMF)

summary(LMA=lmer(prev_fix_dur~ Age + (1|item), data=RS))
#########################################################
## Does Prev_line_fix_dur predict fix_dur?
fixvs=ggplot(RS, aes(fix_dur, prev_fix_dur, colour = Age, fill = Age)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(fixvs)
summary(LMFV<- lmer(fix_dur~ prev_fix_dur + (1|item), data= RS))

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


################################################################################################
# Saccade Length by age 

Mlen=melt()


summary(LengSac<-lmer(sacc_len ~ Age + (1|item) +(1|sub), data= Intra_line))
efct=effect("Age",LengSac)
plot(efct)
lensim=powerSim(LengSac,nsim=40)
lensim
sacclen=ggplot(data = raw_fix, aes(x = Age, y = sacclen, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()


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


################################################################
Sim666=powerSim(GLM666,nsim=20)
Sim666


# Number of fixations 

NS3=melt(raw_fix,id=c('sub', 'item', 'Age'), 
         measure=c("fix_num"), na.rm=TRUE)
NS3<- cast(NS3, Age ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

ggplot(data = raw_fix, aes(x = Age, y = fix_num, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge")+
  geom_violin()




write.csv(RS,"RS.csv")


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





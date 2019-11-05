#Question analysis


# Martin R. Vasilev, 2019

# Data pre-processing:
rm(list= ls())

# Manual pre-processing of asc files:

# Calvin 1-21
# Victoria- 22- 43
# Martin 44+

# EyeDoctor_PadLines(data_dir = data_dir, paddingSize = 5)

# Install/ load R package used in preprocessing:
install.packages('EMreading')
library('EMreading')
install.packages('reshape')
library('reshape')
if('EMreading' %in% rownames(installed.packages())==FALSE){
  if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
  install_github('martin-vasilev/EMreading')
}else{
  library(EMreading)
}

library(lme4)
setwd("H:/Profile/Desktop/worb 2/SocioSpacial")
data_dir= ("H:/Profile/Desktop/worb 2/SocioSpacial/data")
#data_dir= "E:/FontSizeData" # Victoria

packages= c("reshape", "lme4", "ggplot2", "MASS", "arm", "effects", "lattice",
            "mgcv", "itsadug", 'ggpubr') # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

write.csv2(Quest,"LabCodeC/corpus/QuestAnaly")

###########################
# Comprehension accuracy: #
###########################

if(!file.exists("data/QuestDa.Rda")){
  QuestDa<- Question(data_list = data_dir, maxtrial = 100)
  save(Quest, file= "data/QuestDa.Rda")
  write.csv2(QuestDa, "data/QuestDa.csv")
} else{
  load("data/Quest.Rda")
}


library(reshape)
DesQuest<- melt(QuestDa, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuestBySub<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuestBySub
##############################
#Check Comprehension Data
###############################
#Compared By Subject
CompQ=split(QuestDa,QuestDa$dependnum)
CompQ=CompQ$`3`

CompDesQuest<- melt(CompQ, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
CompmQuest<- cast(CompDesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

#Compared by Item
CompQ=split(QuestDa,QuestDa$dependnum)
CompQ=CompQ$`3`

CompDesQuest<- melt(CompQ, id=c('sub', 'item', 'cond'), 
                    measure=c("accuracy"), na.rm=TRUE)
CompmQuest<- cast(CompDesQuest, item ~ variable
                  ,function(x) c(M=signif(mean(x),3)
                                 , SD= sd(x) ))
plot(CompmQuest$item,CompmQuest$accuracy_M)



#Questions Without COmprehension Data
NoCompQ=rbind(Q0$`1`,Q0$`2`)
NoCompQ=melt(NoCompQ, id=c('sub', 'item', 'cond'), 
              measure=c("accuracy"), na.rm=TRUE)
NoCompQ<- cast(NoCompQ, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))
plot(NoCompQ$item,NoCompQ$accuracy_M)


# Items by condition
Q0=split(QuestDa,QuestDa$dependnum)
Q1=Q0$`1`
Q2=Q0$`2`
Q1s=split(Q1,Q1$cond)
Q2s=split(Q2,Q2$cond)

AmbiSocQ1=rbind(Q1s$`1`,Q1s$`3`)
NamibiSocQ1=rbind(Q1s$`2`,Q1s$`4`)
AmbiSpaQ1=rbind(Q1s$`5`,Q1s$`7`)
NambiSpaQ1=rbind(Q1s$`6`,Q1s$`8`)

AmbiSocQ2=rbind(Q2s$`5`,Q2s$`8`)
NamibiSocQ2=rbind(Q2s$`6`,Q2s$`7`)
AmbiSpaQ2=rbind(Q2s$`1`,Q2s$`4`)
NambiSpaQ2=rbind(Q2s$`2`,Q2s$`3`)

AmbiSoc0=rbind(AmbiSocQ1,AmbiSocQ2)
AmbiSpa0=rbind(AmbiSpaQ1,AmbiSpaQ2)
NambiSoc0=rbind(NamibiSocQ1,NamibiSocQ2)
NambiSpa0=rbind(NambiSpaQ1,NambiSpaQ2)

AmbiGen=rbind(AmbiSoc0,AmbiSpa0)
NambiGen=rbind(NambiSoc0,NambiSpa0)

SpaGen=rbind(AmbiSpa0,NambiSpa0)
SocGen=rbind(AmbiSoc0,NambiSoc0)

AMG=melt(AmbiGen, id=c('sub', 'item', 'cond'), 
             measure=c("accuracy"), na.rm=TRUE)
AMG<- cast(AMG, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

NAMG=melt(NambiGen, id=c('sub', 'item', 'cond'), 
          measure=c("accuracy"), na.rm=TRUE)
NAMG<- cast(NAMG, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

#Plots

#General Ambi Vs Nambi
p = ggplot() + 
  geom_line(data = AMG, aes(x = item, y = accuracy_M), color = "blue") +
  geom_line(data = NAMG, aes(x = item, y = accuracy_M), color = "red") +
  xlab('item') +
  ylab('accuracy')
print(p)

#General Social vs Spatial
SPAG=melt(SpaGen, id=c('sub', 'item', 'cond'), 
         measure=c("accuracy"), na.rm=TRUE)
SPAG<- cast(SPAG, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

SOCG=melt(SocGen, id=c('sub', 'item', 'cond'), 
          measure=c("accuracy"), na.rm=TRUE)
SOCG<- cast(SOCG, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))


G=  ggplot() + 
  geom_line(data = SPAG, aes(x = item, y = accuracy_M), color = "blue") +
  geom_line(data = SOCG, aes(x = item, y = accuracy_M), color = "red") +
  xlab('item') +
  ylab('accuracy')
print(G)
#####################
# General Q1 vs Q2 
#######################
Q1DesQuest<- melt(Q1, id=c('sub', 'item', 'cond'), 
                  measure=c("accuracy"), na.rm=TRUE)
Q1Q<- cast(Q1DesQuest, item ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

Q2DesQuest<- melt(Q2, id=c('sub', 'item', 'cond'), 
                  measure=c("accuracy"), na.rm=TRUE)
Q2Q<- cast(Q2DesQuest, item ~ variable
           ,function(x) c(M=signif(mean(x),3)
           , SD= sd(x) ))
H=  ggplot() + 
  geom_line(data = Q1Q, aes(x = item, y = accuracy_M), color = "blue") +
  geom_line(data = Q2Q, aes(x = item, y = accuracy_M), color = "red") +
  xlab('item') +
  ylab('accuracy')
print(H) 
#################
# ASPA vs NSPA
##################
ASPA=melt(AmbiSpa0, id=c('sub', 'item', 'cond'), 
         measure=c("accuracy"), na.rm=TRUE)
ASPA<- cast(ASPA, sub ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

NSPA=melt(NambiSpa0, id=c('sub', 'item', 'cond'), 
          measure=c("accuracy"), na.rm=TRUE)
NSPA<- cast(NSPA, sub ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

Y=  ggplot() + 
  geom_line(data = ASPA, aes(x = item, y = accuracy_M), color = "blue") +
  geom_line(data = NSPA, aes(x = item, y = accuracy_M), color = "red") +
  xlab('item') +
  ylab('accuracy')
print(Y)
###################
#ASOC vs NSOC
####################
ASOC=melt(AmbiSoc0, id=c('sub', 'item', 'cond'), 
          measure=c("accuracy"), na.rm=TRUE)
ASOC<- cast(ASOC, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

NSOC=melt(NambiSoc0, id=c('sub', 'item', 'cond'), 
          measure=c("accuracy"), na.rm=TRUE)
NSOC<- cast(NSOC, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

X=  ggplot() + 
  geom_line(data = ASOC, aes(x = item, y = accuracy_M), color = "blue") +
  geom_line(data = NSOC, aes(x = item, y = accuracy_M), color = "red") +
  xlab('item') +
  ylab('accuracy')
print(X)


###############################
# ASOC vs ASPA & NSOC vs NSPA
###############################
J=  ggplot() + 
  geom_line(data = ASOC, aes(x = item, y = accuracy_M), color = "blue") +
  geom_line(data = ASPA, aes(x = item, y = accuracy_M), color = "red") +
  xlab('item') +
  ylab('accuracy')
print(J)

R=  ggplot() + 
  geom_line(data = NSOC, aes(x = item, y = accuracy_M), color = "blue") +
  geom_line(data = NSPA, aes(x = item, y = accuracy_M), color = "red") +
  xlab('item') +
  ylab('accuracy')
print(R)

#####################
#Accuracy by duration
#####################

Overalls=rbind(Q1,Q2)
TD=melt(Overalls, id=c('sub', 'item', 'cond','duration_ms'), 
          measure=c("duration_ms"), na.rm=TRUE)
TDS<- cast(TD, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

R=  ggplot() + 
  geom_line(data = TDS, aes(x = item, y = duration_ms_M), color = "blue") +
  xlab('item') +
  ylab('duration_ms')
print(R)
######################
# Sorted Out Data Set#
######################

SDS=NULL
SDS$item=1:24
SDS$sub=NULL
SDS$Q1ASPA=NULL
SDS$Q
SDS$Q2cond=NULL
SDS$



###################
#GLMM for accuracy#

       
                                      
Overalls$cond<- as.factor(Overalls$cond)
Overalls$dependnum<- as.factor(Overalls$dependnum)
Overalls$item=as.factor(Overalls$item)

#contrasts(Overalls$cond)<- c(-.5, 0.5)
#contrasts(Overalls$dependnum)<- c(-1, 1)

  TGLM<- glmer(accuracy ~ cond + (1|item)  +(1|sub), data = Overalls, family= binomial)

  summary(TGLM)


round(coef(summary(TGLM)), 2)

#Plots:
TGLMP= Effect(("cond"), TGLM)

summary(TGLMP)
# 
GD= as.data.frame(TGLMP)
TGLMP<- ggplot(GD, aes(x= cond, y=fit, ymax= upper, ymin= lower,
                        color=cond, linetype= cond, fill= cond, shape= cond)) + theme_bw (22)+
  geom_line(size= 1)+ geom_point(size=4)+
   labs(x= "condition", y= "accuracy", 
        color= "", shape= '', linetype= '', fill= '') +
   geom_ribbon(alpha= 0.2, color= NA) + theme(legend.position = c(0.87, 0.88), legend.title=element_blank(),
                                             legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
                                             panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
                                            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
                                              strip.background = element_rect(colour="white", fill="white"),
                                             strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"))





####
#Sim

fixef(TGLM)["size"]<- 0.05
PS=powerSim(TGLM,fixed("size", "z"),nsim=150)
summary(PS)



####
#Checker

Q0C=split(Q1,Q1$cond)
Question1Ambiguous=rbind(Q0C$`1`,Q0C$`3`,Q0C$`5`,Q0C$`7`)
Question1NonAmbiguous=rbind(Q0C$`2`,Q0C$`4`,Q0C$`6`,Q0C$`8`)

Q3C=split(Q2,Q2$cond)
Question2AmbiguousC=rbind(Q3C$`1`,Q3C$`4`,Q3C$`5`,Q3C$`8`)
Question2NonAmbiguous=rbind(Q3C$`2`,Q3C$`3`,Q3C$`6`,Q3C$`7`)

















################
# Trial times: #
################

if(!file.exists("data/Trial_time.Rda")){
  
  Trialt<- trialTime(data_list = data_dir, maxtrial = 100)
  save(Trialt, file= "data/Trial_time.Rda")
  write.csv2(Trialt, "data/Trial_time.csv")
}else{
  load("data/Trial_time.Rda")
}

DesTime<- melt(Trialt, id=c('sub', 'item', 'cond'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, cond ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
mTime


##################
# Raw Fixations: #
##################

if(!file.exists("preproc/raw_fix.Rda")){
  # extract raw data & merge it with da1 files:
  raw_fix<- preprocFromDA1(data_dir = data_dir, maxtrial = 100, padding = 5, tBlink = 100)
  save(raw_fix, file= "preproc/raw_fix.Rda")
  write.csv2(raw_fix, file= "preproc/raw_fix.csv")
}


##############################
# Preprocessing of raw data: #
##############################

# See pre-registered protocol for data preprocessing criteria:
# https://osf.io/9sngw#analysis-plan.data-exclusion

load("preproc/raw_fix.Rda")

raw_fix$hasText<- NULL


#######################################
# first, let's code some new variables:

raw_fix_new<- NULL

raw_fix$prev_RS<- NA
raw_fix$next_RS<- NA
raw_fix$prevChar<-NA
raw_fix$nextChar<- NA
raw_fix$prevX<- NA
raw_fix$nextX<- NA
raw_fix$prevY<- NA
raw_fix$prev_max_char_line<- NA

for(i in 1:64){
  n<- subset(raw_fix, sub==i)
  nitems<- unique(n$item)
  cat(i); cat(" ")
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    l1<- subset(m, line==1)
    max_l1<- l1$max_char_line[1]
    
    for(k in 1:nrow(m)){
      if(k==1){
        m$prev_RS[k]<- 0
        m$next_RS[k]<- 0
        m$next_RS[k+1]<- 0
        
        ####
        m$nextChar[k]<- m$char_line[k+1] # next char
        m$nextX[k]<- m$xPos[k+1]
        
      }else{
        if(m$Rtn_sweep[k]==1){
          m$prev_RS[k-1]<- 1
          
          if(k+1 <= nrow(m)){
            m$next_RS[k+1]<- 1
          }
          
        }else{
          m$prev_RS[k-1]<- 0
          
          if(k+1 <= nrow(m)){
            m$next_RS[k+1]<- 0
          }
        }
        ###
        m$prevChar[k]<- m$char_line[k-1] # prev char
        m$prevX[k] <- m$xPos[k-1] # prev x
        m$prevY[k]<- m$yPos[k-1]
        
        if(k+1<= nrow(m)){
          m$nextChar[k]<- m$char_line[k+1] # next char
          m$nextX[k]<- m$xPos[k+1] # next x
        }
        
        
      }
      
      if(k== nrow(m)){
        m$prev_RS[k]<- 0
      }
      
      ## map previous line length (for launch site calculation):
      if(!is.na(m$line[k])){
        if(m$line[k]==2){
          m$prev_max_char_line[k]<- max_l1
        }else{
          m$prev_max_char_line[k]<- NA
        }
      }else{
        if(m$Rtn_sweep[k]==1){
          m$prev_max_char_line[k]<- max_l1
        }
      }
      
    } # end of m
    raw_fix_new<- rbind(raw_fix_new, m)
  } # end of j
  
  
}

raw_fix<- raw_fix_new;
rm(raw_fix_new)



nAllTrials<- 64*100

########################################
# check number of trials per subject
nTrials<- NULL

for(i in 1:64){
  n<- subset(raw_fix, sub==i)
  nTrials[i]<- length(unique(n$item))
}
nTrials

# 3 trials were discarded during manual processing (due to track losses, etc.)

nDiscardedTrials<- nAllTrials- sum(nTrials)


##############################
# check if there is only 1 return sweep per trial (expected, due to the two lines):

noRS_sub<- NULL # no return sweeps this trial
noRS_item<- NULL # no return sweeps this trial
for(i in 1:64){
  n<- subset(raw_fix, sub==i)
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    if(sum(m$Rtn_sweep)>1){ # >1 RS detected..
      cat(sprintf("Subject %g, item %g, %g return sweeps\n", i, nitems[j], sum(m$Rtn_sweep)))
    }
    
    if(sum(m$Rtn_sweep)==0){ # no return sweeps detected
      cat(sprintf("Subject %g, item %g, %g return sweeps\n", i, nitems[j], sum(m$Rtn_sweep)))
      noRS_sub<- c(noRS_sub, i)
      noRS_item<- c(noRS_item, nitems[j])
      
    }
  }
}

# remove trials with no return sweeps:
for(i in 1:length(noRS_sub)){
  out<- which(raw_fix$sub== noRS_sub[i]& raw_fix$item== noRS_item[i])
  raw_fix<- raw_fix[-out,]
  
}
nNoRS<- length(noRS_sub)

#########################
# let's merge fixations smaller than 80 ms

raw_fix_new<- cleanData(raw_fix, removeOutsideText = F, removeBlinks = F, combineNearbySmallFix = T, 
                        combineMethod = "pix", combineDist = 14, removeSmallFix = F, 
                        removeOutliers = F, keepRS = T)
raw_fix<- raw_fix_new
rm(raw_fix_new)

less80<- raw_fix[which(raw_fix$fix_dur<80 & raw_fix$Rtn_sweep==1), ]


# remove remaining fixations less than 80 only if they are not return sweeps:
#raw_fix<- raw_fix[which(raw_fix$fix_dur<80 & raw_fix$Rtn_sweep==1), ]
raw_fix<- raw_fix[-which(raw_fix$fix_dur<80), ]

# Some of the discarded <80 ms fixations may be return sweep ones. Therefore, we will remap fixations for such trials,
# by taking the next fixation as the return sweep one

nsubs<- unique(raw_fix$sub)

new<- NULL
for(i in 1:length(nsubs)){ # for each subject..
  n<- subset(raw_fix, sub== nsubs[i])
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){ # for each item..
    m<- subset(n, item== nitems[j])
    
    if(sum(m$Rtn_sweep)>0){
      new<- rbind(new, m)
    }else{
      line= 1
      for(k in 1:nrow(m)){
        if(!is.na(m$line[k])){
          if(m$line[k]> line){
            line= line+1
            m$Rtn_sweep[k]<- 1
            if(m$xPos[k+1]> m$xPos[k]){
              m$Rtn_sweep_type[k]<- "accurate" 
            }else{
              m$Rtn_sweep_type[k]<- "undersweep"
            }
            
            a<- which(less80$sub== m$sub[1] & less80$item== m$item[1])
            if(length(a)>0){
              if(!is.na(less80$char_line[a])){
                m$prevChar[k]<- less80$prevChar[a]
                m$prevX[k]<- less80$prevX[a]
                m$prevY[k]<- less80$prevY[a] 
              }else{
                m$prevChar[k]<- m$char_trial[k-1]
                m$prevX[k]<- m$xPos[k-1]
                m$prevY[k]<- m$yPos[k-1] 
              }
              
            }else{
              m$prevChar[k]<- m$char_trial[k-1]
              m$prevX[k]<- m$xPos[k-1]
              m$prevY[k]<- m$yPos[k-1] 
            }
            
          }
        }
      } 
      new<- rbind(new, m)
      cat(sprintf('changed sub %g item %g \n', m$sub[1], m$item[1]))
    }
    
  }
  cat(i); cat("\n")
}

raw_fix<- new; rm(new)


### double-check to make sure there are no trials with 0 RS..
noRS_sub<- NULL # no return sweeps this trial
noRS_item<- NULL # no return sweeps this trial
for(i in 1:64){
  n<- subset(raw_fix, sub==i)
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    if(sum(m$Rtn_sweep)>1){ # >1 RS detected..
      cat(sprintf("Subject %g, item %g, %g return sweeps\n", i, nitems[j], sum(m$Rtn_sweep)))
    }
    
    if(sum(m$Rtn_sweep)==0){ # no return sweeps detected
      cat(sprintf("Subject %g, item %g, %g return sweeps\n", i, nitems[j], sum(m$Rtn_sweep)))
      noRS_sub<- c(noRS_sub, i)
      noRS_item<- c(noRS_item, nitems[j])
      
    }
  }
}

# one more trial is removed as there are no more fixations on the second line
for(i in 1:length(noRS_sub)){
  out<- which(raw_fix$sub== noRS_sub[i]& raw_fix$item== noRS_item[i])
  raw_fix<- raw_fix[-out,]
  
}
nNoRS<- nNoRS+ length(noRS_item)


##########################
# check for blinks occuring on return sweep saccade/fixation:
RS_blinks<- raw_fix[which(raw_fix$Rtn_sweep==1), ]
RS_blinks<- subset(RS_blinks, blink==1 | prev_blink==1 | after_blink==1)

nBlinks<- nrow(RS_blinks)

for(i in 1:nrow(RS_blinks)){
  raw_fix<- raw_fix[-which(raw_fix$sub== RS_blinks$sub[i]& raw_fix$item== RS_blinks$item[i]),]
}

# remove also blinks that did not occur next to return sweeps:

raw_fix<- raw_fix[-which(raw_fix$blink==1 | raw_fix$prev_blink==1 | raw_fix$after_blink==1),]


# confirm we got the correct num of trials:
RS_blinks<- raw_fix[which(raw_fix$Rtn_sweep==1), ]
nrow(RS_blinks)== nAllTrials- nBlinks- nNoRS- nDiscardedTrials

RS_blinks<- subset(RS_blinks, blink==1 | prev_blink==1 | after_blink==1)

if(nrow(RS_blinks)==0){
  cat("GOOD!")
  rm(RS_blinks)
}else{
  cat(":(")
}

table(raw_fix$blink)
table(raw_fix$prev_blink)
table(raw_fix$after_blink)
# remove blink columns
raw_fix$blink<- NULL
raw_fix$prev_blink<- NULL
raw_fix$after_blink<- NULL


####################
# check for outliers:

outliers<- raw_fix[which(raw_fix$fix_dur>1000),]

outliers<- subset(outliers, Rtn_sweep==1 | prev_RS==1 | next_RS==1)

nOutliers<- nrow(outliers)

for(i in 1:nrow(outliers)){
  raw_fix<- raw_fix[-which(raw_fix$sub== outliers$sub[i] & raw_fix$item== outliers$item[i]), ]
}

# remove remaining outlier fixations (not next to return sweeps)
raw_fix<- raw_fix[-which(raw_fix$fix_dur>1000),]

# remove fixations outside screen bounds:
raw_fix<- raw_fix[-which(raw_fix$outOfBnds==1 & raw_fix$Rtn_sweep==0 & raw_fix$prev_RS==0 & raw_fix$next_RS==0),]

# not really sure why we still have these, but we remove them here:
raw_fix<- raw_fix[-which(raw_fix$prevX<0 |raw_fix$nextX<0),]


###################################################
# let's verify we have the correct number of trials:
RS<- subset(raw_fix, Rtn_sweep==1)

nrow(RS)+ nOutliers+ nBlinks+ nNoRS+ nDiscardedTrials == nAllTrials

# now let's print a summary:

fileConn<- file("preproc/Preproc_summary.txt", "w")

writeLines(sprintf("- %#.2f percent of trials manually discarded due to tracking loss, etc.",  
                   round((nDiscardedTrials/nAllTrials)*100, 2)), fileConn)

writeLines(sprintf("- %#.2f percent of trials discarded due to the lack of a return sweep in the trial",  
                   round((nNoRS/nAllTrials)*100, 2)), fileConn)

writeLines(sprintf("- %#.2f percent of trials discarded due to blinks on or around return sweeps",  
                   round((nBlinks/nAllTrials)*100, 2)), fileConn)

writeLines(sprintf("- %#.2f percent of trials discarded due to outliers",  
                   round((nOutliers/nAllTrials)*100, 2)), fileConn)

writeLines(sprintf("- %#.2f percent of trials remaining for analysis",  
                   round((nrow(RS)/nAllTrials)*100, 2)), fileConn)

close(fileConn)


###################
# re-organise file:
raw_fix$sent<- NULL # all items are 1 sentence
raw_fix$outOfBnds<- NULL # remove above
raw_fix$time_since_start<- NULL # no longer necessary
raw_fix$prev_RS<- NULL
raw_fix$next_RS<- NULL

cl<- colnames(raw_fix)
raw_fix<- raw_fix[, c(cl[1:15], cl[24:29], cl[16:23])]

# We lost some fixations (e.g., due to blinks, etc.), so let's make sure we update prevX and prevChar
# before calculating remaining measures below:

nsubs<- unique(raw_fix$sub)

new<- NULL
for(i in 1:length(nsubs)){ # for each subject..
  n<- subset(raw_fix, sub== nsubs[i])
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){ # for each item..
    m<- subset(n, item== nitems[j])
    
    for(k in 1:nrow(m)){ # for each fixation
      if(k>1){
        if(m$prevX[k]!= m$xPos[k-1]){
          m$prevX[k]<- m$xPos[k-1]
          m$prevY[k]<- m$yPos[k-1]
          m$prevChar[k]<- m$char_line[k-1]
          cat(sprintf('updated sub %g item %g fix %g \n', m$sub[1], m$item[1], m$fix_num[k]))
        }
      }
    } # end of k
    
    new<- rbind(new, m)
    
  } # end of j
} # end of i

raw_fix<- new; rm(new)


# add landing position relative to line start (in letters):
raw_fix$LandStartLet<- raw_fix$char_line

# landing position relative to line start (in degrees per visual angle)

offset= 200 # x offset in pixels
DPP<- 0.02461393513610085 # degree per pixel in the experiment

raw_fix$LandStartVA<- (raw_fix$xPos - 200)*DPP

# code undersweep probability:
raw_fix$undersweep_prob<- 0
raw_fix$undersweep_prob[which(raw_fix$Rtn_sweep_type== "undersweep")]<- 1


# code (absolute) launch site distance in letters:
raw_fix$launchDistLet<- abs(raw_fix$char_line- raw_fix$prevChar)

# code (absolute) launch site distance in visual angle:
raw_fix$launchDistVA<- abs(raw_fix$xPos-raw_fix$prevX)*DPP


# recode saccade length:
raw_fix$sacc_len<- abs(raw_fix$char_line- raw_fix$prevChar)


##################################
## Finally, save data for analysis:
Alldata<- raw_fix
save(Alldata, file= "data/Alldata.Rda")
write.csv(Alldata, "data/Alldata.csv")

###
RS<- subset(raw_fix, Rtn_sweep==1)

# code launch site (as a function of end of line):
RS$launchSite<- RS$prev_max_char_line- RS$prevChar

# code launch site, but this time in visual angle:

ppl<- 0
lineEnd<- 0
RS$launchSiteVA<- NA
for(i in 1:nrow(RS)){
  if(RS$cond[i]== 1 | RS$cond[i]== 3){
    ppl<- 12 # small font
  }else{
    ppl<- 16 # big font
  }
  lineEnd<- 200+ RS$prev_max_char_line[i]*ppl
  RS$launchSiteVA[i]<- (lineEnd - RS$prevX[i])*DPP
}


save(RS, file= "data/Return_sweep.Rda")
write.csv(RS, file= "data/Return_sweep.csv")


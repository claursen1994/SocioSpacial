#Pilot Data Analysis
# Install Packages and Libraries.
#Packages#
rm(list= ls())

install.packages("wordcloud")
install.packages("tm")
install.packages("arm")
install.packages("MASS")
install.packages("lattice")
install.packages("lme4")
install.packages("effects")
install.packages("SnowballC")
install.packages("RColorBrewer")
install.packages("ngram")
install.packages("quanteda")
install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("digest")
install.packages("EMreading")
install.packages("simr")
install.packages("jtools")
#Library#

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ngram")
library("koRpus")
library("readxl")
library("quanteda")
library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("lme4")
library("effects")
library("tidyverse")
library("EMreading")
library("EMreading")
library("lme4")
library("simr")
library("jtools")
rm(list= ls())
data_dir= ("H:/Profile/Desktop/worb 2/SocioSpacial/SoSpaPilotASC")

#Load or read in Data

load("preproc/raw_fix.Rda")
if(!file.exists("preproc/raw_fix.Rda")){
  # extract raw data & merge it with da1 files:
  raw_fix<- preprocFromDA1(data_dir = data_dir, maxtrial = 100, padding = 5, tBlink = 100)
  save(raw_fix, file= "preproc/raw_fix.Rda")
  write.csv2(raw_fix, file= "preproc/raw_fix.csv")
}


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

# Add in Pilot Age Groups, this can't be done automatically as the above script gives new sub numbers
library(readxl)
PilotAges <- read_excel("preproc/PilotAges.xlsx")

raw_fix=merge(raw_fix,PilotAges)
# Additional important data
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
m
#Set up contrast for LMMS etc
RS$Age<- as.factor(RS$Age)
contrasts(RS$Age)<- c(1, -1)

################################################# Return Sweeps ########################################
library(lme4)

#################################Check and Mark 2nd pass in Return Sweeps. 


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

################################ UNDERSWEEP PROBABILITY##############################################

##################################### Make models
contrasts(RS$Age)<- c(1, -1)

summary(GLM0<- glmer(undersweep_prob~ Age + (Age|item), data= RS, family= binomial))

#Effect of Age on undersweep Probability
ef0=effect("Age", GLM0)
summary(ef0)
plot(ef0)




################################# Get power of effects for:
#UnderSweepProbability
#General sim
#make fitted model
USPSIM=powerSim(GLM0,nsim=32)
USPSIM

############################################# SKIP RATE ############################################################

Skip_Raw_fix=raw_fix
Skip_Raw_fix22=wordMeasures(Skip_Raw_fix)
Skip_Raw_fix22$skip=NULL
# If Nfix 1 was 0 then the word was skipped the first time.
Skip_Raw_fix22$skip<- ifelse(Skip_Raw_fix22$nfix1=="0", 1, 0)

# If Nfix1 was 0 and Nfix2 was 1 or more then the word was fixated later 

Skip_Raw_fix22$hold<- ifelse(Skip_Raw_fix22$nfix2>"0",1,0 )
Skip_Raw_fix22$Return2SkipWord= ifelse(Skip_Raw_fix22$skip+Skip_Raw_fix22$hold=="2",1,0)
Skip_Raw_fix22$hold=NULL

Skips=merge(Skip_Raw_fix22,Skip_Raw_fix)

## Add variables for Zipf frequence and Length of word.##
Skips$wordID=as.character(Skips$wordID)
Skips$Length=nchar(Skips$wordID)
#Add in zipf scores for further analysis 

#This can take a while
lex2=read_table2("SUBTLEX-UK/SUBTLEX-UK.txt")
#lex = read_xlsx("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.xlsx")
Skips$Zipf<- NA
Skips$freq<-NA
for(i in 1:nrow(Skips)){
  a<- which(lex2$Spelling== Skips$wordID[i])
  if(length(a)>0){
    Skips$Zipf[i]<- lex2$`LogFreq(Zipf)`[a]
    Skips$freq[i]<- lex2$FreqCount[a]
  }
}


############################################ Make Model
# center by means 
Skips$Length=center(Skips$Length)
Skips$Zipf=center(Skips$Zipf)
#
summary(GLM1<- glmer(skip~ Age*Length*Zipf +(1|item)+ (1|sub), data= Skips, family= binomial))
ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)

########################### Simulate
SKIPSIM=powerSim(GLM1,nsim=16, test=fixed("Age*Length"))
SKIPSIM


##################################### Regressions #######################################
# Return to skipped words
summary(GLM2<- glmer(Return2SkipWord~ Age +(1|item)+ (1|sub), data= Skips, family= binomial))
RegEF=effect("Age", GLM2)
summary(RegEF)
plot(RegEF)

####################### Simulate 
Regsim=powerSim(GLM2,nsim=32)
Regsim


##################################### Launch Site #######################################

summary(LaunchLM <- lmer(launchSite~Age+(1|item)+(Age|sub),data=RS))
LaunchEF=effect("Age",LaunchLM)
summary(LaunchEF)
plot(LaunchEF)

#This seems to be a null effect... 

#################################### Landing position ###################################
summary(LandLM <- lmer(landStart~launchSite+Age+(launchSite+Age|item)+(launchSite+Age|sub),data=RS))
LandEF=effect("Age",LandLM)
summary(LandEF)
plot(LandEF)

#Simulation 
Landsim=powerSim(LandLM,nsim=64)#, test=fixed("Age"))
Landsim
############################## Different saccade and Fixation Types ##########################


################################################# Line initial fixations 
# These are fix_dur in RS and Line 1 first fixations

Lineinit= RS
LI=subset(raw_fix,raw_fix$line==1)
LI$launchSite<- LI$prev_max_char_line- LI$prevChar
LI$landStart<- LI$char_line
LI$undersweep_prob<- ifelse(LI$Rtn_sweep_type=="undersweep", 1, 0)
LI=subset(LI,LI$fix_num==2)
Lineinit=rbind(Lineinit,LI)
rm(LI)
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
Line_final=as.data.frame(Line_final)
Line_final=merge(raw_fix,Line_final)
# This doesn't give us the amount we should have...
# We are missing Line final fixations on the last line
# Add columns so that these match later on for duplicate removal
Line_final$launchSite<- Line_final$prev_max_char_line- Line_final$prevChar
Line_final$landStart<- Line_final$char_line
Line_final$undersweep_prob<- ifelse(Line_final$Rtn_sweep_type=="undersweep", 1, 0)


################ Add columns so that these match later on for duplicate removal
# Intra-line fixations
# Make the columns match so they can be bound
raw_fix2=raw_fix
raw_fix2$remove=NULL
raw_fix2$launchSite<- raw_fix2$prev_max_char_line- raw_fix2$prevChar
raw_fix2$landStart<- raw_fix2$char_line
raw_fix2$undersweep_prob<- ifelse(raw_fix2$Rtn_sweep_type=="undersweep", 1, 0)

##rbind the columns and then remove all ones that are the same in line Final 
Intra_line=rbind(raw_fix2,Line_final) 
Intra_line=Intra_line[!duplicated(Intra_line,fromLast = FALSE)&!duplicated(Intra_line,fromLast = TRUE),]
# Bind in line initial and remove the duplicates
Intra_line=rbind(Intra_line,Lineinit)
Intra_line=Intra_line[!duplicated(Intra_line,fromLast = FALSE)&!duplicated(Intra_line,fromLast = TRUE),]

#Mark Fix types

Und_RS_line_init$Fix_type=c("Undersweep_init")
Acc_RS_line_init$Fix_type=c("Accurate_init")
Line_final$Fix_type=c("Line_Final")
Intra_line$Fix_type=c("Intra_line")

#Merge back into full df for some reason 

All_fix=rbind(Und_RS_line_init,Acc_RS_line_init)
All_fix2=rbind(Line_final,Intra_line)
All_fix=rbind(All_fix,All_fix2)
rm(All_fix2)
All_fix$Fix_type=as.factor(All_fix$Fix_type)
# Make new RS data frame so differences can be checked there more easily
NRS=subset(All_fix, All_fix$Rtn_sweep==1)
NRS2=subset(NRS,NRS$Fix_type=="Accurate_init")
NRS=subset(NRS,NRS$Fix_type=="Undersweep_init")
NRS=rbind(NRS,NRS2)
rm(NRS2)


##################### Check for Age effects within fixation groups ###########################
#fix type and age
contrasts(RS$Age)<- c(1, -1)
contrasts(All_fix$Fix_type)=contr.treatment(4)
summary(allfixtypelm<- lmer(log(fix_dur)~ Age * Fix_type + (1|item)+ (1|sub), data= All_fix))

ef3=effect("Age:Fix_type", allfixtypelm)
summary(ef3)
plot(ef3)

## Simulate 
FIXTYPESIM=powerSim(allfixtypelm,nsim=32, test=fixed("Age:Fix_type"))
FIXTYPESIM
####




#  Age effects of Intra line saccade Legnths 
summary(saclen<-lmer(sacc_len~ Age + (1|item)+ (1|sub) , data= Intra_line))
ef8=effect("Age",saclen)
plot(ef8)

SLSIM=powerSim(saclen,nsim=32)
SLSIM









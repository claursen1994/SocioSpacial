#Pilot Data Analysis
# Install Packages and Libraries.
#Packages#

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

rm(list= ls())
#data_dir= ("H:/Profile/Desktop/worb 2/SocioSpacial/SoSpaPilotASC")
data_dir=("E:/CalvinsDumb_work_Stuff/Proc/List Files") # From External Drive
# data_dir= 'D:/Data/Aging'  # Martin

#Load or read in Data

load("preproc/raw_fix.Rda")
if(!file.exists("preproc/raw_fix.Rda")){
  # extract raw data & merge it with da1 files:
  raw_fix<- preprocFromDA1(data_dir = data_dir, maxtrial = 100, padding = 5, tBlink = 150)
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

nsubs<- unique(raw_fix$sub)

# sub 3, item 22

for(i in 1: length(nsubs)){
  n<- subset(raw_fix, sub== nsubs[i])
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
        if(!is.na(m$Rtn_sweep[k])& m$Rtn_sweep[k]==1){
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
      # if(!is.na(m$line[k])){
      #   if(m$line[k]==2){
      #     m$prev_max_char_line[k]<- max_l1
      #   }else{
      #     m$prev_max_char_line[k]<- NA
      #   }
      # }else{
      #   if(m$Rtn_sweep[k]==1){
      #     m$prev_max_char_line[k]<- max_l1
      #   }
      # }
      
      if(k>1){
        m$prev_max_char_line[k]<- m$max_char_line[k-1] 
      }else{
        m$prev_max_char_line[k]<- NA
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

blinks<- which(raw_fix$blink== 1 | raw_fix$prev_blink== 1 | raw_fix$after_blink== 1)
raw_fix<- raw_fix[-blinks,]

raw_fix$blink<- NULL
raw_fix$prev_blink<- NULL
raw_fix$after_blink<- NULL

#raw_fix<- subset(raw_fix, blink==0 & prev_blink==0 & after_blink==0)


# remove outliers:
out<- which(raw_fix$fix_dur<80 | raw_fix$fix_dur>1000)
raw_fix<- raw_fix[-out,]


# remove fixations that were not coded in Eye-doctor:
raw_fix<- subset(raw_fix, line>0)


## Remove lines with only one fixation (since they are both accurate and line-final)
new_dat<- NULL

nsubs<- unique(raw_fix$sub)

for(i in 1:length(nsubs)){
  n<- subset(raw_fix, sub== nsubs[i])
  
  nitems<- unique(n$item)
  #cat(i); cat(" ")
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    nlines<- unique(m$line)
    
    for(k in 1:length(nlines)){
      o<- subset(m, line== nlines[k])
      
      if(nrow(o)>1){
        new_dat<- rbind(new_dat, o)
      }else{
        cat(sprintf("Subject %i, item %i, line% i has 1 fixation- REMOVED", nsubs[i], nitems[j], nlines[k]))
        cat("\n")
      }
      
    }
    
    
    
  }
  
}

raw_fix<- new_dat; rm(new_dat)

# Calculate word-level measures here:
FD<- wordMeasures(raw_fix)

raw_fix<- raw_fix[-which(is.na(raw_fix$SFIX)),]



## map fixation types:
raw_fix$Fix_type<- raw_fix$Rtn_sweep_type
for(i in 1:nrow(raw_fix)){

  if(is.na(raw_fix$SFIX[i+1])){
    raw_fix$Fix_type[i]<- 'intra-line'
    next
  }
  
  if(is.na(raw_fix$SFIX[i])){
    next
  }
  
  if(i!= nrow(raw_fix)){
    
    if(raw_fix$Rtn_sweep[i+1]==1){
      raw_fix$Fix_type[i]<- 'line-final'
    }else{
      if(is.na(raw_fix$Fix_type[i])){
        raw_fix$Fix_type[i]<- 'intra-line'
      }
    }
    
  }else{
    if(is.na(raw_fix$Fix_type[i])){
      raw_fix$Fix_type[i]<- 'intra-line'
    }
  }
    

}




write.csv(raw_fix,"raw_fix_data.csv")
write.csv(FD,"FD.csv")

################################################# Return Sweeps ########################################
FD=read.csv("FD.csv")
FD$X=NULL
raw_fix=read.csv("raw_fix_data.csv")
raw_fix$X=NULL
PilotAges <- read_excel("preproc/PilotAges.xlsx")
# Let's check that we didn't lose over 1/3 of return sweeps in a trial. If so let's delete them. 

noRS_sub<- NULL # no return sweeps this trial
noRS_item<- NULL # no return sweeps this trial
for(i in 1:16){
  n<- subset(raw_fix, sub==i)
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    if(sum(m$Rtn_sweep)>14){ # >14 minimum number of return sweeps in all trials..
      cat(sprintf("Subject %g, item %g, %g return sweeps\n", i, nitems[j], sum(m$Rtn_sweep)))
    }
    
    if(sum(m$Rtn_sweep)<7){ #over  1/3 of the return sweeps are missing using min return sweeps in all trials as baseline
      cat(sprintf("Subject %g, item %g, %g return sweeps\n", i, nitems[j], sum(m$Rtn_sweep)))
      noRS_sub<- c(noRS_sub, i)
      noRS_item<- c(noRS_item, nitems[j])
      
    }
  }
}
for(i in 1:length(noRS_sub)){
  out<- which(raw_fix$sub== noRS_sub[i]& raw_fix$item== noRS_item[i])
  raw_fix<- raw_fix[-out,]
  
}
nNoRS<- length(noRS_sub)
RS=NULL
RS<- subset(raw_fix, Rtn_sweep==1)

## Nice

RS$launchSite<- RS$prev_max_char_line- RS$prevChar
RS$landStart<- RS$char_line
RS$undersweep_prob<- ifelse(RS$Rtn_sweep_type=="undersweep", 1, 0)

# Take a look at some means

library(reshape)
Des<- melt(RS, id=c('sub', 'item', 'Age'), 
           measure=c("landStart", "undersweep_prob", "launchSite", "sacc_dur") , na.rm=TRUE)

m<- cast(Des, Age+sub ~ variable
         , function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))
m
#Set up contrast for LMMS etc
RS$Age<- as.factor(RS$Age)
contrasts(RS$Age)<- c(1, -1)

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
#Take out practice trials 
RS2=RS
Prac=NULL
Prac$item=c(25:26)
RS2 <- RS2[!(RS2$item %in% Prac$item),]
RS=RS2
rm(RS2)
raw_fix <- raw_fix[!(raw_fix$item %in% Prac$item),]
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




################################# Get power of effects for:
#UnderSweepProbability
#General sim
#make fitted model
#fixef(GLM0)["Age1"]<-0.15
#powerSim(GLM0)

#model1=extend(GLM0,along="sub", n=80)

#USPSIM=powerSim(model1,nsim=32 )

#PC1=powerCurve(model1, along = "sub", breaks = c(16,24,32,40,48,56,64,72,80))
#plot(PC1)
#USPSIM

############################################# SKIP RATE ############################################################
# 
# Skip_Raw_fix=raw_fix
# Skip_Raw_fix22=wordMeasures(Skip_Raw_fix)
# Skip_Raw_fix22$skip=NULL
# # If Nfix 1 was 0 then the word was skipped the first time.
# Skip_Raw_fix22$skip<- ifelse(Skip_Raw_fix22$nfix1=="0", 1, 0)
# 
# # If Nfix1 was 0 and Nfix2 was 1 or more then the word was fixated later 
# 
# Skip_Raw_fix22$hold<- ifelse(Skip_Raw_fix22$nfix2>"0",1,0 )
# Skip_Raw_fix22$Return2SkipWord= ifelse(Skip_Raw_fix22$skip+Skip_Raw_fix22$hold=="2",1,0)
# Skip_Raw_fix22$hold=NULL
# 
# Skips=merge(Skip_Raw_fix22,Skip_Raw_fix)
# 
# ## Add variables for Zipf frequence and Length of word.##
# Skips$wordID=as.character(Skips$wordID)
# Skips$Length=nchar(Skips$wordID)
# #Add in zipf scores for further analysis 

Skips<- FD; rm(FD)

#This can take a while
Skips$cleanwordID<- tolower(Skips$wordID)
#install.packages("tm")
#library("tm")
Skips$cleanwordID<-removePunctuation(Skips$cleanwordID)

lex2=read_table2("SUBTLEX-UK/SUBTLEX-UK.txt")
#lex = read_xlsx("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.xlsx")
Skips$Zipf<- NA
Skips$freq<-NA
for(i in 1:nrow(Skips)){
  a<- which(lex2$Spelling== Skips$cleanwordID[i])
  if(length(a)>0){
    Skips$Zipf[i]<- lex2$`LogFreq(Zipf)`[a]
    Skips$freq[i]<- lex2$FreqCount[a]
  }
}
Skips$Length=nchar(Skips$cleanwordID)

############################################ Make Model
# center by means 

Skips$Length=center(Skips$Length)
Skips$Zipf=center(Skips$Zipf)
#
#merge in Ages
Skips=merge(Skips,PilotAges)
#Run Model
summary(GLM1<- glmer(skip_1st~ Age*Length*Zipf +(1|item)+ (1|sub), data= Skips, family= binomial))
ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)

########################### Simulate
fixef(GLM1)["Agey"]<--0.51
fixef(GLM1)["Agy:Lngth:Z"]<--0.041
#powerSim(GLM1)

model2=extend(GLM1,along="sub", n=80)
powerSim(GLM1, test=fixed("Age"),nsim=10)
#USPSIM=powerSim(model1,nsim=32 )

PC2=powerCurve(model2, along = "sub", breaks = c(16,24,32,40,48,56,64,72,80), test = fixed("Age"),nsim = 3,
               sim = model2, seed=10)
plot(PC2)
#USPSIM


##################################### Regressions #######################################
# Return to skipped words
#contrast treat first pass skips 
Skips$skip_1st<-as.factor(Skips$skip_1st)
contrasts(Skips$skip_1st)=contr.treatment(2)

summary(GLM2<- glmer(regress~ Age +(1|item)+ (1|sub), data= Skips, family= binomial))
RegEF=effect("Age", GLM2)
summary(RegEF)
plot(RegEF)

####################### Simulate 
fixef(GLM2)["Agey"]<--0.67
doTest(GLM2,test=fixed("Age"))
powerSim(GLM2, test= fixed("Age"),nsim=10)

model3=extend(GLM2,along="sub", n=80)
rm(model3)
#USPSIM=powerSim(model1,nsim=32 )

PC3=powerCurve(model3, along = "sub", breaks = c(16,24,32,40,48,56,64,72,80),test = fixed("Age"),nsim=3,
               sim = model3, seed=10)
plot(PC3)
#USPSIM
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
#powerSim(model4,test = fixed("Age"),nsim=10)

model4=extend(LaunchLM,along="sub", n=80)

#USPSIM=powerSim(model1,nsim=32 )

PC4=powerCurve(model4, along = "sub", breaks = c(16,24,32,40,48,56,64,72,80),test = fixed("Age"),nsim=1000,
               sim = model4, seed=10)
plot(PC4)
chk<-lastResult()
chk$errors
rm(model4)
#USPSIM
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
#doTest(LandLM, test=fixed("launchSite:Age"))
#powerSim(LandLM,nsim=20, test=fixed ("Age"))

model5=extend(LandLM,along="sub", n=80)

#USPSIM=powerSim(model1,nsim=32 )

PC5=powerCurve(model5, along = "sub", breaks = c(16,24,32,40,48,56,64,72,80),test = fixed("Age"),nsim=1000,
               sim = model5, seed=10)

plot(PC5)
chk<-lastResult()
chk$errors
rm(ef7)
#USPSIM
#Yes
############################## Different saccade and Fixation Types ##########################



################################################# Line initial fixations 
# These are fix_dur in RS and Line 1 first fixations

# Lineinit= RS
# LI=subset(raw_fix,raw_fix$line==1)
# LI$launchSite<- LI$prev_max_char_line- LI$prevChar
# LI$landStart<- LI$char_line
# LI$undersweep_prob<- ifelse(LI$Rtn_sweep_type=="undersweep", 1, 0)
# LI=subset(LI,LI$fix_num==2)
# Lineinit=rbind(Lineinit,LI)
# rm(LI)
# ##############################
# # Line init following accurate or undersweep return sweep
# 
# Acc_RS_line_init=subset(RS,RS$Rtn_sweep_type=="accurate")
# Und_RS_line_init=subset(RS,RS$Rtn_sweep_type=="undersweep")
# 
# #######################
# # Line final 
# 
# Line_final=NULL
# Line_final$sub=RS$sub
# Line_final$item=RS$item
# Line_final$seq=RS$seq
# Line_final$fix_num=RS$fix_num-1
# Line_final=as.data.frame(Line_final)
# Line_final=merge(raw_fix,Line_final)
# # This doesn't give us the amount we should have...
# # We are missing Line final fixations on the last line
# # Add columns so that these match later on for duplicate removal
# Line_final$launchSite<- Line_final$prev_max_char_line- Line_final$prevChar
# Line_final$landStart<- Line_final$char_line
# Line_final$undersweep_prob<- ifelse(Line_final$Rtn_sweep_type=="undersweep", 1, 0)
# 
# 
# ################ Add columns so that these match later on for duplicate removal
# # Intra-line fixations
# # Make the columns match so they can be bound
# raw_fix2=raw_fix
# raw_fix2$remove=NULL
# raw_fix2$launchSite<- raw_fix2$prev_max_char_line- raw_fix2$prevChar
# raw_fix2$landStart<- raw_fix2$char_line
# raw_fix2$undersweep_prob<- ifelse(raw_fix2$Rtn_sweep_type=="undersweep", 1, 0)
# 
# ##rbind the columns and then remove all ones that are the same in line Final 
# Intra_line=rbind(raw_fix2,Line_final) 
# Intra_line=Intra_line[!duplicated(Intra_line,fromLast = FALSE)&!duplicated(Intra_line,fromLast = TRUE),]
# # Bind in line initial and remove the duplicates
# Intra_line=rbind(Intra_line,Lineinit)
# Intra_line=Intra_line[!duplicated(Intra_line,fromLast = FALSE)&!duplicated(Intra_line,fromLast = TRUE),]
# 
# #Mark Fix types
# 
# Und_RS_line_init$Fix_type=c("Undersweep_init")
# Acc_RS_line_init$Fix_type=c("Accurate_init")
# Line_final$Fix_type=c("Line_Final")
# Intra_line$Fix_type=c("Intra_line")
# 
# #Merge back into full df for some reason 
# 
# All_fix=rbind(Und_RS_line_init,Acc_RS_line_init)
# All_fix2=rbind(Line_final,Intra_line)
# All_fix=rbind(All_fix,All_fix2)
# rm(All_fix2)
# All_fix$Fix_type=as.factor(All_fix$Fix_type)
# # Make new RS data frame so differences can be checked there more easily
# NRS=subset(All_fix, All_fix$Rtn_sweep==1)
# NRS2=subset(NRS,NRS$Fix_type=="Accurate_init")
# NRS=subset(NRS,NRS$Fix_type=="Undersweep_init")
# NRS=rbind(NRS,NRS2)
# rm(NRS2)


##################### Check for Age effects within fixation groups ###########################
#fix type and age
contrasts(RS$Age)<- c(1, -1)
raw_fix$Fix_type<- as.factor(raw_fix$Fix_type)
raw_fix$Fix_type<- factor(raw_fix$Fix_type, levels= c('intra-line', 'accurate', 'undersweep', 'line-final'))
contrasts(raw_fix$Fix_type)





################################# Fixation Duration 
summary(allfixtypelm<- lmer(log(fix_dur)~  Age * Fix_type+ (1|item)+ (1|sub), data= raw_fix))

allfixtypelm

ef7=effect("Age:Fix_type", allfixtypelm)
summary(ef7)
plot(ef7)

## Simulate
#####################################################################################################################
# Remember Simr can't yet look at interactions  in the same run yet, so we need to look at them seperately
#####################################################################################################################

#Sim0=powerSim(allfixtypelm,test=fcompare(log(fix_dur)~  Age * Fix_type), nsim=20)



#powerSim(allfixtypelm, test=fixed("Agey"),nsim=10)
#doTest(allfixtypelm,test= fcompare (~Age+Fix_type), nsim=10)
#powerSim(allfixtypelm,test= fcompare (log(fix_dur)~  Age * Fix_type), nsim=10 )



#model6=extend(allfixtypelm,along="sub", n=80)

#PC6=powerCurve(model6, along = "sub", breaks = c(56,72),test = fixed("Age","z"),nsim=10,
               #sim = model6, seed=10)

#plot(PC6)
#chk<-lastResult()
#chk$errors
#chk$warnings

 
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

PC7=powerCurve(model7, along = "sub", breaks = c(16,72),test = fixed("Age"),nsim=50,
               sim = model7, seed=10)

plot(PC7)
chk<-lastResult()
chk$errors
chk$warnings

#intra line model
summary(IntraFixlm<- lmer(log(fix_dur)~  Age + (1|item)+ (1|sub), data= IntraFix))

model8=extend(IntraFixlm,along="sub", n=80)

PC8=powerCurve(model8, along = "sub", breaks = c(56,72),test = fixed("Age"),nsim=15,
               sim = model8, seed=10)

plot(PC8)
chk<-lastResult()
chk$errors
chk$warnings

# Line final fixation model
summary(LineFinlm<- lmer(log(fix_dur)~  Age + (1|item)+ (1|sub), data= LineFinFix))

model9=extend(LineFinlm,along="sub", n=80)

PC9=powerCurve(model9, along = "sub", breaks = c(56,72),test = fixed("Age"),nsim=50,
               sim = model9, seed=10)

plot(PC9)
chk<-lastResult()
chk$errors
chk$warnings

#Accurate model
summary(AccurateFixlm<- lmer(log(fix_dur)~  Age + (1|item)+ (1|sub), data= AccurateFix))

model10=extend(AccurateFixlm,along="sub", n=80)

PC10=powerCurve(model10, along = "sub", breaks = c(56,72, 80),test = fixed("Age"),nsim=40,
               sim = model10, seed=10)

plot(PC10)
chk<-lastResult()
chk$errors
chk$warnings




#####################################################################################################################
# This script creates the files needed to run the analysis, if you have the files:
# Skips.csv, raw_fix.csv and RS.csv you don't need to run this script. Though you can of course want to if you wish. 
# 
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

##
# If you have new data that's not from the pilot you have to specify this in the loop below. 
# This would also mean you are running the actual data through the pilot script, which, while not wrong, is just silly.

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
################################################# Return Sweeps ########################################


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

#####################
# Skip Scrip
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


# Finally save the stuff we need 
write.csv(raw_fix,"raw_fix.csv")
write.csv(Skips,"Skips.csv")
write.csv(RS,"RS.csv")


# Nice
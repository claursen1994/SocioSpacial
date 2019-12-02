

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
summary(GLM1<- glmer(undersweep_prob~ Age + (1|item), data= RS, family= binomial))






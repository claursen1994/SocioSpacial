#Working Skip rates!


# Make sure EM reading is there

if('devtools' %in% rownames(installed.packages())==FALSE){
  install.packages('devtools')
  library(devtools)
}else{
  library(devtools)
}
install_github('martin-vasilev/EMreading')



#Load in takes a bit of time, and only works after raw_fix has been created
#Skip_raw_fix2 has the ages prebound so lets add in that  
Skip_Raw_fix=read.csv("Skip_Raw_fix2.csv")

library("EMreading")

Skip_Raw_fix22=wordMeasures(Skip_Raw_fix)
Skip_Raw_fix22$skip=NULL
# If Nfix 1 was 0 then the word was skipped the first time.
Skip_Raw_fix22$skip<- ifelse(Skip_Raw_fix22$nfix1=="0", 1, 0)

# If Nfix1 was 0 and Nfix2 was 1 or more then the word was fixated later 

Skip_Raw_fix22$hold<- ifelse(Skip_Raw_fix22$nfix2>"0",1,0 )
Skip_Raw_fix22$Return2SkipWord= ifelse(Skip_Raw_fix22$skip+Skip_Raw_fix22$hold=="2",1,0)
Skip_Raw_fix22$hold=NULL

Skips=merge(Skip_Raw_fix22,Skip_Raw_fix)

# Get Averages
library(reshape)
#For Skips
avgskp<- melt(Skips, id=c('sub', 'item', 'cond','Age'), 
              measure=c("skip"), na.rm=TRUE)
avgskp<- cast(avgskp, Age ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
avgskp

#For Returning to skipped word
RTNAVG<- melt(Skips, id=c('sub', 'item', 'cond','Age'), 
              measure=c("Return2SkipWord"), na.rm=TRUE)
RTNAVG<- cast(RTNAVG, Age ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
RTNAVG


# Make model
summary(GLM1<- glmer(skip~ Age
                     + (1|item)+ (1|sub), data= Skips, family= binomial))
ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)
install.packages("simr")
library("simr")


Sim666=powerSim(GLM1,nsim=20)
Sim666
# Not much power but we can take a look and see at how many re-fixations were made
RTNAVG<- melt(Skips, id=c('sub', 'item', 'cond','Age'), 
              measure=c("nfix2"), na.rm=TRUE)
RTNAVG<- cast(RTNAVG, Age ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
RTNAVG

summary(GLM2<- lmer(nfix2~ Age
                     + (1|item)+ (1|sub), data= Skips))
ef2=effect("Age", GLM2)
summary(ef2)
plot(ef2)

Sim666=powerSim(GLM2,nsim=20)
Sim666

######################
#Question analysis
######################


# Martin R. Vasilev, 2019

# Data pre-processing:
rm(list= ls())

# Manual pre-processing of asc files:

# Calvin 1-21
# Victoria- 22- 43
# Martin 44+
#############################################################
########### Library and padding##############################

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
#######################################################################################

###setwd("H:/Profile/Desktop/worb 2/SocioSpacial")



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

##########################################################################################
# Comprehension accuracy: 
###########################
data_dir= ("H:/Profile/Desktop/worb 2/SocioSpacial/SoSpaPilotASC")
data_dir=("E:/CalvinsDumb_work_Stuff/SoSpaPilotASC/Files4Quest")
if(!file.exists("data/QuestDa.Rda")){
  QuestDa<- Question(data_list = data_dir, maxtrial = 100)
  save(Quest, file= "data/QuestDa.Rda")
  write.csv2(QuestDa, "data/QuestDa.csv")
} else{
  load("data/QuestDa.Rda")
}


#########################################################################################
# Additional older people from the older version of the experiment 
data_dir= ("H:/Profile/Desktop/Burger")

if(!file.exists("data/QuestDa12.Rda")){
  QuestDa12<- Question(data_list = data_dir, maxtrial = 100)
  save(QuestDa12, file= "data/QuestDa12.Rda")
  write.csv2(QuestDa12, "data/QuestDa12.csv")
} else{
  load("data/QuestDa12.Rda")
}

QuestDa12$sub=c(QuestDa12$sub*99)
#######################
#Make some stuff to look at
############################
write.csv2(QuestDa,"data/QuestDa1.csv")
write_excel_csv(QuestDa,"data/QuestDa1.xls")

#############################################################################
# Some comprehension questions didn't come out properly so lets remove them#
#############################################################################
na.omit(QuestDa)
QuestDa=rbind(QuestDa,QuestDa12)

########################################
# Add details of age in excel file etc#
#######################################
#Load that shit back in#
#QuestDa=read.csv2("LabCodeC/QuestDa.csv")
########################################

######################################################
#Practice questions tend to be fucky so I remove them
QuestDat= filter(QuestDa, QuestDa$item<25)
QuestDa=QuestDat

############
#Average 
QuestDa=as.data.frame(QuestDa)
library(reshape)
DesQuest<- melt(QuestDa, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuestBySub<- cast(DesQuest, item ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuestBySub



#################################
#Split the Question Data
################################

Q0=split(QuestDa,QuestDa$dependnum)




##############################
#Check Comprehension Data
###############################
#Compared By Subject

CompQ=Q0$`3`
SubQuest<- melt(CompQ, id=c('sub', 'item', 'cond'), 
                  measure=c("accuracy"), na.rm=TRUE)
SubQuest<- cast(SubQuest, sub ~ variable
                  ,function(x) c(M=signif(mean(x),3)
                                 , SD= sd(x) ))

#Compared by Item

CompQ=Q0$`3`

CompmQuest<- melt(CompQ, id=c('sub', 'item', 'cond'), 
                    measure=c("accuracy"), na.rm=TRUE)
CompmQuest<- cast(CompmQuest, item ~ variable
                  ,function(x) c(M=signif(mean(x),3)
                                 , SD= sd(x) ))
plot(CompmQuest$item,CompmQuest$accuracy_M)



#Questions Without COmprehension Data
NoCompQ=rbind(Q0$`1`,Q0$`2`)
NoCompQ=melt(NoCompQ, id=c('sub', 'item', 'cond'), 
              measure=c("accuracy"), na.rm=TRUE)
NoCompQ<- cast(NoCompQ, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))
plot(NoCompQ$item,NoCompQ$accuracy_M)


# Split and play zone #
#######################

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
AmbiSoc0$State=c("AmbiSoc")
AmbiSpa0=rbind(AmbiSpaQ1,AmbiSpaQ2)
AmbiSpa0$State=c("AmbiSpa")
NambiSoc0=rbind(NamibiSocQ1,NamibiSocQ2)
NambiSoc0$State=c("NambiSoc")
NambiSpa0=rbind(NambiSpaQ1,NambiSpaQ2)
NambiSpa0$State=c("NambiSpa")

AmbiGen=rbind(AmbiSoc0,AmbiSpa0)
NambiGen=rbind(NambiSoc0,NambiSpa0)

SpaGen=rbind(AmbiSpa0,NambiSpa0)
SocGen=rbind(AmbiSoc0,NambiSoc0)

#############################
# General Q1 vs Q2 by cond ##
#############################

Q1$cond = factor(Q1$cond)
U0=ggplot(Q1, aes(seq, accuracy, colour = cond, fill = cond)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)

Q2$cond = factor(Q2$cond)
U1=ggplot(Q2, aes(seq, accuracy, colour = cond, fill = cond)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U1)

Qi=rbind(Q1,Q2)

U2=ggplot(Qi, aes(seq, accuracy, colour = cond, fill = cond)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U2)
############################################################################################
# By Item Zone

###############################################
# General Ambiguous Social Vs Amiguous Spatial#

GASAS=rbind(AmbiSoc0,AmbiSpa0)

GASAS$State = factor(GASAS$State)
G1=ggplot(GASAS, aes(seq, accuracy, colour = State, fill = State)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G1)

#########################################
# General NonAmbiguous Social Vs Spatial#
GNSAS=rbind(NambiSoc0,NambiSpa0)

GNSAS$State = factor(GNSAS$State)
G2=ggplot(GNSAS, aes(seq, accuracy, colour = State, fill = State)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G2)


########################################
# Whole AmbiVs Nambi ###################
GAVN=rbind(GNSAS,GASAS)
G3=ggplot(GAVN, aes(item, accuracy, colour = State, fill = State)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G3)


########################################
# The Same but by Trial Number      ###

G4=ggplot(GAVN, aes(seq, accuracy, colour = State, fill = State)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G4)


############################################################################
# Lets see if there's a difference between accuracy of Q1 and Q2 in general#
GAVN$dependnum=as.factor(GAVN$dependnum)
G5=ggplot(GAVN, aes(seq, accuracy, colour = dependnum, fill = dependnum)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G5)

#############################################################################
# Lets split up the Stimuli by ambiguity again 
#################################################
GAVN0=split(GAVN,GAVN$State)
GAVNSOC= rbind(GAVN0$NambiSoc,GAVN0$AmbiSoc)

########################################################################################################
#  by depend Num
########################
# Social
G6=ggplot(GAVNSOC, aes(seq, accuracy, colour = dependnum, fill = dependnum)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G6)

#########################
#Spatial 

GAVNSPA=rbind(GAVN0$NambiSpa,GAVN0$AmbiSpa)

G7=ggplot(GAVNSPA, aes(seq, accuracy, colour = dependnum, fill = dependnum)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G7)

#################################
# Ambiguous 
GAVNAMBI=rbind(GAVN0$AmbiSoc,GAVN0$AmbiSpa)

G8=ggplot(GAVNAMBI, aes(seq, accuracy, colour = dependnum, fill = dependnum)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G8)

################################
# NonAmbiguous 
GAVNNAMBI=rbind(GAVN0$NambiSoc,GAVN0$NambiSpa)

G9=ggplot(GAVNNAMBI, aes(seq, accuracy, colour = dependnum, fill = dependnum)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G9)

##############################################################################################################
# Accuracy by time taken to answer 
GAVN$duration_ms=as.factor(GAVN$duration_ms)

G10=ggplot(GAVN, aes(cond, duration_ms)) +
  geom_point() +
  geom_smooth(method = "loess", 
              method.args = list(family = "symetric"),
              se = FALSE) 
print(G10)

#############################################################################################################
# Time taken to answer by condition 
G11=ggplot(GAVN, aes(duration_ms, accuracy, colour = cond, fill = cond)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G11)

###################################################################################################

# Seperated between young and old
####################################
# Remember to look at which participants were young and whcih were old 

#YOGAVN=split(GAVN,GAVN$sub)
#YGAVN=rbind(YOGAVN$`1`,YOGAVN$`3`,YOGAVN$`4`,YOGAVN$`6`)
#OGAVN=rbind(YOGAVN$`2`,YOGAVN$`5`,YOGAVN$`99`,YOGAVN$`198`)
#YGAVN$Age=c("Young")
#OGAVN$Age=c("Old")
#GAVN=rbind(YGAVN,OGAVN)

library(readxl)
Aegis <- read_excel("H:/Profile/Desktop/Aegis.xlsx")
GAVN=merge(Aegis,GAVN)
GAVN$Age=as.factor(GAVN$Age)
############################################
# Accuracy in general
A1=ggplot(GAVN, aes(seq, accuracy, colour = Age, fill = Age)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(A1)

# Accuracy by State
A2=ggplot(GAVN, aes(seq, accuracy, colour = State, fill = State, linetype= Age )) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) #+ facet_wrap(~Age)
print(A2)
##########################################################################################
### Probability of chosing there isn't enough information 
RespGAVN1=GAVN
RespGAVN1$Resp=NULL
RespGAVN0=split(RespGAVN1,RespGAVN1$subresp)
RespGAVN2=rbind(RespGAVN0$`3`)
RespGAVN2$resp=c(1)
RespGavn0=rbind(RespGAVN0$`1`,RespGAVN0$`2`)
RespGavn0$resp=c(0)
RespGAVN1=rbind(RespGavn0,RespGAVN2)

A3=ggplot(RespGAVN1, aes(seq, resp, colour = State, fill = State, linetype= Age )) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) + facet_wrap(~Age)
print(A3)


############################################################################################
# Check Constructions

Const <- read_excel("H:/Profile/Desktop/Const.xlsx")

GAVN$construct=NULL

GAVN=merge(Const,GAVN)
SPACEGAVN=split(GAVN,GAVN$State)
SPACEGAVN=rbind(SPACEGAVN$NambiSpa,SPACEGAVN$AmbiSpa)
SPACEGAVN$construct=as.factor(SPACEGAVN$construct)


######################################
#General
A4=ggplot(SPACEGAVN, aes(seq, accuracy, linetype= construct )) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
#facet_wrap(~Age)
print(A4)

######################################
# By state
A5=ggplot(SPACEGAVN, aes(seq, accuracy,colour= State, linetype= construct )) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
#facet_wrap(~Age)
print(A5)

####################################
# General by age
A6=ggplot(SPACEGAVN, aes(seq, accuracy,colour= Age, linetype= construct )) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE )
print(A6)


############################################################################################
#Build a model on what we've learned. 
#Basic State effect on accuracy
GAVN$Age=as.factor(GAVN$Age)
GAVN$State=as.factor(GAVN$State)
GAVNG=glmer(accuracy~State+ (1 | item)+(1|sub),
            data = GAVN, family = binomial)
summary(GAVNG)
Qef=effect("State",GAVNG)
plot(Qef)
###
# Age*State
GAVN$Age=as.factor(GAVN$Age)
GAVN$State=as.factor(GAVN$State)
GAVNG1=glmer(accuracy~Age*State+ (1 | item)+(1|sub),
            data = GAVN, family = binomial)
summary(GAVNG1)
Qef1=effect("Age*State",GAVNG1)
plot(Qef1)
#
##Age Only

GAVNG2=glmer(accuracy~Age+ (1|item)+(1|sub),
             data = GAVN, family = binomial)
summary(GAVNG2)
Qef2=effect("Age",GAVNG2)
plot(Qef2)

##########
# If incorrect was there any age related reason for what was chosen?
IncGAVN=subset(GAVN,GAVN$accuracy==0)
IncGAVN$chose3=NULL
IncGAVN$chose3<- ifelse(IncGAVN$subresp=="3", 1, 0)
IncGAVNG=glmer(chose3~Age+ (1|item)+(1|sub),
             data = IncGAVN, family=binomial)
summary(IncGAVNG)
Qef3=effect("Age",IncGAVNG)
plot(Qef3)
# Younger readers slightly more likely to have chosen 3 as a "guess?"
# Is there a time difference as to weather they got it right or not? Can we tell guessing from this?
#WHen not chosing 3 what was chosen? 
Incandnotthree=subset(IncGAVN,IncGAVN$chose3==0)
preference=lmer(subresp~Age*State+ (1|item)+(1|sub),
               data = Incandnotthree)
summary(preference)
Qef3=effect("Age*State",preference)
plot(Qef3)
#############
# Without splitting by ambiguity we can't go on so let's do that so we can ask
# What mental model was prefered?

Splitconds=split(GAVN,GAVN$State)
NonAmbGAVN=rbind(Splitconds$NambiSoc,Splitconds$NambiSpa)
AmbGavn=rbind(Splitconds$AmbiSoc,Splitconds$AmbiSpa)

#In Non Ambiguous settings the incorrect answer (not 3) is based on the word order
#we can see how this effects older people
# Option 3 we will count as a guess here so we can see who prefered to guess and who prefered word order

IncNonAm=subset(NonAmbGAVN,NonAmbGAVN$accuracy==0)
IncNonAm$Guess=ifelse(IncNonAm$subresp=="3", 1, 0)

preference3=glmer(Guess~Age+ (1|item)+(1|sub),
                data = IncNonAm, family=binomial)
summary(preference3)
Qef3=effect("Age",preference3)
plot(Qef3)
# Younger people slightly more likely to guess, Older people prefer to not guess...
#What is chosen if they don't guess? WORD ORDER MODEL 
#Let's do the same but for ambiguous, This time we need to make sure we know what the word order model is
IncAm=subset(AmbGavn,AmbGavn$accuracy==0)

preference4=lmer(subresp~Age+ (1|item)+(1|sub),
                  data = IncAm)
summary(preference4)
Qef3=effect("Age",preference4)
plot(Qef3)


# Check what mental model was preffered? 
ggplot(data = IncGAVN, aes(x = Age, y = subresp, fill = State))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
# + geom_jitter()

# Attempt at simulating 
GAVN$Age=as.factor(GAVN$Age)
GAVNG=glmer(accuracy~Age*State + seq+ (1 | duration_ms),
                    data = GAVN, family = binomial)

summary(GAVNG)

A7=ggplot(GAVNG, aes(seq, accuracy, colour = State, fill = State, linetype= Age )) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) + facet_wrap(~Age)
print(A7)

#################################################
# Check some things from the baby model
plot(effect("Age*State",GAVNG))

#################################################
# Trial accuracy by item and Age 

A8=ggplot(GAVNG, aes(item, accuracy, colour = State, fill = State, linetype= Age )) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) + facet_grid(Age~dependnum)
print(A8)


################################################

A9=ggplot(RespGAVN1, aes(seq, resp, colour = State, fill = State, linetype= Age )) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) + facet_wrap(~dependnum)
print(A9)

GAVN=split(GAVN,GAVN$State)

####################################################################
# Create Average responce rates for young and old


AGAVN=split(GAVN,GAVN$Age)
OLDGAVN=AGAVN$Old
YOUNGAVN=AGAVN$Young
OLDGAVN=melt(OLDGAVN, id=c('sub', 'item', 'Age'), 
           measure=c("accuracy"), na.rm=TRUE)
OLDGAVN<- cast(OLDGAVN, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))
YOUNGAVN=melt(YOUNGAVN, id=c('sub', 'item', 'Age'), 
             measure=c("accuracy"), na.rm=TRUE)
YOUNGAVN<- cast(YOUNGAVN, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

OLDGAVN$Age=c("Old")
YOUNGAVN$Age=c("Young")

TGAVN=rbind(OLDGAVN,YOUNGAVN)
TGAVN=merge(TGAVN,GAVN,"item")

plot(MGAVN$item,MGAVN$accuracy_M)

A10=ggplot(TGAVN, aes(seq, accuracy_M, colour=State ,  shape= Age.x ,size=5 )) +
  geom_point() 

print(A10)

TGAVN$seq=TGAVN$seq/24












AMG=melt(AmbiGen, id=c('sub', 'item', 'cond'), 
             measure=c("accuracy"), na.rm=TRUE)
AMG<- cast(AMG, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

NAMG=melt(NambiGen, id=c('sub', 'item', 'cond'), 
          measure=c("accuracy"), na.rm=TRUE)
NAMG<- cast(NAMG, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

#Plots
library("ggplot2")
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
ASPA<- cast(ASPA, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

NSPA=melt(NambiSpa0, id=c('sub', 'item', 'cond'), 
          measure=c("accuracy"), na.rm=TRUE)
NSPA<- cast(NSPA, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))

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


















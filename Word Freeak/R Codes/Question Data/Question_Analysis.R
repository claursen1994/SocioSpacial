########################################################################
# Analysis of Social and Spatial Questions 
rm(list= ls())

# Load in Required packages. 
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



# Set Directory


getwd()
setwd("H:/Profile/Desktop/worb 2/SocioSpacial/Word Freeak/R Codes/Question Data")


# Load in pilot data.
#library(readr)
#QuestDa <- read_csv2("QuestDa.csv")
#View(QuestDa)

data_dir=("E:/CalvinsDumb_work_Stuff/SoSpaPilotASC/Files4Quest")
if(!file.exists("data/QuestDa.Rda")){
  QuestDa<- Question(data_list = data_dir, maxtrial = 100)
  save(QuestDa, file= "data/QuestDa.Rda")
  write.csv2(QuestDa, "data/QuestDa.csv")
} else{
  load("data/QuestDa.Rda")
}
na.omit(QuestDa)
######################################################
#Practice questions tend to be fucky so I remove them
Prac=NULL
Prac$item=c(25:26)
QuestDa <- QuestDa[!(QuestDa$item %in% Prac$item),]

library(readxl)
SocialWordO <- read_excel("SocialWordO.xlsx")

SpatialWordO <- read_excel("SpatialWordO.xlsx")

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

SubQuest


#Questions Without COmprehension Data
NoCompQ=rbind(Q0$`1`,Q0$`2`)
NoCompQ=melt(NoCompQ, id=c('sub', 'item', 'cond'), 
             measure=c("accuracy"), na.rm=TRUE)
NoCompQ<- cast(NoCompQ, item ~ variable,function(x) c(M=signif(mean(x),3), SD= sd(x) ))
plot(NoCompQ$item,NoCompQ$accuracy_M)

library(readxl)

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

#Ambig
GASAS=rbind(AmbiSoc0,AmbiSpa0)
#Nambig
GNSAS=rbind(NambiSoc0,NambiSpa0)
#General Ambiguous and non ambiguous 
GAVN=rbind(GNSAS,GASAS)
# Add in Ages 
library(readxl)
Aegis <- read_excel("Aegis.xlsx")
GAVN=merge(Aegis,GAVN)
GAVN$Age=as.factor(GAVN$Age)
GAVN0=split(GAVN,GAVN$State)
GAVNSOC= rbind(GAVN0$NambiSoc,GAVN0$AmbiSoc)
GAVNSPA=rbind(GAVN0$NambiSpa,GAVN0$AmbiSpa)
GAVNAMBI=rbind(GAVN0$AmbiSoc,GAVN0$AmbiSpa)
GAVNNAMBI=rbind(GAVN0$NambiSoc,GAVN0$NambiSpa)
#Add word order models
WordOrds <- read_excel("WordOrds.xlsx")
GAVN=merge(GAVN,WordOrds)
GAVN$WOSel= ifelse(GAVN$subresp==GAVN$`Which Ans is WO`,1,0)
#
#Add constructs
Const <- read_excel("H:/Profile/Desktop/Const.xlsx")
GAVN=merge(GAVN,Const)

GAVN$State=as.factor(GAVN$State)
contrasts(GAVN$State)=contr.treatment(4)
contrasts(GAVN$Age)=contr.treatment(2)
# Investigative LMMs

# General non comp questions.
summary( GLM1<- glmer(accuracy~ Age*State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age*State", GLM1)
summary(ef1)
plot(ef1)

# Social Ambi v Nambi

summary( GLM1<- glmer(accuracy~ Age*State + (1|item)+(1|sub), data= GAVNSOC, family=binomial))
ef1=effect("Age:State", GLM1)
summary(ef1)
plot(ef1)

# Spatial Ambi v Nambi
summary( GLM1<- glmer(accuracy~ Age*State + (1|item)+(1|sub), data= GAVNSPA, family=binomial))
ef1=effect("Age:State", GLM1)
summary(ef1)
plot(ef1)

# Ambi Soc vs Ambi Spa

summary( GLM1<- glmer(accuracy~ Age*State + (1|item)+(1|sub), data= GAVNAMBI, family=binomial))
ef1=effect("Age:State", GLM1)
summary(ef1)
plot(ef1)

# Nambi Soc Vs Nambi Spa 

summary( GLM1<- glmer(accuracy~ Age*State + (1|item)+(1|sub), data= GAVNNAMBI, family=binomial))
ef1=effect("Age:State", GLM1)
summary(ef1)
plot(ef1)

GAVN$dif=GAVN$`Is Dif?`
GAVN$`Is Dif?`=NULL
# Is the likelihood to select word order dependent on age?

summary( WO<- glmer(WOSel~ Age + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age", WO)
summary(ef1)
plot(ef1)

#Makes sense for there to be no difference as the word order is often the right answer, But what
#About the Ambiguous situations

GAVN$WOSel=as.factor(GAVN$WOSel)
IncGAVN=subset(GAVN,GAVN$accuracy==0)
AmbiGAVN=subset(GAVN,GAVN$correctresp==3)
NambiGAVN=subset(GAVN,GAVN$correctresp!= 3)

#Word order selection likelihood
#Items
summary( WO<- glmer(WOSel~ Age*item + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:item", WO)
summary(ef1)
plot(ef1)


#Seqences
summary( WO<- glmer(WOSel~ Age*State*seq + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:State:seq", WO)
summary(ef1)
plot(ef1)
# Compare to Accuracy
summary( WO<- glmer(accuracy~ Age*State*seq + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:State:seq", WO)
summary(ef1)
plot(ef1)


## Check with constructions Involved
#Split of social as this has no construct
GAVNSPA=subset(GAVN,GAVN$State!="AmbiSoc")
GAVNSPA=subset(GAVNSPA,GAVNSPA$State!="NambiSoc")

summary( WOC<- glmer(accuracy~ Age*construct*seq + (1|item)+(1|sub), data= GAVNSPA, family=binomial))
ef1=effect("Age:construct:seq", WOC)
summary(ef1)
plot(ef1)



###############################################################################################
# Not much revealed by general accuracy but what about by trial order....
#General
summary( GLM1<- glmer(accuracy~ Age+seq+State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age+seq+State", GLM1)
summary(ef1)
plot(ef1)

## Social Ambi v Nambi
summary( GLM1<- glmer(accuracy~ Age*seq*State + (1|item)+(1|sub), data= GAVNSOC, family=binomial))
ef1=effect("Age:seq:State", GLM1)
summary(ef1)
plot(ef1)

# Spatial Ambi v Nambi
summary( GLM1<- glmer(accuracy~ Age*seq*State + (1|item)+(1|sub), data= GAVNSPA, family=binomial))
ef1=effect("Age:seq:State", GLM1)
summary(ef1)
plot(ef1)

# Ambi Soc vs Ambi Spa

summary( GLM1<- glmer(accuracy~ Age*seq*State + (1|item)+(1|sub), data= GAVNAMBI, family=binomial))
ef1=effect("Age:seq:State", GLM1)
summary(ef1)
plot(ef1)

# Nambi Soc Vs Nambi Spa 

summary( GLM1<- glmer(accuracy~ Age*seq*State + (1|item)+(1|sub), data= GAVNNAMBI, family=binomial))
ef1=effect("Age:seq:State", GLM1)
summary(ef1)
plot(ef1)


# SubResp Over Seq
IncGAVN=subset(GAVN,GAVN$accuracy==0)
IncGAVN$chose3=NULL
IncGAVN$chose3<- ifelse(IncGAVN$subresp=="3", 1, 0)
GAVN$chose3<- ifelse(GAVN$subresp=="3",1,0 )

IncGAVN=subset(GAVN,GAVN$accuracy==0)
summary( GLM1<- glmer(chose3~ Age*seq + (1|item)+(1|sub), data= IncGAVN, family= binomial))
ef1=effect("Age:seq", GLM1)
summary(ef1)
plot(ef1)

CorrGAVN=subset(GAVN,GAVN$accuracy==1)
summary( GLM1<- glmer(chose3~ Age*seq + (1|item)+(1|sub), data= CorrGAVN, family= binomial))
ef1=effect("Age:seq", GLM1)
summary(ef1)
plot(ef1)

summary( GLM1<- glmer(chose3~ Age + (1|item)+(1|sub), data= GAVN, family= binomial))
ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)



# SPlit by dependnum to see the differences  
GAVN$dependnum=as.factor(GAVN$dependnum)
summary( GLM1<- glmer(accuracy~ Age*seq*dependnum + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:seq+dependnum", GLM1)
summary(ef1)
plot(ef1)

Dep1=split(GAVN,GAVN$dependnum)
Dep2=Dep1$`2`
Dep1=Dep1$`1`

#Accuracy of Q1 over trial
summary( GLM1<- glmer(accuracy~ Age*State*seq + (1|item)+(1|sub), data= Dep1, family=binomial))
ef1=effect("Age:State:seq", GLM1)
summary(ef1)
plot(ef1)

# Accuracy of Q2 over trial 
summary( GLM2<- glmer(accuracy~ Age*State*seq + (1|item)+(1|sub), data= Dep2, family=binomial))
ef2=effect("Age:State:seq", GLM2)
summary(ef2)
plot(ef2)

#Time taken to answer over trial
summary( GLM3<- lmer(duration_ms~ Age*State*seq + (1|item)+(1|sub), data= GAVN))
ef3=effect("Age:State:seq", GLM3)
summary(ef3)
plot(ef3)


# Create a list with no option 3

NO3GAVN=subset(GAVN,GAVN$chose3==0)


FF<- melt(NO3GAVN, id=c('sub', 'item', 'Age','State'), 
            measure=c("subresp"), na.rm=TRUE)
FF<- cast(FF, item ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

FF
NO3GAVN$subresp=as.factor(NO3GAVN$subresp)
NO3GAVN$resp<- ifelse(NO3GAVN$subresp=="2", 1, 0)
G9=ggplot(NO3GAVN, aes(item, resp, colour = Age, fill = State)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G9)































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




GASAS=rbind(AmbiSoc0,AmbiSpa0)

GASAS$State = factor(GASAS$State)
G1=ggplot(GASAS, aes(seq, accuracy, colour = State, fill = State)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(G1)


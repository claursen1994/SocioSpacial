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
GAVN$chose3<- ifelse(GAVN$subresp=="3",1,0 )
GAVN$dif=GAVN$`Is Dif?`
GAVN$`Is Dif?`=NULL
#
#Add constructs
Const <- read_excel("H:/Profile/Desktop/Const.xlsx")
GAVN=merge(GAVN,Const)
GAVN$State=as.factor(GAVN$State)
contrasts(GAVN$State)=contr.treatment(4)
contrasts(GAVN$Age)=contr.treatment(2)
#add in clearer paragraph orders
GAVN$Porder= ifelse(GAVN$cond==1,"Asoc-Aspa",
                    ifelse(GAVN$cond==2,"Nsoc-Nspa",
                           ifelse(GAVN$cond==3,"Asoc-Nspa",
                                  ifelse(GAVN$cond==4,"Nsoc-Aspa",
                                         ifelse(GAVN$cond==5,"Aspa-Asoc",
                                                ifelse(GAVN$cond==6,"Nspa-Nsoc",
                                                       ifelse(GAVN$cond==7,"Aspa-Nsoc",
                                                              ifelse(GAVN$cond==8,"Nspa-Asoc",0))))))))


# What came 1st?

GAVN$stPQ=as.factor( ifelse(GAVN$cond==1,"Asoc",
                  ifelse(GAVN$cond==2,"Nsoc",
                         ifelse(GAVN$cond==3,"Asoc",
                                ifelse(GAVN$cond==4,"Nsoc",
                                       ifelse(GAVN$cond==5,"Aspa",
                                              ifelse(GAVN$cond==6,"Nspa",
                                                     ifelse(GAVN$cond==7,"Aspa",
                                                            ifelse(GAVN$cond==8,"Nspa",0)))))))))

# Ambiguous selection of Non ambiguous correct order
#ONLy for use with Ambiguous situations
GAVN$corr4Nambi=ifelse(GAVN$subresp==GAVN$ANS,1,0)
GAVN$ISANSWO=as.factor(GAVN$`Is Ans WO?`)
contrasts(GAVN$ISANSWO)=contr.treatment(2)
#Delete the fragments and leftovers

rm(list=ls()[! ls() %in% c("GAVN","Aegis","Const","CompQ")])

##############################################################################################
######################################## Investigative LMMs###################################

# Accuracy of General non comp questions.
summary( GLM1<- glmer(accuracy~ Age*State+ (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:State", GLM1)
summary(ef1)
plot(ef1)

# Is the likelihood to select word order dependent on age?

summary( WO<- glmer(WOSel~ Age*State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:State", WO)
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

############################################################################################
#What about Ambiguous only?
AGAVN=subset(GAVN,GAVN$State!="NambiSoc")
AGAVN=subset(AGAVN,AGAVN$State!="NambiSpa")
#Accuracy 
summary( WO<- glmer(accuracy~ Age*State*seq + (1|item)+(1|sub), data= AGAVN, family=binomial))
ef1=effect("Age:State:seq", WO)
summary(ef1)
plot(ef1)
#Selecting the word order
summary( WO<- glmer(WOSel~ Age*State + (1|item)+(1|sub), data= AGAVN, family=binomial))
ef1=effect("Age:State", WO)
summary(ef1)
plot(ef1)
#selecting opt 3
summary( WO<- glmer(chose3~ Age*State + (1|item)+(1|sub), data= AGAVN, family=binomial))
ef1=effect("Age:State", WO)
summary(ef1)
plot(ef1)
############################################################################################
# What about non ambiguous only?
NGAVN=subset(GAVN,GAVN$State!="AmbiSoc")
NGAVN=subset(NGAVN,NGAVN$State!="AmbiSpa")
#Accuracy 
summary( WO<- glmer(accuracy~ Age*State + (1|item)+(1|sub), data= NGAVN, family=binomial))
ef1=effect("Age:State", WO)
summary(ef1)
plot(ef1)
#Selecting the word order
summary( WO<- glmer(WOSel~ Age*State + (1|item)+(1|sub), data= NGAVN, family=binomial))
ef1=effect("Age:State", WO)
summary(ef1)
plot(ef1)
#selecting opt 3
summary( WO<- glmer(chose3~ Age*State + (1|item)+(1|sub), data= NGAVN, family=binomial))
ef1=effect("Age:State", WO)
summary(ef1)
plot(ef1)

#If word order is the answer how is accuracy changed?

summary( WO<- glmer(accuracy~ ISANSWO*Age*State + (1|item)+(1|sub), data= NGAVN, family=binomial))
ef1=effect("ISANSWO:Age:State", WO)
summary(ef1)
plot(ef1)




#More General

#Seqence accuracy
summary( WO<- glmer(WOSel~ Age*State*seq + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:State:seq", WO)
summary(ef1)
plot(ef1)
# Compare to Accuracy
summary( WO<- glmer(accuracy~ Age*State*seq + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:State:seq", WO)
summary(ef1)
plot(ef1)
# compare with selecting 3
summary( WO<- glmer(chose3~ Age*State*seq + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:State:seq", WO)
summary(ef1)
plot(ef1)

# Look at Ambiguous only

################################# Check with constructions Involved
#Split of social as this has no construct
GAVNSPA=subset(GAVN,GAVN$State!="AmbiSoc")
GAVNSPA=subset(GAVNSPA,GAVNSPA$State!="NambiSoc")

# Accuracy based on contructions 
summary( WOC<- glmer(accuracy~ Age*construct*State + (1|item)+(1|sub), data= GAVNSPA, family=binomial))
ef1=effect("Age:construct:State", WOC)
summary(ef1)
plot(ef1)

#Word order selection
summary( WOC<- glmer(WOSel~ Age*construct*State + (1|item)+(1|sub), data= GAVNSPA, family=binomial))
ef1=effect("Age:construct:State", WOC)
summary(ef1)
plot(ef1)
# Choosing there is not enough info 

summary( WOC<- glmer(chose3~ Age*construct*State + (1|item)+(1|sub), data= GAVNSPA, family=binomial))
ef1=effect("Age:construct:State", WOC)
summary(ef1)
plot(ef1)


#Remove instances where the answer was not the word order model.
NWOGAVN=subset(GAVN,GAVN$`Is Ans WO?`==0)
WOGAVN=subset(GAVN,GAVN$`Is Ans WO?`==1)
#Where the answer was not word order
summary( WOC<- glmer(accuracy~ Age*State*seq + (1|item)+(1|sub), data= NWOGAVN, family=binomial))
ef1=effect("Age:State:seq", WOC)
summary(ef1)
plot(ef1)
#Where the answer was word order
summary( WOC<- glmer(accuracy~ Age*State*seq + (1|item)+(1|sub), data= WOGAVN, family=binomial))
ef1=effect("Age:State:seq", WOC)
summary(ef1)
plot(ef1)

#remove the ones that do not have word order as an answer 
WOGAVN2=subset(GAVN,GAVN$`Which Ans is WO`!=0)
#Seqences
summary( WO<- glmer(WOSel~ Age*State*seq + (1|item)+(1|sub), data= WOGAVN2, family=binomial))
ef1=effect("Age:State:seq", WO)
summary(ef1)
plot(ef1)
# Compare to Accuracy
summary( WO<- glmer(accuracy~ Age*State*seq + (1|item)+(1|sub), data= WOGAVN2, family=binomial))
ef1=effect("Age:State:seq", WO)
summary(ef1)
plot(ef1)



###############################################################################################
# Not much revealed by general accuracy but what about by trial order....
#General
summary( GLM1<- glmer(accuracy~ Age+seq+State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age+seq+State", GLM1)
summary(ef1)
plot(ef1)
######
# Accuracy in depend num 1 vs 2 in age 
summary( GLM1<- glmer(accuracy~ Age*dependnum + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:dependnum", GLM1)
summary(ef1)
plot(ef1)




# Split by dependnum to see the differences  
#accuracy
GAVN$dependnum=as.factor(GAVN$dependnum)
summary( GLM1<- glmer(accuracy~ Age*dependnum*State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:dependnum:State", GLM1)
summary(ef1)
plot(ef1)
#Chose 3
summary( GLM1<- glmer(chose3~ Age*dependnum*State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:dependnum:State", GLM1)
summary(ef1)
plot(ef1)
#selection of word order
summary( GLM1<- glmer(WOSel~ Age*dependnum*State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:dependnum:State", GLM1)
summary(ef1)
plot(ef1)


#########################Time taken to answer
# Selecting word order
summary( GLM1<- glmer(WOSel~ Age*duration_ms*State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:duration_ms:State", GLM1)
summary(ef1)
plot(ef1)
# answering accuracy
summary( GLM1<- glmer(accuracy~ Age*duration_ms*State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:duration_ms:State", GLM1)
summary(ef1)
plot(ef1)
# choosing 3
summary( GLM1<- glmer(chose3~ Age*duration_ms*State + (1|item)+(1|sub), data= GAVN, family=binomial))
ef1=effect("Age:duration_ms:State", GLM1)
summary(ef1)
plot(ef1)
#which question took the longest?
summary( GLM1<- lmer(duration_ms~ Age + (1|item)+(1|sub), data= GAVN))
ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)

############ Conditions ###############################
#accuracy

summary( GLM1<- glmer(accuracy~ Porder*Age*dependnum + (1|item)+(1|sub), data= GAVN, family= binomial))
ef1=effect("Porder:Age:dependnum", GLM1)
summary(ef1)
plot(ef1)

# chose3 
summary( GLM1<- glmer(chose3~ Porder*Age*dependnum + (1|item)+(1|sub), data= GAVN, family= binomial))
ef1=effect("Porder:Age:dependnum", GLM1)
summary(ef1)
plot(ef1)

# Word worder selection
summary( GLM1<- glmer(WOSel~ Porder*Age*dependnum + (1|item)+(1|sub), data= GAVN, family= binomial))
ef1=effect("Porder:Age:dependnum", GLM1)
summary(ef1)
plot(ef1)

# TO understand fully whats going on we need to understand the different tates individually.

AmbiSoc=subset(GAVN,GAVN$State=="AmbiSoc")
NambiSoc=subset(GAVN,GAVN$State=="NambiSoc")
AmbiSpa=subset(GAVN,GAVN$State=="AmbiSpa")
NambiSpa=subset(GAVN,GAVN$State=="NambiSpa")

# This is the interesting one
#AmbiSoc
# does question behaviour change?
summary( GLM1<- glmer(accuracy ~ seq*Age*dependnum + (1|item)+(1|sub), data= AmbiSoc, family= binomial))
ef1=effect("seq:Age:dependnum", GLM1)
summary(ef1)
plot(ef1)


#chose 3
summary( GLM1<- glmer(chose3 ~ seq*Age*dependnum + (1|item)+(1|sub), data= AmbiSoc, family= binomial))
ef1=effect("seq:Age:dependnum", GLM1)
summary(ef1)
plot(ef1)

# WOSel

summary( GLM1<- glmer(WOSel ~ seq*Age*dependnum + (1|item)+(1|sub), data= AmbiSoc, family= binomial))
ef1=effect("seq:Age:dependnum", GLM1)
summary(ef1)
plot(ef1)


# We need to Split by Age

oGAVN=subset(GAVN,GAVN$Age=="o")
yGAVN=subset(GAVN,GAVN$Age=="y")

U0=ggplot(oGAVN, aes(seq, accuracy, colour = Porder, linetype = dependnum)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)

U0=ggplot(yGAVN, aes(seq, accuracy, colour = Porder, linetype = dependnum)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)


# Does Spatial or social inhibit accuracy? if put 1st?

GAVN2=subset(GAVN,GAVN$dependnum==2)
GAVN0=split(GAVN2,GAVN2$State)
ASOC=GAVN0$AmbiSoc
NSOC=GAVN0$NambiSoc
ASPA=GAVN0$AmbiSpa
NSPA=GAVN0$NambiSpa


GAVN1=subset(GAVN,GAVN$dependnum==1)
GAVN1$stPQ=c("x1st")
GAVN9=split(GAVN1,GAVN1$State)
ASOC1=GAVN9$AmbiSoc
NSOC1=GAVN9$NambiSoc
ASPA1=GAVN9$AmbiSpa
NSPA1=GAVN9$NambiSpa

GAVN=rbind(GAVN1,GAVN2)
GAVN8=split(GAVN,GAVN$State)
ASOC5=GAVN8$AmbiSoc
NSOC5=GAVN8$NambiSoc
ASPA5=GAVN8$AmbiSpa
NSPA5=GAVN8$NambiSpa
#ASOC
#accuracy
summary( GLM1<- glmer(accuracy ~ stPQ*Age*seq + (1|item)+(1|sub), data= ASOC, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

# WOSel
summary( GLM1<- glmer(WOSel ~ stPQ*Age*seq + (1|item)+(1|sub), data= ASOC, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

# chose3
summary( GLM1<- glmer(chose3 ~ stPQ*Age*seq + (1|item)+(1|sub), data= ASOC, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)


#ASPA
#accuracy
summary( GLM1<- glmer(accuracy ~ stPQ*Age*seq + (1|item)+(1|sub), data= ASPA, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

# WOSel
summary( GLM1<- glmer(WOSel ~ stPQ*Age*seq + (1|item)+(1|sub), data= ASPA, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

# chose3
summary( GLM1<- glmer(chose3 ~ stPQ*Age*seq + (1|item)+(1|sub), data= ASPA, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)


#NSOC
#accuracy
summary( GLM1<- glmer(accuracy ~ stPQ*Age*seq + (1|item)+(1|sub), data= NSOC, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

# WOSel
summary( GLM1<- glmer(WOSel ~ stPQ*Age*seq + (1|item)+(1|sub), data= NSOC, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

# chose3
summary( GLM1<- glmer(chose3 ~ stPQ*Age*seq + (1|item)+(1|sub), data= NSOC, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)


#NSPA
#accuracy
summary( GLM1<- glmer(accuracy ~ stPQ*Age*seq + (1|item)+(1|sub), data= NSPA, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

# WOSel
summary( GLM1<- glmer(WOSel ~ stPQ*Age*seq + (1|item)+(1|sub), data= NSPA, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

# chose3
summary( GLM1<- glmer(chose3 ~ stPQ*Age*seq + (1|item)+(1|sub), data= NSPA, family= binomial))
ef1=effect("stPQ:Age:seq", GLM1)
summary(ef1)
plot(ef1)

#Average 





#ASOC1 Graph
U0=ggplot(ASOC1, aes(seq, accuracy, colour = Age, linetype = stPQ)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)

#ASOC 2 
U0=ggplot(ASOC, aes(seq, accuracy, colour = Age, linetype = stPQ)) +
geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)



# ASPA 1 Graph
U0=ggplot(ASPA1, aes(seq, accuracy, colour = Age, linetype = stPQ)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)
# ASPA 2 Graph
U0=ggplot(ASPA, aes(seq, accuracy, colour = Age, linetype = stPQ)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)


# NSPA 1 Graph
U0=ggplot(NSPA1, aes(seq, accuracy, colour = Age, linetype = stPQ)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)
# NSPA 2 Graph
U0=ggplot(NSPA, aes(seq, accuracy, colour = Age, linetype = stPQ)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)

# NSOC 1 Graph
U0=ggplot(NSOC1, aes(seq, accuracy, colour = Age, linetype = stPQ)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)
# NSOC 2 Graph
U0=ggplot(NSOC, aes(seq, accuracy, colour = Age, linetype = stPQ)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 
print(U0)

#ASOC again THIS ONE!!!
summary( GLM1<- glmer(accuracy ~ stPQ*Age + (1|item)+(1|sub), data= ASOC5, family= binomial))
ef1=effect("stPQ:Age", GLM1)
summary(ef1)
plot(ef1)

#NSOC
summary( GLM1<- glmer(accuracy ~ stPQ*Age + (1|item)+(1|sub), data= NSOC5, family= binomial))
ef1=effect("stPQ:Age", GLM1)
summary(ef1)
plot(ef1)

#ASPA
summary( GLM1<- glmer(accuracy ~ stPQ*Age + (1|item)+(1|sub), data= ASPA5, family= binomial))
ef1=effect("stPQ:Age", GLM1)
summary(ef1)
plot(ef1)



#NSPA
summary( GLM1<- glmer(accuracy ~ stPQ*Age + (1|item)+(1|sub)+(1|dif), data= NSPA5, family= binomial))
ef1=effect("stPQ:Age", GLM1)
summary(ef1)
plot(ef1)
GAVN$dependnum=as.factor(GAVN$dependnum)
summary( GLM1<- glmer(chose3 ~ Age*seq*State + (1|item)+(1|sub), data= GAVN, family= binomial))
ef1=effect("Age*seq*State", GLM1)
summary(ef1)
plot(ef1)

# Q2 Accuracy
summary( GLM1<- glmer(WOSel ~ Age*State*seq + (1|item)+(1|sub)+(1|dif), data= GAVN1, family= binomial))
ef1=effect("Age*State:seq", GLM1)
summary(ef1)
plot(ef1)








#### Proportional differences in model preferences.In Ambig SOCIAL
install.packages("dplyr")
library("dplyr")

GASOC=subset(rbind(ASOC,ASOC1),accuracy==0)
GASPA=subset(rbind(ASPA,ASPA1),accuracy==0)

GASOC$item=as.factor(GASOC$item)
GASPA$item=as.factor(GASPA$item)

#Social
chrisPlotDat = GASOC %>%
  group_by(item, Age, subresp) %>%
  summarise(count = n())

prop = c()

for(i in 1:nrow(chrisPlotDat)){
  
  tmpDat = chrisPlotDat[i,]
  partID = tmpDat$item
  
  allSweeps = sum(chrisPlotDat$count[chrisPlotDat$item == partID])
  
  propTMP = tmpDat$count/allSweeps
  
  prop = rbind(prop, propTMP)
  
  
}

chrisPlotDat$prop = prop

ggplot(data = chrisPlotDat, aes(x = item, y = prop, fill = Age))+
  geom_bar(stat = "summary", fun.y = "median", position = "dodge")+
  geom_violin()




chrisModel = lmer(prop ~ Age*item  + (1|item), data = chrisPlotDat)
summary(chrisModel)

ef1=effect("Age:item", chrisModel)
summary(ef1)
plot(ef1)


#Spatial

#
chrisPlotDat = GASPA %>%
  group_by(item, Age, subresp) %>%
  summarise(count = n())

prop = c()

for(i in 1:nrow(chrisPlotDat)){
  
  tmpDat = chrisPlotDat[i,]
  partID = tmpDat$item
  
  allSweeps = sum(chrisPlotDat$count[chrisPlotDat$item == partID])
  
  propTMP = tmpDat$count/allSweeps
  
  prop = rbind(prop, propTMP)
  
  
}

chrisPlotDat$prop = prop

ggplot(data = chrisPlotDat, aes(x = item, y = prop, fill = Age))+
  geom_bar(stat = "summary", fun.y = "median", position = "dodge")+
  geom_violin()




chrisModel = lmer(prop ~ Age*item  + (1|item), data = chrisPlotDat)
summary(chrisModel)

ef1=effect("Age:item", chrisModel)
summary(ef1)
plot(ef1)

###########################################################################################
#             Word order model preferences



#Social
chrisPlotDat = GASOC %>%
  group_by(item, Age, WOSel) %>%
  summarise(count = n())

prop = c()

for(i in 1:nrow(chrisPlotDat)){
  
  tmpDat = chrisPlotDat[i,]
  partID = tmpDat$item
  
  allSweeps = sum(chrisPlotDat$count[chrisPlotDat$item == partID])
  
  propTMP = tmpDat$count/allSweeps
  
  prop = rbind(prop, propTMP)
  
  
}

chrisPlotDat$prop = prop

ggplot(data = chrisPlotDat, aes(x = item, y = prop, fill = Age))+
  geom_bar(stat = "summary", fun.y = "median", position = "dodge")+
  geom_violin()




chrisModel = lmer(prop ~ Age  + (1|item), data = chrisPlotDat)
summary(chrisModel)

ef1=effect("Age", chrisModel)
summary(ef1)
plot(ef1)


#Spatial

#Social
chrisPlotDat = GASPA %>%
  group_by(item, Age, WOSel) %>%
  summarise(count = n())

prop = c()

for(i in 1:nrow(chrisPlotDat)){
  
  tmpDat = chrisPlotDat[i,]
  partID = tmpDat$item
  
  allSweeps = sum(chrisPlotDat$count[chrisPlotDat$item == partID])
  
  propTMP = tmpDat$count/allSweeps
  
  prop = rbind(prop, propTMP)
  
  
}

chrisPlotDat$prop = prop

ggplot(data = chrisPlotDat, aes(x = item, y = prop, fill = Age))+
  geom_bar(stat = "summary", fun.y = "median", position = "dodge")+
  geom_violin()




chrisModel = lmer(prop ~ Age  + (1|item), data = chrisPlotDat)
summary(chrisModel)

ef1=effect("Age", chrisModel)
summary(ef1)
plot(ef1)







#Proportionality has too much variations. 

GASOC$subrespTTT=ifelse(GASOC$subresp==1,0,1)

ggplot(data = GASOC, aes(x = item, y = subrespTTT, fill = Age))+
  geom_bar(stat = "summary", fun.y = "median", position = "dodge")

U0=ggplot(GASOC, aes(item, subrespTTT, colour = Age)) +
  geom_bar()

print(U0)





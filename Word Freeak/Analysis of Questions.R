######################################################
#Descriptive Statistics of Spatial and Social Questions#
##########################################################################################
#Notes:                                                                                  #
#~Soc stands for Social                                                                  #
#~Spat,Space and maybe some other things stand for Spacial                               #
#~Ambi stands for Ambiguous                                                              #
#~Nambi Stands for Non-Ambiguous                                                         #
#Trickle down changes should be made to the excel files "AllStim","AllstimSoc"and        #
#AllstimSpace or whatever you wanna call them.                                          #
##########################################################################################
#Sorted out code#
#
rm(list= ls())
getwd()

#Packages#
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
#Data
########

AllQuest <- read_excel("Stimuli/Questions.xlsx")





#Clean the questions and pull out the descriptives 
AllQuest$SoCleanQ=NULL
AllQuest$SpCleanQ=NULL


AllQuest$SoCleanQ=tolower(AllQuest$`Social Question`)
AllQuest$SpCleanQ=tolower(AllQuest$`Spatial Question`)

AllQuest$SoCleanQ=removePunctuation(AllQuest$`Social Question`)
AllQuest$SpCleanQ=removePunctuation(AllQuest$`Spatial Question`)

AllQuest$SocQNW=sapply(AllQuest$SoCleanQ, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
AllQuest$SpaQNW=sapply(AllQuest$SpCleanQ, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

#txt file creation for aditional analysis 
###################################################
#Clean Txt file creation for frequency analysis etc
###################################################
#Remember to make this folder!!!!!!!!!!!
#######################
for (i in AllQuest$Item) {
  write(AllQuest$SoCleanQ[i], paste0("Word Freeak/TextFiles/CleanSocQuest/CleanSocQuest",
                                     AllQuest$Item[i], ".txt", sep="" ))}

#Spatial
for(i in AllQuest$Item){
  write(AllQuest$SpCleanQ[i], paste0("Word Freeak/TextFiles/CleanSpaQuest/CleanSpaQuest",
                                     AllQuest$Item[i], ".txt", sep="" ))}

####################################################
#Dirty Txt file creation, needed for Flesch.Kincaid
####################################################
#SOcial
#######################


for (i in AllQuest$Item) {
  write(AllQuest$`Social Question`[i], paste0("Word Freeak/TextFiles/SocQuest/SocQuest",
                                     AllQuest$Item[i], ".txt", sep="" ))}
###########
#Spatial
###########
for (i in AllQuest$Item) {
  write(AllQuest$`Spatial Question`[i], paste0("Word Freeak/TextFiles/SpaQuest/SpaQuest",
                                           AllQuest$Item[i], ".txt", sep="" ))}

#Get Num Fuc 4 file sorting#
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}


################
# Flesch-Kincaid
################

###############



SocQkinc=textstat_readability(AllQuest$`Social Question`,measure="Flesch.Kincaid")

AllQuest$SocQKinc<-Sockinc$Flesch.Kincaid

SpaQkinc=textstat_readability(AllQuest$`Spatial Question`,measure="Flesch.Kincaid")

AllQuest$SpaKinc<-SpaQkinc$Flesch.Kincaid


#####################
#T Tests for qustions#

NWQTT=t.test(AllQuest$SocQNW,AllQuest$SpaQNW)
FKQTT=t.test(AllQuest$SocQKinc,AllQuest$SpaKinc)


#Mini Means
FCSoc=mean(AllQuest$SocQKinc)
FCSpa=mean(AllQuest$SpaKinc)

NWSocM=mean(AllQuest$SocQNW)
NWSpaM=mean(AllQuest$SpaQNW)
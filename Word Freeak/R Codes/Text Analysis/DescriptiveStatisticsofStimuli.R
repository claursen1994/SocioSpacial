######################################################
#Descriptive Statistics of Spatial and Social Stimuli#
##########################################################################################
#Notes:                                                                                  #
#~Soc stands for Social                                                                  #
#~Spat,Space and maybe some other things stand for Spacial                               #
#~Ambi stands for Ambiguous                                                              #
#~Nambi Stands for Non-Ambiguous                                                         #
#Trickle down changes should be made to the excel file 'MDMK'    
# To Change the Simuli in their CSV Files for Matlab Run The Descriptive Statistics of Stimuli 1st,
#Followed by Analysis of Statistics, and then Matlab Stimuli Maker
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
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("EMreading")
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
#Data
########
# Note: MDMK is a huge excel file that Contains a great deal of excel formulas which makes it easier to
# make trickle down changes and makes it easy to edit stimuli that are then read and analyised
# these changes should be applied into columns W,X,Y,Z for the Stimuli, changes to the questions are 
# made in AC, AH and AM. These columns don't have names making it harder to accidentally change them.

#For older version of allstim use  read_excel("Stimuli/AllStim.xlsx"

MDMK= read_excel("Stimuli/MDMK.xlsx")

AllStim=NULL
AllStim$Item=c(1:52)
AllStim$Ambi=MDMK$AllStimAmbi
AllStim$Spat=MDMK$AllStimSpat
AllStim$Soc=MDMK$AllStimSoc
AllStim=as.data.frame(AllStim)

AllStim=na.omit(AllStim)
write_csv2(AllStim,"Stimuli/Allstim.csv")


  
write.csv(Allstim,"Stimuli/Allstim.csv")


AllStimSoc=NULL
AllStimSoc$Item=c(1:52)
AllStimSoc$Ambi=AllStim$Ambi
AllStimSoc$Soc=AllStim$Soc
AllStimSoc=as.data.frame(AllStimSoc)
write_csv2(AllStimSoc,"Stimuli/AllStimSoc.csv")

AllStimSpace=NULL
AllStimSpace$Item=c(1:52)
AllStimSpace$Ambi=AllStim$Ambi
AllStimSpace$Spat=AllStim$Spat
AllStimSpace=as.data.frame(AllStimSpace)
write_csv2(AllStimSpace,"Stimuli/AllStimSpace.csv")


#Master Table 
MasterTable <- read_excel("Output Table/MasterTable.xlsx")

###############################
#Text Cleaning and Descriptives
###############################
#Social
#######
wbsoc=AllStimSoc
wbsoc$word_clean<- tolower(wbsoc$Soc)
wbsoc$length<- nchar(wbsoc$Soc)
wbsoc$word_clean<- removePunctuation(wbsoc$word_clean)

wbsoc$Nword <- sapply(wbsoc$word_clean, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

########
#Spatial
########
wbspa=AllStimSpace
wbspa$word_clean<- tolower(wbspa$Spat)
wbspa$length<- nchar(wbspa$Spat)
wbspa$word_clean<- removePunctuation(wbspa$word_clean)

wbspa$Nword <- sapply(wbspa$word_clean, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

###################################################
#Clean Txt file creation for frequency analysis etc
###################################################
#SOcial
#######################
for (i in wbsoc$Item) {
  write(wbsoc$word_clean[i], paste0("Word Freeak/TextFiles/SocTxt/NewSoc/Soc",
                                    wbsoc$Item[i], ".txt", sep="" ))}

#Spatial
for(i in wbspa$Item){
  write(wbspa$word_clean[i], paste0("Word Freeak/TextFiles/SpaceTxt/NewSPace/Space",
                                    wbspa$Item[i], ".txt", sep="" ))}

####################################################
#Dirty Txt file creation, needed for Flesch.Kincaid
####################################################
#SOcial
#######################


for (i in AllStimSoc$Item) {
  write(AllStimSoc$Soc[i], paste0("Word Freeak/TextFiles/OriginalSocial/SocO",
                                  AllStimSoc$Item[i], ".txt", sep="" ))}
###########
#Spatial
###########
for(i in AllStimSoc$Item){
  write(AllStimSoc$Soc[i], paste0("Word Freeak/TextFiles/OriginalSpace/Space0",
                                  AllStimSoc$Item[i], ".txt", sep="" ))}

#Not sure what this does but it's needed
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

###############
# File Sort etc
# social:
###############

#setwd("Word Freeak/TextFiles/SocTxt")


soc<- list.files("Word Freeak/TextFiles/SocTxt/NewSoc/")
n<- get_num(soc)
soc<- soc[order(n, soc)]
soc<- paste("Word Freeak/TextFiles/SocTxt/NewSoc/", soc, sep= '')

item<- NULL
word_num<- NULL
curr_item= NULL
wordID= NULL

files<- soc

for(i in 1:length(files)){ # for each text page..
  
  text<- readLines(files[i]) # read in text
  string<- unlist(strsplit(text, " ")) # split by word
  curr_item<- get_num(files[i]) # item (file #)
  
  item<- c(item, rep(curr_item, length(string)))
  word_num<- c(word_num, 1:length(string)) # word number in each text
  wordID<- c(wordID, string)
}

soc_wb<- data.frame(item, word_num, wordID)

unique(soc_wb$item)





############################
#Zipf analysis from SUBTLEX)
############################
#Social
############################

library(readr)
lex2=read_table2("SUBTLEX-UK/SUBTLEX-UK.txt")
#lex = read_xlsx("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.xlsx")
soc_wb$Zipf<- NA
soc_wb$freq<-NA
for(i in 1:nrow(soc_wb)){
  a<- which(lex2$Spelling== soc_wb$wordID[i])
  if(length(a)>0){
    soc_wb$Zipf[i]<- lex2$`LogFreq(Zipf)`[a]
    soc_wb$freq[i]<- lex2$FreqCount[a]
  }
}

# Check how many and Omit NA values
NASoc=rbind(soc_wb$Zipf==NA,soc_wb$item,soc_wb$wordID)
soc_wbN=na.omit(soc_wb)

#Mean Zipf scores per paragraph 
Socmeans=tapply(soc_wbN$Zipf, soc_wbN$item, mean)
Socmeans=as.data.frame(Socmeans)

#Put into Master table
MasterTable$SocMeanWordFrequency=Socmeans$Socmeans

###############
# File Sort etc
# Spatial
###############


get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

space<- list.files("Word Freeak/TextFiles/Spacetxt/NewSpace")
n<- get_num(space)
space<- space[order(n, space)]
space<- paste("Word Freeak/TextFiles/Spacetxt/NewSpace/", space, sep= '')

item<- NULL
word_num<- NULL
curr_item= NULL
wordID= NULL

files<- space

for(i in 1:length(files)){ # for each text page..
  
  text<- readLines(files[i]) # read in text
  string<- unlist(strsplit(text, " ")) # split by word
  curr_item<- get_num(files[i]) # item (file #)
  
  item<- c(item, rep(curr_item, length(string)))
  word_num<- c(word_num, 1:length(string)) # word number in each text
  wordID<- c(wordID, string)
}

space_wb<- data.frame(item, word_num, wordID)

unique(space_wb$item)





############################
#Zipf analysis from SUBTLEX)
############################
#Spatial
############################
library(readr)
lex2=read_table2("SUBTLEX-UK/SUBTLEX-UK.txt")
#lex = read_xlsx("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.xlsx")
space_wb$Zipf<- NA
space_wb$freq<-NA
for(i in 1:nrow(space_wb)){
  a<- which(lex2$Spelling== space_wb$wordID[i])
  if(length(a)>0){
    space_wb$Zipf[i]<- lex2$`LogFreq(Zipf)`[a]
    space_wb$freq[i]<- lex2$FreqCount[a]
  }
}

# Check how many and Omit NA values
NASoc=rbind(space_wb$Zipf==NA,space_wb$item,space_wb$wordID)
space_wbN=na.omit(space_wb)

#Mean Zipf scores per paragraph 
Spacemeans=tapply(space_wbN$Zipf, space_wbN$item, mean)
Spacemeans=as.data.frame(Spacemeans)

#Put into Master table
MasterTable$SpaMeanWordFrequency=Spacemeans$Spacemeans


###############################
#Mean Word Length per paragraph
###############################

#######
#Social
#######
soc_wb$wordID=as.character(soc_wb$wordID)
soc_wb$MeanWordLength= nchar(soc_wb$wordID)
socMWL=tapply(soc_wb$MeanWordLength , soc_wb$item, mean)
MasterTable$SocMeanWordLength<-socMWL

########
#Spatial
########
space_wb$wordID=as.character(space_wb$wordID)
space_wb$MeanWordLength= nchar(space_wb$wordID)
spaMWL=tapply(space_wb$MeanWordLength , space_wb$item, mean)
MasterTable$SpaMeanWordLength<-spaMWL

################
# Flesch-Kincaid
################
#Social
###############

AllStimSoc=read_csv2("Stimuli/AllStimSoc.csv")
#Soc=AllStim$Soc
#Soc=as.data.frame(Soc)
Sockinc=textstat_readability(AllStimSoc$Soc,measure="Flesch.Kincaid")

MasterTable$`SocF-K`<-Sockinc$Flesch.Kincaid

################
# Flesch-Kincaid
################
#Space
################
AllStimSpace=read_csv2("Stimuli/AllStimSpace.csv")
#Spacer=AllStim$Spat
Spacekinc=textstat_readability(AllStimSpace$Spat ,measure="Flesch.Kincaid")
MasterTable$`SpaF-K`<-Spacekinc$Flesch.Kincaid

#############
#Word Count#
############
#Social

MasterTable$SocNwords <- sapply(AllStimSoc$Soc, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

#Spacial

MasterTable$SpaNwords <- sapply(AllStimSpace$Spat, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

#################
#Check#
View(MasterTable)
##################
#Save Master table 
##################

write.csv (MasterTable,"Output Table/CompletedMasterTable.csv")


#Remember to check the Master Table to see if it's all there.


##############
#Reading Ease#
#For curiosity#
##############

#FTSo=textstat_readability(AllStimSoc$Soc ,measure="Flesch")
#FTSp=textstat_readability(AllStimSpace$Spat ,measure="Flesch")

#MasterTable$ReadingeaseSoc=FTSo$Flesch
#MasterTable$ReadingeaseSpat=FTSp$Flesch
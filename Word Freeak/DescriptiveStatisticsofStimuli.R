#Sorted out code#

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

AllStim <- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Stimuli/AllStim.xlsx")
AllStimSoc <- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Stimuli/AllStimSoc.xlsx")
AllStimSpace <- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Stimuli/AllStimSpace.xlsx")

#Master Table 
MasterTable <- read_excel("~/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Output Table/MasterTable.xlsx")

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
  write(wbsoc$word_clean[i], paste0("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Word Freeak/TextFiles/SocTxt/NewSoc/Soc",
                                    wbsoc$Item[i], ".txt", sep="" ))}

#Spatial
for(i in wbspa$Item){
  write(wbspa$word_clean[i], paste0("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Word Freeak/TextFiles/SpaceTxt/NewSPace/Space",
                                    wbspa$Item[i], ".txt", sep="" ))}

####################################################
#Dirty Txt file creation, needed for Flesch.Kincaid
####################################################
#SOcial
#######################
setwd("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Word Freeak/TextFiles/")
for (i in AllStimSoc$Item) {
  write(AllStimSoc$Soc[i], paste0("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Word Freeak/TextFiles/OriginalSocial/Soc0",
                                  AllStimSoc$Item[i], ".txt", sep="" ))}
###########
#Spatial
###########
for(i in AllStimSoc$Item){
  write(AllStimSoc$Soc[i], paste0("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Word Freeak/TextFiles/OriginalSpace/Space0",
                                  AllStimSoc$Item[i], ".txt", sep="" ))}

#Not sure what this does but it's needed
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

###############
# File Sort etc
# social:
###############

setwd("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Word Freeak/TextFiles/SocTxt")
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

soc<- list.files("NewSoc")
n<- get_num(soc)
soc<- soc[order(n, soc)]
soc<- paste("NewSoc/", soc, sep= '')

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
lex2=read_table2("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/SUBTLEX-UK/SUBTLEX-UK.txt")
lex = read_xlsx("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.xlsx")
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

setwd("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Word Freeak/TextFiles/Spacetxt")
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

space<- list.files("NewSpace")
n<- get_num(space)
space<- space[order(n, space)]
space<- paste("NewSpace/", space, sep= '')

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
lex2=read_table2("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/SUBTLEX-UK/SUBTLEX-UK.txt")
lex = read_xlsx("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.xlsx")
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
setwd("H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Word Freeak/TextFiles")


Sockinc=textstat_readability(AllStimSoc$Soc,measure="Flesch.Kincaid")

MasterTable$`SocF-K`<-Sockinc$Flesch.Kincaid

################
# Flesch-Kincaid
################
#Space
################
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

write.csv (MasterTable,"H:/Profile/Desktop/SpatSoc Stimuli/SocioSpacial/Output Table/CompletedMasterTable.csv")


#Remember to check the Master Table to see if it's all there.


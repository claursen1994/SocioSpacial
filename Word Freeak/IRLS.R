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

AllStim <- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStim.xlsx")
AllStimSpace=read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStimSpace.xlsx")
AllStimSoc=read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStimSoc.xlsx")





#Text Cleaning and Descriptives
#Social
wbsoc=AllStimSoc
wbsoc$word_clean<- tolower(wbsoc$Soc)
wbsoc$length<- nchar(wbsoc$Soc)
wbsoc$word_clean<- removePunctuation(wbsoc$word_clean)

wbsoc$Nword <- sapply(wbsoc$word_clean, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

#Spatial
wbspa=AllStimSpace
wbspa$word_clean<- tolower(wbspa$Spat)
wbspa$length<- nchar(wbspa$Spat)
wbspa$word_clean<- removePunctuation(wbspa$word_clean)

wbspa$Nword <- sapply(wbspa$word_clean, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

#Txt file creation
#SOcial
for (i in wbsoc$Item) {
  write(wbsoc$word_clean[i], paste0("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SocTxt/NewSoc/Soc",
                                    wbsoc$Item[i], ".txt", sep="" ))}

#Spatial
for(i in wbspa$Item){
  write(wbspa$word_clean[i], paste0("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SpaceTxt/NewSPace/Space",
                                    wbspa$Item[i], ".txt", sep="" ))}


#Not sure what this does but it's needed
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}


#Sort etc
d<- list.files("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SocTxt/NewSoc")
n<- get_num(d)
d<- d[order(n, d)]
d<- paste("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SocTxt/NewSoc/",d, sep= '')

t<- list.files("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SpaceTxt/NewSPace")
n<- get_num(t)
t<- t[order(n, t)]
t<- paste("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SpaceTxt/NewSPace/",t, sep= '')

files<- c(d, t)

item<- NULL
line<- NULL
word<- NULL
curr_item= NULL

for(i in 1:length(files)){ # for each text page..
  text[i]<- readLines(files[i])
  
  for(j in 1:length(text)){ # for each line in text
    string<- unlist(strsplit(text[j], " "))
    word_string<- gsub("#", "", string[96])
    
    curr_item<- get_num(files[i])
    line<- c(line, j)
    word<- c(word, word_string)
    item<- c(item, curr_item)
  }
  
}
wb<- data.frame(item, line, word)



text=as.data.frame(text)


#The Hanged man ( Zipf analysis from SUBTLEX)

library(readr)
lex2=read_table2("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.txt")
lex = read_xlsx("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.xlsx")
wbsoc$Zipf<- NA
wbsoc$freq<-NA
for(i in 1:nrow(wbsoc)){
  a<- which(lex$Spelling== wbsoc$word_clean[i])
  if(length(a)>0){
    wbsoc$Zipf[i]<- lex2$`LogFreq(Zipf)`[a]
    wbsoc$freq[i]<- lex2$FreqCount[a]
  }
}




for(i in AllStimSoc$Item){ # for each text page.. 
  text[i]<- readLines(AllStimSoc$Soc[i]) 
  
}



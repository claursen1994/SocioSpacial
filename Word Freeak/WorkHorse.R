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
install.packages("koRpus")
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

SubTLex <- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/Copy of SUBTLEX-UK.xlsx")





#Stimuli List
AllStim <- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStim.xlsx")
View(AllStim)


#Same List but Diffferent Layout#
AllStim2<- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStim2.xlsx")
AllStim3<- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStim3.xlsx")
AllStimSpace=read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStimSpace.xlsx")
AllStimSoc=read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStimSoc.xlsx")


Ambispace=AllStim2$AmbiSpat
Ambispace=as.data.frame(Ambispace)
Nambispace=AllStim2$NambiSpat
Nambispace=as.data.frame(Nambispace)
Ambisoc=AllStim2$AmbiSoc
Ambisoc=as.data.frame(Ambisoc)
Nambisoc=AllStim2$NambiSoc
Nambisoc=as.data.frame(Nambisoc)


#Text File Loops for processing#

for (i in AllStimSpace$Item) {
  write(AllStimSpace$Spat[i], paste0("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/Spacetxt/Space.txt",
                                     AllStimSpace$Item[i], ".txt", sep="" ))}
for(i in AllStimSoc$Item){
  write(AllStimSoc$Soc[i], paste0("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/Soctxt/Soc.txt",
                                  AllStimSpace$Item[i], ".txt", sep="" ))}










# Kincaid readability
#Make sure the correct langauge packs are installed for the koRpus library and tokenise#


set.kRp.env("H:/Profile/Desktop/TreeTagger", lang="en")
tagged.results <- treetag("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/Soctxt/Soc.txt1.txt", treetagger="manual", lang="en", 
                          TT.options=list(path="H:/Profile/Desktop/TreeTagger", preset="en"))
flesch.kincaid("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/Soctxt/Soc.txt1.txt", force.lang="en", hyphen = null, parameters = c,asl = 0.39)



#Descriptive details#
#Word Count#
SpaceWordcount=wordcount(AllStim$Spat, sep = " ", count.function = sum)
SocialWordcount=wordcount(AllStim$Soc, sep = " ", count.function = sum)
TotalWordCount=SocialWordcount+SpaceWordcount

NonAmbiSpaceWC=wordcount(AllStim2$NambiSpat, sep = " ", count.function = sum)
AmbiSpaceWC=wordcount(AllStim2$AmbiSpat,sep = " ", count.function = sum)

NonAmbiSocWC=wordcount(AllStim2$NambiSoc, sep = " ", count.function = sum)
AmbiSocWC=wordcount(AllStim2$NambiSoc, sep = " ", count.function = sum)







AllStim <- Corpus(VectorSource(AllStim))
head(AllStim)





#Cleaning#
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
AllStim <- tm_map(AllStim, toSpace, "/")
AllStim <- tm_map(AllStim, toSpace, "@")
AllStim <- tm_map(AllStim, toSpace, "\\|")
AllStim <- tm_map(AllStim, toSpace, ".")
AllStim <- tm_map(AllStim, toSpace, ",")
AllStim <- tm_map(AllStim, removeNumbers)
AllStim <- tm_map(AllStim, removeWords, stopwords("english"))
AllStim <- tm_map(AllStim, removeWords, c("A", "The","a","the","and","for","and","was"))
AllStim <- tm_map(AllStim, removePunctuation)
AllStim <- tm_map(AllStim, stripWhitespace)
AllStim<- tm_map(AllStim, content_transformer(tolower))
AllStim=as.data.frame(AllStim)

dtm <- TermDocumentMatrix(AllStim)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#Document Matrix#
dtm <- TermDocumentMatrix(AmbiItemCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)






corpus<-Corpus(VectorSource(df$text))
corpus<-tm_map(corpus, content_transformer(tolower))
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removeWords, stopwords('english'))
#corpus<-tm_map(corpus, stemDocument, language = "english") 
corpus<-tm_map(corpus, removePunctuation)

tdm<-TermDocumentMatrix(corpus)

tdmatrix<-as.matrix(tdm)
wordfreq<-sort(rowSums(tdmatrix), decreasing = TRUE)
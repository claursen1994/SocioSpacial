library(readr) 
FD <- read_excel("H:/Profile/Desktop/SpatSoc Stimuli/AllStim.xlsx")


get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))} 
FD$Item<- get_num(FD$Item) 


# Take only the first word on a line: 
FD<- subset(FD, wordnum==2) # num 1 is empty space before line 




# get file names: 
d<- list.files("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/Soctxt") 
n<- get_num(d) 
d<- d[order(n, d)] 
d<- paste("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/Soctxt/",d, sep= '') 
 

t<- list.files("Experiment/TikTokText") 
n<- get_num(t) 
t<- t[order(n, t)] 
t<- paste("Experiment/TikTokText/",t, sep= '') 


files<- c(d, t) 


item<- NULL 
line<- NULL 
word<- NULL 
curr_item= NULL 


for(i in 1:length(files)){ # for each text page.. 
text<- readLines(files[i]) 
  
for(j in 1:length(text)){ # for each line in text 
string<- unlist(strsplit(text[j], " ")) 
word_string<- gsub("#", "", string[1]) 
 
  curr_item<- get_num(files[i]) 
line<- c(line, j) 
  word<- c(word, word_string) 
  item<- c(item, curr_item) 
   } 
   
} 


wb<- data.frame(item, line, word) 
wb$word<- as.character(wb$word) 
wb$length<- nchar(wb$word) 
wb$word_clean<- wb$word 
wb$item[161:nrow(wb)]<-wb$item[161:nrow(wb)] +25 
 wb$word_clean<- tolower(wb$word_clean) 


# remove quotation marks, etc.: 
for(i in 1:nrow(wb)){ 
if(substr(wb$word_clean[i], 1, 1)== "'"){ 
    wb$word_clean[i]<- substr(wb$word_clean[i], 2, nchar(wb$word_clean[i])) 
    } 
   
   if(is.element(substr(wb$word_clean[i], nchar(wb$word_clean[i]), nchar(wb$word_clean[i])), c("'", ",", ".", "!", ";", ":"))){ 
      wb$word_clean[i]<- substr(wb$word_clean[i], 1, nchar(wb$word_clean[i])-1) 
    } 
  if(is.element(substr(wb$word_clean[i], nchar(wb$word_clean[i]), nchar(wb$word_clean[i])), c(",", ".", "!"))){ 
       wb$word_clean[i]<- substr(wb$word_clean[i], 1, nchar(wb$word_clean[i])-1) 
     } 
   
    if(!is.element(wb$word_clean[i], c("I", "I'm"))){ 
      wb$word_clean[i]<- tolower(wb$word_clean[i]) 
   } 
 } 




 ## find lexical frequencies 
library(readr) 
lex <- read_table2("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/SUBTLEX-UK.txt") 
 wb$Zipf<- NA 
 wb$freq<-NA 
for(i in 1:nrow(wb)){ 
a<- which(lex$Spelling== wb$word_clean[i]) 
  if(length(a)>0){ 
    wb$Zipf[i]<- lex$`LogFreq(Zipf)`[a] 
    wb$freq[i]<- lex$FreqCount[a] 
   } 
 } 
  


 sprintf("%f percent of words are not entries in the database", 100*(length(which(is.na(wb$Zipf)))/ nrow(wb))) 
 

 FD$freq<- NA 
 FD$Zipf<- NA 
 FD$word_clean<- NA 
 for(i in 1:nrow(FD)){ 
  a<- which(wb$item== FD$item[i] & wb$line== (FD$line[i]+1)) 
     
  if(length(a)>0){ 
     FD$freq[i]<- wb$freq[a] 
        FD$Zipf[i]<- wb$Zipf[a] 
        FD$word_clean[i]<- wb$word_clean[a] 
     } 
  } 
 

 FD$subject<- as.factor(FD$subject) 


 FD$condition<- factor(FD$condition) 
 levels(FD$condition)<- c("Normal","Bold") 
FD$item <- factor(FD$item) 


 contrasts(FD$condition) 
 

 FD$word_len<- nchar(FD$word_clean) 
 FD$AltGaze<- as.numeric(FD$AltGaze) 
 FD$logFreq<- log(FD$freq) 
 FD$word_lenC<- scale(FD$word_len) 
 FD$TotalTime<- as.numeric(FD$TotalTime) 

AllFiles=scan("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SocTxt/Soc.txt2.txt",what="character")head(Soctxt1,100)
setwd("H:/Profile/Desktop/SpatSoc Stimuli")
Soctxt1 <- removePunctuation(Soctxt1)
Soctxt1 <- tolower(Soctxt1)
Soctxt1<-removeWords(Soctxt1,"a")

head(Soctxt1,100)

Soctxt1.table <- table(Soctxt1)
Soctxt1.table <- sort(Soctxt1.table, decreasing = TRUE)
Soctxt1.table[1:10]


a.Soctxt = data.frame(rank = c(1:length(Soctxt1.table)), freq =
                        + as.numeric(Soctxt1.table), type = names(Soctxt1.table))
a.Soctxt$logfreq <- log2(a.Soctxt$freq)
a.Soctxt$logrank <- log2(a.Soctxt$rank)

head(a.Soctxt,10)

a.Soctxt$spelling=a.Soctxt$type

for(i in 1:length(files)){ # for each text page.. 
  text<- scan(d[i], what="character")
  text= removePunctuation(text)
  text=tolower(text)
  text=removeWords("a")
  }
readlines
for(i in 1:length(files)){ # for each text page.. 
  text[i]=scan("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SocTxt/",what="character")

  for(j in 1:length(text[i])){ # for each line in text 
    string<- unlist(strsplit(text[j], " ")) 
    word_string<- gsub("#", "", string[1]) 
    
    curr_item<- get_num(files[i]) 
    line<- c(line, j) 
    word<- c(word, word_string) 
    item<- c(item, curr_item) 
  } 
  
} 
end



AllFiles=scan("H:/Profile/Desktop/SpatSoc Stimuli/Word Freeak/TextFiles/SocTxt/Soc.txt1.txt")#("Soc.txt1.txt")

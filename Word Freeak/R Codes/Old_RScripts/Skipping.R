##########################################################
# Skip rate 
rm(list= ls())
# Load in Skipcheck that has max word numbers in it needed to skip percentage 

SkipChecker=read.csv("Skips.csv")

# Raw_fix has been edited in excel to detect skips 
#SkipChecker$Stim=NULL
SkipChecker$word_IDs=NULL
#SkipChecker$Placer=NULL

## Create text files of the Stimuli
for(i in SkipChecker$Placer){
  write(SkipChecker$Stim[i], paste0("Word Freeak/TextFiles/StimTXT",
                                    SkipChecker$Stim[i], ".txt", sep="" ))}


### Load in from txt files 
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

space<- list.files("Word Freeak/TextFiles/StimTXT")
n<- get_num(space)
space<- space[order(n, space)]
space<- paste("Word Freeak/TextFiles/StimTXT/", space, sep= '')

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

Stim_wb<- data.frame(item, word_num, wordID)
Stim_wb$Placer=Stim_wb$item
Stim_wb$item=NULL
Stim_wb=merge(Stim_wb,SkipChecker)
Stim_wb$Stim=NULL
Stim_wb$X=NULL

### Item is actually placer which we need to sort by to get the conditions back

#Load in takes a bit of time
Skip_Raw_fix=read.csv("raw_fix.csv")
# Remove NA, Return sweeps and also second pass reads



Skip_Raw_fix=subset(Skip_Raw_fix,Skip_Raw_fix$Rtn_sweep!=1)

Skip_Raw_fix$Max_word_num=NULL
SkipChecker$Max_word_num=SkipChecker$NumWords
SkipChecker$NumWords=NULL
SkipChecker$X=NULL
Skip_Raw_fix2=merge(SkipChecker,Skip_Raw_fix)
Skip_Raw_fix2$Stim=NULL
Skip_Raw_fix2$Placer=NULL
Skip_Raw_fix2$X=NULL

Skip_Raw_fix2$remove<-  0
newDatas<- NULL
nsubs<- unique(Skip_Raw_fix2$sub)

for( i in 1:length(nsubs)){
  n<- subset(Skip_Raw_fix2, sub== nsubs[i])
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    nlines<- unique(m$line)
    
    for(k in 1:length(nlines)){
      o<- subset(m, line== nlines[k])
      if(nrow(o)>1){
        o$remove[2:nrow(o)]=1
        cat(sprintf('Second pass Skip_Raw_fix2: subject %i, item %i, cond %i, line %i ', o$sub[1],
                    o$item[1], o$cond[1], o$line[1]))
      }
      
      newDatas<- rbind(newDatas, o)
    }
    
  }
}

Skip_Raw_fix2<- subset(Skip_Raw_fix2, remove==0)
Skip_Raw_fix2$remove<- NULL
rm(newDatas)

# I don't know why this next bit just fixes the problem but it does,
# what problem you ask? Well if you do this there isn't one.
write.csv(Skip_Raw_fix2,"Skip_Raw_fix2.csv")
Skip_Raw_fix2=read.csv("Skip_Raw_fix2.csv")
#
#
#
TSkip_Raw_fix=as.data.frame(Skip_Raw_fix2)
#TSkip_Raw_fix <- as.data.frame(TSkip_Raw_fix[!is.na(TSkip_Raw_fix)])
# COunt the Skips

TSkip_Raw_fix$prevword=NULL
TSkip_Raw_fix$Skip=NULL 
TSkip_Raw_fix$prevword=lag(TSkip_Raw_fix$word,n=1)

# remove skipping NA value of the first line 
TSkip_Raw_fix=subset(TSkip_Raw_fix,TSkip_Raw_fix$prevword!=is.na(TSkip_Raw_fix$prevword))
TSkip_Raw_fix=subset(TSkip_Raw_fix,TSkip_Raw_fix$word!=is.na(TSkip_Raw_fix$word))
# count the skips
for(i in 1:nrow(TSkip_Raw_fix)){
  if((TSkip_Raw_fix$word[i]-TSkip_Raw_fix$prevword[i])<1.1){
    # paste into position i of vector m
    TSkip_Raw_fix$Skip[i] <- paste0(0) 
  } else {
    TSkip_Raw_fix$Skip[i] <- paste0(1)
  }
}


TSkip_Raw_fix$skip_prob<- ifelse(TSkip_Raw_fix$Skip=="1", 1, 0)


#COunt the skips and divide by total number of words. 
#Count skips by Sub and item
library(data.table)
skp3=data.table(TSkip_Raw_fix)

skp3=skp3[,.(Totalskips=sum(skip_prob)), by=.(sub,item)]

skp3=as.data.frame(skp3)
tskp=merge(TSkip_Raw_fix,skp3)

tskp$percent_skipped=tskp$Totalskips/tskp$Max_word_num*100

TSkip_Raw_fix=merge(TSkip_Raw_fix,tskp)



# add in number of characters 
TSkip_Raw_fix$wordID=as.character(TSkip_Raw_fix$wordID)
TSkip_Raw_fix$wordIDnchar=nchar(TSkip_Raw_fix$wordID)
#Add in zipf scores for further analysis 

#This can take a while
lex2=read_table2("SUBTLEX-UK/SUBTLEX-UK.txt")
#lex = read_xlsx("//bournemouth.ac.uk/data/staff/home/claursen/Profile/Desktop/SpatSoc Stimuli/SUBTLEX-UK.xlsx")
TSkip_Raw_fix$Zipf<- NA
TSkip_Raw_fix$freq<-NA
for(i in 1:nrow(TSkip_Raw_fix)){
  a<- which(lex2$Spelling== TSkip_Raw_fix$wordID[i])
  if(length(a)>0){
    TSkip_Raw_fix$Zipf[i]<- lex2$`LogFreq(Zipf)`[a]
    TSkip_Raw_fix$freq[i]<- lex2$FreqCount[a]
  }
}

# Is Zipf Responcible for skipping? 
contrasts(TSkip_Raw_fix$Age)<- c(1, -1)
summary(GLM1<- glmer(skip_prob~ Age +Zipf +  (1|item)+ (1|sub), data= TSkip_Raw_fix, family= binomial))
ef1=effect("Zipf", GLM1)
summary(ef1)
plot(ef1)
# maybe...
# Age effect without zipf
summary(GLM1<- glmer(skip_prob~ Age +  (1|item)+ (1|sub), data= TSkip_Raw_fix, family= binomial))
ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)


# Take out short words if you want to 
#TSkip_Raw_fix=subset(Skip_Raw_fix3,Skip_Raw_fix3$wordIDnchar>4)

#


write.csv(TSkip_Raw_fix,"Skip_Raw_fix.csv")
TSkip_Raw_fix=read.csv("Skip_Raw_fix.csv")
# Violin of percentage of words skipped
ggplot(data = TSkip_Raw_fix, aes(x = Age, y = percent_skipped, fill = Age))+
  geom_bar(stat = "summary", fun.y = "mean", color= "red",position = "dodge")+
  geom_violin()
#gg
GLM1P=ggplot(TSkip_Raw_fix, aes(Zipf, skip_prob, colour = Age, fill = Age)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 

print(GLM1P)


  



#Get averages
library(reshape)
avgskp<- melt(Skip_Raw_fix3, id=c('sub', 'item', 'cond','Age'), 
               measure=c("skip_prob"), na.rm=TRUE)
avgskp<- cast(avgskp, Age ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
avgskp
# Make model
summary(GLM1<- glmer(Skip~ Age
                       + (1|item)+ (1|sub), data= Skip_Raw_fix, family= binomial))
ef1=effect("Age", GLM1)
summary(ef1)
plot(ef1)
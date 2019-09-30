

# Martin R. Vasilev, 2018-2019 

# 28.02.2019- Updated version for the revision


# NOTE: make sure to open the project file (Oz.Rproj) in RStudio to make sure all directories are
# opened correctly


rm(list=ls())

# load/ install packages:
if('lme4' %in% rownames(installed.packages())==FALSE){
  install.packages('lme4'); library(lme4)} else{ library(lme4)}

if('reshape' %in% rownames(installed.packages())==FALSE){
  install.packages('reshape'); library(reshape)} else{ library(reshape)}

if('ggplot2' %in% rownames(installed.packages())==FALSE){
  install.packages('ggplot2'); library(ggplot2)} else{ library(ggplot2)}

if('MASS' %in% rownames(installed.packages())==FALSE){
  install.packages('MASS'); library(MASS)} else{ library(MASS)}

if('car' %in% rownames(installed.packages())==FALSE){
  install.packages('car'); library(car)} else{ library(car)}

if('effects' %in% rownames(installed.packages())==FALSE){
  install.packages('effects'); library(effects)} else{ library(effects)}


# Load and prepare data files:
dat <- read.csv("data/OZdata.csv", na.strings = "na")
#dat <- read.csv("OZ32fixdist.csv", na.strings = "na")


get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
dat$subject<- get_num(dat$subject)
dat$subject<- as.factor(dat$subject)

dat$condition<- factor(dat$condition)
levels(dat$condition)<- c("Normal","Bold")
dat$item <- factor(dat$item)

# remove outliers:
out<- which(dat$fixduration >= 1000)
cat(paste(round((length(out)/nrow(dat))*100, 3)), "% of fixations removed as outliers")
outliers<- dat[out,]
a<-table(outliers$FixType)

dat<- dat[-out, ]


# subset data for fixation type analysis:

# Note: In this experiment, there were some lines that are very short (i.e, even containing a single short word).
# This is a natural consequence of this type of text that was used in the study. As a result of this, there are 
# cases where such lines were fixated only once. Because in such case the fixation is both an accurate sweep and 
# a line-final fixation, we exclude them from this particular analysis. Such fixations accounted for 0.22% of all
# fixations.
table(dat$FixType)

dat1<- subset(dat, FixType!= "both")
dat1$FixType<- droplevels(dat1$FixType)
table(dat1$FixType) # good

# change contrast coding so that intra-line (i.e., typical fixation) is the baseline:
contrasts(dat1$FixType)
dat1$FixType<- factor(dat1$FixType, levels = c("intra-line", "line-final", "accurate-sweep", "under-sweep"))
contrasts(dat1$FixType)


# recalculate intra-line fixations to include only first-pass ones:

datNew<- NULL
dat1$secPass<- NA
currLine= 0

for(i in 1:length(unique(dat1$subject))){
  n<- subset(dat1, subject== i)
  
  nitems<- as.numeric(as.character(unique(n$item)))
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    currLine= 0
    
    for(k in 1:nrow(m)){
      if(m$line[k]>currLine){
        currLine= m$line[k]
      }
      if(m$line[k]< currLine){
        m$secPass[k]<- 1
      }else{
        m$secPass[k]<- 0
      }
    }
    datNew<- rbind(datNew, m)
  }
}

datNew<- subset(datNew, secPass==0)
datNew$secPass<- NULL
dat1<- datNew; rm(datNew)


# Comprehension accuracy:

# Note: questions 1-5 are from "Dorothy" and questions 6-10 are from "Tiktok".
q <- read.csv("data/OZquest.csv", na.strings = "na")
q$condition<- factor(q$condition)
levels(q$condition)<- c("Normal","Bold")
contrasts(q$condition)

q$subject<- as.factor(q$subject)
q$item<- as.factor(q$item)

DesQ<- melt(q, id=c('subject', 'item', 'condition'), 
                measure=c("accuracy") , na.rm=TRUE)

mQ<- cast(DesQ, condition ~ variable
              , function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))
# subject average accuracy:
mQ2<- cast(DesQ, subject ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

# Accuracy model:
if(!file.exists("Models/GM1.Rda")){
  # model does not converge with a random slope for items.
  GM1<- glmer(accuracy ~ condition + (condition|subject)+ (1|item), family= binomial, data=q)
  save(GM1, file= "Models/GM1.Rda")
  round(coef(summary(GM1)),2)
}else{
  load("Models/GM1.Rda")
  round(coef(summary(GM1)),2)
}


#### Fixation type:
DesType<- melt(dat1, id=c('subject', 'item', 'condition', 'FixType'), 
            measure=c('fixduration') , na.rm=TRUE)

mType<- cast(DesType, condition+FixType ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

mType$fixduration_SD<- round(mType$fixduration_SD)

# Plot:
p <- ggplot(dat1, aes(x=condition, y=fixduration, fill= condition)) + 
    geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
            outlier.size= 1, outlier.shape=8, coef= 4)+
  geom_violin(weight= 2, alpha= 0.3, scale= "count") + theme_bw(22) + 
    scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
    theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
          axis.line = element_line(colour = "black", size=1),
          panel.border = element_rect(colour = "black", size=1, fill = NA),
          legend.position="none", plot.title = element_text(hjust = 0.5))+facet_grid(.~ FixType) + 
    theme(strip.text.x = element_text(size = 20,  face="bold",family="serif"),
          strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
          legend.key = element_rect(colour = "#000000", size=1),
          plot.title = element_text(size = 20))+
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))+
  stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
    xlab("Condition")+ ylab("Fixation duration") +ggtitle("Fixation type"); p
ggsave(p, filename = "Plots/FixbyType.pdf", width = 12, height = 8)
ggsave(p, filename = "Plots/FixbyType.png", width = 12, height = 8, dpi= 300)
  

# Model:
contrasts(dat1$condition)
contrasts(dat1$FixType)
# Fixation type x Text type model:
if(!file.exists("Models/LM1.Rda")){
  LM1<- lmer(log(fixduration)~ condition * FixType + (condition|subject)+ (condition|item), data= dat1)
  save(LM1, file= "Models/LM1.Rda")
  summary(LM1)
}else{
  load("Models/LM1.Rda")
  summary(LM1)
}
round(coef(summary(LM1)),3)



#####

# Landing profile analysis:

# remove the first line from each item: there is no return sweep there because participants start reading
# with a gaze box as the start of the sentence (line 0 in the dataset).
dat2<- subset(dat, line!=0)

# Take only first fixation on each line:
dat2<- subset(dat2, FixType== "under-sweep" | FixType== "accurate-sweep" | FixType== "both")
dat2$FixType<- droplevels(dat2$FixType)
dat2$FixType<- as.character(dat2$FixType)
table(dat2$FixType)

# recode single fixations on a line occuring on short lines (see above)
# these are considered accurate sweeps since readers did not make any more fixations on the line
# and have presumably processed its contents
for(i in 1:nrow(dat2)){
  if(dat2$FixType[i]== "both"){
    dat2$FixType[i]= "accurate-sweep"
    dat2$undersweep[i]=1
  }
}

table(dat2$FixType)

# Code landing position from the start of each line:
dat2$lineStartLand<- dat2$currentX- dat2$StartLineX 

# Return sweep saccade length:
dat2$sacc_len<- abs(dat2$priorX - dat2$currentX)

# center saccade length to improve model scaling:
dat2$sacc_lenC<- scale(dat2$sacc_len)


# scale word length:
dat2$Len1C<- scale(dat2$Len1)

# Launch Site:
# here, we need to substract the empty region before the start of a line to get the launch site
# from the beginning of the text margin:
dat2$launch<- abs(dat2$priorX- dat2$StartLineX)
dat2$launchC<- scale(dat2$launch)
dat2$lineStartLandC<- scale(dat2$lineStartLand)



#### Descriptive statistics
DesSacc<- melt(dat2, id=c('subject', 'item', 'condition', 'FixType'), 
               measure=c('launch', 'lineStartLand', 'undersweep') , na.rm=TRUE)

mSacc<- cast(DesSacc, condition ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
write.csv2(mSacc, 'Sacc_descr.csv')

# REVISION:
# What is the % of overshoot fixations (i.e., landing to the left of the first letter on the line):

overshoot<- dat2[which(dat2$lineStartLand< 0),]

sprintf("Participants overshoot the line start and landed to the left of the first letter %g percent of the time",
        round((nrow(overshoot)/nrow(dat2))*100, 2))

dat2$pOvershoot<- NULL
for(i in 1:nrow(dat2)){
  
  if(dat2$lineStartLand[i]< 0){
    dat2$pOvershoot[i]<- 1
  }else{
    dat2$pOvershoot[i]<- 0
  }
  
}

DesOver<- melt(dat2, id=c('subject', 'item', 'condition'), 
               measure=c('pOvershoot') , na.rm=TRUE)

mOver<- cast(DesOver, condition ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))


# Model: return sweep launch site as a function of experimental condition:

if(!file.exists("Models/LSM.Rda")){
  LSM<- lmer(launch~ condition+ (condition|subject)+ (condition|item), REML= T,
             data= dat2)
  save(LSM, file= "Models/LSM.Rda")
}else{
  load("Models/LSM.Rda")
}
summary(LSM)


# Model: new line landing position
contrasts(dat2$condition)

if(!file.exists("Models/LM2.Rda")){
  LM2<- lmer(lineStartLand~ condition*launchC*Len1C+ (condition|subject)+ (condition|item),
             data= dat2)
  save(LM2, file= "Models/LM2.Rda")
}else{
  load("Models/LM2.Rda")
}

summary(LM2)
round(coef(summary(LM2)),3)

write.csv2(round(coef(summary(LM2)),3), "Models/LM2.csv")

coef(summary)


#################
# set up "data":

b<- coef(summary(LM2))[1:2,1]  # slope/ intercepts in model
names(b)<- c("(Intercept)",   "condition1") # change names
rnd<- VarCorr(LM2)
res<- 3.8471

sub <- c(1:60)
item<- c(1:24)
cond <- c('Normal', 'Bold')



X <- expand.grid(condition=cond, sub= sub, item= item)

X$condition<- as.factor(X$condition)
contrasts(X$condition)<- c(0, 1)

# make fake data:
model1 <- makeLmer(y ~ condition + (condition|sub) + (condition|item), fixef=b, VarCorr= rnd, sigma=res, data=X)

summary(model1)

powerSim(model1, nsim= 100)



effect('condition', LM2)

plot(effect('condition', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position")

plot(effect('launchC', LM2))

plot(effect('launchC', LM2))
plot(effect('launchC:Len1C', LM2))


#### Interaction plot:
Int<- effect('launchC:Len1C', LM2)
Int<- as.data.frame(Int)

QWL<- c(-1.8330659, -0.9770474, -0.1210288,  0.7349897,  3.7310546)
WL<- c(2, 4, 6, 8, 15)

Int$WL<- 0
Int$WL[1:5]<- WL[1]
Int$WL[6:10]<- WL[2]
Int$WL[11:15]<- WL[3]
Int$WL[16:20]<- WL[4]
Int$WL[21:25]<- WL[5]

LP<- c(0, 19, 39, 58, 77)
Int$LP<- rep(LP,5)


W1<- subset(Int, WL== 2)
W2<- subset(Int, WL== 4)
W3<- subset(Int, WL== 6)
W4<- subset(Int, WL== 8)
W5<- subset(Int, WL== 15)

##################
pdf("Plots/Inter_plot.pdf", width = 8, height = 7.5)
#png("Plots/Inter_plot.png", width = 2400, height = 2000, res = 300)
# par(mar = rep(2, 4))

plot(W1$LP, W1$fit, col= "burlywood3" , pch= 16, cex= 3, ylim= c(4,8.5), family= "serif",
     xlab= "Launch position (char.)", ylab=  "Landing position (char.)",
     cex.lab=1.5, cex.axis= 1.3)
lines(W1$LP, W1$fit, col= "burlywood3", lwd=3)
text(W1$LP, W1$fit+0.004, "2", font= 2, col= "white")

###
points(W2$LP, W2$fit, col= "darkorange" , pch= 16, cex= 3)
lines(W2$LP, W2$fit, col= "darkorange", lwd=3)
text(W2$LP, W2$fit+0.004, "4", font= 2, col= "white")

###
points(W3$LP, W3$fit, col= "darkgreen" , pch= 16, cex= 3)
lines(W3$LP, W3$fit, col= "darkgreen", lwd=3)
text(W3$LP, W3$fit+0.004, "6", font= 2, col= "white")

###
points(W4$LP, W4$fit, col= "darkblue" , pch= 16, cex= 3)
lines(W4$LP, W4$fit, col= "darkblue", lwd=3)
text(W4$LP, W4$fit+0.004, "8", font= 2, col= "white")

###
points(W5$LP, W5$fit, col= "darkorchid" , pch= 16, cex= 3)
lines(W5$LP, W5$fit, col= "darkorchid", lwd=3)
text(W5$LP, W5$fit+0.004, "15", font= 2, col= "white")

legend(45, 5.2, legend=c("2 letters (0% quantile)", "4 letters (25% quantile)", "6 letters (50% quantile)",
                         "8 letters (75% quantile)", "15 letters (100% quantile)"), lwd=3,
       col=c("burlywood3", "darkorange", "darkgreen", "darkblue", "darkorchid"), lty= rep(1, 5), cex=1,
       title= expression(bold("Word length (quantile)")), bty = "n")

dev.off()


###### Return sweep probability:
if(!file.exists("Models/GM2.Rda")){
  GM2<- glmer(undersweep ~ condition*launchC + (condition|subject)+ (1|item),
              family= binomial, data= dat2)
  save(GM2, file= "Models/GM2.Rda")
}else{
  load("Models/GM2.Rda")
}

summary(GM2)
round(coef(summary(GM2)),3)
write.csv(round(coef(summary(GM2)),3), "Models/GM2.csv")

# main effects:
plot(effect('launchC', GM2))
plot(effect('condition ', GM2))



### Word-level analysis (on first word on the line):

library(readr)
FD <- read_csv("data/OZword_data.csv")

get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
FD$subject<- get_num(FD$subject)

# Take only the first word on a line:
FD<- subset(FD, wordnum==2) # num 1 is empty space before line


# get file names:
d<- list.files("Experiment/DorothyText")
n<- get_num(d)
d<- d[order(n, d)]
d<- paste("Experiment/DorothyText/",d, sep= '')

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
wb$length<- nchar(wb$word)+1
wb$word_clean<- wb$word
wb$item[161:nrow(wb)]<-wb$item[161:nrow(wb)] +25
#wb$word_clean<- tolower(wb$word_clean)

hist(wb$length, col= 'Steelblue', xlab= 'Word length', family= 'serif',
     main = "Word length frequency", cex.lab=1.4, cex.axis= 1.2, xlim= c(2, 16), freq= T)
abline(v= quantile(wb$length), col= "darkred", lwd=2)

round((table(wb$length)/ nrow(wb))*100,2)


hist(dat2$Len1, col= 'Steelblue', xlab= 'Word length', family= 'serif',
     main = "Word length frequency", cex.lab=1.4, cex.axis= 1.2, xlim= c(2, 16))
abline(v= quantile(dat2$Len1), col= "darkred", lwd=2)

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
lex <- read_table2("data/SUBTLEX-UK.txt")
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

# Model:
library(lme4)

if(!file.exists("Models/WM1.Rda")){
  WM1<- lmer(log(AltGaze)~ condition*logFreq*word_lenC + (condition|subject)+ (condition|item),
             REML=T, data= FD)
  save(WM1, file= "Models/WM1.Rda")
}else{
  load("Models/WM1.Rda")
}
summary(WM1)
round(coef(summary(WM1)),3)
write.csv(round(coef(summary(WM1)),3), "Models/WM1.csv")

if(!file.exists("Models/WM2.Rda")){
  WM2<- lmer(log(TotalTime)~ condition*logFreq*word_lenC + (condition|subject)+ (condition|item),
             REML=T, data= FD)
  save(WM2, file= "Models/WM2.Rda")
}else{
  load("Models/WM2.Rda")
}

summary(WM2)
round(coef(summary(WM2)),3)
write.csv(round(coef(summary(WM2)),3), "Models/WM2.csv")



###############################
# reading time analysis:

load("data/Rdata.Rda")
Rdata$time_s= Rdata$time/1000 # in seconds

Rdata$cond<- factor(Rdata$cond)
levels(Rdata$cond)<- c("Normal","Bold")


# Descriptives:
library(reshape)
Des<- melt(Rdata, id=c('sub', 'item', 'cond'), 
           measure=c("time_s") , na.rm=TRUE)

m<- cast(Des, cond ~ variable
         , function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))


# statistical analysis:
library(lme4)

contrasts(Rdata$cond)

summary(R_M<- lmer(log(time_s)~ cond+ (cond|sub)+ (cond|item), REML= T, data= Rdata))
round(coef(summary(R_M)),3)



############ Plot landing position densities by subject:

# Plot:
p <- ggplot(dat2, aes(x=condition, y= lineStartLand, fill= subject)) + 
  # geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
  #              outlier.size= 1, outlier.shape=8, coef= 4)+
  geom_violin(weight= 2, alpha= 0.3, scale= "count") + 
  theme_bw(22) + 
 # scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
  theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
        axis.line = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", size=1, fill = NA),
        legend.position="none", plot.title = element_text(hjust = 0.5))+#facet_grid(.~ subject) + 
  theme(strip.text.x = element_text(size = 20,  face="bold",family="serif"),
        strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
        legend.key = element_rect(colour = "#000000", size=1),
        plot.title = element_text(size = 20), legend.position="right")+
  # scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))+
  #stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
  xlab("Condition")+ ylab("Landing position (char)") +
  ggtitle("Landing position distributions by subject"); p
ggsave(p, filename = "Plots/Land_pos_by_sub.pdf", width = 12, height = 8)


###############################################################################################################################

# Compare landing position after corrective saccade to an accurate-returnsweep saccade:

dat1$corrective<- NA

for(i in 1:nrow(dat1)){
  if(dat1$FixType[i]== "under-sweep"){
    dat1$corrective[i+1]<- "corrective"
  }
  if(dat1$FixType[i]== "accurate-sweep"){
    dat1$corrective[i]= "accurate-sweep"
  }
}

dat3<- subset(dat1, !is.na(corrective))

# Code landing position from the start of each line:
dat3$lineStartLand<- dat3$currentX- dat3$StartLineX 


Des<- melt(dat3, id=c('subject', 'item', 'condition', "corrective"), 
            measure=c("lineStartLand") , na.rm=TRUE)

mS<- cast(Des, corrective ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))


# Plot:
p <- ggplot(dat3, aes(x=corrective, y=lineStartLand, fill= corrective)) + 
  geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
               outlier.size= 1, outlier.shape=8, coef= 4)+
  geom_violin(weight= 2, alpha= 0.3, scale= "count") + theme_bw(22) + 
  scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
  theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
        axis.line = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", size=1, fill = NA),
        legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  theme(strip.text.x = element_text(size = 20,  face="bold",family="serif"),
        strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
        legend.key = element_rect(colour = "#000000", size=1),
        plot.title = element_text(size = 20))+
#  scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))+
  stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
  xlab("Saccade type")+ ylab("Landing position (char.)"); p
ggsave(p, filename = "Plots/corrective.pdf", width = 8, height = 8)
ggsave(p, filename = "Plots/corrective.png", width = 8, height = 8, dpi= 300)

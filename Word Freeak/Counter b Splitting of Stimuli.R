################################################################
#Table creation for stimuli that goes into the counter ballance#
################################################################
#Data Splitting to form into stimuli#

AmbiSpace= split.data.frame(AllStimSpace,AllStimSpace$Ambi)

NambiSpace=AmbiSpace$n
AmbiSpace=AmbiSpace$y


AmbiSoc= split.data.frame(AllStimSoc,AllStimSoc$Ambi)
NambiSoc=AmbiSoc$n
AmbiSoc=AmbiSoc$y

NambiSpace$Item=c(1:24)
NambiSoc$Item=c(1:24)

################################
#Sorted by Ambiguity text files#
################################

for (i in AmbiSpace$Item) {
  write(AmbiSpace$Spat[i], paste0("CounterB/Sorted Texts/SpaAmbi/SpaAmbi",
                                    AmbiSpace$Item[i], ".txt", sep="" ))}


for (i in AmbiSoc$Item) {
  write(AmbiSoc$Soc[i], paste0("CounterB/Sorted Texts/SocAmbi/SocAmbi",
                                  AmbiSoc$Item[i], ".txt", sep="" ))}


for (i in NambiSoc$Item) {
  write(NambiSoc$Soc[i], paste0("CounterB/Sorted Texts/SocNambi/SocNambi",
                               NambiSoc$Item[i], ".txt", sep="" ))}

for (i in NambiSpace$Item) {
  write(NambiSpace$Spat[i], paste0("CounterB/Sorted Texts/SpaNambi/SpaNambi",
                                NambiSpace$Item[i], ".txt", sep="" ))}

########################
#Condition creation 1-8#
########################
#Make lists#
SpaAmbiList = list.files("CounterB/Sorted Texts/SpaAmbi/")
SocAmbiList = list.files("CounterB/Sorted Texts/SocAmbi/")
SpaNambiList= list.files("CounterB/Sorted Texts/SpaNambi/")
SocNambiList= list.files("CounterB/Sorted Texts/SocNambi/")
#Sort Lists#

n<- get_num(SpaAmbiList)
SpaAmbiList<- SpaAmbiList[order(n, SpaAmbiList)]
SpaAmbiList<- paste("CounterB/Sorted Texts/SpaAmbi/", SpaAmbiList, sep= '')

n<- get_num(SocAmbiList)
SocAmbiList<- SocAmbiList[order(n, SocAmbiList)]
SocAmbiList<- paste("CounterB/Sorted Texts/SocAmbi/", SocAmbiList, sep= '')

n<- get_num(SpaNambiList)
SpaNambiList<- SpaNambiList[order(n, SpaNambiList)]
SpaNambiList<- paste("CounterB/Sorted Texts/SpaNambi/", SpaNambiList, sep= '')

n<- get_num(SocNambiList)
SocNambiList<- SocNambiList[order(n, SocNambiList)]
SocNambiList<- paste("CounterB/Sorted Texts/SocNambi/", SocNambiList, sep= '')

#Condition 1 SpaAmbi SoNambi order 1#


Cond1=read_xlsx("CounterB/Conditions/Cond2.xlsx")
Cond1$Cond=c(1)
Cond1$Stimulus=cbind(AmbiSoc$Soc,AmbiSpace$Spat)
write.csv(Cond1,"CounterB/Conditions/tst.csv",col.names = TRUE)
for (i in Cond1$Item){
  write(Cond1$Stimulus[i],paste0("CounterB/Conditions/Cond1txt/Cond1",
                                 Cond1$Item[i],".txt",sep=""))}

Cond1$Stimulus=cbind(AmbiSpace$Spat,NambiSoc$Soc)
Cond1$Stimulus=cbind(Cond1$Stimulus)

for (i in Cond1$Item) {
  write(Cond1$Stimulus[i], paste0("CounterB/Conditions/Cond1txt/",
                                  Cond1$Item[i], ".txt", sep="" ))}





Cond5=read_xlsx("CounterB/Conditions/Cond5.xlsx")
Cond5$Stimulus=cbind(AmbiSoc$Soc,NambiSpace$Spat)



















#Dud code
####################################################

sent=read_excel("Stimuli/ActualMaster.xlsx")

sent$ID=NULL
sent$ID=c(1:192)
sent$stimuli=sent$Stimulus

#Making the txt files

for (i in sent$ID) {
  write(sent$Stimulus[i], paste0("Word Freeak/TextFiles/AllStimuli/Stim",
                                  sent$ID[i], ".txt", sep="" ))}
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
soc<- list.files("Word Freeak/TextFiles/AllStimuli/")
n<- get_num(soc)
soc<- soc[order(n, soc)]
soc<- paste("Word Freeak/TextFiles/AllStimuli/", soc, sep= '')


write.csv(sent,"Stimuli/sent.csv",col.names = TRUE)


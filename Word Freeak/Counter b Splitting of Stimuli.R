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























#Dud code
####################################################
#StimuliCondi=read_xlsx("Stimuli/StimuliCondi.xlsx")
#StimuliCondi$Item==c(1:24)
#StimuliCondi$Cond=c(1:8,24)


#merged=NULL

#merged=cbind(AmbiSpace$Spat,NambiSoc$Soc)


#Cond1=read_xlsx("CounterB/Cond1.xlsx")
#Cond1$Stimulus=merged



#for (i in Cond1$Item) {
  write(Cond1$Stimulus[i], paste0("CounterB/Cond1Text/Cond1",
                                    Cond1$Item[i], ".txt", sep="" ))}






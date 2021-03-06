##########################################################################################
#Analysis of the Master Table's descriptive information of Spatial and Social Information#
##########################################################################################
#This code requires files and data created by "Descriptive Statistics of Stimuli" ########
##########################################################################################
#############( ͡° ͜ʖ ͡°)( ͡° ͜ʖ ͡°)( ͡° ͜ʖ ͡°)( ͡° ͜ʖ ͡°)( ͡° ͜ʖ ͡°)( ͡° ͜ʖ ͡°)( ͡° ͜ʖ ͡°)#######################
##########################################################################################
#Notes:                                                                                  #
#~Soc stands for Social                                                                  #
#~Spat,Space and maybe some other things stand for Spacial                               #
#~Ambi stands for Ambiguous                                                              #
#~Nambi Stands for Non-Ambiguous                                                         #
#Trickle down changes should be made to the excel files "AllStim","AllstimSoc"and        #
#AllstimSpace or whatever you wanna call them.                                          #
##########################################################################################
#
######################################
##Septerate MasterTable by ambiguity##
######################################
M=split(MasterTable,MasterTable$Ambiguity)
AmbiMaster=as.data.frame(M$y)
NambiMaster=as.data.frame(M$n)

#Take a look to check they came out okay#
View(AmbiMaster)
View(NambiMaster)

##############################
#Table of means for reference#
#Ignore if you're not interested#
#################################

#write.csv (NambiMaster,"Output Table/NambiMasterTable.csv")
#write.csv(AmbiMaster,"Output Table/AmbiMasterTable.csv")

############################################
#T tests between Ambiguous and non-Ambiguous
############################################
#Number of words Space
SpNWT=t.test(AmbiMaster$SpaNwords,NambiMaster$SpaNwords)

#Number of words Social
SoNWT=t.test(AmbiMaster$SocNwords,NambiMaster$SocNwords)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#For Curiosity's sake ignore#
#Number of words AmbiSocial vs AmbiSpacial
#ASoSpNWT=t.test(AmbiMaster$SpaNwords,AmbiMaster$SocNwords)
#Number of words NambiSocial vs NambiSpacial
#NSoSpNWT=t.test(NambiMaster$SpaNwords,NambiMaster$SocNwords)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#################
#Kincaid T tests#
#################
SpaKT=t.test(AmbiMaster$`SpaF-K`,NambiMaster$`SpaF-K`)
SockT=t.test(AmbiMaster$`SocF-K`,NambiMaster$`SocF-K`)

##################
#Mean Word Length#
##################
SpaMWLT=t.test(AmbiMaster$SpaMeanWordLength,NambiMaster$SpaMeanWordLength)
SocMWLT=t.test(AmbiMaster$SocMeanWordLength,NambiMaster$SocMeanWordLength)

#####################
#Mean Word Frequency#
#####################

SpaMWFT=t.test(AmbiMaster$SpaMeanWordFrequency,NambiMaster$SpaMeanWordFrequency)
SocMWFT=t.test(AmbiMaster$SocMeanWordFrequency,NambiMaster$SocMeanWordFrequency)
#
Tdatafull=NULL
#Tdatafull$Value=c
Tdatafull$SameStimDiffAmbi=c(SpaKT$p.value,SockT$p.value,SpaMWLT$p.value,SocMWLT$p.value,SpaMWFT$p.value,SocMWFT$p.value,SoNWT$p.value,SpNWT$p.value)
Tdatafull=as.data.frame(Tdatafull)
####################################
#Reading Ease Kincaid for curiosity
#ReadingEaseSpa=t.test(AmbiMaster$ReadingeaseSpat,NambiMaster$ReadingeaseSpat)
#ReadingEaseSoc=t.test(AmbiMaster$ReadingeaseSoc,NambiMaster$ReadingeaseSoc)

PPP=mean(AmbiMaster$SocMeanWordLength)
PPT=mean(NambiMaster$SocMeanWordLength)

#######################################################################################
#ASocial Vs ASpatial#
#NSocial vs NSpatial#
#######################################################################################
#Number of words##
##################
#Ambi
ANW=t.test(AmbiMaster$SpaNwords,AmbiMaster$SocNwords)

#Nambi
NNW=t.test(NambiMaster$SocNwords,NambiMaster$SpaNwords)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#For Curiosity's sake ignore#
#Number of words AmbiSocial vs AmbiSpacial
#ASoSpNWT=t.test(AmbiMaster$SpaNwords,AmbiMaster$SocNwords)
#Number of words NambiSocial vs NambiSpacial
#NSoSpNWT=t.test(NambiMaster$SpaNwords,NambiMaster$SocNwords)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#################
#Kincaid T tests#
#################
AmbiKT=t.test(AmbiMaster$`SpaF-K`,AmbiMaster$`SocF-K`)
NambiKT=t.test(NambiMaster$`SpaF-K`,NambiMaster$`SocF-K`)

##################
#Mean Word Length#
##################
AmbiMWLT=t.test(AmbiMaster$SpaMeanWordLength,AmbiMaster$SocMeanWordLength)
NambiMWLT=t.test(NambiMaster$SpaMeanWordLength,NambiMaster$SocMeanWordLength)

#####################
#Mean Word Frequency#
#####################

AmbiMWFT=t.test(AmbiMaster$SpaMeanWordFrequency,AmbiMaster$SocMeanWordFrequency)
NambiWFT=t.test(NambiMaster$SpaMeanWordFrequency,NambiMaster$SocMeanWordFrequency)
#
Tdatafull$DiffStimSameAmbi=c(ANW$p.value,NNW$p.value,AmbiKT$p.value,NambiKT$p.value,
                             AmbiMWLT$p.value,NambiMWLT$p.value,AmbiMWFT$p.value,NambiWFT$p.value)

####################################################
#Global
####################################################
AllNW=t.test(MasterTable$SpaNwords,MasterTable$SocNwords)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#For Curiosity's sake ignore#
#Number of words AmbiSocial vs AmbiSpacial
#ASoSpNWT=t.test(AmbiMaster$SpaNwords,AmbiMaster$SocNwords)
#Number of words NambiSocial vs NambiSpacial
#NSoSpNWT=t.test(NambiMaster$SpaNwords,NambiMaster$SocNwords)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#################
#Kincaid T tests#
#################
AllKT=t.test(MasterTable$`SpaF-K`,MasterTable$`SocF-K`)


##################
#Mean Word Length#
##################
AllMWLT=t.test(MasterTable$SpaMeanWordLength,MasterTable$SocMeanWordLength)


#####################
#Mean Word Frequency#
#####################

AllMWFT=t.test(MasterTable$SpaMeanWordFrequency,MasterTable$SocMeanWordFrequency)

#
Tdatafull$All=c(AllNW$p.value,AllKT$p.value,
                             AllMWLT$p.value,AllMWFT$p.value)

sd(MasterTable$`SpaF-K`)
mean(MasterTable$`SpaF-K`)
sd(MasterTable$`SocF-K`)
mean(MasterTable$`SocF-K`)
sd(MasterTable$SpaNwords)
mean(MasterTable$SpaNwords)
sd(MasterTable$SocNwords)
mean(MasterTable$SpaNwords)

Checktable=NULL
Checktable$Item=MasterTable$Item
Checktable$Ambi=MasterTable$Ambiguity
Checktable$NWDiff=((MasterTable$SpaNwords)-(MasterTable$SocNwords))
Checktable$KDiff=((MasterTable$`SpaF-K`)-(MasterTable$`SocF-K`))
Checktable=as.data.frame(Checktable)

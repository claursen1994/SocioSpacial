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
#T tests between Ambiguous and non Ambiguous
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

Tdatafull=c(SpaKT$p.value,SockT$p.value,SpaMWLT$p.value,SocMWLT$p.value,SpaMWFT$p.value,SocMWFT$p.value,SoNWT$p.value,SpNWT$p.value)
Tdatafull=as.data.frame(Tdatafull)



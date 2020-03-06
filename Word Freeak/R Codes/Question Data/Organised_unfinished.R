###############################################################
# Question Analysis organised for presenting to normal people
###############################################################
#Can only be Run After Question_Analysis.R

#Make means
#Mean accuracy

DesQuest<- melt(GAVN, id=c('sub', 'item', 'State', 'Age','dependnum'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuestBySub<- cast(DesQuest, Age ~ State ~ dependnum + variable
                   ,function(x) c(M=signif(mean(x),3)
                                  , SD= sd(x) ))
mQuestBySub

#Mean time
DesQuest<- melt(GAVN, id=c('sub', 'item', 'State', 'Age','dependnum'), 
                measure=c("duration_ms"), na.rm=TRUE)
mQuestBySub<- cast(DesQuest, Age ~ State ~ dependnum + variable
                   ,function(x) c(M=signif(mean(x),3)
                                  , SD= sd(x) ))
mQuestBySub

#######################################################################


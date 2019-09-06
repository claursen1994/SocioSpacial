################################################
# Making of Stimuli files for matlab          #
###############################################
#sent#

NumQuest=read_excel("LabCodeC/corpus/Old and Uneeded/NumericQuest.xlsx")

sent=NULL
sent$item=MDMK$item
sent$cond=MDMK$cond
sent$P1=MDMK$P1
sent$P2=MDMK$P2
sent$Stimulus=MDMK$Stimulus
sent=as.data.frame(sent)
#write.csv2(sent,"LabCodeC/corpus/sent.csv")# for some reason csv 2 not working. 
write.csv(sent,"LabCodeC/corpus/sent.csv")
######################################################
#quest#

Quest=NULL
Quest$Q1=MDMK$Q1
Quest$Q1o1=NumQuest$Q1O1
Quest$Q1o2=NumQuest$Q1O2
Quest$Q1o3=NumQuest$Q1O3
Quest$Q1corr_ans=NumQuest$Q1corr_ans
Quest$Q2=MDMK$Q2
Quest$Q2o1=NumQuest$Q2O1
Quest$Q2o2=NumQuest$Q2O2
Quest$Q2o3=NumQuest$Q2O3
Quest$Q2corr_ans=NumQuest$Q2corr_ans
Quest$Q3=MDMK$Q3
Quest$Q3o1=NumQuest$Q3O1
Quest$Q3o2=NumQuest$Q3O2
Quest$Q3o3=NumQuest$Q3O3
Quest$Q3corr_ans=NumQuest$Q3corr_ans
Quest=as.data.frame(Quest)
write.csv(Quest,"LabCodeC/corpus/Quest.csv")

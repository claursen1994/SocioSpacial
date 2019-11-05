################################################
# Making of Stimuli files for matlab          #
###############################################
#sent#


MDMK= read_excel("Stimuli/MDMK.xlsx")

sent=NULL
sent$item=MDMK$item
sent$cond=MDMK$cond
sent$P1=MDMK$P1
sent$P2=MDMK$P2
sent$Stimulus=MDMK$Stimulus
sent=as.data.frame(sent)

#write.csv2(sent,"LabCodeC/corpus/sent.csv")# for some reason csv 2 not working. 
write.csv(sent,"LabCodeC/corpus/sent.csv")
write.table(sent, file = "LabCodeC/corpus/sent.txt",
            
            append = FALSE, sep = "\t", col.names = TRUE ,row.names = FALSE, quote = TRUE)
######################################################
#quest#

Quest=NULL
Quest$Q1=MDMK$Q1
Quest$Q1o1=MDMK$Q1O1
Quest$Q1o2=MDMK$Q1O2
Quest$Q1o3=MDMK$Q1O3
Quest$Q1corr_ans=MDMK$Q1corr_ans
Quest$Q2=MDMK$Q2
Quest$Q2o1=MDMK$Q2O1
Quest$Q2o2=MDMK$Q2O2
Quest$Q2o3=MDMK$Q2O3
Quest$Q2corr_ans=MDMK$Q2corr_ans
Quest$Q3=MDMK$Q3
Quest$Q3o1=MDMK$Q3O1
Quest$Q3o2=MDMK$Q3O2
Quest$Q3o3=MDMK$Q3O3
Quest$Q3corr_ans=MDMK$Q3corr_ans
Quest=as.data.frame(Quest)
write.csv(Quest,"LabCodeC/corpus/Quest.csv")
write.table(Quest, file = "LabCodeC/corpus/Quest.txt",
            
            append = FALSE, sep = "\t", col.names = TRUE ,row.names = FALSE, quote = TRUE)

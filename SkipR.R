#Working Skip rates!


# Make sure EM reading is there

if('devtools' %in% rownames(installed.packages())==FALSE){
  install.packages('devtools')
  library(devtools)
}else{
  library(devtools)
}
install_github('martin-vasilev/EMreading')



#Load in takes a bit of time, and only works after raw_fix has been created
Skip_Raw_fix=read.csv("raw_fix.csv")

library("EMreading")

Skip_Raw_fix22=wordMeasures(Skip_Raw_fix)
Skip_Raw_fix22$skip=NULL
# If Nfix 1 was 0 then the word was skipped the first time.
Skip_Raw_fix22$skip<- ifelse(Skip_Raw_fix22$nfix1=="0", 1, 0)

# If Nfix1 was 0 and Nfix2 was 1 or more then the word was fixated later 

Skip_Raw_fix22$hold<- ifelse(Skip_Raw_fix22$nfix2>"0",1,0 )
Skip_Raw_fix22$Return2SkipWord= ifelse(Skip_Raw_fix22$skip+Skip_Raw_fix22$hold=="2",1,0)
Skip_Raw_fix22$hold=NULL


# OBI Work 060920



library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(xlsx)
library(stringr)
library(questionr)




library(stringi)
library(jtools)
library(viridis)

options(scipen=999)

surveys  <- read.xlsx2("C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/New Survey List.xlsx",sheetName="Files",startRow=1,header=TRUE,
                       colClasses = c("numeric", "character", "character", "character",  "numeric", "character", "character", "character"));


surveys <- filter(surveys, Survey!="")


surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")

results.rsquare <- read.csv("C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/RSquare Blank.csv")
rsquare_single <- results.rsquare

setwd("C:/Users/KristinBietsch/Files/DHSLoop")

for (row in 1:nrow(surveys)) {
  women_data <- surveys[row, "IRfile"]
  surveyname <- surveys[row, "Survey"]
  
  women <- read_dta(women_data)
  
  women <- women %>% filter(v502==1)
  women <- women %>% filter(v013>=1 & v013<=7)
  
  women <- women %>% mutate(obi=v008-b3_01,
                            bi_cat=case_when(v201==0 & v213==0 ~ -1 ,
                                             v213==1 ~ 0,
                                             obi < 12 ~ 1,
                                             obi >=12 &  obi < 24 ~ 2,
                                             obi >=24 & obi < 36 ~ 3,
                                             obi >=36 & obi < 48 ~ 4,
                                             obi >=48 & obi < 60 ~ 5,
                                             obi >=60 & obi < 72 ~ 6,
                                             obi >=72 & obi < 84 ~ 7,
                                             obi >=84 & obi < 96 ~ 8,
                                             obi >=96 & obi < 108 ~ 9,
                                             obi >=108 & obi < 120 ~ 10,
                                             obi >=120 & obi < 132 ~ 11,
                                             obi >=132 & obi < 144 ~ 12,
                                             obi >=144 & obi < 156 ~ 13,
                                             obi >=156 & obi < 168 ~ 14,
                                             obi >=168 & obi < 180 ~ 15,
                                             obi >=180 & obi < 192 ~ 16,
                                             obi >=192 & obi < 204 ~ 17,
                                             obi >=204 & obi < 216 ~ 18,
                                             obi >=216 & obi < 228 ~ 19,
                                             obi >=228 & obi < 240 ~ 20,
                                             obi >=240  ~ 21),
                            bi_mean=case_when(bi_cat==-1 ~ 0,
                                              bi_cat>=0 ~ bi_cat + .5))
  
  women <- women %>% mutate(ceb=case_when(v201==0 ~ 0,
                                          v201==1 ~ 1,
                                          v201==2 ~ 2,
                                          v201==3 ~ 3,
                                          v201==4 ~ 4,
                                          v201==5 ~ 5,
                                          v201>=6 ~ 6),
                            age=case_when(v013==1 ~ "Age1519", v013==2 ~ "Age2024", v013==3 ~ "Age2529", v013==4 ~ "Age3034", v013==5 ~ "Age3539", v013==6 ~ "Age4044", v013==7 ~ "Age4549"),
                            Parity=paste("Parity", ceb, sep=""),
                            ageparity=paste(age, Parity, sep="_"))
  
  women$sampleweights <- women$v005/1000000
  
  if(surveyname=="miniIAIR73FL" | surveyname=="miniIAIR52FL"){
    women$stratum <- women$v022 
  } else{
    women$stratum <- women$v024 + (women$v025)*1000  
    
  }
  
  
  if(surveyname=="miniIAIR52FL"){
    women$id <- women$v001 
  } else{
    women$id <- women$v021 
    
  }
  
  design.women <- svydesign(ids=~id, strata=~stratum, weights=~sampleweights, data=women, nest=TRUE)
  
  
  options(survey.lonely.psu="adjust")
  
  rsquare <- attr(summ(svyglm( obi ~ ceb*v013   ,  design.women )), "rsq")
  rsquare_single$Rsquare <- rsquare
  rsquare_single$Survey <- surveyname
  results.rsquare <- bind_rows(results.rsquare, rsquare_single)
  
}

results.rsquare <- filter(results.rsquare, Survey!="Survey")
surveys <- full_join(surveys, results.rsquare, by="Survey")

write.xlsx(as.data.frame(surveys), "C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/OBI_Data_060920RSquare.xlsx", sheetName="Regression RSquare",   col.names=TRUE, row.names=TRUE, append=FALSE,  showNA=FALSE)

# OBI Work 060820

# calculate age distribution of married women of reproductive age
# regressions- age, parity, interaction
# parity distribution by age group
# age distribution by parity group
# mean obi by parity*age group (20.5 for the 20+ interval)


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


results.agedist <- read.csv("C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/Age_Distribution_Blank.csv")
#results.regression <- read.csv("C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/Regression_Results_Blank.csv")
results.par_within_age <- read.csv("C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/Par_within_Age_Blank.csv")
results.age_within_par <- read.csv("C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/Age_within_Par_Blank.csv")
#results.meanobi.agepar <- results.par_within_age

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
  
  
  
  
  table.age <- as.data.frame(prop.table(wtd.table(women$v013, weights = women$sampleweights, digits = 3, normwt = FALSE,  na.rm = TRUE, na.show = FALSE, exclude = NULL))) %>% mutate(Var1=case_when(Var1==1 ~ "Age1519", Var1==2 ~ "Age2024", Var1==3 ~ "Age2529", Var1==4 ~ "Age3034", Var1==5 ~ "Age3539", Var1==6 ~ "Age4044", Var1==7 ~ "Age4549")) %>% spread(Var1, Freq)
  table.age$Survey <- surveyname


    
  dist.age.within.par <- as.data.frame(prop.table(wtd.table(women$v013, women$ceb, weights = women$sampleweights, digits = 3, normwt = FALSE,  na.rm = TRUE, na.show = FALSE, exclude = NULL),2)) %>% mutate(Var1=case_when(Var1==1 ~ "Age1519", Var1==2 ~ "Age2024", Var1==3 ~ "Age2529", Var1==4 ~ "Age3034", Var1==5 ~ "Age3539", Var1==6 ~ "Age4044", Var1==7 ~ "Age4549")) %>% mutate(Var2=paste("Parity", Var2, sep=""), ParityAge=paste(Var2, Var1, sep="_")) %>% select(-Var1, -Var2) %>% spread(ParityAge, Freq) 
  dist.age.within.par$Survey <- surveyname
  
  
 dist.par.within.age <- as.data.frame(prop.table(wtd.table(women$v013, women$ceb, weights = women$sampleweights, digits = 3, normwt = FALSE,  na.rm = TRUE, na.show = FALSE, exclude = NULL),1)) %>% mutate(Var1=case_when(Var1==1 ~ "Age1519", Var1==2 ~ "Age2024", Var1==3 ~ "Age2529", Var1==4 ~ "Age3034", Var1==5 ~ "Age3539", Var1==6 ~ "Age4044", Var1==7 ~ "Age4549")) %>% mutate(Var2=paste("Parity", Var2, sep=""), AgeParity=paste(Var1, Var2, sep="_")) %>% select(-Var1, -Var2) %>% spread(AgeParity, Freq)
 dist.par.within.age$Survey <- surveyname
 
  
  
 

  results.agedist <-  bind_rows(results.agedist, table.age)
  results.par_within_age <-  bind_rows(results.par_within_age, dist.par.within.age)
  results.age_within_par <-  bind_rows(results.age_within_par, dist.age.within.par)

  
}


write.xlsx(as.data.frame(results.agedist), "C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/OBI_Data_060820.xlsx", sheetName="Age Distribution",   col.names=TRUE, row.names=TRUE, append=FALSE,  showNA=FALSE)
write.xlsx(as.data.frame(results.par_within_age), "C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/OBI_Data_060820.xlsx", sheetName="Parity within Age",   col.names=TRUE, row.names=TRUE, append=TRUE,  showNA=FALSE)
write.xlsx(as.data.frame(results.age_within_par), "C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/OBI_Data_060820.xlsx", sheetName="Age within Parity",   col.names=TRUE, row.names=TRUE, append=TRUE,  showNA=FALSE)



# obi as a loop

library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(xlsx)
library(stringr)
options(scipen=999)

surveys  <- read.xlsx2("C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/New Survey List.xlsx",sheetName="Files",startRow=1,header=TRUE,
                       colClasses = c("numeric", "character", "character", "character",  "numeric", "character", "character", "character"));

results <- read.csv("C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/Blank Results.csv")


surveys <- filter(surveys, Survey!="")



surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")

setwd("C:/Users/KristinBietsch/Files/DHSLoop")

for (row in 1:nrow(surveys)) {
  women_data <- surveys[row, "IRfile"]
  surveyname <- surveys[row, "Survey"]
  
  women <- read_dta(women_data)
  
  women <- women %>% filter(v502==1)
  women <- women %>% filter(v013!=0)
  
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
                                             obi >=240  ~ 21))
  
  women$sampleweights <- women$v005/1000000
  design.women <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women, nest=TRUE)
  options(survey.lonely.psu="adjust")
  
  mean <- as.data.frame(svymean(~as.factor(women$bi_cat),  design.women, na.rm=TRUE))
  
  mean <- mean %>% rownames_to_column( "BI") %>% select(-SE)
  
  mean$BI <- str_remove_all(mean$BI, "[as.factor(women$bi_cat)]")
  
  mean <- mean %>% 
    mutate(BI=case_when(BI== -1 ~ "NevPreg" ,
                        BI== 0 ~ "Preg", 
                        BI== 1 ~ "U1" ,
                        BI== 2 ~ "Y1.2" ,
                        BI== 3 ~ "Y2.3" ,
                        BI== 4 ~ "Y3.4" ,
                        BI== 5 ~ "Y4.5" ,
                        BI== 6 ~ "Y5.6" ,
                        BI== 7 ~ "Y6.7" ,
                        BI== 8 ~ "Y7.8" ,
                        BI== 9 ~ "Y8.9" ,
                        BI== 10 ~ "Y9.10" ,
                        BI== 11 ~ "Y10.11" ,
                        BI== 12 ~ "Y11.12" ,
                        BI== 13 ~ "Y12.13" ,
                        BI== 14 ~ "Y13.14" ,
                        BI== 15 ~ "Y14.15" ,
                        BI== 16 ~ "Y15.16" ,
                        BI== 17 ~ "Y16.17" ,
                        BI== 18 ~ "Y17.18" ,
                        BI== 19 ~ "Y18.19" ,
                        BI== 20 ~ "Y19.20" ,
                        BI== 21 ~ "Over20")) %>%
    spread(BI, mean)
  
  
  
  mean$Survey <- surveyname
  
  results <- bind_rows(results, mean)
  
  
}
names(data)

header <- surveys %>% select(Survey, Country, Grouped.Regions, Sub.Region, Region, Year, New)
data <- full_join(header, results, by="Survey")

data <- select(data, Survey   ,       Country ,        Grouped.Regions, Sub.Region,      Region ,         Year      ,      New     ,        NevPreg  , Preg , U1 ,             Y1.2,
               Y2.3    ,        Y3.4  ,          Y4.5,            Y5.6 ,           Y6.7 ,          
               Y7.8 ,           Y8.9  ,          Y9.10 ,            Y10.11  ,        Y11.12  ,        Y12.13  ,        Y13.14  ,        Y14.15  ,        Y15.16,         
               Y16.17 ,         Y17.18 ,         Y18.19  ,        Y19.20  ,       Over20        )

write.csv(data,"C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/Distribution_22_Group_051120.csv", na="")
#assign(paste("BI_", surveyname, sep = ""), data.frame(mean))


#write.csv(BI_AFIR70FL,"C:/Users/KristinBietsch/files/Track20/Open Birth Interval Ross/afghan.csv")

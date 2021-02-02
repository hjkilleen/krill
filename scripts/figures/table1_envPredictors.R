#Table 1
#Environmental Predictors Table
# Wed Jan 27 14:52:06 2021 ------------------------------

#LIBRARIES
#====
library(readxl)
library(knitr)
library(kableExtra)
library(tikzDevice)
#====

#SET UP
#====
ep <- read_xlsx("data/envPredictors.xlsx")#load table
ep[1:3,6] <- paste0("10km", "$^2$")#add km^2 to spatial scale column
#====

#TABLE
#====
ep %>% #knit table
  kbl() %>% 
  kable_classic()
#====
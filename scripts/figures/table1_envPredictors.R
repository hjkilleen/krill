#Table 1
#Environmental Predictors Table
# Wed Jan 27 14:52:06 2021 ------------------------------

#LIBRARIES
#====
library(reshape2)
library(knitr)
library(kableExtra)
#====

#SET UP
#====
ep <- read_csv("data/envPredictors.csv")
ep[1:3,6] <- paste0("10km", "$^2$")
#====

#TABLE
#====
ep %>% 
  kbl() %>% 
  kable_classic()
#====
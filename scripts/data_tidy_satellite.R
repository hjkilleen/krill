#Tidy satellite data
# Fri Dec 11 16:11:36 2020 ------------------------------

#LIBRARIES & SOURCES
#====
library(lubridate)
source("scripts/data_load.R")
#====

#TIDY SATTELITE DATA
#====
env$date <- mdy(env$time64)
env$year <- as.character(substring(env$date, 1, 4))
env <- filter(env, dtime != 0)#get rid of zero day (UTC correction)
#====
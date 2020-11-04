# Tue Nov  3 14:28:25 2020 ------------------------------

#Full krill lengths model

library(lme4)
library(tidyverse)
library(ggeffects)
library(moments)
library(ggpubr)
library(lubridate)
source("scripts/functions/model_simulation.R")
source("scripts/functions/length_frequency.R")
load("data/allLengthsEnv.rda")

str(allLengthsEnv)

env <- read.csv("data/zoo_selgroups_HadSST_relabundance_5aug2019_plumchrusV_4regions_final_satsstall.csv")

str(env)
#might need to filter to only times w/in 48 hours with UTC adjustment...check with Chelle
env2 <- summarize(group_by_at(env, .vars = c("station", "time64")), chla = mean(chlor_a, na.rm= TRUE)) 
env2$date <- mdy(as.character(env2$time64))

allLengthsEnv <- left_join(allLengthsEnv, env2)

#MODEL
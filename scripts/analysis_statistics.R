#Statistical tests of raw length data
# Wed Jan 27 17:54:24 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(lme4)
library(ggeffects)
library(moments)
library(ggpubr)
source("scripts/functions/model_simulation.R")
source("scripts/functions/length_frequency.R")
load("data/allLengthsEnv.rda")
#====

#Test for normality of each species distribution
#====
#structure of all krill lengths
hist(allLengths$length)
qqnorm(allLengths$length)
#Agostino test for normality
moments::agostino.test(allLengths$length)
#Total kurtosis (-3 for excess kurtosis)
moments::kurtosis(filter(allLengths, species == "ND")$length)-3
#=====

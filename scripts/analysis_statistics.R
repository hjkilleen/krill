#Statistical tests of raw length data
# Tue Jun  1 13:13:56 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(lme4)
library(ggeffects)
library(moments)
library(ggpubr)
source("scripts/functions/model_simulation.R")
source("scripts/functions/length_frequency.R")
load("data/allLengthsEnv.rda")
load("data/allLengthsEnvEP.rda")
load("data/allLengthsEnvND.rda")
load("data/allLengthsEnvTS.rda")
#====

#Test for normality of each species distribution
#====
#Euphausia pacifica
#structure of krill lengths
hist(ep$length)
qqnorm(ep$length)
#Agostino test for normality
moments::agostino.test(ep$length)
#Total kurtosis (-3 for excess kurtosis)
moments::kurtosis(ep$length)-3

#Thysanoessa spinifera
#structure of krill lengths
hist(ts$length)
qqnorm(ts$length)
#Agostino test for normality
moments::agostino.test(ts$length)
#Total kurtosis (-3 for excess kurtosis)
moments::kurtosis(ts$length)-3

#Nematoscelis difficilis
#structure of krill lengths
hist(nd$length)
qqnorm(nd$length)
#Agostino test for normality
moments::agostino.test(nd$length)
#Total kurtosis (-3 for excess kurtosis)
moments::kurtosis(nd$length)-3
#=====

#One sample t-test to calculate mean length of each species and 95% confidence interval
#=====
#Euphausia pacifica
t.test(ep$length*attr(ep$length, "scaled:scale") + attr(ep$length, "scaled:center"))

#Thysanoessa spinifera
t.test(ts$length*attr(ts$length, "scaled:scale") + attr(ts$length, "scaled:center"))

#Nematoscelis difficilis
t.test(nd$length*attr(nd$length, "scaled:scale") + attr(nd$length, "scaled:center"))
#=====
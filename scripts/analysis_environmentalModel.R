#Environmental model
# Thu Jan 28 15:15:57 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(lme4)
library(tidyverse)
load("data/allLengthsEnv.rda")
load("data/allLengthsEnvEP.rda")
load("data/allLengthsEnvND.rda")
load("data/allLengthsEnvTS.rda")
source("scripts/functions/model_simulation.R")
#====

#SETUP
#=======
#Model variation in krill length throughout the heatwave.
#Change relevant variables to factors for multilevel analysis
allLengthsEnv$station <- as.factor(allLengthsEnv$station)
allLengthsEnv$year <- as.factor(allLengthsEnv$year)
allLengthsEnv$species <- as.factor(allLengthsEnv$species)

#Drop 2011-2013 N. difficilis due to limited sampling
# allLengthsEnv <- allLengthsEnv[!(allLengthsEnv$species == "ND" & allLengthsEnv$year %in% c("2011", "2012", "2013")),]
# nd <- nd[!(nd$year %in% c("2011", "2012", "2013")),]
#====

#MULTILEVEL MODELING
#====
pm <- lmer(length ~ year + sex + year:sex + (1|station), data = allLengthsEnv)#Pooled species model
epm <- lmer(length ~ year + sex + year:sex + (1|station), data = ep)#EP
tsm <- lmer(length ~ year + sex + year:sex + (1|station), data = ts)#TS
ndm <- lmer(length ~ year + sex + year:sex + (1|station), data = nd)#ND

pmc <- data.frame(predictor = attr(fixef(pm), "names"),#extract fixed effects coefficients
                  coefficient = as.vector(fixef(pm)))
epc <- data.frame(predictor = attr(fixef(epm), "names"),
                  coefficient = as.vector(fixef(epm)))
tsc <- data.frame(predictor = attr(fixef(tsm), "names"),
                  coefficient = as.vector(fixef(tsm)))
ndc <- data.frame(predictor = attr(fixef(ndm), "names"),
                  coefficient = as.vector(fixef(ndm)))
interannualCoefficients <- list(pmc, epc, tsc, ndc)#merge as list
save(interannualCoefficients, file = "data/interannualCoefficients.rda")#save list
#====

#SIMULATION
#====
pmsim <- fsim.glmm(pm, nsim = 1000)#generate simulated values
epsim <- fsim.glmm(epm, nsim = 1000)
tssim <- fsim.glmm(tsm, nsim = 1000)
ndsim <- fsim.glmm(ndm, nsim = 1000)

pmsimsum <- simsum(pmsim)#summary statistics for simulated data
epsimsum <- simsum(epsim)
tssimsum <- simsum(tssim)
ndsimsum <- simsum(ndsim)

save(pmsimsum, file = "data/interannualPooled.rda")#save rda for simulated data
save(epsimsum, file = "data/interannualEP.rda")
save(tssimsum, file = "data/interannualTS.rda")
save(ndsimsum, file = "data/interannualND.rda")
#====

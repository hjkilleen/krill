#Interannual model
# Wed Jan 27 17:54:48 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(lme4)
library(tidyverse)
library(superheat)
library(sjPlot)
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
#====

#MULTILEVEL MODELING
#====
pm.noSex <- lmer(length ~ year + (1|station), data = allLengthsEnv)#Pooled species model without sex interaction
epm.noSex <- lmer(length ~ year + (1|station), data = ep)#EP
tsm.noSex <- lmer(length ~ year + (1|station), data = ts)#TS
ndf <- filter(nd, year != "2011", year != "2012", year != "2013")#drop observations from 2011-2013, too few samples
ndm.noSex <- lmer(length ~ year + (1|station), data = ndf)#ND

pmc.noSex <- data.frame(predictor = attr(fixef(pm.noSex), "names"),#extract fixed effects coefficients
                  coefficient = as.vector(fixef(pm.noSex)))
epc.noSex <- data.frame(predictor = attr(fixef(epm.noSex), "names"),
                  coefficient = as.vector(fixef(epm.noSex)))
tsc.noSex <- data.frame(predictor = attr(fixef(tsm.noSex), "names"),
                  coefficient = as.vector(fixef(tsm.noSex)))
ndc.noSex <- data.frame(predictor = attr(fixef(ndm.noSex), "names"),
                  coefficient = as.vector(fixef(ndm.noSex)))
interannualCoefficients.noSex <- list(pmc.noSex, epc.noSex, tsc.noSex, ndc.noSex)#merge as list
save(interannualCoefficients.noSex, file = "output/interannualCoefficients_noSex.rda")#save list

#Generate output supplementary tables
tab_model(epm.noSex, title = "E. pacifica (year only)", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))
tab_model(tsm.noSex, title = "T. spinifera (year only)", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))
tab_model(ndm.noSex, title = "N. difficilis (year only)", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))

#Drop 2011-2013 N. difficilis due to limited sampling
allLengthsEnv <- allLengthsEnv[!(allLengthsEnv$species == "ND" & allLengthsEnv$year %in% c("2011", "2012", "2013")),]
nd <- nd[!(nd$year %in% c("2011", "2012", "2013")),]

pm <- lmer(length ~ year + sex + year:sex + (1|station), data = allLengthsEnv)#Pooled species model with sex interaction
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
save(interannualCoefficients, file = "output/interannualCoefficients.rda")#save list

#Generate output tables
tab_model(epm, title = "E. pacifica (year and sex)", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))
tab_model(tsm, title = "T. spinifera (year and sex)", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))
tab_model(ndm, title = "N. difficilis (year and sex)", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))
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

save(pmsimsum, file = "output/interannualPooled.rda")#save rda for simulated data
save(epsimsum, file = "output/interannualEP.rda")
save(tssimsum, file = "output/interannualTS.rda")
save(ndsimsum, file = "output/interannualND.rda")
#====


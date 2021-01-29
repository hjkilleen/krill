#Environmental model
#Model environmental drivers of krill length
# Thu Jan 28 15:15:57 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(lme4)
library(tidyverse)
library(arm)
library(MuMIn)
load("data/allLengthsEnv.rda")
load("data/allLengthsEnvEP.rda")
load("data/allLengthsEnvND.rda")
load("data/allLengthsEnvTS.rda")
source("scripts/functions/model_simulation.R")
#====

#SETUP
#=======
allLengthsEnv$station <- as.factor(allLengthsEnv$station)#Change relevant variables to factors for multilevel analysis
allLengthsEnv$species <- as.factor(allLengthsEnv$species)

pc <- allLengthsEnv[complete.cases(allLengthsEnv),]#filter to only complete cases
epc <- ep[complete.cases(ep),]#filter to only complete cases
tsc <- ts[complete.cases(ts[,-18]),]#filter to only complete cases, but drop temp_100 column
ndc <- nd[complete.cases(nd),]#filter to only complete cases
#====

#MULTILEVEL MODELING
#====
#Pooled species
#Optimize random effects structure using maximum likelihood
p.int <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1|station), data = pc, na.action = na.fail, REML = FALSE) #model with random intercept 

p.intSlope <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1+temp_2|station), data = pc, na.action = na.fail, REML = FALSE) #model with random intercept 
anova(p.int, p.intSlope)#compare models with different random effect structure
#Slope intercept is the optimal random effects structure
pm <- p.intSlope

#Euphausia pacifica
#Optimize random effects structure using maximum likelihood
ep.int <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1|station), data = epc, na.action = na.fail, REML = FALSE) #model with random intercept 

ep.intSlope <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1+temp_2|station), data = epc, na.action = na.fail, REML = FALSE) #model with random intercept 

anova(ep.int, ep.intSlope)#compare models with different random effect structure
#Slope intercept is the optimal random effects structure

#Optimize fixed effect structure using AIC
ep.model.set <- dredge(ep.intSlope)

ep.top.model <- get.models(ep.model.set, subset = 1)
epm <- ep.top.model[[1]]

#Thysanoessa spinifera
#Optimize random effects structure using maximum likelihood
ts.int <- lmer(length ~ sex*temp_2  + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1|station), data = tsc, na.action = na.fail, REML = FALSE) #model with random intercept 

ts.intSlope <- lmer(length ~ sex*temp_2 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1+temp_2|station), data = tsc, na.action = na.fail, REML = FALSE) #model with random intercept 

anova(ts.int, ts.intSlope)#compare models with different random effect structure
#Slope intercept is the optimal random effects structure

#Optimize fixed effect structure using AIC
ts.model.set <- dredge(ts.intSlope)

ts.top.model <- get.models(ts.model.set, subset = 1)
tsm <- ts.top.model[[1]]

#Nematocelis difficilis
#Optimize random effects structure using maximum likelihood
nd.int <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1|station), data = ndc, na.action = na.fail, REML = FALSE) #model with random intercept 

nd.intSlope <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1+temp_2|station), data = ndc, na.action = na.fail, REML = FALSE) #model with random intercept 
#Singular fit
#Slope intercept is the optimal random effects structure

#Optimize fixed effect structure using AIC
nd.model.set <- dredge(nd.int)

nd.top.model <- get.models(nd.model.set, subset = 1)
ndm <- nd.top.model[[1]]

#Extract model coefficients for fixed effects
pmc <- data.frame(predictor = attr(fixef(pm), "names"),#extract fixed effects coefficients
                  coefficient = as.vector(fixef(pm)))
epc <- data.frame(predictor = attr(fixef(epm), "names"),
                  coefficient = as.vector(fixef(epm)))
tsc <- data.frame(predictor = attr(fixef(tsm), "names"),
                  coefficient = as.vector(fixef(tsm)))
ndc <- data.frame(predictor = attr(fixef(ndm), "names"),
                  coefficient = as.vector(fixef(ndm)))
environmentalCoefficients <- list(pmc, epc, tsc, ndc)#merge as list
save(environmentalCoefficients, file = "output/environmentalCoefficients.rda")#save list
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

save(pmsimsum, file = "output/environmentalPooled.rda")#save rda for simulated data
save(epsimsum, file = "output/environmentalEP.rda")
save(tssimsum, file = "output/environmentalTS.rda")
save(ndsimsum, file = "output/environmentalND.rda")
#====

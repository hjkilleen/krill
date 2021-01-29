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
epci <- as.data.frame(confint(epm))

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
tsci <- as.data.frame(confint(tsm))

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
ndci <- as.data.frame(confint(ndm))

#Pooled species
#Optimize random effects structure using maximum likelihood
#Terms that did not appear in species models removed from global model. 
p.int <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1|station), data = pc, na.action = na.fail, REML = FALSE) #model with random intercept 

p.intSlope <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1+temp_2|station), data = pc, na.action = na.fail, REML = FALSE) #model with random intercept 
anova(p.int, p.intSlope)#compare models with different random effect structure
#Slope intercept is the optimal random effects structure
pm <- p.intSlope
pci <- as.data.frame(confint(pm))

#Extract model coefficients for fixed effects
pmc <- data.frame(predictor = attr(fixef(pm), "names"),#extract fixed effects coefficients
                  Pcoefficient = as.vector(fixef(pm)),
                  PLCL = pci[-c(1:4),]$`2.5 %`,
                  PUCL = pci[-c(1:4),]$`97.5 %`)
epc <- data.frame(predictor = attr(fixef(epm), "names"),
                  Ecoefficient = as.vector(fixef(epm)),
                  ELCL = epci[-c(1:4),]$`2.5 %`,
                  EUCL = epci[-c(1:4),]$`97.5 %`)
tsc <- data.frame(predictor = attr(fixef(tsm), "names"),
                  Tcoefficient = as.vector(fixef(tsm)),
                  TLCL = tsci[-c(1:4),]$`2.5 %`,
                  TUCL = tsci[-c(1:4),]$`97.5 %`)
tsc$predictor <- as.character(tsc$predictor)#change predictor to character to allow editing
tsc$predictor[8] <- "sexM:chla"#rename interaction terms to facilitate binding
tsc$predictor[9] <- "sexM:moci_spring"
tsc$predictor <- as.factor(tsc$predictor)#change back to factor
ndc <- data.frame(predictor = attr(fixef(ndm), "names"),
                  Ncoefficient = as.vector(fixef(ndm)),
                  NLCL = ndci[-c(1:2),]$`2.5 %`,
                  NUCL = ndci[-c(1:2),]$`97.5 %`)
ndc$predictor <- as.character(ndc$predictor)#change predictor to character to allow editing
ndc$predictor[7] <- "sexM:cuti"#rename interaction terms to facilitate binding
ndc$predictor[8] <- "sexM:moci_spring"
ndc$predictor <- as.factor(ndc$predictor)#change back to factor
environmentalCoefficients <- left_join(pmc, epc, by = "predictor")#merge as list
environmentalCoefficients <- left_join(environmentalCoefficients, tsc, by = "predictor")
environmentalCoefficients <- left_join(environmentalCoefficients, ndc, by = "predictor")
save(environmentalCoefficients, file = "output/environmentalCoefficients.rda")#save list
#====
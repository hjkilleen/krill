#Environmental model
#Model environmental drivers of krill length
# Tue Jul 13 14:29:46 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(lme4)
library(tidyverse)
library(arm)
library(MuMIn)
library(sjPlot)
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
ts <- ts[,-18]#drop temp_100 column
tsc <- ts[complete.cases(ts),]#filter to only complete cases
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
tab_model(epm, title = "E. pacifica", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))

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
tab_model(tsm, title = "T. spinifera", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))

#Nematocelis difficilis
#Optimize random effects structure using maximum likelihood
nd.int <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1|station), data = ndc, na.action = na.fail, REML = FALSE) #model with random intercept 

nd.intSlope <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + sst_sd + sex:sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1+temp_100|station), data = ndc, na.action = na.fail, REML = FALSE) #model with random intercept 
#Singular fit
#Slope intercept is the optimal random effects structure

#Optimize fixed effect structure using AIC
nd.model.set <- dredge(nd.int)

nd.top.model <- get.models(nd.model.set, subset = 1)
ndm <- nd.top.model[[1]]
ndci <- as.data.frame(confint(ndm))
tab_model(ndm, title = "N. difficilis", show.p = FALSE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))

#Pooled species
#Optimize random effects structure using maximum likelihood
#Terms that did not appear in species models removed from global model. 
p.int <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1|station), data = pc, na.action = na.fail, REML = FALSE) #model with random intercept 

p.intSlope <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + sex:chla + moci_spring + sex:moci_spring + cuti + sex:cuti + (1+temp_2|station), data = pc, na.action = na.fail, REML = FALSE) #model with random intercept 
anova(p.int, p.intSlope)#compare models with different random effect structure
#Slope intercept is the optimal random effects structure
pm <- p.intSlope
pci <- as.data.frame(confint(pm))
#Save models
save(epm, file = "output/environmentalEP.rda")
save(tsm, file = "output/environmentalTS.rda")
save(ndm, file = "output/environmentalND.rda")
save(pm, file = "output/environmentalPooled.rda")
#====

#EXTRACT SUMMARY STATISTICS
#====
#Extract model coefficients for fixed effects
pmc <- data.frame(predictor = attr(fixef(pm), "names"),#extract fixed effects coefficients
                  Pcoefficient = as.vector(fixef(pm)),
                  PLCL = pci[-c(1:4),]$`2.5 %`,
                  PUCL = pci[-c(1:4),]$`97.5 %`)
epc <- data.frame(predictor = attr(fixef(epm), "names"),
                  Ecoefficient = as.vector(fixef(epm)),
                  ELCL = epci[-c(1:4),]$`2.5 %`,
                  EUCL = epci[-c(1:4),]$`97.5 %`)
epc$predictor <- as.character(epc$predictor)#change predictor to character to allow editing
epc$predictor <- as.factor(epc$predictor)#change back to factor
tsc <- data.frame(predictor = attr(fixef(tsm), "names"),
                  Tcoefficient = as.vector(fixef(tsm)),
                  TLCL = tsci[-c(1:4),]$`2.5 %`,
                  TUCL = tsci[-c(1:4),]$`97.5 %`)
tsc$predictor <- as.character(tsc$predictor)#change predictor to character to allow editing
tsc$predictor[7] <- "sexM:chla"#rename interaction terms to facilitate binding
tsc$predictor[8] <- "sexM:moci_spring"
tsc$predictor <- as.factor(tsc$predictor)#change back to factor
ndc <- data.frame(predictor = attr(fixef(ndm), "names"),
                  Ncoefficient = as.vector(fixef(ndm)),
                  NLCL = ndci[-c(1:2),]$`2.5 %`,
                  NUCL = ndci[-c(1:2),]$`97.5 %`)
ndc$predictor <- as.character(ndc$predictor)#change predictor to character to allow editing
ndc$predictor[6] <- "sexM:chla"#rename interaction terms to facilitate binding
ndc$predictor[7] <- "sexM:cuti"
ndc$predictor[8] <- "sexM:moci_spring"
ndc$predictor <- as.factor(ndc$predictor)#change back to factor
environmentalCoefficients <- left_join(pmc, epc, by = "predictor")#merge as list
environmentalCoefficients <- left_join(environmentalCoefficients, tsc, by = "predictor")
environmentalCoefficients <- left_join(environmentalCoefficients, ndc, by = "predictor")
save(environmentalCoefficients, file = "output/environmentalCoefficients.rda")#save list
#====

#Random Effects Statistical Testing 
#====
#Set up dataframes
regions <- read_csv("data/regions.csv")

#Ep
ep.ranef <- ranef(epm)$station
ep.ranef$station <- as.numeric(rownames(ep.ranef))
names(ep.ranef) <- c("intercept", "temp_2", "station")
ep.ranef <- left_join(ep.ranef, regions)
ep.ranef$region[ep.ranef$region=="north"] <- "core"
ep.ranef$region[ep.ranef$region=="north_central"] <- "core"

#Ts
ts.ranef <- ranef(tsm)$station
ts.ranef$station <- as.numeric(rownames(ts.ranef))
names(ts.ranef) <- c("intercept", "temp_2", "station")
ts.ranef <- left_join(ts.ranef, regions)
ts.ranef$region[ts.ranef$region=="north"] <- "core"
ts.ranef$region[ts.ranef$region=="north_central"] <- "core"

#Nd
nd.ranef <- ranef(ndm)$station
nd.ranef$station <- as.numeric(rownames(nd.ranef))
names(nd.ranef) <- c("intercept", "station")
nd.ranef <- left_join(nd.ranef, regions)
nd.ranef$region[nd.ranef$region=="north"] <- "core"
nd.ranef$region[nd.ranef$region=="north_central"] <- "core"

#Two-sample wilcoxon test for onshore vs offshore
#intercepts
wilcox.test(intercept~shore, ep.ranef, alternative = "two.sided")
wilcox.test(intercept~shore, ts.ranef, alternative = "two.sided")
wilcox.test(intercept~shore, nd.ranef, alternative = "two.sided")

#slope
wilcox.test(temp_2~shore, ep.ranef, alternative = "two.sided")
wilcox.test(temp_2~shore, ts.ranef, alternative = "two.sided")
wilcox.test(temp_2~shore, filter(ts.ranef, latitude<38), alternative = "two.sided")

#Kruskal-Wallis test for regional variation
#intercepts
#Ep
kruskal.test(intercept ~ region, data = ep.ranef)
pairwise.wilcox.test(ep.ranef$intercept, ep.ranef$region, p.adjust.method = "BH")#pairwise wilcoxon test
ggplot(ep.ranef) + 
  geom_boxplot(aes(x = region, y = intercept))

#Ts
kruskal.test(intercept ~ region, data = ts.ranef)

#Nd
kruskal.test(intercept ~ region, data = nd.ranef)

#slope
#Ep
kruskal.test(temp_2 ~ region, data = ep.ranef)
pairwise.wilcox.test(ep.ranef$temp_2, ep.ranef$region, p.adjust.method = "BH")#pairwise wilcoxon test
ggplot(ep.ranef) + 
  geom_boxplot(aes(x = region, y = temp_2))

#Ts
kruskal.test(temp_2 ~ region, data = ts.ranef)

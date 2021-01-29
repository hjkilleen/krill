#Table 3
#Environmental model results

# Wed Jan 27 17:44:51 2021 ------------------------------

#LIBRARIES
#====
library(tidyverse)
library(dplyr)
library(ggpubr)
load("output/environmentalCoefficients.rda")
#====

#SETUP
#====
envCoef <- dplyr::select(environmentalCoefficients, "Ecoefficient", "Tcoefficient", "Ncoefficient", "Pcoefficient")
rownames(envCoef) <- environmentalCoefficients$predictor
envCoef <- envCoef[-c(1:2),]#remove intercept score
envCoef <- as.matrix(envCoef)
#====

#TABLE
#====
superheat(envCoef, heat.pal = c("#b35806", "white", "#542788"))
#====

#SUBFIGURE B - SEXUAL DIMORPHISM
#====

#====

#MERGE FIGURES
#====
#====
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
names(envCoef) <- c("E. pacifica", "T. spinifera", "N. difficilis", "Pooled")
envCoef <- arrange(envCoef, -row_number())#reverse row order

envCoef <- envCoef[-c(12:13),]#remove intercept score
vals <- envCoef#copy
vals[is.na(vals)] <- 0#make matrix of coefficient values

ecMat <- as.matrix(envCoef)#transform to matrix for heatmap
vals <- as.matrix(vals)
#====

#TABLE
#====
t <- superheat(ecMat, 
          heat.pal = c("#b35806", "white", "#542788"), 
          X.text = round(vals, 3), 
          heat.na.col = "black",
          row.title = "Environmental Predictor",
          row.title.size = 10)
#====

#SAVE 
#====
png("figures/manuscript/table3_environmentalModel.png", height = 900, width = 800)
t
dev.off()
#====
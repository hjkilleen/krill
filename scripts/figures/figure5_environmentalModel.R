#Figure5
#Environmental model results

# Wed Jan 27 17:44:51 2021 ------------------------------

#LIBRARIES
#====
library(tidyverse)
library(dplyr)
library(superheat)
load("output/environmentalCoefficients.rda")
#====

#SETUP
#====
envCoef <- dplyr::select(environmentalCoefficients, "Ecoefficient", "Tcoefficient", "Ncoefficient")
rownames(envCoef) <- environmentalCoefficients$predictor
names(envCoef) <- c("E. pacifica", "T. spinifera", "N. difficilis")
envCoef <- arrange(envCoef, -row_number())#reverse row order

envCoef <- envCoef[-c(12:13),]#remove sex & intercept score
vals <- envCoef#copy
vals[is.na(vals)] <- 0#make matrix of coefficient values

ecMat <- as.matrix(envCoef)#transform to matrix for heatmap
vals <- as.matrix(vals)
#====

#TABLE
#====
superheat(ecMat, 
          heat.pal = c("#b35806", "white", "#542788"), 
          X.text = round(vals, 3), 
          heat.na.col = "black",
          row.title = "Environmental Predictor",
          X.text.size = 8,
          #row.title = "Predictor",
          row.title.size = 11, print.plot = TRUE, left.label.text.size = 6, bottom.label.text.size = 6, legend.text.size = 20)
#====

#SAVE 
#====
png("figures/manuscript/figure5_environmentalModel.png", height = 800, width = 1100)
superheat(ecMat, 
          heat.pal = c("#b35806", "white", "#542788"), 
          X.text = round(vals, 3), 
          heat.na.col = "black",
          row.title = "Environmental Predictor",
          X.text.size = 8,
          row.title.size = 11, print.plot = TRUE, left.label.text.size = 6, bottom.label.text.size = 10, legend.text.size = 20)
dev.off()
#====
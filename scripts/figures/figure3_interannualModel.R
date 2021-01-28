#Figure 3
#Interannual model results

# Wed Jan 27 17:44:51 2021 ------------------------------

#LIBRARIES
#====
library(tidyverse)
library(ggpubr)
load("data/interannualPooled.rda")
load("data/interannualEP.rda")
load("data/interannualTS.rda")
load("data/interannualND.rda")
load("data/interannualCoefficients.rda")
#====

#SETUP
#====

#====

#SUBFIGURE A - COEFFICIENTS
#====

#====

#SUBFIGURE B - PLOT
#====
pmsum <- summarize(group_by(pmsimsum, year), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
epsum <- summarize(group_by_at(epsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
tssum <- summarize(group_by_at(tssimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ndsum <- summarize(group_by_at(ndsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))

ggplot() + 
  geom_line(data = pmsum, aes(x = as.numeric(year), y = sim.mean), color = "grey", size = 2) +
  geom_line(data = epsum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "#E69F00", size = 2) +
  geom_line(data = tssum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "#56B4E9", size = 2) +
  geom_line(data = ndsum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "#009E73", size = 2) + 
  labs(y = "Length (mm)", x = "Year", linetype = "Sex") +
  ylim(min = 18, max = 30) + 
  theme(text = element_text(size = 20),
        legend.position = c(0.8, 0.82))
#====

#SUBFIGURE C - SEXUAL DIMORPHISM
#====

#====

#MERGE FIGURES
#====

#====
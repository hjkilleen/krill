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
pmsum <- summarize(group_by_at(pmsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))#group simulated data for plotting
epsum <- summarize(group_by_at(epsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
tssum <- summarize(group_by_at(tssimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ndsum <- summarize(group_by_at(ndsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))

pdiffs <- dcast(pmsum, year~sex, value.var = "sim.mean", mean)#gather differences between simulated male and female lengths 
pdiffs$diff <- pdiffs$F - pdiffs$M
pdiffs$prop <- pdiffs$diff/mean(pdiffs$F)
pdiffs$species <- rep("P", nrow(pdiffs))

epdiffs <- dcast(epsum, year~sex, value.var = "sim.mean", mean)
epdiffs$diff <- epdiffs$F - epdiffs$M
epdiffs$prop <- epdiffs$diff/mean(epdiffs$F)
epdiffs$species <- rep("EP", nrow(epdiffs))

tsdiffs <- dcast(tssum, year~sex, value.var = "sim.mean", mean)
tsdiffs$diff <- tsdiffs$F - tsdiffs$M
tsdiffs$prop <- tsdiffs$diff/mean(tsdiffs$F)
tsdiffs$species <- rep("TS", nrow(tsdiffs))

nddiffs <- dcast(ndsum, year~sex, value.var = "sim.mean", mean)
nddiffs$diff <- nddiffs$F - nddiffs$M
nddiffs$prop <- nddiffs$diff/mean(nddiffs$F)
nddiffs$species <- rep("ND", nrow(nddiffs))

diffs <- rbind(epdiffs, tsdiffs)#bind species and pooled differences
diffs <- rbind(diffs, nddiffs)
diffs <- rbind(diffs, pdiffs)

x <- summarize(group_by_at(diffs, vars(year, species)), mean = mean(prop))
#====

#SUBFIGURE A - COEFFICIENTS
#====

#====

#SUBFIGURE B - PLOT
#====
b <- ggplot() + 
  geom_line(data = pmsum, aes(x = as.numeric(as.character(year)), y = sim.mean, color = "grey"), size = 2) +
  geom_line(data = epsum, aes(x = as.numeric(as.character(year)), y = sim.mean, linetype = sex, color = "#E69F00"), size = 2) +
  geom_line(data = tssum, aes(x = as.numeric(as.character(year)), y = sim.mean, linetype = sex, color = "#56B4E9"), size = 2) +
  geom_line(data = ndsum, aes(x = as.numeric(as.character(year)), y = sim.mean, linetype = sex, color = "#009E73"), size = 2) + 
  labs(y = "Length (mm)", x = "Year", linetype = "Sex") +
  ylim(min = 18, max = 30) + 
  scale_linetype_discrete(guide = guide_legend(override.aes = list(size = 1, color = "black"))) + 
  scale_color_identity(guide = "legend", labels = c("N. difficilis", "T. spinifera", "E. pacifica", "Pooled"), name = "Species") +
  theme(text = element_text(size = 20))
#====

#SUBFIGURE C - SEXUAL DIMORPHISM
#====
c <- ggplot(x) + 
  geom_line(aes(x = year, y = mean, group = species, color = species), size = 2) + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(name = "Species", values = c("#E69F00", "#009E73", "grey", "#56B4E9"), labels = c("E. pacifica", "N. difficilis","Pooled", "T. spinifera")) + 
  labs(x = "Year", y = "Size difference F - M (% of mean length)") +
  theme(text = element_text(size = 20), legend.position = "none")
#====

#MERGE FIGURES
#====

#====
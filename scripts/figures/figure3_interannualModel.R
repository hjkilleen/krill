#Figure 3
#Interannual model results

# Wed Jan 27 17:44:51 2021 ------------------------------

#LIBRARIES
#====
library(tidyverse)
library(ggpubr)
load("output/interannualPooled.rda")
load("output/interannualEP.rda")
load("output/interannualTS.rda")
load("output/interannualND.rda")
load("output/aleScale.rda")
load("output/epScale.rda")
load("output/tsScale.rda")
load("output/ndScale.rda")
load("output/interannualCoefficients.rda")
#====

#SETUP
#====
pmsum <- summarize(group_by_at(pmsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))#group simulated data for plotting
epsum <- summarize(group_by_at(epsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
tssum <- summarize(group_by_at(tssimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ndsum <- summarize(group_by_at(ndsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))

aleStMeans <- summarize(group_by_at(allLengthsEnv, vars(station, year, species)), stationMean = mean(length))#generate mean values for each species, year, station combination to plot with simulated data. 

# pdiffs <- dcast(pmsum, year~sex, value.var = "sim.mean", mean)#gather differences between simulated male and female lengths 
# pdiffs$diff <- pdiffs$F - pdiffs$M
# pdiffs$prop <- pdiffs$diff/mean(pdiffs$F)
# pdiffs$species <- rep("P", nrow(pdiffs))

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
#diffs <- rbind(diffs, pdiffs)

x <- summarize(group_by_at(diffs, vars(year, species)), mean = mean(prop))
x <- x[!(x$species == "ND" & x$year %in% c("2011", "2012", "2013")),]#filter out N. difficilis 2011-2013 due to lack of males

pmsum <- summarize(group_by_at(pmsimsum, vars(year)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))#group simulated data for plotting without sex
epsum <- summarize(group_by_at(epsimsum, vars(year)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
tssum <- summarize(group_by_at(tssimsum, vars(year)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ndsum <- summarize(group_by_at(ndsimsum, vars(year)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
#====

#SUBFIGURE A - PLOT
#====
a <- ggplot() + 
  geom_line(data = pmsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(aleScale, "scaled:scale") + attr(aleScale, "scaled:center")), color = "grey"), size = 2) +
  geom_line(data = epsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(epScale, "scaled:scale") + attr(epScale, "scaled:center")), color = "#E69F00"), size = 2) +
  geom_line(data = tssum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(tsScale, "scaled:scale") + attr(tsScale, "scaled:center")), color = "#56B4E9"), size = 2) +
  geom_line(data = ndsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(ndScale, "scaled:scale") + attr(ndScale, "scaled:center")), color = "#009E73"), size = 2) + 
  geom_point(data = filter(aleStMeans, species == "EP"), aes(x = as.numeric(as.character(year)), y = (stationMean*attr(aleScale, "scaled:scale") + attr(aleScale, "scaled:center")), color = "#E69F00"), alpha = 1) +
  geom_point(data = filter(aleStMeans, species == "TS"), aes(x = as.numeric(as.character(year)), y = (stationMean*attr(aleScale, "scaled:scale") + attr(aleScale, "scaled:center")), color = "#56B4E9"), alpha = 1) +
  geom_point(data = filter(aleStMeans, species == "ND"), aes(x = as.numeric(as.character(year)), y = (stationMean*attr(aleScale, "scaled:scale") + attr(aleScale, "scaled:center")), color = "#009E73"), alpha = 1) +
  labs(y = "Length (mm)", x = "Year", linetype = "Sex") +
  ylim(min = 19, max = 26) + 
  scale_linetype_discrete(guide = guide_legend(override.aes = list(size = 1, color = "black"))) + 
  scale_color_identity(guide = "legend", labels = c("N. difficilis", "T. spinifera", "E. pacifica", "Pooled"), name = "Species") +
  geom_rect(aes(xmin = 2013.5, ymin = 19, xmax = 2014.5, ymax = 26), color = "white", fill = "white") +
  theme_classic(base_size = 20)
a <- a + theme(legend.position = "top", axis.title = element_blank())
#====

#SUBFIGURE B - SEXUAL DIMORPHISM
#====
b <- ggplot(x) + 
  geom_bar(aes(x = as.numeric(as.character(year)), y = mean, group = species, fill = species), stat = 'identity', position = 'dodge') + 
  geom_hline(yintercept = 0) + 
  scale_fill_manual(name = "Species", values = c("#E69F00", "#009E73", "#56B4E9"), labels = c("E. pacifica", "N. difficilis", "T. spinifera")) + 
  labs(x = "Year", y = "Size difference\nF - M (% of mean length)") +
  theme_classic(base_size = 20)
b <- b + theme(legend.position = "none")
#====

#MERGE FIGURES
#====
ggarrange(a, b, ncol = 1, nrow = 2, align = "v", labels = c("A", "B"))
#====
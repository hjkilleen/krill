#Figure 3
#Interannual model results

# Wed Jan 27 17:44:51 2021 ------------------------------

#LIBRARIES
#====
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(reshape2)
library(superheat)
load("output/interannualPooled.rda")
load("output/interannualEP.rda")
load("output/interannualTS.rda")
load("output/interannualND.rda")
load("output/aleScale.rda")
load("output/epScale.rda")
load("output/tsScale.rda")
load("output/ndScale.rda")
load("output/interannualCoefficients.rda")
load("output/interannualCoefficients_noSex.rda")
load("data/allLengthsEnv.rda")
#====

#SETUP
#====
ic.noSex <- data.frame(ep = interannualCoefficients.noSex[[2]][,2],#create matrix for heatmap of year effects without sex interaction
                 ts = interannualCoefficients.noSex[[3]][,2],
                 nd = interannualCoefficients.noSex[[4]][,2])
ic.noSex[2:7,1] <- ic.noSex[2:7,1] + ic.noSex$ep[1]#add or subtract intercept value, so all coefficients are relative to 0 in 2011
ic.noSex[2:7,2] <- ic.noSex[2:7,2] + ic.noSex$ts[1]
ic.noSex[2:7,3] <- ic.noSex[2:7,3] + ic.noSex$nd[1]
row.names(ic.noSex) <- c("2011", "2012", "2013", "2015", "2016", "2017", "2018")#change column names
names(ic.noSex) <- c("E. pacifica", "T. spinifera", "N. difficilis")#change row names
icMat.noSex <- as.matrix(ic.noSex)

epsum <- summarize(group_by_at(epsimsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))#group simulated data for plotting with sex interaction
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
#====

#SUBFIGURE A - INTERANNUAL COEFFICIENTS
#====
a <- superheat(icMat.noSex, 
          scale = TRUE,
          heat.pal = c("#b35806", "white", "#542788"), 
          X.text = round(icMat.noSex, 3), 
          X.text.size = 8,
          heat.na.col = "black",
          #row.title = "Predictor",
          row.title.size = 20, print.plot = TRUE, left.label.text.size = 6, bottom.label.text.size = 6, legend.text.size = 20)
#====

#SUBFIGURE B - TIME SERIES PLOT
#====
b <- ggplot() + 
  geom_rect(data = epsum, aes(xmin = 2014.5, ymin = -Inf, xmax = 2015.5, ymax = Inf), fill = "red", alpha = 0.01) +
  geom_line(data = epsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(epScale, "scaled:scale") + attr(epScale, "scaled:center")), color = "#E69F00", linetype = sex), size = 2) +
  geom_line(data = tssum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(tsScale, "scaled:scale") + attr(tsScale, "scaled:center")), color = "#56B4E9", linetype = sex), size = 2) +
  geom_line(data = ndsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(ndScale, "scaled:scale") + attr(ndScale, "scaled:center")), color = "#009E73", linetype = sex), size = 2) + 
  #geom_point(data = filter(aleStMeans, species == "EP"), aes(x = as.numeric(as.character(year)), y = (stationMean*attr(aleScale, "scaled:scale") + attr(aleScale, "scaled:center")), color = "#E69F00"), alpha = 1) +
  #geom_point(data = filter(aleStMeans, species == "TS"), aes(x = as.numeric(as.character(year)), y = (stationMean*attr(aleScale, "scaled:scale") + attr(aleScale, "scaled:center")), color = "#56B4E9"), alpha = 1) +
  #geom_point(data = filter(aleStMeans, species == "ND"), aes(x = as.numeric(as.character(year)), y = (stationMean*attr(aleScale, "scaled:scale") + attr(aleScale, "scaled:center")), color = "#009E73"), alpha = 1) +
  labs(y = "Length (mm)", x = "Year", linetype = "Sex") +
  geom_rect(aes(xmin = 2013.5, ymin = -Inf, xmax = 2014.5, ymax = Inf), color = "white", fill = "white") +
  #ylim(min = 19, max = 26) + 
  scale_linetype_discrete(guide = guide_legend(override.aes = list(size = 1, color = "black"))) + 
  scale_color_identity(guide = "legend", labels = c("N. difficilis", "T. spinifera", "E. pacifica"), name = "Species") +
  theme_classic(base_size = 20)
b <- b + theme(legend.position = "none", axis.title.x = element_blank())
#====

#SUBFIGURE C - SEXUAL DIMORPHISM
#====
c <- ggplot(x) + 
  geom_rect(aes(xmin = 2014.5, ymin = -Inf, xmax = 2015.5, ymax = Inf), fill = "red", alpha = 0.01) +
  geom_bar(aes(x = as.numeric(as.character(year)), y = mean, group = species, fill = species), stat = 'identity', position = 'dodge') + 
  geom_hline(yintercept = 0) + 
  scale_fill_manual(name = "Species", values = c("#E69F00", "#009E73", "#56B4E9"), labels = c("E. pacifica", "N. difficilis", "T. spinifera")) + 
  labs(x = "Year", y = "Size difference\nF - M (% of mean length)") +
  annotate("text", x = 2011, y = 2.8, label = "Females\nlarger", size = 5) + 
  annotate("text", x = 2011, y = -.5, label = "Males larger", size = 5) +
  theme_classic(base_size = 20)
c <- c + theme(legend.position = "none")
#====

#LEGENDS
#====
get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p1 <- ggplot() + #plot to establish color legend
  geom_line(data = epsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(epScale, "scaled:scale") + attr(epScale, "scaled:center")), color = "#E69F00"), size = 2) +
  geom_line(data = tssum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(tsScale, "scaled:scale") + attr(tsScale, "scaled:center")), color = "#56B4E9"), size = 2) +
  geom_line(data = ndsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(ndScale, "scaled:scale") + attr(ndScale, "scaled:center")), color = "#009E73"), size = 2) +
  scale_color_identity(guide = "legend", labels = c("N. difficilis", "T. spinifera", "E. pacifica"), name = "Species") + 
  theme(legend.direction = "horizontal", legend.text = element_text(size = 15), legend.title = element_text(size = 15))
l1 <- get_legend(p1)#extract color legend

p2 <- ggplot() + #plot for linetype legend
  geom_line(data = epsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(epScale, "scaled:scale") + attr(epScale, "scaled:center")), linetype = sex), size = 2) +
  geom_line(data = tssum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(tsScale, "scaled:scale") + attr(tsScale, "scaled:center")), linetype = sex), size = 2) +
  geom_line(data = ndsum, aes(x = as.numeric(as.character(year)), y = (sim.mean*attr(ndScale, "scaled:scale") + attr(ndScale, "scaled:center")), linetype = sex), size = 2) + 
  labs(y = "Length (mm)", x = "Year", linetype = "Sex") +
  scale_linetype_discrete(guide = guide_legend(override.aes = list(size = 1, color = "black"))) + 
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15), legend.background = element_rect(fill = "transparent", color = "transparent"))
l2 <- get_legend(p2)#extract linetype legend
#====

#MERGE FIGURES
#====
b <- b + annotation_custom(grob = l2, xmin = 2010.5, xmax = 2012, ymin = 26, ymax = 28)
b <- arrangeGrob(l1, b, heights = c(1, 10))

ggarrange(a$plot, ggarrange(b, c, nrow = 2, align = "v", labels = c("B", "C"), font.label = list(size = 20)), nrow = 1, labels = "A", font.label = list(size = 20))#arrange subfigures
ggsave("figures/manuscript/figure3_interannualModel.jpeg", width = 14, height = 8, dpi = 300)
#====


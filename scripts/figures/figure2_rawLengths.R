#Figure 2
#Krill raw length frequencies
# Wed Feb 17 09:23:04 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(ggpubr)
library(tidyverse)
load("data/allLengthsEnv.rda")
source("scripts/functions/length_frequency.R")
#====

#SUBFIGURE A
#====
#Histogram of krill lengths by species
aLE_tally <- add_tally(group_by_at(allLengthsEnv, vars(year, species)))
aLE_tally$species = factor(aLE_tally$species, levels=c("EP", "TS", "ND"))
allLengthsEnv$species = factor(allLengthsEnv$species, levels=c("EP", "TS", "ND"))
labs <- c("E. pacifica", "T. spinifera", "N. difficilis")
names(labs) <- c("EP", "TS", "ND")
a <- ggplot(allLengthsEnv, aes(x = as.factor(year), y = length.unscaled, group = year, fill = as.factor(year))) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values = c("#ffffff80", "#00000080", "#ffffff80", "#00000080", "#ffffff80", "#00000080", "#ffffff80"), labels = c("2011", "2012", "2013", "2015", "2016", "2017", "2018")) +
  facet_wrap(.~species, nrow = 3, ncol = 1, labeller = labeller(species = labs)) + 
  geom_text(data = summarize(group_by_at(aLE_tally, vars(species, year)), n=mean(nnn), max = max(length.unscaled)), aes(x = as.factor(year), y = max + 5, label = paste("n=", n, sep = " ")), color = "black", size = 4) +
  geom_text(data = summarize(group_by_at(aLE_tally, vars(year, species)), mean = round(mean(length.unscaled), 1), max = max(length.unscaled)), aes(x = as.factor(year), y = max + 13, label = paste("mean=", mean, sep = " ")), color = "black", size = 4) +
  labs(x = "Year", y = "Length (mm)") + 
  theme_bw() +
  theme(strip.text = element_text(face = "italic")) +
  theme(text = element_text(size = 20), legend.position = "none")
#====

#SUBFIGURE B
#====
#CV by year
#=====
x <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(year, species)), cv = cv(length.unscaled)))
x$year <- as.factor(x$year)
x$species = factor(x$species, levels=c("EP", "TS", "ND"))
b <- ggplot(x, aes(x = year, y = cv, color = species, group = species)) +
  geom_point(size = 2) + 
  geom_line() + 
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73"), labels = c("E. pacifica", "T. spinifera", "N. difficilis")) +
  labs(x = "Year", y = "Coefficient\nof Variation", color = "Species") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "bottom")
#====

#MERGED FIGURE
#====
ggarrange(a, b, labels = c("A", "B"), ncol = 1, nrow = 2, heights = c(10, 4), align = "v", label.x = .05)
ggsave("figures/manuscript/figure2_krillViolinCV.jpeg", width = 8, height = 11, dpi = 400)
#====

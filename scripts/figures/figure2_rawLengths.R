#Figure 2
#Krill raw length frequencies

# Wed Jan 27 15:52:46 2021 ------------------------------
#LIBRARIES & SOURCES
#====
library(ggpubr)
load("data/allLengthsEnv.rda")
#====

#SUBFIGURE A
#====
#Histogram of krill lengths by species
aLE_tally <- add_tally(group_by_at(allLengthsEnv, vars(year, species)))
labs <- c("E. pacifica", "T. spinifera", "N. difficilis")
names(labs) <- c("EP", "TS", "ND")
a <- ggplot(filter(allLengthsEnv), aes(x = as.factor(year), y = length, group = year, fill = as.factor(year))) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values = c("#ffffff80", "#00000080", "#ffffff80", "#00000080", "#ffffff80", "#00000080", "#ffffff80"), labels = c("2011", "2012", "2013", "2015", "2016", "2017", "2018")) +
  facet_wrap(.~species, nrow = 3, ncol = 1, labeller = labeller(species = labs)) + 
  geom_text(data = summarize(group_by_at(aLE_tally, vars(species, year)), n=mean(nnn), max = max(length)), aes(x = as.factor(year), y = max + 5, label = paste("n=", n, sep = " ")), color = "black", size = 4) +
  geom_text(data = summarize(group_by_at(aLE_tally, vars(year, species)), mean = round(mean(length), 1), max = max(length)), aes(x = as.factor(year), y = max + 10, label = paste("mean=", mean, sep = " ")), color = "black", size = 4) +
  labs(x = "Year", y = "Length (mm)") + 
  theme(text = element_text(size = 20), legend.position = "none")
#====

#SUBFIGURE B
#====
#CV by year
#=====
x <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(year, species)), cv = cv(length)))
x$year <- as.factor(x$year)
x$species <- as.factor(x$species)
b <- ggplot(x, aes(x = year, y = cv, color = species, group = species)) +
  geom_point() + 
  geom_line() + 
  labs(x = "Year", y = "Coefficient\nof Variation", color = "Species") +
  theme(text = element_text(size = 15), legend.position = c(0.95, 0.7))
#====

#MERGED FIGURE
#====
ggarrange(a, b, labels = c("A", "B"), ncol = 1, nrow = 2, heights = c(2, 0.7), align = "v")
#====

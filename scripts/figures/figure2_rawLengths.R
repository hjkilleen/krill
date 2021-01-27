#Figure 2
#Krill raw length frequencies

# Wed Jan 27 15:52:46 2021 ------------------------------
#LIBRARIES & SOURCES
#====

#====

#SUBFIGURE A
#====

#====

#SUBFIGURE B
#====
#Histogram of krill lengths by species
aLE_tally <- add_tally(group_by_at(allLengthsEnv, vars(year, species)))
labs <- c("E. pacifica", "T. spinifera", "N. difficilis")
names(labs) <- c("EP", "TS", "ND")
ggplot(filter(allLengthsEnv), aes(x = as.factor(year), y = length, group = year, fill = as.factor(year))) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values = c("#ffffff80", "#00000080", "#00ffff80", "#ff000080", "#00ff0080", "#0000ff80", "#ffff0080"), labels = c("2011", "2012", "2013", "2015", "2016", "2017", "2018")) +
  facet_wrap(.~species, nrow = 3, ncol = 1, labeller = labeller(species = labs)) + 
  geom_text(data = summarize(group_by_at(aLE_tally, vars(species, year)), n=mean(n), max = max(length)), aes(x = as.factor(year), y = max + 5, label = paste("n=", n, sep = " ")), color = "black", size = 4) +
  geom_text(data = summarize(group_by_at(aLE_tally, vars(year, species)), mean = round(mean(length), 1), max = max(length)), aes(x = as.factor(year), y = max + 10, label = paste("mean=", mean, sep = " ")), color = "black", size = 4) +
  labs(x = "Year", y = "Length (mm)") + 
  theme(text = element_text(size = 20), legend.position = "none") +
  ggsave("figures/manuscript/fig1.jpg", width = 7, height = 9)
#====

#SUBFIGURE C
#====
#CV by year
#=====
ggplot(summarize(group_by_at(allLengthsEnv, vars(year, species)), cv = cv(length)), aes(x = year, y = cv, color = species)) +
  geom_point(group = "species") + 
  geom_smooth(se = FALSE) + 
  labs(x = "Year", y = "Coefficient of Variation(CV)") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/manuscript/fig2.jpg", width = 5, height = 5)
#====

#MERGED FIGURE
#====

#====
#AFS Figures
#Figures for presentation given at the American Fisheries Society Annual Meeting in September 2020

# Thu Aug 13 19:30:48 2020 ------------------------------
summaryM9 <- summary(M9, correlation = FALSE)
summaryM3 <- summary(M3, correlation = FALSE)
save(summaryM9, file = "output/M9.rda")
save(summaryM3, file = "output/M3.rda")
  
#Figure 1 - Mean species variation by year
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
  ggsave("figures/afs2020/fig1.jpg", width = 7, height = 9)

#Figure 2a - Differences among species in response to surface temperature
# simM9 <- fsim.glmm(M9)
# simsumM9 <- simsum(simM9)
sum <- summarize(group_by_at(simsumM9, vars(species, temp_2)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot() + 
  geom_point(data = allLengthsEnv, aes(x = temp_2, y = length, color = as.factor(species)), alpha = 0.1) + #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  #facet_wrap(as.factor(allLengthsEnv$species)) + 
  geom_smooth(data = sum, aes(x = temp_2, y = sim.mean, color = species)) + 
  labs(y = "Length (mm)", x = "Temp (C)", title = "Simulated krill lengths by temperature at surface") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig2a.jpg", width = 9, height = 7)

#Figure 2b - Differences among species in response to temperature at 100 m
sum <- summarize(group_by_at(simsumM9, vars(species, temp_100)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot() + 
  geom_point(data = allLengthsEnv, aes(x = temp_100, y = length, color = as.factor(species)), alpha = 0.1) + #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  #facet_wrap(as.factor(allLengthsEnv$species)) + 
  geom_smooth(data = sum, aes(x = temp_100, y = sim.mean, color = species)) + 
  labs(y = "Length (mm)", x = "Temp (C)", title = "Simulated krill lengths by temperature at 100 m") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig2b.jpg", width = 9, height = 7)

#Figure 3a - Model with different effects north and south of PC
x <- filter(allLengthsEnvDep, latitude >34.4)
y <- filter(allLengthsEnvDep, latitude <=34.4)
x <- mutate(x, NS = "N")
y <- mutate(y, NS = "S")
NS <-rbind(x, y)
MNS <- lmer(length ~ species*sex + temp_2 + species:temp_2 + NS:temp_2 + sex:temp_2 + temp_100 + species:temp_100 + NS:temp_100 + sex:temp_100 + dist + species:dist + (1|station.x), data = NS, REML = FALSE)

simMNS <- fsim.glmm(MNS)
simsumMNS <- simsum(simMNS)

ggplot() +
  geom_point(data = NS, aes(temp_2, length, color = species), alpha = 0.2) + 
  geom_smooth(data = simsumMNS, method = "lm", aes(x = temp_2, y = sim.mean, color = species)) +
  facet_wrap(~NS, nrow=1, ncol=2, scales = "free_y") +
  labs(y = "Length (mm)", x = "Temp (C)", title = "Krill lengths by temperature at surface N vs. S") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig3a.jpg", width = 9, height = 7)

#Figure 3b - Model with different effects north and south of PC
ggplot() +
  geom_point(data = NS, aes(temp_100, length, color = species), alpha = 0.2) + 
  geom_smooth(data = simsumMNS, method = "lm", aes(x = temp_100, y = sim.mean, color = species)) +
  facet_wrap(~NS, nrow=1, ncol=2, scales = "free_y") +
  labs(y = "Length (mm)", x = "Temp (C)", title = "Krill lengths by temperature at 100 m N vs. S") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig3b.jpg", width = 9, height = 7)

#Figure 4a - Differences among regions 
sum <- summarize(group_by_at(simsumM9, vars(species, temp_2, region)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot() + 
  geom_point(data = filter(allLengthsEnv, region != "NA"), aes(x = temp_2, y = length, color = as.factor(species)), alpha = 0.1) + #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  geom_smooth(data = sum, aes(x = temp_2, y = sim.mean, color = species)) + 
  facet_wrap(~region) +
  labs(y = "Length (mm)", x = "Temp (C)", title = "Simulated krill lengths by temperature at surface for each region") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig4a.jpg", width = 9, height = 7)

#Figure 4b - Differences among regions 
sum <- summarize(group_by_at(simsumM9, vars(species, temp_100, region)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot() + 
  geom_point(data = filter(allLengthsEnv, region != "NA"), aes(x = temp_100, y = length, color = as.factor(species)), alpha = 0.1) + #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  geom_smooth(data = sum, aes(x = temp_100, y = sim.mean, color = species)) + 
  facet_wrap(~region) +
  labs(y = "Length (mm)", x = "Temp (C)", title = "Simulated krill lengths by temperature at 100 m for each region") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig4b.jpg", width = 9, height = 7)

#4c - Differences among regions using year instead of temperature (M3)
# simM3 <- fsim.glmm(M3) 
# simsumM3 <- simsum(simM3)

sum <- summarize(group_by_at(simsumM3, vars(species, year, region)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot() + 
  geom_point(data = filter(allLengths, region != "NA"), aes(x = year, y = length, color = species), alpha = 0.1) + 
  facet_wrap(vars(region)) +   
  #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  geom_line(data = sum, aes(x = as.numeric(year), y = sim.mean, color = species)) + 
  geom_rect(data = sum, aes(xmin = 4, xmax = 6, ymin = 0, ymax = Inf), alpha = 0.01, fill = "red") +
  labs(y = "Length (mm)", x = "Year", title = "Krill lengths during a marine heatwave") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig4c.jpg", width = 9, height = 7)

#Figure 5a - Differences among regions using year instead of temperature (M3), sex
sum <- summarize(group_by_at(simsumM3, vars(species, year, region, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot() + 
  geom_point(data = filter(allLengths, region != "NA"), aes(x = year, y = length, color = species), alpha = 0.1) + 
  facet_wrap(vars(region)) +   
  #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  geom_line(data = sum, aes(x = as.numeric(year), y = sim.mean, color = species, linetype = sex)) + 
  geom_rect(data = sum, aes(xmin = 4, xmax = 6, ymin = 0, ymax = Inf), alpha = 0.01, fill = "red") +
  labs(y = "Length (mm)", x = "Year", title = "Krill lengths during a marine heatwave - sex") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig5a.jpg", width = 9, height = 7)

#Figure 5b - Same without 2013
sum <- filter(summarize(group_by_at(simsumM3, vars(species, year, region, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95)), year != 2013)
ggplot() + 
  geom_point(data = filter(allLengths, region != "NA", year !=2013), aes(x = year, y = length, color = species), alpha = 0.1) + 
  facet_wrap(vars(region)) +   
  #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  geom_line(data = sum, aes(x = as.numeric(year), y = sim.mean, color = species, linetype = sex)) + 
  geom_rect(data = sum, aes(xmin = 3, xmax = 5, ymin = 0, ymax = Inf), alpha = 0.01, fill = "red") +
  labs(y = "Length (mm)", x = "Year", title = "Krill lengths during a marine heatwave - sex") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig5b.jpg", width = 9, height = 7)

#Figure 6 - Differences among regions using year instead of temperature (M3), shore
sum <- summarize(group_by_at(simsumM3, vars(species, year, region, shore)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot() + 
  geom_point(data = filter(allLengths, region != "NA"), aes(x = year, y = length, color = species), alpha = 0.1) + 
  facet_wrap(vars(region)) +   
  #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  geom_line(data = sum, aes(x = as.numeric(year), y = sim.mean, color = species, linetype = shore)) + 
  geom_rect(data = sum, aes(xmin = 4, xmax = 6, ymin = 0, ymax = Inf), alpha = 0.01, fill = "red") +
  labs(y = "Length (mm)", x = "Year", title = "Krill lengths during a marine heatwave - shore") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig6a.jpg", width = 9, height = 7)

#Figure 6b - Same without 2013
sum <- filter(summarize(group_by_at(simsumM3, vars(species, year, region, shore)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95)), year != 2013)
ggplot() + 
  geom_point(data = filter(allLengths, region != "NA", year !=2013), aes(x = year, y = length, color = species), alpha = 0.1) + 
  facet_wrap(vars(region)) +   
  #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  geom_line(data = sum, aes(x = as.numeric(year), y = sim.mean, color = species, linetype = shore)) + 
  geom_rect(data = sum, aes(xmin = 3, xmax = 5, ymin = 0, ymax = Inf), alpha = 0.01, fill = "red") +
  labs(y = "Length (mm)", x = "Year", title = "Krill lengths during a marine heatwave - shore") +
  theme(text = element_text(size = 20)) + 
  ggsave("figures/afs2020/fig6b.jpg", width = 9, height = 7)

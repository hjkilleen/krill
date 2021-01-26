#AFS Figures
#Figures for presentation given at the American Fisheries Society Annual Meeting in September 2020
library(reshape2)

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

#Euphausia pacifica
Me1 <- lmer(length ~ year*sex + shore + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="EP"))

Me2 <- lmer(length ~ year + sex + shore + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="EP"))
#Model evaluation
anova(Me1, Me2)
sjPlot::tab_model(Me1, 
                  show.re.var= TRUE, 
                  dv.labels= "Spatial and Temporal Effects on Krill Length")
sjPlot::plot_model(Me1)
lattice::dotplot(ranef(Me1,condVar=TRUE))
save(Me1, file = "output/Me1.rda")

#Thysanoessa spinifera
Mt1 <- lmer(length ~ year*sex + shore + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="TS"))

Mt2 <- lmer(length ~ year*sex + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="TS"))
#Model evaluation
anova(Mt1, Mt2)
sjPlot::tab_model(Mt2, 
                  show.re.var= TRUE, 
                  dv.labels= "Spatial and Temporal Effects on Krill Length")
sjPlot::plot_model(Mt2)
lattice::dotplot(ranef(Mt2,condVar=TRUE))
save(Mt2, file = "output/Mt2.rda")

#Nematocelis difficilis
Mn1 <- lmer(length ~ year*sex + shore + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="ND"))

Mn2 <- lmer(length ~ year*sex + shore:year + (1|station), data = filter(allLengthsEnv, species =="ND"))
#Model evaluation
anova(Mn1, Mn2)
sjPlot::tab_model(Mn2, 
                  show.re.var= TRUE, 
                  dv.labels= "Spatial and Temporal Effects on Krill Length", file = "output/Mn2.doc")
sjPlot::plot_model(Mn2)
lattice::dotplot(ranef(Mn2,condVar=TRUE))
save(Mn2, file = "output/Mn2.rda")

#Simulation
Me1sim <- fsim.glmm(Me1, nsim = 1000)
Mt2sim <- fsim.glmm(Mt2, nsim = 1000)
Mn2sim <- fsim.glmm(Mn2, nsim = 1000)

Me1simsum <- simsum(Me1sim)
Mt2simsum <- simsum(Mt2sim)
Mn2simsum <- simsum(Mn2sim)

# Me1simsum$year <- as.numeric(Me1simsum$year)
# Mt2simsum$year <- as.numeric(Mt2simsum$year)
# Mn2simsum$year <- as.numeric(Mn2simsum$year)

#species differences
Me1sum <- summarize(group_by_at(Me1simsum, vars(year)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
Mt2sum <- summarize(group_by_at(Mt2simsum, vars(year)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
Mn2sum <- summarize(group_by_at(Mn2simsum, vars(year)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
x <- allLengthsEnv
x$year <- as.numeric(x$year)
ggplot() + 
  geom_point(data = allLengthsEnv, aes(x = year, y = length, color = species), alpha = 0.1) +
  geom_line(data = Me1sum, aes(x = as.numeric(year), y = sim.mean), color = "red") +
  geom_line(data = Mt2sum, aes(x = as.numeric(year), y = sim.mean), color = "blue") +
  geom_line(data = Mn2sum, aes(x = as.numeric(year), y = sim.mean), color = "green") + 
  annotate(geom = "rect", xmin = 3.5, xmax = 5.5, ymin = 10, ymax = Inf, alpha = 0.2, fill = "red") +
  labs(y = "Length (mm)", x = "Year") +
  ylim(min = 10, max = 35) + 
  theme(text = element_text(size = 20)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggsave("figures/afs2020/fig2.jpg", width = 8, height =8)

#sex differences
Me1sum <- summarize(group_by_at(Me1simsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
Mt2sum <- summarize(group_by_at(Mt2simsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
Mn2sum <- summarize(group_by_at(Mn2simsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))

ggplot() + 
  geom_point(data = allLengthsEnv, aes(x = year, y = length, color = species), alpha = 0.1) +
  geom_line(data = Me1sum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "red") +
  geom_line(data = Mt2sum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "blue") +
  geom_line(data = Mn2sum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "green") +
  annotate(geom = "rect", xmin = 3.5, xmax = 5.5, ymin = 10, ymax = Inf, alpha = 0.2, fill = "red") +
  labs(y = "Length (mm)", x = "Year") +
  ylim(min = 10, max = 35) + 
  theme(text = element_text(size = 20)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggsave("figures/afs2020/fig3.jpg", width = 8, height =8)

#differences plot
epdiffs <- dcast(Me1sum, year~sex, value.var = "sim.mean", mean)
epdiffs$diff <- epdiffs$F - epdiffs$M
epdiffs$prop <- epdiffs$diff/mean(epdiffs$F)
epdiffs$species <- rep("EP", nrow(epdiffs))

tsdiffs <- dcast(Mt2sum, year~sex, value.var = "sim.mean", mean)
tsdiffs$diff <- tsdiffs$F - tsdiffs$M
tsdiffs$prop <- tsdiffs$diff/mean(tsdiffs$F)
tsdiffs$species <- rep("TS", nrow(tsdiffs))


nddiffs <- dcast(Mn2sum, year~sex, value.var = "sim.mean", mean)
nddiffs$diff <- nddiffs$F - nddiffs$M
nddiffs$prop <- nddiffs$diff/mean(nddiffs$F)
nddiffs$species <- rep("ND", nrow(nddiffs))

diffs <- rbind(epdiffs, tsdiffs)
diffs <- rbind(diffs, nddiffs)

x <- summarize(group_by_at(diffs, vars(year, species)), mean = mean(prop))

ggplot(x) + 
  geom_line(aes(x = year, y = mean, group = species, color = species)) + 
  geom_hline(yintercept = 0) + 
  labs(x = "Year", y = "Size difference F - M (% of mean length)") +
  theme(text = element_text(size = 20)) +
  ggsave("figures/afs2020/fig4.jpg", width = 8, height =8)
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





#Maps
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
metadata <- read_csv("data/stationMetadata.csv")
metadata <- summarize(group_by(metadata, station), lat = mean(station_latitude), lon = mean(station_longitude))
#2015
Mel3simsum$station <- as.factor(Mel3simsum$station)
y <- summarize(group_by(Mel3simsum, station), temp_2 = mean(na.exclude(temp_2)), length = mean(na.exclude(sim.mean)))
y$station <- as.factor(y$station)
metadata$station <- as.factor(metadata$station)
x <- left_join(y, metadata)
allSites2015 <- na.exclude(c(sites2015$onshore, sites2015$offshore))
  ggplot(data = world) +
    geom_sf(color = "black", fill = "bisque2") +
    geom_sf(data = states, fill = NA) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    geom_point(data = i, aes(x = lon, y = lat), size = 2, 
               shape = 23, fill = "darkred") +
    geom_label(data = i, aes(x = lon, y = lat, label = station), nudge_y = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-125.5, -116.75), ylim = c(32.0, 42.30), expand = FALSE) + 
    theme(text = element_text(size = font_size))




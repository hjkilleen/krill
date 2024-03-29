
#Euphausia pacifica
Me1 <- lmer(length ~ year*sex + shore + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="EP"))

Me2 <- lmer(length ~ year + sex + shore + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="EP"))

Me3 <- lmer(length ~ year + sex + year:sex + (1|station), data = ep)
#Model evaluation
anova(Me1, Me2)
anova(Me3)
sjPlot::tab_model(Me3, 
                  show.re.var= TRUE, 
                  dv.labels= "Spatial and Temporal Effects on Krill Length")
sjPlot::plot_model(Me3)
lattice::dotplot(ranef(Me3,condVar=TRUE))
save(Me1, file = "output/Me1.rda")

#Thysanoessa spinifera
Mt1 <- lmer(length ~ year*sex + shore + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="TS"))

Mt2 <- lmer(length ~ year*sex + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="TS"))

Mt3 <- lmer(length ~ year + sex + year:sex + (1|station), data = ts)
#Model evaluation
anova(Mt1, Mt2)
sjPlot::tab_model(Mt3, 
                  show.re.var= TRUE, 
                  dv.labels= "Spatial and Temporal Effects on Krill Length")
sjPlot::plot_model(Mt2)
lattice::dotplot(ranef(Mt2,condVar=TRUE))
save(Mt2, file = "output/Mt2.rda")

#Nematocelis difficilis
Mn1 <- lmer(length ~ year*sex + shore + shore:year + shore:sex + (1|station), data = filter(allLengthsEnv, species =="ND"))

Mn2 <- lmer(length ~ year*sex + shore:year + (1|station), data = filter(allLengthsEnv, species =="ND"))

Mn3 <- lmer(length ~ year + sex + year:sex + (1|station), data = nd)
#Model evaluation
anova(Mn1, Mn2)
sjPlot::tab_model(Mn3, 
                  show.re.var= TRUE, 
                  dv.labels= "Spatial and Temporal Effects on Krill Length")
sjPlot::plot_model(Mn2)
lattice::dotplot(ranef(Mn2,condVar=TRUE))
save(Mn2, file = "output/Mn2.rda")

#Simulation

#shore differences
Me1sum <- summarize(group_by_at(Me1simsum, vars(year, shore)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
Mt2sum <- summarize(group_by_at(Mt2simsum, vars(year, shore)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
Mn2sum <- summarize(group_by_at(Mn2simsum, vars(year, shore)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))

shore <- ggplot() + 
  geom_point(data = allLengthsEnv, aes(x = year, y = length, color = species), alpha = 0.1) +
  geom_line(data = Me1sum, aes(x = as.numeric(year), y = sim.mean, linetype = shore), color = "red") +
  geom_line(data = Mt2sum, aes(x = as.numeric(year), y = sim.mean, linetype = shore), color = "blue") +
  geom_line(data = Mn2sum, aes(x = as.numeric(year), y = sim.mean, linetype = shore), color = "green") + 
  # geom_ribbon(data = Me1sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, linetype = sex), color = "red", alpha = 0.2) + 
  # geom_ribbon(data = Mt2sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, linetype = sex), color = "blue", alpha = 0.2) + 
  # geom_ribbon(data = Mn2sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, linetype = sex), color = "green", alpha = 0.2) +
  #geom_rect(data = Me1sum, aes(xmin = 2015, xmax = 2017, ymin = 0, ymax = Inf), alpha = 0.01, fill = "red") +
  labs(y = "Length (mm)", x = "Year") +
  ylim(min = 10, max = 35) +
  theme(text = element_text(size = 20))

#Combine plots and save
fig3 <- ggarrange(sex, shore, labels = c("A", "B"), ncol = 2, nrow = 1)
ggsave(fig3, file = "figures/manuscript/fig3.jpg", width= 12, height = 5)

#Environmental Model
#Full linear model
Ml1 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + sex:temp_2 + temp_100 + species:temp_100 + sex:temp_100 + (1|station), data = allLengthsEnv)
Ml2 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + sex:temp_2 + species:temp_100 + sex:temp_100 + (1|station), data = allLengthsEnv)
Ml3 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + sex:temp_2 + species:temp_100 + (1|station), data = allLengthsEnv)
#model comparison and evaluation
anova(Ml1, Ml2, Ml3)
sjPlot::tab_model(Ml2, 
                  show.re.var= TRUE, 
                  dv.labels= "Environmental Effects on Krill Length", file = "output/Ml2.doc")
sjPlot::plot_model(Ml2)
lattice::dotplot(ranef(Ml2,condVar=TRUE))
save(Ml2, file = "output/Ml2.rda")

Ml2sim <- fsim.glmm(Ml2)
Ml2simsum <- simsum(Ml2sim)

#EP Model
ep$year <- as.factor(ep$year)
ep$station <- as.factor(ep$station)
Mel1 <- lmer(length ~ year + sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = ep)
Mel2 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = ep)
Mel3 <- lmer(length ~ temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = ep)
Mel4 <- lmer(length ~ temp_2 + sex:temp_2 + sex:temp_100 + (1|station), data = ep)
sjPlot::plot_model(Mel3)

Mel3sim <- fsim.glmm(Mel3)
Mel3simsum <- simsum(Mel3sim)


#TS Model
ts$year <- as.factor(ts$year)
ts$station <- as.factor(ts$station)

Mtl1 <- lmer(length ~ year + sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = ts)
Mtl2 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = ts)
Mtl3 <- lmer(length ~ sex + temp_2 + sex:temp_2 + sex:temp_100 + (1|station), data = ts)
Mtl4 <- lmer(length ~ sex + temp_2 + sex:temp_2 + (1|station), data = ts)

sjPlot::plot_model(Mtl2)

Mtl2sim <- fsim.glmm(Mtl2)
Mtl2simsum <- simsum(Mtl2sim)

#ND Model
nd$year <- as.factor(nd$year)
nd$station <- as.factor(nd$station)

Mnl1 <- lmer(length ~ year + sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = nd)
Mnl2 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = nd)
Mnl3 <- lmer(length ~ temp_2 + sex:temp_2 + sex:temp_100 + temp_100 + (1|station), data = nd)

sjPlot::plot_model(Mnl2)

Mnl2sim <- fsim.glmm(Mnl2)
Mnl2simsum <- simsum(Mnl2sim)

ggplot(allLengthsEnv) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel3simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl2simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "blue") + 
  geom_line(data = summarize(group_by(Mnl2simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#N vs S same plot
x <- filter(allLengthsEnv, latitude >34.4)
y <- filter(allLengthsEnv, latitude <=34.4)

#north simulations
Mel3 <- lmer(length ~ temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = filter(x, species == "EP"))
Mtl2 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = filter(x, species == "TS"))
Mnl2 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = filter(x, species == "ND"))
Mel3sim <- fsim.glmm(Mel3)
Mel3simsum <- simsum(Mel3sim)
Mtl2sim <- fsim.glmm(Mtl2)
Mtl2simsum <- simsum(Mtl2sim)
Mnl2sim <- fsim.glmm(Mnl2)
Mnl2simsum <- simsum(Mnl2sim)

n <- ggplot(x) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel3simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl2simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "blue") + 
  geom_line(data = summarize(group_by(Mnl2simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#south simulations
Mel3 <- lmer(length ~ temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = filter(y, species == "EP"))
Mtl2 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = filter(y, species == "TS"))
Mnl2 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = filter(y, species == "ND"))
Mel3sim <- fsim.glmm(Mel3)
Mel3simsum <- simsum(Mel3sim)
Mtl2sim <- fsim.glmm(Mtl2)
Mtl2simsum <- simsum(Mtl2sim)
Mnl2sim <- fsim.glmm(Mnl2)
Mnl2simsum <- simsum(Mnl2sim)

s <- ggplot(y) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel3simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mnl2simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#together
plot <- ggarrange(n, s, labels = c("North", "South"), ncol = 2, nrow = 1)
ggsave(plot, file = "figures/afs2020/nVSs.jpg")

#explore temp_2 effect with a spline
library(splines)
Ms <- lm(length ~ bs(temp_2, degree = 2), data = allLengthsEnv)
Mes <- lm(length ~ bs(temp_2, degree = 2), data = filter(allLengthsEnv, species == "EP"))
Mts <- lm(length ~ bs(temp_2, degree = 2), data = filter(allLengthsEnv, species == "TS"))
Mns <- lm(length ~ bs(temp_2, degree = 2), data = filter(allLengthsEnv, species == "ND"))
summary(Mns)
#plot splines
ep <- filter(allLengthsEnv, species == "EP")
ts <- filter(allLengthsEnv, species == "TS")
nd <- filter(allLengthsEnv, species == "ND")
temp_2_new <- seq(8, 19, by = 0.1)
plot(ep$temp_2, ep$length, pch = 16)
lines(temp_2_new, predict(Mes, data.frame(temp_2 = temp_2_new), col = "blue"))
plot(ts$temp_2, ts$length, pch = 16)
lines(temp_2_new, predict(Mts, data.frame(temp_2 = temp_2_new), col = "blue"))
plot(nd$temp_2, nd$length, pch = 16)
lines(temp_2_new, predict(Mns, data.frame(temp_2 = temp_2_new), col = "blue"))
summary(Mes)
summary(Mts)
summary(Mns)

#all species
plot(allLengthsEnv$temp_2, allLengthsEnv$length, pch = 16)
lines(temp_2_new, predict(Ms, data.frame(temp_2 = temp_2_new), col = "blue"))
summary(Ms)
#Nonlinear analysis seems appropriate for all but TS and the full model, though nonlinear terms could be added in for species interactions. 

#Nonlinear environment model
Mnl1 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + sex:temp_2 + temp_100 + species:temp_100 + sex:temp_100 + I(temp_2^2) + I(temp_100^2) + (1|station), data = allLengthsEnv)
Mnl2 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + sex:temp_2 + temp_100 + species:temp_100 + I(temp_2^2) + I(temp_100^2) + (1|station), data = allLengthsEnv)
#model evaluation
anova(Mnl1)
sjPlot::tab_model(Mnl2, 
                  show.re.var= TRUE, 
                  dv.labels= "Environmental Effects on Krill Length", file = "output/Mnl2.doc")
sjPlot::plot_model(Mnl1)
lattice::dotplot(ranef(Mnl2,condVar=TRUE))
save(Mnl2, file = "output/Mnl2.rda")
#visualization
Mnl2sim <- fsim.glmm(Mnl2, nsim = 1000)
Me1simsum <- simsum(Me1sim)
Me1sum <- summarize(group_by_at(Me1simsum, vars(year, sex)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))

sex <- ggplot() + 
  geom_point(data = allLengthsEnv, aes(x = year, y = length, color = species), alpha = 0.1) +
  geom_line(data = Me1sum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "red") +
  geom_line(data = Mt2sum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "blue") +
  geom_line(data = Mn2sum, aes(x = as.numeric(year), y = sim.mean, linetype = sex), color = "green") + 
  # geom_ribbon(data = Me1sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, linetype = sex), color = "red", alpha = 0.2) + 
  # geom_ribbon(data = Mt2sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, linetype = sex), color = "blue", alpha = 0.2) + 
  # geom_ribbon(data = Mn2sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, linetype = sex), color = "green", alpha = 0.2) +
  #geom_rect(data = Me1sum, aes(xmin = 2015, xmax = 2017, ymin = 0, ymax = Inf), alpha = 0.01, fill = "red") +
  labs(y = "Length (mm)", x = "Year") +
  ylim(min = 10, max = 35) + 
  theme(text = element_text(size = 20))









#A full model with surface and subsurface temperature as fixed effects
M5 <- lmer(length ~ temp_2 + temp_100 + shore + sex + species + (1|station.x), data = allLengthsRecentEnv)
M6 <- lmer(length ~ temp_2 + temp_100 + temp_2:region + temp_100:region + temp_2:species + temp_100:species + temp_2:species:region + temp_100:species:region + shore + sex + species + (1|station.x), data = allLengthsRecentEnv)
#only low temps
M7 <- lmer(length ~ temp_2*species + temp_100*species + shore + sex + species + (1|station.x), data = filter(allLengthsRecentEnv, temp_2<13.5))
#only high temps 
M8 <- lmer(length ~ temp_2*species + temp_100*species + shore + sex + species + (1|station.x), data = (filter(allLengthsRecentEnv, temp_2>=13.5)))
AIC(M3, M6, M9)
summary(M6)
1 - var(resid(M6))/var(allLengthsRecentEnv$length) #R=33%

#Use distance from shore explicitly
#merge with distance from shore data
allLengthsEnvDep <- left_join(allLengthsEnv, select(regions, station, dist), by = c("station.x" = "station"))
#model
M9 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + region:temp_2 + sex:temp_2 + temp_100 + species:temp_100 + region:temp_100 + sex:temp_100 + dist + species:dist + (1|station.x), data = allLengthsEnvDep, REML = FALSE)
#remove sex:temp interaction
M10 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + region:temp_2 + temp_100 + species:temp_100 + region:temp_100 + dist + species:dist + (1|station.x), data = allLengthsEnvDep, REML = FALSE)
#remove region:temp interaction
M11 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + temp_100 + species:temp_100 + dist + species:dist + (1|station.x), data = allLengthsEnvDep, REML = FALSE)
AIC(M9, M10, M11)
M12 <- lm(length ~ temp_2, data = allLengthsRecentEnv)
M13 <- lmer(length ~ temp_2 + (1|station.x), data = allLengthsRecentEnv)
summary(M13)
#==========
#simulation for M9
#========
simM9 <- fsim.glmm(M9) #simulates data across bins of variable values, cont.expansion is for prediction, nsim is number os simulations to run. Returns two lists 1) full factorial of all parameter values, 2) provides the response variables for those values
#sim sum provides confidence intervals etc.
simsumM9 <- simsum(simM9)
#simsumM3 <- filter(simsumM3, species != "ND") #drop ND for NRT presentation
#plot sets up ggplot for simulated data
sum <- summarize(group_by_at(simsumM9, vars(species, temp_2, region)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot(simsumM9) + 
  geom_point(aes(x = temp_2, y = sim.mean, color = species), alpha = 0.1) + #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  facet_wrap(simsumM9$region) + 
  geom_smooth(aes(x = temp_2, y = sim.mean, color = species, linetype = sex)) + 
  labs(y = "Length (mm)", x = "Temp (C)", title = "Simulated krill lengths by temperature") +
  theme(text = element_text(size = 20))
#==========
#A model with a random intercept for station
Me1 <- lmer(length ~ year + region + sex + shore + (1|station), data = epRecent, REML = FALSE)
Me2 <- lmer(length ~ year + sex + shore + (1|station), data = epRecent, REML = FALSE)
Me3 <- lmer(length ~ year*region + sex + shore + (1|station), data = epRecent, REML = FALSE)
Me4 <- lmer(length ~ year + year:region + sex + shore + (1|station), data = epRecent, REML = FALSE) #Me3/Me4 have lowest AIC of the four alternatives. Prevalence of interactions suggests that a strictly physical approach would be fruitful. 
#test fit
1 - var(resid(M2))/var(allLengths$length) #R=30%
1 - var(resid(Me3))/var(epRecent$length) #R=12%
plot(Me3)

AIC(Me1, Me2, Me3, Me4)
summar(Me3)
anova(Me3)
fixef(Me3)
sjPlot::plot_model(Me3)

pred.Me3 <- ggpredict(Me3, terms = c("year"))  # this gives overall predictions for the model

#confidence interval plot
ggCaterpillar(Me3)

simM3 <- fsim.glmm(M3) #simulates data across bins of variable values, cont.expansion is for prediction, nsim is number os simulations to run. Returns two lists 1) full factorial of all parameter values, 2) provides the response variables for those values
#sim sum provides confidence intervals etc.
simsumM3 <- simsum(simM3)
#simsumM3 <- filter(simsumM3, species != "ND") #drop ND for NRT presentation
#plot sets up ggplot for simulated data
View(simsumMe3)
#plot
sum <- summarize(group_by_at(simsumM3, vars(species, year, region)), sim.mean = mean(sim.mean), lower.95 = mean(lower.95), upper.95 = mean(upper.95))
ggplot(simsumM3) + 
  geom_point(aes(x = year, y = sim.mean, color = species), alpha = 0.1) + 
  facet_wrap(vars(region)) +   
  #geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2) + 
  geom_line(data = sum, aes(x = as.numeric(year), y = sim.mean, color = species)) + 
  labs(y = "Length (mm)", x = "Year", title = "Simulated krill lengths during a marine heatwave") +
  theme(text = element_text(size = 20))

ggplot(simsumM1) +
  geom_density_ridges(aes(x = sim.mean, y = year, group = year, fill = year)) +
  facet_wrap(simsumM1$species)

#A model with a random intercept for station AND a random slope to investigate the hypothesis that different stations respond differently across years
Me2 <- lmer(length ~ year + region + sex + shore + (year|station), data = epRecent, REML = TRUE)
summary(Me2) #improved treatment of random effects compared to model Me1 use REML=TRUE

#TS
Mt1 <- lmer(length ~ year + region + sex + shore + (1|station), data = tsRecent, REML = FALSE)
summary(Mt1)

#1 keep going on the Multilevel path, try some different model specifications and plot some simulated data. Use IC to determine how many predictors to use. Don't impute, making too big of assumptions. Bayesian helps with this too. Take note when simulating the model for combinations of predictors variables that weren't present in the dataset. 
#Bayesian = better job of using information from data rich sites to inform data poor sites. 
#take the physical approach and use actual temperatures, etc. but be careful about collinearity. Consider using EOF/PCA.
#Best approach is probably to use actual physical variables and a bayesian approach

al <- summarize(group_by_at(filter(allLengths, year != "2011", year != "2012", species != "ND"), vars(year, station, species)), mean = mean(length), median = median(length), cv = cv(length), skew = skewness(length), kurtosis = kurtosis(length))

ggplot(al) + 
  geom_boxplot(aes(year, cv)) + 
  facet_wrap(vars(species)) + 
  theme_bw(base_size = 20) + 
  ylim(0, 35) + 
  labs(x = "Year", y = "Coefficient of Variation")

#In answer to Jeff & Bill
#LvT for each species with a linear regression 
ggplot(allLengthsEnv) +
  geom_point(aes(latitude, length, color = temp_2)) + 
  geom_smooth(method='lm', aes(x = latitude, y = length), alpha = 0.5) + 
  #geom_smooth(data = filter(simsumM9), method = "lm", aes(x = temp_100, y = sim.mean), color = "red") + 
  facet_wrap(~species, nrow=1, ncol=3, scales = "free_y")

x <- summarize(group_by_at(allLengthsEnv, vars(station.x, year.x, species)), temp_2 = mean(temp_2), temp_100 = mean(temp_100), length = mean(length))
ggplot(x) +
  geom_point(aes(temp_2, length, color = year.x)) + 
  facet_wrap(~species, nrow=1, ncol=3, scales = "free_y")
ggplot(x) +
  geom_point(aes(temp_100, length, color = year.x)) + 
  facet_wrap(~species, nrow=1, ncol=3, scales = "free_y")

ggplot(filter(allLengthsEnv, species == "EP")) +
  geom_point(aes(temp_2, length, color = latitude)) +
  geom_smooth(data = filter(simsumM9, species == "EP"), aes(temp_2, sim.mean))

geom_ribbon(data = sum, aes(x = as.numeric(year), ymin = lower.95, ymax = upper.95, fill = species), alpha = 0.2)

#Split EP into two regions, north and south of PC
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
  facet_wrap(~NS, nrow=1, ncol=2, scales = "free_y")


ep.model <- lmer(length ~ chla + moci_spring + sex + sst_sd + temp_100 + temp_2 + (1 + temp_2 | station) + sex:temp_100 + sex:temp_2, data = ep)
kappa.mer(ep.model)
car::vif(ep.model)#check for multicollinearilty using variance inflation factor <5 = ok
lattice::dotplot(ranef(ep.model,condVar=TRUE))
sjPlot::plot_model(ep.model)
sjPlot::tab_model(ep.model, 
                  show.re.var= TRUE, 
                  dv.labels= "Environmental Effects on E. pacifica Length")
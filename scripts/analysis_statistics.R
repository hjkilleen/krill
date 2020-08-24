# Tue Aug  4 18:44:17 2020 ------------------------------

#LIBRARIES & SOURCES

library(lme4)
library(ggeffects)
source("scripts/functions/model_simulation.R")
load("data/allLengthsEnv.rda")

#Test for normality of each species distribution
#====
#structure of all krill lengths
hist(allLengths$length)
qqnorm(allLengths$length)
#Agostino test for normality
moments::agostino.test(allLengths$length)
#Total kurtosis (-3 for excess kurtosis)
kurtosis(allLengths$length)-3
#=====

#CV by year
#=====
ep.summ <- summarize(group_by_at(ep, vars(year, region)), cv = cv(length))
ggplot(ep.summ, aes(x = year, y = cv, color = region))+
  geom_point() + 
  geom_line(aes(group = region)) + 
  ggtitle("E. pacifica length variability")

ts.summ <- summarize(group_by_at(filter(ts, region != "south"), vars(year, region)), cv = cv(length))
ggplot(ts.summ, aes(x = year, y = cv, color = region))+
  geom_point() + 
  geom_line(aes(group = region)) + 
  ggtitle("T. spinifera length variability (no South)")

nd.summ <- summarize(group_by_at(filter(nd, region != "north", region != "north_central"), vars(year, region)), cv = cv(length))
ggplot(nd.summ, aes(x = year, y = cv, color = region))+
  geom_point() + 
  geom_line(aes(group = region)) + 
  ggtitle("N. difficilis length variability (South & Central Only)")
#=========

#MULTILEVEL MODELING
#=======
#Model variation in krill length throughout the heatwave.
#Change relevant variables to factors for multilevel analysis
allLengthsEnv$station <- as.factor(allLengthsEnv$station)
allLengthsEnv$year <- as.factor(allLengthsEnv$year)
#A full model with all species=
M1 <- lmer(length ~ year*region + sex*species + species:year + sex:year + shore + shore:year + (1|station), data = allLengthsEnv)

M2 <- lmer(length ~ year*region + sex + species + species:year + sex:year + shore + shore:year + (1|station), data = allLengthsEnv)

M3 <- lmer(length ~ year*region + sex + species + species:year + shore + shore:year + (1|station), data = allLengthsEnv)

M4 <- lmer(length ~ year*region + sex*species + shore + (1|station), data = allLengthsEnv)

#Model comparison
AIC(M1, M2, M3, M4)
summary (M1)
sjPlot::tab_model(M3, 
                  show.re.var= TRUE, 
                  dv.labels= "Spatial and Temporal Effects on Krill Length")
sjPlot::plot_model()

#Visualization


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

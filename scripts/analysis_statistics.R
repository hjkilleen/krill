# Tue Aug  4 18:44:17 2020 ------------------------------

#LIBRARIES & SOURCES

library(lme4)
library(ggeffects)
source("scripts/analysis_lf_explore.R")
source("scripts/data_load_roms.R")
source("scripts/functions/model_simulation.R")

jpeg("figures/sideBySideAllLengths.jpg")
par(mfrow = c(1,3))
hist(ep$length, main = "E. pacifica Lengths")
hist(ts$length, main = "T. spinifera Lengths")
hist(nd$length, main = "N. difficilis Lengths")
dev.off()

#Environmental indicators table
#========
env <- read_xlsx("data/environment.xlsx")
env.means <- group_by_at(env, vars(year, region)) %>% 
  summarize(lat = mean(lat), oni = mean(oni), pdo = mean(pdo), npgo = mean(npgo), sst_anom = mean(sst_anom), temp_anom_one.hun = mean(temp_anom_one.hun), temp_anom_two.hun = mean(temp_anom_two.hun), uw_anom = mean(uw_anom))
formattable(env.means, list(`oni` = color_bar("#FA614B"), `pdo` = color_bar("#71CA97"), `npgo` = color_bar("#FA614B"), `sst_anom` = color_bar("#71CA97"), `temp_anom_one.hun` = color_bar("#FA614B"), `temp_anom_two.hun` = color_bar("#71CA97")))
#===========

#Three-Way ANOVA with individual krill as observational units
#==========
ep <- filter(allLengths, species == "EP")
ts <- filter(allLengths, species == "TS")
nd <- filter(allLengths, species == "ND")
#Test for normality of each species distribution
#structure of all krill lengths
hist(ep$length)
hist(ts$length)
hist(nd$length)
qqnorm(ep$length)
qqnorm(ts$length)
qqnorm(nd$length)
#Agostino test for normality
agostino.test(ep$length)
agostino.test(ts$length)
agostino.test(nd$length)
#Total kurtosis (-3 for excess kurtosis)
3-kurtosis(ep$length)
3-kurtosis(ts$length)
3-kurtosis(nd$length)

#All three species have approximately normal length distributions with slight negative skew and are slightly divergent from noraml levels of kurtosis (leptokurtic for EP & TS, platykurtic for ND). Though the distributions are non-normal, the kurtosis is small enough for each species that an ANOVA is appropriate. 

#Make summary table of means for each species
ep.summ <- summarise(group_by_at(ep, vars(year, region, shore)), mean = mean(length))

#EP ANOVA
ep.aov <- aov(length~year*region*shore, data = ep)
summary.aov(ep.aov)
TukeyHSD(ep.aov, which = "region")
#ANOVA shows that there is a significant difference in the mean length of krill from different years, regions, and crossshore distributions. All interactions (two-way and three-way) were also significannt. However, looking at the TukeyHSD test statistics for pairwise comparisons reveals that these differences are not very great in magnitude, generally 1-3mm, which is arguably biologically insignificant. However, this should be tested once we have information on krill abundance, from which we can look at biomass rather than length frequency alone. 

#One thing that stands out in the TukeyHSD results is that the north region seems to be different from all of the others across years. All years were different from eachopther except 2018 & 2017 in pairwise comparisons, 2011 seems to be more so and shows the largest differences in all pairwise comparisons with other years. 

#TS ANOVA
ts.aov <- aov(length~year*region*shore, data = ts)
summary.aov(ts.aov)
TukeyHSD(ts.aov, which = "year")
#Similar takeaways from EP. 2018 is similar to both 2017 and 2012. 2011 means were quite a bit smaller than all other years, as shown in the pairwise comparisons (3-4.5 mm).All regions were different, but not by much, except for south-central. 

#ND ANOVA
nd.aov <- aov(length~year*region*shore, data = nd)
summary.aov(nd.aov)
TukeyHSD(nd.aov, which = "year")
#All factors and interactions were significant except for region:shore. Less variability across years for NDs, 2011 remains the most different from all years with smaller mean lenghts (1-3.3 mm). The north is different from all other regions, but not by much. Otherwise, regions are similar. Onshore krill are ~0.5mm longer than offshore krill. 
#==========

#Two-way nested ANOVA with stations as observational units
#===========
#EP
#if you want to include sex as a factor in the final analysis then it needs to be added in the grouping variables below.
ep.st <- summarize(group_by_at(ep, vars(station, year, sex)), mean.length = mean(length))
ep.st <- left_join(ep.st, regions, by = "station")
ep.st.aov <- aov(mean.length~year*region*sex, data = ep.st)
summary.aov(ep.st.aov)

#TS
ts.st <- summarize(group_by_at(ts, vars(station, year, sex)), mean.length = mean(length))
ts.st <- left_join(ts.st, regions, by = "station")
ts.st.aov <- aov(mean.length~year*region*sex, data = ts.st)
summary.aov(ts.st.aov)
#===========

#CV by year
#=========
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

#Multilevel modeling
#=======
#use mean length for each station
epSt <- summarise(group_by_at(ep, vars(station, year, region, sex, shore)), length = mean(length))
#drop 2011 & 2012 until I can explore Baldo's length choice (SL1/SL2/something else)
allLengthsRecent <- filter(allLengths, year != "2011", year != "2012")
epRecent <- filter(ep, year != "2011", year != "2012")
tsRecent <- filter(ts, year != "2011", year != "2012")

#A full model with all species
M1 <- lmer(length ~ year*region + sex*species + shore + (1|station), data = allLengthsRecent)
M2 <- lmer(length ~ year*region + sex*species + species:year + shore + shore:year + (1|station), data = allLengthsRecent)
M3 <- lmer(length ~ year*region + sex*species + species:year + sex:year + shore + shore:year + (1|station), data = allLengths)
M4 <- lmer(length ~ year*region + sex + species + species:year + sex:year + shore + shore:year + (1|station), data = allLengthsRecent)
AIC(M1, M2, M3, M4)
summary (M3)
fixef(M3)
sjPlot::tab_model(M3, 
                  show.re.var= TRUE, 
                  dv.labels= "Spatial and Temporal Effects on Krill Length")
sjPlot::plot_model()

#Explore lengthEnv relationships with plots
lowTemp <- filter(lengthEnv, temp_2<13.5)
highTemp <- filter(lengthEnv, temp_2>=13.5)
ggplot(lengthEnv, aes(x = temp_100, y = (length-mean(length)), color = species)) + 
  geom_point(data = lowTemp, aes(x = temp_2, y = length), alpha = 0.9) +
  geom_point(data = highTemp, aes(x= temp_2, y = length), alpha = 0.1)
ggplot(lengthEnv) + 
  geom_histogram(aes(length, color = species))
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

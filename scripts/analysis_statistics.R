# Mon May 18 14:12:59 2020 ------------------------------

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

sign_formatter <- formatter("span",
                            style = x ~ style(color = ifelse(x > 0, "green",
                                                             ifelse(x < 0, "red", "black"))))
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
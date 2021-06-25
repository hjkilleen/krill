#Environmental model
#Model environmental drivers of krill length
# Thu Jan 28 15:15:57 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(lme4)
library(tidyverse)
library(arm)
library(MuMIn)
library(sjPlot)
load(file = "data/allLengthsEnvEP.rda")
load("data/allLengthsEnvND.rda")
load("data/allLengthsEnvTS.rda")
load("output/environmentalEP.rda")
source("scripts/functions/model_simulation.R")
#====

#SETUP
#=======
epc <- ep[complete.cases(ep),]#filter to only complete cases
epc <- summarize(group_by_at(epc, vars(station, year)), length = mean(length), temp_2 = mean(temp_2), cuti = mean(cuti), chla = mean(chla))#group by station, year, and sex

ts <- ts[,-18]#drop temp_100 column
tsc <- ts[complete.cases(ts),]#filter to only complete cases
tsc <- summarize(group_by_at(tsc, vars(station, year)), length = mean(length), temp_2 = mean(temp_2), cuti = mean(cuti), chla = mean(chla))#group by station, year, and sex

ndc <- nd[complete.cases(nd),]#filter to only complete cases
ndc <- summarize(group_by_at(ndc, vars(station, year)), length = mean(length), temp_2 = mean(temp_2), cuti = mean(cuti), chla = mean(chla))#group by station, year, and sex
#====

#LINEAR MODELING
#====
ep.13 <- filter(ep, year == "2013")
epm.13 <- lmer(length~temp_2 + cuti + chla + (1|station), data = ep.13)

ep.15 <- filter(ep, year == "2015")
epm.15 <- lmer(length ~ sex*temp_2 + cuti + sex:cuti + chla + sex:chla + (1|station), data = ep.15)

ep.16 <- filter(ep, year == "2016")
epm.16 <- lmer(length~temp_2 + cuti + chla + (1|station), data = ep.16)

ep.est <- data.frame(model = c("2013", "2015_F", "2015_M", "2016", "All"),
                     intercept = c(coef(summary(epm.13))[1,1], coef(summary(epm.15))[1,1], coef(summary(epm.15))[1,1]+coef(summary(epm.15))[6,1], coef(summary(epm.16))[1,1], coef(summary(epm))[1,1]),
                     slope = c(coef(summary(epm.13))[2,1], coef(summary(epm.15))[3,1], coef(summary(epm.15))[3,1]+coef(summary(epm.15))[6,1], coef(summary(epm.16))[2,1], coef(summary(epm))[7,1]),
                     xmin = c(min(ep.13$temp_2), min(ep.15$temp_2), min(ep.15$temp_2), min(ep.16$temp_2), min(ep$temp_2)),
                     xmax = c(max(ep.13$temp_2), max(ep.15$temp_2), max(ep.15$temp_2), max(ep.16$temp_2), max(ep$temp_2)))
ep.est$ymin = ep.est$intercept+(ep.est$slope*ep.est$xmin)
ep.est$ymax = ep.est$intercept+(ep.est$slope*ep.est$xmax)

#SST
ggplot(filter(epc, year == "2013" | year == "2015" | year == "2016"), aes(x = temp_2, y = length)) + 
  geom_boxplot(data = ep.13, aes(group=temp_2), color = "#56B4E9", width = 0.05) + 
  geom_boxplot(data = ep.15, aes(group = temp_2), color = "#D55E00", width = 0.05) +
  geom_boxplot(data = ep.16, aes(group = temp_2), color = "#0072B2", width = 0.05) +
  geom_segment(aes(x = ep.est$xmin[5], y = ep.est$ymin[5], xend = ep.est$xmax[5], yend = ep.est$ymax[5]), size = 1.5, color = "#999999") +
  geom_segment(aes(x = ep.est$xmin[1], y = ep.est$ymin[1], xend = ep.est$xmax[1], yend = ep.est$ymax[1]), size = 1.5, color = "#56B4E9") + 
  geom_segment(aes(x = ep.est$xmin[4], y = ep.est$ymin[4], xend = ep.est$xmax[4], yend = ep.est$ymax[4]), size = 1.5, color = "#0072B2") + 
  geom_segment(aes(x = ep.est$xmin[2], y = ep.est$ymin[2], xend = ep.est$xmax[2], yend = ep.est$ymax[2]), size = 1.5, color = "#D55E00") + #female
  geom_segment(aes(x = ep.est$xmin[3], y = ep.est$ymin[3], xend = ep.est$xmax[3], yend = ep.est$ymax[3]), size = 1.5, color = "#D55E00", linetype = "dashed") + #male
  labs(x = "SST (°C)", y = "Mean length (mm)", title = "E. pacifica") + 
  theme_classic() + 
  theme(plot.title = element_text(face = "italic"))


#CUTI
ggplot(filter(epc, year == "2013" | year == "2015" | year == "2016"), aes(x = cuti, y = length)) + 
  geom_point(aes(color=year)) + 
  geom_abline(aes(xmin = min(ep.13$cuti), xmax = max(ep.13$cuti)), intercept = coef(summary(epm.13))[1,1], slope = coef(summary(epm.13))[3,1], color = "grey") + 
  geom_abline(intercept = coef(summary(epm.15))[1,1], slope = coef(summary(epm.15))[4,1], color = "red") + #female
  geom_abline(intercept = coef(summary(epm.15))[1,1], slope = coef(summary(epm.15))[4,1]+coef(summary(epm.15))[7,1], color = "red", linetype = "dashed") + #male
  geom_abline(intercept = coef(summary(epm.16))[1,1], slope = coef(summary(epm.16))[3,1], color = "darkgrey") + 
  geom_abline(intercept = coef(summary(epm))[1,1], slope = coef(summary(epm))[3,1], color = "black")

#Chlorophyll
ggplot(filter(epc, year == "2013" | year == "2015" | year == "2016"), aes(x = chla, y = length)) + 
  geom_point(aes(color=year)) + 
  geom_abline(aes(xmin = min(ep.13$chla), xmax = max(ep.13$chla)), intercept = coef(summary(epm.13))[1,1], slope = coef(summary(epm.13))[4,1], color = "grey") + 
  geom_abline(intercept = coef(summary(epm.15))[1,1], slope = coef(summary(epm.15))[5,1], color = "red") + #female
  geom_abline(intercept = coef(summary(epm.15))[1,1], slope = coef(summary(epm.15))[5,1]+coef(summary(epm.15))[8,1], color = "red", linetype = "dashed") + #male
  geom_abline(intercept = coef(summary(epm.16))[1,1], slope = coef(summary(epm.16))[4,1], color = "darkgrey") + 
  geom_abline(intercept = coef(summary(epm))[1,1], slope = coef(summary(epm))[4,1], color = "black")

#Thysanoessa spinifera
#Optimize random effects structure using maximum likelihood
ts.int <- lmer(length ~ temp_2 + chla + cuti + (1|station), data = tsc, na.action = na.fail, REML = FALSE) #model with random intercept 

tab_model(ts.int, title = "T. spinifera", show.p = TRUE, CSS = list(
  css.depvarhead = '+color: red;',
  css.centeralign = 'text-align: center;', 
  css.firsttablecol = 'font-weight: bold;', 
  css.summary = 'color: blue;'
))

#LINEAR MODELING
tsc <- summarize(group_by_at(tsc, vars(station, sex)), length = mean(length), temp_2 = mean(temp_2), cuti = mean(cuti), chla = mean(chla))
tsmh2 <- lm(length~temp_2 + cuti + chla, data = tsc)

summary(tsmh2)

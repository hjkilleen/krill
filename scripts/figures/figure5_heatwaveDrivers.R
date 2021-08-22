#Plot models of MHW environmental drivers of krill length

# Sun Aug 22 11:24:06 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(lme4)
library(tidyverse)
library(arm)
library(MuMIn)
library(sjPlot)
library(ggpubr)
load("data/allLengthsEnvEP.rda")
load("data/allLengthsEnvND.rda")
load("data/allLengthsEnvTS.rda")
load("data/allLengthsEnv.rda")
source("scripts/functions/model_simulation.R")
#====

#SETUP
#=======
epc <- ep[complete.cases(ep),]#filter to only complete cases
epc <- summarize(group_by_at(epc, vars(station, year)), length = mean(length), temp_2 = mean(temp_2), cuti = mean(cuti), chla = mean(chla))#group by station and year

ts <- ts[,-18]#drop temp_100 column
tsc <- ts[complete.cases(ts),]#filter to only complete cases
tsc <- summarize(group_by_at(tsc, vars(station, year)), length = mean(length), temp_2 = mean(temp_2), cuti = mean(cuti), chla = mean(chla))#group by station and year

ndc <- nd[complete.cases(nd),]#filter to only complete cases
ndc <- summarize(group_by_at(ndc, vars(station, year)), length = mean(length), temp_2 = mean(temp_2), cuti = mean(cuti), chla = mean(chla))#group by station and year
#====

#LINEAR MODELING
#====
#Euphausia pacifica overall and annual models
epm <- lmer(length~temp_2 + cuti + chla + (1|station), data = ep)

ep.13 <- filter(ep, year == "2013")
epm.13 <- lmer(length~temp_2 + cuti + chla + sex:temp_2 + sex:cuti + sex:chla + (1|station), data = ep.13)

ep.15 <- filter(ep, year == "2015")
epm.15 <- lmer(length ~ temp_2 + sex:temp_2 + cuti + sex:cuti + chla + sex:chla + (1|station), data = ep.15)

ep.17 <- filter(ep, year == "2017")
epm.17 <- lmer(length~temp_2 + cuti + chla + sex:temp_2 + sex:cuti + sex:chla + (1|station), data = ep.17)

#Thysanoessa spinifera overall and annual models
tsm <- lmer(length~temp_2 + cuti + chla + (1|station), data = ts)

ts.13 <- filter(ts, year == "2013")
tsm.13 <- lmer(length~temp_2 + cuti + chla + sex:temp_2 + sex:cuti + sex:chla + (1|station), data = ts.13)

ts.15 <- filter(ts, year == "2015")
tsm.15 <- lmer(length ~ temp_2 + sex:temp_2 + cuti + sex:cuti + chla + sex:chla + (1|station), data = ts.15)

ts.17 <- filter(ts, year == "2017")
tsm.17 <- lmer(length~temp_2 + cuti + chla + sex:cuti + sex:temp_2 + sex:chla + (1|station), data = ts.17)
#linear model because stations <5

#Nematoscelis difficilis overall and annual models
ndm <- lmer(length~temp_2 + cuti + chla + (1|station), data = nd)

nd.15 <- filter(nd, year == "2015")
ndm.15 <- lmer(length ~ temp_2 + sex:temp_2 + cuti + sex:cuti + chla + sex:chla + (1|station), data = nd.15)

nd.17 <- filter(nd, year == "2017")
ndm.17 <- lmer(length~temp_2 + cuti + chla + sex:cuti + sex:temp_2 + sex:chla + (1|station), data = nd.17)
#linear model because station < 5
#=====

#PREPARE DATA FOR PLOTTING
#====
#unscale overall and annual dataframes
# ep.dfs <- list(ep.13, ep.15, ep.17, ep)
# for(i in seq(1:4)){
#   ep.dfs[[i]]$length <- ep.dfs[[i]]$length*attr(ep$length, "scaled:scale") + attr(ep$length, "scaled:center")#unscale axes
#   ep.dfs[[i]]$temp_2 <- ep.dfs[[i]]$temp_2*attr(ep$temp_2, "scaled:scale") + attr(ep$temp_2, "scaled:center")
#   ep.dfs[[i]]$cuti <- ep.dfs[[i]]$cuti*attr(ep$cuti, "scaled:scale") + attr(ep$cuti, "scaled:center")
#   ep.dfs[[i]]$chla <- ep.dfs[[i]]$chla*attr(ep$chla, "scaled:scale") + attr(ep$chla, "scaled:center")
# }
# 
# ts.dfs <- list(ts.13, ts.15, ts.17, ts)
# for(i in seq(1:4)){
#   ts.dfs[[i]]$length <- ts.dfs[[i]]$length*attr(ts$length, "scaled:scale") + attr(ts$length, "scaled:center")#unscale axes
#   ts.dfs[[i]]$temp_2 <- ts.dfs[[i]]$temp_2*attr(ts$temp_2, "scaled:scale") + attr(ts$temp_2, "scaled:center")
#   ts.dfs[[i]]$cuti <- ts.dfs[[i]]$cuti*attr(ts$cuti, "scaled:scale") + attr(ts$cuti, "scaled:center")
#   ts.dfs[[i]]$chla <- ts.dfs[[i]]$chla*attr(ts$chla, "scaled:scale") + attr(ts$chla, "scaled:center")
# }
# 
# nd.dfs <- list(nd.15, nd.17, nd)
# for(i in seq(1:3)){
#   nd.dfs[[i]]$length <- nd.dfs[[i]]$length*attr(nd$length, "scaled:scale") + attr(nd$length, "scaled:center")#unscale axes
#   nd.dfs[[i]]$temp_2 <- nd.dfs[[i]]$temp_2*attr(nd$temp_2, "scaled:scale") + attr(nd$temp_2, "scaled:center")
#   nd.dfs[[i]]$cuti <- nd.dfs[[i]]$cuti*attr(nd$cuti, "scaled:scale") + attr(nd$cuti, "scaled:center")
#   nd.dfs[[i]]$chla <- nd.dfs[[i]]$chla*attr(nd$chla, "scaled:scale") + attr(nd$chla, "scaled:center")
# }
#====

#PLOTTING
#====
#Euphausia pacifica
#SST
#Model estimates for segents
ep.est.sst <- data.frame(model = c("2013_F", "2013_M", "2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(epm.13))[1,1], coef(summary(epm.13))[1,1], coef(summary(epm.15))[1,1], coef(summary(epm.15))[1,1], coef(summary(epm.17))[1,1], coef(summary(epm.17))[1,1], coef(summary(epm))[1,1]),
                     slope = c(coef(summary(epm.13))[2,1], coef(summary(epm.13))[2,1]+coef(summary(epm.13))[5,1], coef(summary(epm.15))[2,1], coef(summary(epm.15))[2,1]+coef(summary(epm.15))[5,1], coef(summary(epm.17))[2,1], coef(summary(epm.17))[2,1]+coef(summary(epm.17))[5,1], coef(summary(epm))[2,1]),
                     xmin = c(min(ep.13$temp_2), min(ep.13$temp_2), min(ep.15$temp_2), min(ep.15$temp_2), min(ep.17$temp_2), min(ep.17$temp_2), min(ep$temp_2)),
                     xmax = c(max(ep.13$temp_2), max(ep.13$temp_2), max(ep.15$temp_2), max(ep.15$temp_2), max(ep.17$temp_2), max(ep.17$temp_2), max(ep$temp_2)))
ep.est.sst$ymin = ep.est.sst$intercept+(ep.est.sst$slope*ep.est.sst$xmin)
ep.est.sst$ymax = ep.est.sst$intercept+(ep.est.sst$slope*ep.est.sst$xmax)

#plot
ep.sst.plot <- ggplot(filter(epc, year == "2013" | year == "2015" | year == "2017"), aes(x = temp_2, y = length)) + 
  geom_boxplot(data = ep.13, aes(group=temp_2), color = "#56B4E9", width = 0.05, alpha = 0.5) + 
  geom_boxplot(data = ep.15, aes(group = temp_2), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = ep.17, aes(group = temp_2), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = ep.est.sst$xmin[7], y = ep.est.sst$ymin[7], xend = ep.est.sst$xmax[7], yend = ep.est.sst$ymax[7]), size = 1, color = "black") +
  geom_segment(aes(x = ep.est.sst$xmin[1], y = ep.est.sst$ymin[1], xend = ep.est.sst$xmax[1], yend = ep.est.sst$ymax[1]), size = 1, color = "#56B4E9") + #female
  geom_segment(aes(x = ep.est.sst$xmin[2], y = ep.est.sst$ymin[2], xend = ep.est.sst$xmax[2], yend = ep.est.sst$ymax[2]), size = 1, color = "#56B4E9", linetype = "dotted") + #male
  geom_segment(aes(x = ep.est.sst$xmin[5], y = ep.est.sst$ymin[5], xend = ep.est.sst$xmax[5], yend = ep.est.sst$ymax[5]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = ep.est.sst$xmin[6], y = ep.est.sst$ymin[6], xend = ep.est.sst$xmax[6], yend = ep.est.sst$ymax[6]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = ep.est.sst$xmin[3], y = ep.est.sst$ymin[3], xend = ep.est.sst$xmax[3], yend = ep.est.sst$ymax[3]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = ep.est.sst$xmin[4], y = ep.est.sst$ymin[4], xend = ep.est.sst$xmax[4], yend = ep.est.sst$ymax[4]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = "\n\nmean length", title = "SST") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.y = element_text(size = 16))

#CUTI
ep.est.cuti <- data.frame(model = c("2013_F", "2013_M", "2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(epm.13))[1,1], coef(summary(epm.13))[1,1], coef(summary(epm.15))[1,1], coef(summary(epm.15))[1,1], coef(summary(epm.17))[1,1], coef(summary(epm.17))[1,1], coef(summary(epm))[1,1]),
                     slope = c(coef(summary(epm.13))[3,1], coef(summary(epm.13))[3,1]+coef(summary(epm.13))[6,1], coef(summary(epm.15))[3,1], coef(summary(epm.15))[3,1]+coef(summary(epm.15))[6,1], coef(summary(epm.17))[3,1], coef(summary(epm.17))[3,1]+coef(summary(epm.17))[6,1], coef(summary(epm))[3,1]),
                     xmin = c(min(ep.13$cuti), min(ep.13$cuti), min(ep.15$cuti), min(ep.15$cuti), min(ep.17$cuti), min(ep.17$cuti), min(ep$cuti)),
                     xmax = c(max(ep.13$cuti), max(ep.13$cuti), max(ep.15$cuti), max(ep.15$cuti), max(ep.17$cuti), max(ep.17$cuti), max(ep$cuti)))
ep.est.cuti$ymin = ep.est.cuti$intercept+(ep.est.cuti$slope*ep.est.cuti$xmin)
ep.est.cuti$ymax = ep.est.cuti$intercept+(ep.est.cuti$slope*ep.est.cuti$xmax)

#plot
ep.cuti.plot <- ggplot(filter(epc, year == "2013" | year == "2015" | year == "2017"), aes(x = cuti, y = length)) + 
  geom_boxplot(data = ep.13, aes(group=cuti), color = "#56B4E9", width = 0.05, alpha = 0.5) + 
  geom_boxplot(data = ep.15, aes(group = cuti), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = ep.17, aes(group = cuti), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = ep.est.cuti$xmin[7], y = ep.est.cuti$ymin[7], xend = ep.est.cuti$xmax[7], yend = ep.est.cuti$ymax[7]), size = 1, color = "black") +
  geom_segment(aes(x = ep.est.cuti$xmin[1], y = ep.est.cuti$ymin[1], xend = ep.est.cuti$xmax[1], yend = ep.est.cuti$ymax[1]), size = 1, color = "#56B4E9") + #female
  geom_segment(aes(x = ep.est.cuti$xmin[2], y = ep.est.cuti$ymin[2], xend = ep.est.cuti$xmax[2], yend = ep.est.cuti$ymax[2]), size = 1, color = "#56B4E9", linetype = "dotted") + #male
  geom_segment(aes(x = ep.est.cuti$xmin[5], y = ep.est.cuti$ymin[5], xend = ep.est.cuti$xmax[5], yend = ep.est.cuti$ymax[5]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = ep.est.cuti$xmin[6], y = ep.est.cuti$ymin[6], xend = ep.est.cuti$xmax[6], yend = ep.est.cuti$ymax[6]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = ep.est.cuti$xmin[3], y = ep.est.cuti$ymin[3], xend = ep.est.cuti$xmax[3], yend = ep.est.cuti$ymax[3]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = ep.est.cuti$xmin[4], y = ep.est.cuti$ymin[4], xend = ep.est.cuti$xmax[4], yend = ep.est.cuti$ymax[4]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = " ", title = "CUTI") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5))

#Chlorophyll
ep.est.chla <- data.frame(model = c("2013_F", "2013_M", "2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(epm.13))[1,1], coef(summary(epm.13))[1,1], coef(summary(epm.15))[1,1], coef(summary(epm.15))[1,1], coef(summary(epm.17))[1,1], coef(summary(epm.17))[1,1], coef(summary(epm))[1,1]),
                     slope = c(coef(summary(epm.13))[4,1], coef(summary(epm.13))[4,1]+coef(summary(epm.13))[7,1], coef(summary(epm.15))[4,1], coef(summary(epm.15))[4,1]+coef(summary(epm.15))[7,1], coef(summary(epm.17))[4,1], coef(summary(epm.17))[4,1]+coef(summary(epm.17))[7,1], coef(summary(epm))[4,1]),
                     xmin = c(min(ep.13$chla), min(ep.13$chla), min(ep.15$chla), min(ep.15$chla), min(ep.17$chla), min(ep.17$chla), min(ep$chla)),
                     xmax = c(max(ep.13$chla), max(ep.13$chla), max(ep.15$chla), max(ep.15$chla), max(ep.17$chla), max(ep.17$chla), max(ep$chla)))
ep.est.chla$ymin = ep.est.chla$intercept+(ep.est.chla$slope*ep.est.chla$xmin)
ep.est.chla$ymax = ep.est.chla$intercept+(ep.est.chla$slope*ep.est.chla$xmax)

#plot
ep.chla.plot <- ggplot(filter(epc, year == "2013" | year == "2015" | year == "2017"), aes(x = chla, y = length)) + 
  geom_boxplot(data = ep.13, aes(group=chla), color = "#56B4E9", width = 0.05, alpha = 0.5) + 
  geom_boxplot(data = ep.15, aes(group = chla), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = ep.17, aes(group = chla), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = ep.est.chla$xmin[7], y = ep.est.chla$ymin[7], xend = ep.est.chla$xmax[7], yend = ep.est.chla$ymax[7]), size = 1, color = "black") +
  geom_segment(aes(x = ep.est.chla$xmin[1], y = ep.est.chla$ymin[1], xend = ep.est.chla$xmax[1], yend = ep.est.chla$ymax[1]), size = 1, color = "#56B4E9") + #female
  geom_segment(aes(x = ep.est.chla$xmin[2], y = ep.est.chla$ymin[2], xend = ep.est.chla$xmax[2], yend = ep.est.chla$ymax[2]), size = 1, color = "#56B4E9", linetype = "dotted") + #male
  geom_segment(aes(x = ep.est.chla$xmin[5], y = ep.est.chla$ymin[5], xend = ep.est.chla$xmax[5], yend = ep.est.chla$ymax[5]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = ep.est.chla$xmin[6], y = ep.est.chla$ymin[6], xend = ep.est.chla$xmax[6], yend = ep.est.chla$ymax[6]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = ep.est.chla$xmin[3], y = ep.est.chla$ymin[3], xend = ep.est.chla$xmax[3], yend = ep.est.chla$ymax[3]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = ep.est.chla$xmin[4], y = ep.est.chla$ymin[4], xend = ep.est.chla$xmax[4], yend = ep.est.chla$ymax[4]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = " ", title = expression(paste("Chl-a"))) + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(face = "italic")) + 
  theme(plot.title = element_text(hjust = 0.5))

#Thysanoessa spinifera
#SST
#Model estimates for segents
ts.est.sst <- data.frame(model = c("2013_F", "2013_M", "2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(tsm.13))[1,1], coef(summary(tsm.13))[1,1], coef(summary(tsm.15))[1,1], coef(summary(tsm.15))[1,1], coef(summary(tsm.17))[1,1], coef(summary(tsm.17))[1,1], coef(summary(tsm))[1,1]),
                     slope = c(coef(summary(tsm.13))[2,1], coef(summary(tsm.13))[2,1]+coef(summary(tsm.13))[5,1], coef(summary(tsm.15))[2,1], coef(summary(tsm.15))[2,1]+coef(summary(tsm.15))[5,1], coef(summary(tsm.17))[2,1], coef(summary(tsm.17))[2,1]+coef(summary(tsm.17))[6,1], coef(summary(tsm))[2,1]),
                     xmin = c(min(ts.13$temp_2), min(ts.13$temp_2), min(ts.15$temp_2), min(ts.15$temp_2), min(ts.17$temp_2), min(ts.17$temp_2), min(ts$temp_2)),
                     xmax = c(max(ts.13$temp_2), max(ts.13$temp_2), max(ts.15$temp_2), max(ts.15$temp_2), max(ts.17$temp_2), max(ts.17$temp_2), max(ts$temp_2)))
ts.est.sst$ymin = ts.est.sst$intercept+(ts.est.sst$slope*ts.est.sst$xmin)
ts.est.sst$ymax = ts.est.sst$intercept+(ts.est.sst$slope*ts.est.sst$xmax)

#plot
ts.sst.plot <- ggplot(filter(tsc, year == "2013" | year == "2015" | year == "2017"), aes(x = temp_2, y = length)) + 
  geom_boxplot(data = ts.13, aes(group=temp_2), color = "#56B4E9", width = 0.05, alpha = 0.5) + 
  geom_boxplot(data = ts.15, aes(group = temp_2), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = ts.17, aes(group = temp_2), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = ts.est.sst$xmin[7], y = ts.est.sst$ymin[7], xend = ts.est.sst$xmax[7], yend = ts.est.sst$ymax[7]), size = 1, color = "black") + 
  geom_segment(aes(x = ts.est.sst$xmin[1], y = ts.est.sst$ymin[1], xend = ts.est.sst$xmax[1], yend = ts.est.sst$ymax[1]), size = 1, color = "#56B4E9") + #female
  geom_segment(aes(x = ts.est.sst$xmin[2], y = ts.est.sst$ymin[2], xend = ts.est.sst$xmax[2], yend = ts.est.sst$ymax[2]), size = 1, color = "#56B4E9", linetype = "dotted") + #male
  geom_segment(aes(x = ts.est.sst$xmin[5], y = ts.est.sst$ymin[5], xend = ts.est.sst$xmax[5], yend = ts.est.sst$ymax[5]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = ts.est.sst$xmin[6], y = ts.est.sst$ymin[6], xend = ts.est.sst$xmax[6], yend = ts.est.sst$ymax[6]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = ts.est.sst$xmin[3], y = ts.est.sst$ymin[3], xend = ts.est.sst$xmax[3], yend = ts.est.sst$ymax[3]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = ts.est.sst$xmin[4], y = ts.est.sst$ymin[4], xend = ts.est.sst$xmax[4], yend = ts.est.sst$ymax[4]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = "\n\nmean length") + 
  theme_classic(base_size = 20)  + 
  theme(axis.title.y = element_text(size = 16))

#CUTI
ts.est.cuti <- data.frame(model = c("2013_F", "2013_M", "2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(tsm.13))[1,1], coef(summary(tsm.13))[1,1], coef(summary(tsm.15))[1,1], coef(summary(tsm.15))[1,1], coef(summary(tsm.17))[1,1], coef(summary(tsm.17))[1,1], coef(summary(tsm))[1,1]),
                     slope = c(coef(summary(tsm.13))[3,1], coef(summary(tsm.13))[3,1]+coef(summary(tsm.13))[6,1], coef(summary(tsm.15))[3,1], coef(summary(tsm.15))[3,1]+coef(summary(tsm.15))[6,1], coef(summary(tsm.17))[3,1], coef(summary(tsm.17))[3,1]+coef(summary(tsm.17))[5,1], coef(summary(tsm))[3,1]),
                     xmin = c(min(ts.13$cuti), min(ts.13$cuti), min(ts.15$cuti), min(ts.15$cuti), min(ts.17$cuti), min(ts.17$cuti), min(ts$cuti)),
                     xmax = c(max(ts.13$cuti), max(ts.13$cuti), max(ts.15$cuti), max(ts.15$cuti), max(ts.17$cuti), max(ts.17$cuti), max(ts.17$cuti)))
ts.est.cuti$ymin = ts.est.cuti$intercept+(ts.est.cuti$slope*ts.est.cuti$xmin)
ts.est.cuti$ymax = ts.est.cuti$intercept+(ts.est.cuti$slope*ts.est.cuti$xmax)

#plot
ts.cuti.plot <- ggplot(filter(tsc, year == "2013" | year == "2015" | year == "2017"), aes(x = cuti, y = length)) + 
  geom_boxplot(data = ts.13, aes(group=cuti), color = "#56B4E9", width = 0.05, alpha = 0.5) + 
  geom_boxplot(data = ts.15, aes(group = cuti), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = ts.17, aes(group = cuti), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = ts.est.cuti$xmin[5], y = ts.est.cuti$ymin[5], xend = ts.est.cuti$xmax[5], yend = ts.est.cuti$ymax[5]), size = 1, color = "black") + 
  geom_segment(aes(x = ts.est.cuti$xmin[1], y = ts.est.cuti$ymin[1], xend = ts.est.cuti$xmax[1], yend = ts.est.cuti$ymax[1]), size = 1, color = "#56B4E9") + #female
  geom_segment(aes(x = ts.est.cuti$xmin[2], y = ts.est.cuti$ymin[2], xend = ts.est.cuti$xmax[2], yend = ts.est.cuti$ymax[2]), size = 1, color = "#56B4E9", linetype = "dotted") + #male
  geom_segment(aes(x = ts.est.cuti$xmin[5], y = ts.est.cuti$ymin[5], xend = ts.est.cuti$xmax[5], yend = ts.est.cuti$ymax[5]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = ts.est.cuti$xmin[6], y = ts.est.cuti$ymin[6], xend = ts.est.cuti$xmax[6], yend = ts.est.cuti$ymax[6]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = ts.est.cuti$xmin[3], y = ts.est.cuti$ymin[3], xend = ts.est.cuti$xmax[3], yend = ts.est.cuti$ymax[3]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = ts.est.cuti$xmin[4], y = ts.est.cuti$ymin[4], xend = ts.est.cuti$xmax[4], yend = ts.est.cuti$ymax[4]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = " ") + 
  theme_classic(base_size = 20)

#Chlorophyll
ts.est.chla <- data.frame(model = c("2013_F", "2013_M", "2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(tsm.13))[1,1], coef(summary(tsm.13))[1,1], coef(summary(tsm.15))[1,1], coef(summary(tsm.15))[1,1], coef(summary(tsm.17))[1,1], coef(summary(tsm.17))[1,1], coef(summary(tsm))[1,1]),
                     slope = c(coef(summary(tsm.13))[4,1], coef(summary(tsm.13))[4,1]+coef(summary(tsm.13))[7,1], coef(summary(tsm.15))[4,1], coef(summary(tsm.15))[4,1]+coef(summary(tsm.15))[7,1], coef(summary(tsm.17))[4,1], coef(summary(tsm.17))[4,1]+coef(summary(tsm.17))[7,1], coef(summary(tsm))[4,1]),
                     xmin = c(min(ts.13$chla), min(ts.13$chla), min(ts.15$chla), min(ts.15$chla), min(ts.17$chla), min(ts.17$chla), min(ts$chla, na.rm = TRUE)),
                     xmax = c(max(ts.13$chla), max(ts.13$chla), max(ts.15$chla), max(ts.15$chla), max(ts.17$chla), max(ts.17$chla), max(ts$chla, na.rm = TRUE)))
ts.est.chla$ymin = ts.est.chla$intercept+(ts.est.chla$slope*ts.est.chla$xmin)
ts.est.chla$ymax = ts.est.chla$intercept+(ts.est.chla$slope*ts.est.chla$xmax)

#plot
ts.chla.plot <- ggplot(filter(tsc, year == "2013" | year == "2015" | year == "2017"), aes(x = chla, y = length)) + 
  geom_boxplot(data = ts.13, aes(group=chla), color = "#56B4E9", width = 0.05, alpha = 0.5) + 
  geom_boxplot(data = ts.15, aes(group = chla), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = ts.17, aes(group = chla), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = ts.est.chla$xmin[7], y = ts.est.chla$ymin[7], xend = ts.est.chla$xmax[7], yend = ts.est.chla$ymax[7]), size = 1, color = "black") +
  geom_segment(aes(x = ts.est.chla$xmin[1], y = ts.est.chla$ymin[1], xend = ts.est.chla$xmax[1], yend = ts.est.chla$ymax[1]), size = 1, color = "#56B4E9") + #female
  geom_segment(aes(x = ts.est.chla$xmin[2], y = ts.est.chla$ymin[2], xend = ts.est.chla$xmax[2], yend = ts.est.chla$ymax[2]), size = 1, color = "#56B4E9", linetype = "dotted") + #male
  geom_segment(aes(x = ts.est.chla$xmin[5], y = ts.est.chla$ymin[5], xend = ts.est.chla$xmax[5], yend = ts.est.chla$ymax[5]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = ts.est.chla$xmin[6], y = ts.est.chla$ymin[6], xend = ts.est.chla$xmax[6], yend = ts.est.chla$ymax[6]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = ts.est.chla$xmin[3], y = ts.est.chla$ymin[3], xend = ts.est.chla$xmax[3], yend = ts.est.chla$ymax[3]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = ts.est.chla$xmin[4], y = ts.est.chla$ymin[4], xend = ts.est.chla$xmax[4], yend = ts.est.chla$ymax[4]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = " ") + 
  theme_classic(base_size = 20)

#Nematoscelis difficilis
#SST
#Model estimates for segents
nd.est.sst <- data.frame(model = c("2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(ndm.15))[1,1] , coef(summary(ndm.15))[1,1] , coef(summary(ndm.17))[1,1] , coef(summary(ndm.17))[1,1] , coef(summary(ndm))[1,1] ),
                     slope = c(coef(summary(ndm.15))[2,1], coef(summary(ndm.15))[2,1]+coef(summary(ndm.15))[5,1], coef(summary(ndm.17))[2,1], coef(summary(ndm.17))[2,1]+coef(summary(ndm.17))[6,1], coef(summary(ndm))[2,1]),
                     xmin = c(min(nd.15$temp_2), min(nd.15$temp_2), min(nd.17$temp_2), min(nd.17$temp_2), min(nd$temp_2)),
                     xmax = c(max(nd.15$temp_2), max(nd.15$temp_2), max(nd.17$temp_2), max(nd.17$temp_2), max(nd$temp_2)))
nd.est.sst$ymin = nd.est.sst$intercept+(nd.est.sst$slope*nd.est.sst$xmin)
nd.est.sst$ymax = nd.est.sst$intercept+(nd.est.sst$slope*nd.est.sst$xmax)

#plot
nd.sst.plot <- ggplot(filter(ndc, year == "2015" | year == "2017"), aes(x = temp_2, y = length)) + 
  geom_boxplot(data = nd.15, aes(group = temp_2), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = nd.17, aes(group = temp_2), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = nd.est.sst$xmin[5], y = nd.est.sst$ymin[5], xend = nd.est.sst$xmax[5], yend = nd.est.sst$ymax[5]), size = 1, color = "black") +
  geom_segment(aes(x = nd.est.sst$xmin[3], y = nd.est.sst$ymin[3], xend = nd.est.sst$xmax[3], yend = nd.est.sst$ymax[3]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = nd.est.sst$xmin[4], y = nd.est.sst$ymin[4], xend = nd.est.sst$xmax[4], yend = nd.est.sst$ymax[4]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = nd.est.sst$xmin[1], y = nd.est.sst$ymin[1], xend = nd.est.sst$xmax[1], yend = nd.est.sst$ymax[1]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = nd.est.sst$xmin[2], y = nd.est.sst$ymin[2], xend = nd.est.sst$xmax[2], yend = nd.est.sst$ymax[2]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = "\n\nmean length", title = " ") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(face = "italic")) + 
  theme(axis.title.y = element_text(size = 16))

#CUTI
#Model estimates for segents
nd.est.cuti <- data.frame(model = c("2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(ndm.15))[1,1] , coef(summary(ndm.15))[1,1] , coef(summary(ndm.17))[1,1] , coef(summary(ndm.17))[1,1] , coef(summary(ndm))[1,1] ),
                     slope = c(coef(summary(ndm.15))[3,1], coef(summary(ndm.15))[3,1]+coef(summary(ndm.15))[6,1], coef(summary(ndm.17))[3,1], coef(summary(ndm.17))[3,1]+coef(summary(ndm.17))[5,1], coef(summary(ndm))[3,1]),
                     xmin = c(min(nd.15$cuti), min(nd.15$cuti), min(nd.17$cuti), min(nd.17$cuti), min(nd$cuti)),
                     xmax = c(max(nd.15$cuti), max(nd.15$cuti), max(nd.17$cuti), max(nd.17$cuti), max(nd$cuti)))
nd.est.cuti$ymin = nd.est.cuti$intercept+(nd.est.cuti$slope*nd.est.cuti$xmin)
nd.est.cuti$ymax = nd.est.cuti$intercept+(nd.est.cuti$slope*nd.est.cuti$xmax)

#plot
nd.cuti.plot <- ggplot(filter(ndc, year == "2015" | year == "2017"), aes(x = cuti, y = length)) + 
  geom_boxplot(data = nd.15, aes(group = cuti), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = nd.17, aes(group = cuti), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = nd.est.cuti$xmin[5], y = nd.est.cuti$ymin[5], xend = nd.est.cuti$xmax[5], yend = nd.est.cuti$ymax[5]), size = 1, color = "black") +
  geom_segment(aes(x = nd.est.cuti$xmin[3], y = nd.est.cuti$ymin[3], xend = nd.est.cuti$xmax[3], yend = nd.est.cuti$ymax[3]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = nd.est.cuti$xmin[4], y = nd.est.cuti$ymin[4], xend = nd.est.cuti$xmax[4], yend = nd.est.cuti$ymax[4]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = nd.est.cuti$xmin[1], y = nd.est.cuti$ymin[1], xend = nd.est.cuti$xmax[1], yend = nd.est.cuti$ymax[1]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = nd.est.cuti$xmin[2], y = nd.est.cuti$ymin[2], xend = nd.est.cuti$xmax[2], yend = nd.est.cuti$ymax[2]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = " ") + 
  theme_classic(base_size = 20)

#Chlorophyll
#Model estimates for segents
nd.est.chla <- data.frame(model = c("2015_F", "2015_M", "2017_F", "2017_M", "All"),
                     intercept = c(coef(summary(ndm.15))[1,1], coef(summary(ndm.15))[1,1], coef(summary(ndm.17))[1,1], coef(summary(ndm.17))[1,1], coef(summary(ndm))[1,1]),
                     slope = c(coef(summary(ndm.15))[4,1], coef(summary(ndm.15))[4,1]+coef(summary(ndm.15))[7,1], coef(summary(ndm.17))[4,1], coef(summary(ndm.17))[4,1]+coef(summary(ndm.17))[7,1], coef(summary(ndm))[4,1]),
                     xmin = c(min(nd.15$chla), min(nd.15$chla), min(nd.17$chla), min(nd.17$chla), min(nd$chla)),
                     xmax = c(max(nd.15$chla), max(nd.15$chla), max(nd.17$chla), max(nd.17$chla), max(nd.17$chla)))
nd.est.chla$ymin = nd.est.chla$intercept+(nd.est.chla$slope*nd.est.chla$xmin)
nd.est.chla$ymax = nd.est.chla$intercept+(nd.est.chla$slope*nd.est.chla$xmax)

#plot
nd.chla.plot <- ggplot(filter(ndc, year == "2015" | year == "2017"), aes(x = chla, y = length)) + 
  geom_boxplot(data = nd.15, aes(group = chla), color = "#D55E00", width = 0.05, alpha = 0.5) +
  geom_boxplot(data = nd.17, aes(group = chla), color = "#0072B2", width = 0.05, alpha = 0.5) +
  geom_segment(aes(x = nd.est.chla$xmin[5], y = nd.est.chla$ymin[5], xend = nd.est.chla$xmax[5], yend = nd.est.chla$ymax[5]), size = 1, color = "black") +
  geom_segment(aes(x = nd.est.chla$xmin[3], y = nd.est.chla$ymin[3], xend = nd.est.chla$xmax[3], yend = nd.est.chla$ymax[3]), size = 1, color = "#0072B2") + #female
  geom_segment(aes(x = nd.est.chla$xmin[4], y = nd.est.chla$ymin[4], xend = nd.est.chla$xmax[4], yend = nd.est.chla$ymax[4]), size = 1, color = "#0072B2", linetype = "dotted") + #male
  geom_segment(aes(x = nd.est.chla$xmin[1], y = nd.est.chla$ymin[1], xend = nd.est.chla$xmax[1], yend = nd.est.chla$ymax[1]), size = 1, color = "#D55E00") + #female
  geom_segment(aes(x = nd.est.chla$xmin[2], y = nd.est.chla$ymin[2], xend = nd.est.chla$xmax[2], yend = nd.est.chla$ymax[2]), size = 1, color = "#D55E00", linetype = "dotted") + #male
  labs(x = " ", y = " ") + 
  theme_classic(base_size = 20)
#====

#LEGENDS
#====
get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

cols <-c("#56B4E9", "#D55E00", "#0072B2", "black")#color palette

epx <- filter(ep, year == "2013" | year == "2015" | year == "2017" | year == "2018")
p1 <- ggplot(epx, aes(x = temp_2, y = length, color = year)) + #plot to establish color legend
  geom_line( size = 2) +
  scale_color_manual(values = cols, guide = "legend", labels = c("2013", "2015", "2017", "All years"), name = "Year") + 
  theme(legend.text = element_text(size = 20), legend.title = element_text(size = 20), legend.background=element_blank(), legend.key = element_blank(), legend.box = "horizontal", legend.position = "bottom")
l1 <- get_legend(p1)#extract color legend

p2 <- ggplot(epx, aes(x = temp_2, y = length, linetype = sex)) + #plot to establish color legend
  geom_line( size = 1) +
  scale_linetype_discrete(guide = guide_legend(override.aes = list(size = 1, color = "#D55E00")), labels = c("Female", "Male"), name = "Sex") + 
  theme(legend.text = element_text(size = 20), legend.title = element_text(size = 20), legend.background=element_blank(), legend.key = element_blank(), legend.box = "horizontal", legend.position = "bottom")
l2 <- get_legend(p2)#extract color legend
#====

#MERGE & SAVE PLOTS
#====
grid <- ggarrange(ep.sst.plot, ep.cuti.plot, ep.chla.plot, ts.sst.plot, ts.cuti.plot, ts.chla.plot, nd.sst.plot, nd.cuti.plot, nd.chla.plot, ncol = 3, nrow = 3, align = "v")
legends <- ggarrange(l1, l2, nrow = 1, ncol = 2, widths = c(7, 3))
ggarrange(grid, legends, ncol = 1, nrow = 2, heights = c(10, 1))
ggsave("figures/manuscript/figure5_heatwaveDrivers.jpg", width = 11, height = 8, dpi = 400)
#====

# Tue Nov  3 14:28:25 2020 ------------------------------

#Full krill lengths model

#LIBRARIES & SOURCES
library(lme4)
library(tidyverse)
library(ggeffects)
library(moments)
library(ggpubr)
library(lubridate)
library(reshape2)
source("scripts/functions/model_simulation.R")
source("scripts/functions/length_frequency.R")
load("data/allLengthsEnv.rda")

#set up
env <- read.csv("data/zoo_selgroups_HadSST_relabundance_5aug2019_plumchrusV_4regions_final_satsstall.csv")
env$date <- mdy(env$time64)
env$year <- as.character(substring(env$date, 1, 4))
env <- filter(env, dtime != 0)#get rid of zero day (UTC correction)
str(env)

#add chlorophyll variable based on prior 3, 8, 13 days
a <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(station, year)), chla = NA))

for(i in seq(1:nrow(a))){
  if(get.chla(a$station[i], a$year[i], 3) != "NaN"){
    a$chla[i] <- get.chla(a$station[i], a$year[i], 3)
  } else if(get.chla(a$station[i], a$year[i], 8) != "NaN"){
    a$chla[i] <- get.chla(a$station[i], a$year[i], 8)
  } else a$chla[i] <- get.chla(a$station[i], a$year[i], 13)
}

allLengthsEnv <- left_join(allLengthsEnv, a)

#add beuti averaged over past 13 days
beuti <- read_csv("data/BEUTI_daily.csv")#load beuti data, PST day
beuti <- filter(beuti, year>2010, year <2019)
beuti$date <- ymd(paste(beuti$year, beuti$month, beuti$day, sep = "-"))
beuti <- melt(beuti, id.vars = c("year", "month", "day", "date"))#long form data
beuti <- rename(beuti, beuti = value)
beuti$latitude.round <- as.numeric(str_sub(as.character(beuti$variable), 1, 2))

a <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(station, year, latitude.round)), beuti = NA))

for(i in seq(1:nrow(a))){
  a$beuti[i] <- get.beuti(a$station[i], a$year[i], 13)
}

allLengthsEnv <- left_join(allLengthsEnv, a)

#add cuti averaged over past 13 days
cuti <- read_csv("data/cuti_daily.csv")#load cuti data, PST day
cuti <- filter(cuti, year>2010, year <2019)
cuti$date <- ymd(paste(cuti$year, cuti$month, cuti$day, sep = "-"))
cuti <- melt(cuti, id.vars = c("year", "month", "day", "date"))#long form data
cuti <- rename(cuti, cuti = value)
cuti$latitude.round <- as.numeric(str_sub(as.character(cuti$variable), 1, 2))

a <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(station, year, latitude.round)), cuti = NA))

for(i in seq(1:nrow(a))){
  a$cuti[i] <- get.cuti(a$station[i], a$year[i], 13)
}

allLengthsEnv <- left_join(allLengthsEnv, a)

#add sla averaged over past 13 days
a <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(station, year, latitude.round)), sla = NA))

for(i in seq(1:nrow(a))){
  a$sla[i] <- get.sla(a$station[i], a$year[i], 1)
}

allLengthsEnv <- left_join(allLengthsEnv, a)

#add spring moci
moci <- read_csv("data/CaliforniaMOCI_JFM1991-JAS2020.csv")#load MOCI data
moci <- rename(moci, year = Year)

a <- summarise(group_by_at(allLengthsEnv, vars(station, date, year, latitude)))
a$year <- as.numeric(a$year)

aN <- filter(a, latitude >=38)
aC <- filter(a, latitude >=34.5, latitude <38)
aS <- filter(a, latitude >=32, latitude <34.5)

krillList <- list(aN, aC, aS)
mociList <- list(moci[,1:4], moci[,c(1:3, 5)], moci[,c(1:3,6)])

for(i in 1:3){#get spring and winter moci values for each region
  b <- filter(mociList[[i]], Season == "AMJ")
  krillList[[i]] <- left_join(krillList[[i]], b, by = "year")
  krillList[[i]] <- rename(krillList[[i]], moci_spring = names(krillList[[i]])[7])
  krillList[[i]] <- select(krillList[[i]], station, date, year, latitude, moci_spring)
}

a <- bind_rows(krillList)#bind all regions into one df
a$year <- as.character(a$year)
allLengthsEnv <- left_join(allLengthsEnv, a)

save(allLengthsEnv, file = "data/allLengthsEnv.rda")
ep <- filter(allLengthsEnv, species == "EP")
ts <- filter(allLengthsEnv, species == "TS")
nd <- filter(allLengthsEnv, species == "ND")

#Scale parameters
allLengthsEnv$temp_2_z <- scale(allLengthsEnv$temp_2)
allLengthsEnv$temp_100_z <- scale(allLengthsEnv$temp_100)
allLengthsEnv$sst_sd_z <- scale(allLengthsEnv$sst_sd)
allLengthsEnv$chla_z <- scale(allLengthsEnv$chla)
allLengthsEnv$beuti_z <- scale(allLengthsEnv$beuti)
allLengthsEnv$moci_spring_z <- scale(allLengthsEnv$moci_spring)
allLengthsEnv$sla_z <- scale(allLengthsEnv$sla)
allLengthsEnv$cuti_z <- scale(allLengthsEnv$cuti)


#MODEL

#Environmental Model
#Full linear model
allLengthsEnv$station <- as.factor(allLengthsEnv$station)
Ml1 <- lmer(length ~ species*sex + temp_2_z + species:temp_2_z + sex:temp_2_z + temp_100_z + species:temp_100_z + sex:temp_100_z + sst_sd + chla_z + beuti_z + sla + moci_spring_z + cuti_z + (1|station), data = allLengthsEnv)
Ml2 <- lmer(length ~ species*sex + temp_2_z + species:temp_2_z + sex:temp_2_z + species:temp_100_z + sex:temp_100_z + sst_sd + chla_z + beuti_z + sla + moci_spring_z + (1|station), data = allLengthsEnv)
Ml3 <- lmer(length ~ species*sex + temp_2_z + species:temp_2_z + species:temp_100_z + sex:temp_100_z + sst_sd + chla_z + beuti_z + sla + moci_spring_z + (1|station), data = allLengthsEnv)
Ml4 <- lmer(length ~ species*sex + temp_2_z + species:temp_2_z + species:temp_100_z + sex:temp_100_z + sst_sd + chla_z + moci_spring_z + cuti + (1|station), data = allLengthsEnv)#no sla or beuti
Ml5 <- lmer(length ~ species*sex + temp_2_z + species:temp_2_z + species:temp_100_z + sex:temp_100_z + sst_sd + chla_z + beuti_z + sla + moci_spring_z + cuti_z + (1|station), data = allLengthsEnv)#all

summary(Ml3)
summary(Ml4)
AIC(Ml3, Ml4, Ml5)

#all species model comparison and evaluation
anova(Ml3)
sjPlot::tab_model(Ml3, 
                  show.re.var= TRUE, 
                  dv.labels= "Environmental Effects on Krill Length", file = "output/Ml3.doc")
sjPlot::plot_model(Ml3)
lattice::dotplot(ranef(Ml3,condVar=TRUE))
save(Ml3, file = "output/Ml3.rda")

load("output/Ml3.rda")
Ml3sim <- fsim.glmm(Ml3)
Ml3simsum <- simsum(Ml3sim)

ggplot(allLengthsEnv) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Ml3simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#Look at group level variation
regions <- read_csv("data/regions.csv")
groups <- data.frame(
  station = as.numeric(row.names(ranef(Ml5)$station)),
  intercept = ranef(Ml5)$station$'(Intercept)'
)
groups <- left_join(groups, regions)
ggplot(groups) +
  geom_point(aes(x = latitude, y = intercept, color = shore)) + 
  geom_hline(aes(yintercept = 0))

#EP Model
ep$station <- as.factor(ep$station)
Mel1 <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + beuti + sla + moci_spring + sst_sd + (1|station), data = ep)
sjPlot::plot_model(Mel1)
Mel2sim <- fsim.glmm(Mel2)
Mel2simsum <- simsum(Mel2sim)
rm(Mel2sim)
save(Mel2simsum, file = "output/Mel2sim.rda")

#TS Model
ts$station <- as.factor(ts$station)
Mtl1 <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + beuti + sla + moci_spring + sst_sd + (1|station), data = ts)
summary(Mtl1)
sjPlot::plot_model(Mtl1)
Mtl1sim <- fsim.glmm(Mtl1)
Mtl1simsum <- simsum(Mtl1sim)
rm(Mtl1sim)
save(Mtl1simsum, file = "output/Mtl1sim.rda")

#ND Model
nd$station <- as.factor(nd$station)
Mnl1 <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + beuti + sla + moci_spring + sst_sd + (1|station), data = nd)
Mnl2 <- lmer(length ~ sex + temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + (1|station), data = nd)
Mnl3 <- lmer(length ~ sex + temp_2 + temp_100 + sst_sd + chla + (1|station), data = nd)
summary(Mnl1)
sjPlot::plot_model(Mnl2)
Mnl3sim <- fsim.glmm(Mnl3)
Mnl3simsum <- simsum(Mnl3sim)
rm(Mnl3sim)
save(Mnl3simsum, file = "output/Mnl3sim.rda")

#plot all three models together for SST
ggplot(allLengthsEnv) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel2simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl1simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "blue") + 
  geom_line(data = summarize(group_by(Mnl3simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#plot all three models together for SST_sd
ggplot(allLengthsEnv) + 
  geom_point(aes(x = sst_sd, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel2simsum, sst_sd), sim.mean = mean(sim.mean)), aes(x= sst_sd, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl1simsum, sst_sd), sim.mean = mean(sim.mean)), aes(x= sst_sd, y = sim.mean), color = "blue") + 
  geom_line(data = summarize(group_by(Mnl3simsum, sst_sd), sim.mean = mean(sim.mean)), aes(x= sst_sd, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature Std. Dev.", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#plot all three models together for chl a
ggplot(allLengthsEnv) + 
  geom_point(aes(x = chla, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel2simsum, chla), sim.mean = mean(sim.mean)), aes(x= chla, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl1simsum, chla), sim.mean = mean(sim.mean)), aes(x= chla, y = sim.mean), color = "blue") + 
  geom_line(data = summarize(group_by(Mnl3simsum, chla), sim.mean = mean(sim.mean)), aes(x= chla, y = sim.mean), color = "green") + 
  labs(x = "Chlorophyll-a Content", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#N vs S same plot
x <- filter(allLengthsEnv, latitude >34.4)
y <- filter(allLengthsEnv, latitude <=34.4)

#north simulations
Mel2.n <- lmer(length ~ sex*temp_2 + temp_100 + sst_sd + chla + (1|station), data = filter(x, species == "EP"))
Mtl1.n <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + (1|station), data = filter(x, species == "TS"))
Mnl3.n <- lmer(length ~ sex + temp_2 + temp_100 + sst_sd + chla + (1|station), data = filter(x, species == "ND"))
Mel2.n.sim <- fsim.glmm(Mel2.n)
Mel2.n.simsum <- simsum(Mel2.n.sim)
Mtl1.n.sim <- fsim.glmm(Mtl1.n)
Mtl1.n.simsum <- simsum(Mtl1.n.sim)
Mnl3.n.sim <- fsim.glmm(Mnl3.n)
Mnl3.n.simsum <- simsum(Mnl3.n.sim)

n <- ggplot(x) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel2.n.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl1.n.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "blue") + 
  geom_line(data = summarize(group_by(Mnl3.n.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#south simulations
Mel2.s <- lmer(length ~ sex*temp_2 + temp_100 + sst_sd + chla + (1|station), data = filter(y, species == "EP"))
Mtl1.s <- lmer(length ~ sex*temp_2 + temp_100 + sex:temp_100 + sst_sd + chla + (1|station), data = filter(y, species == "TS"))
Mnl3.s <- lmer(length ~ sex + temp_2 + temp_100 + sst_sd + chla + (1|station), data = filter(y, species == "ND"))
Mel2.s.sim <- fsim.glmm(Mel2.s)
Mel2.s.simsum <- simsum(Mel2.s.sim)
Mtl1.s.sim <- fsim.glmm(Mtl1.s)
Mtl1.s.simsum <- simsum(Mtl1.s.sim)
Mnl3.s.sim <- fsim.glmm(Mnl3.s)
Mnl3.s.simsum <- simsum(Mnl3.s.sim)

s <- ggplot(y) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel2.s.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl1.s.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "blue") +
  geom_line(data = summarize(group_by(Mnl3.s.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#together
plot <- ggarrange(n, s, labels = c("North", "South"), ncol = 2, nrow = 1)
ggsave(plot, file = "figures/nVSs.jpg")











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

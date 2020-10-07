# Wed Oct  7 12:09:56 2020 ------------------------------

#Clean Length~Environment Modeling Script


library(lme4)
library(ggeffects)
library(moments)
library(ggpubr)
source("scripts/functions/model_simulation.R")
source("scripts/functions/length_frequency.R")
load("data/allLengthsEnv.rda")


#Environmental Model
#Full linear model
Ml1 <- lmer(length ~ species*sex + temp_2 + species:temp_2 + sex:temp_2 + temp_100 + species:temp_100 + sex:temp_100 + (1|station), data = allLengthsEnv)

#model comparison and evaluation
anova(Ml1)
sjPlot::tab_model(Ml1, 
                  show.re.var= TRUE, 
                  dv.labels= "Environmental Effects on Krill Length", file = "output/Ml1.doc")
sjPlot::plot_model(Ml1)
lattice::dotplot(ranef(Ml1,condVar=TRUE))
save(Ml1, file = "output/Ml1.rda")

Ml1sim <- fsim.glmm(Ml1)
Ml1simsum <- simsum(Ml1sim)

ggplot(allLengthsEnv) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Ml1simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))



#EP Model
ep$station <- as.factor(ep$station)
Mel1 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = ep)
Mel2 <- lmer(length ~  sex + temp_2 + sex:temp_2 + temp_100 + (1|station), data = ep)
sjPlot::plot_model(Mel2)

Mel2sim <- fsim.glmm(Mel2)
Mel2simsum <- simsum(Mel2sim)

#TS Model
ts$station <- as.factor(ts$station)
Mtl1 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = ts)
Mtl2 <- lmer(length ~ sex + temp_2 + temp_100 + sex:temp_100 + (1|station), data = ts)

sjPlot::plot_model(Mtl2)

Mtl2sim <- fsim.glmm(Mtl2)
Mtl2simsum <- simsum(Mtl2sim)

#ND Model
nd$station <- as.factor(nd$station)
Mnl1 <- lmer(length ~ sex + temp_2 + sex:temp_2 + temp_100 + sex:temp_100 + (1|station), data = nd)
Mnl2 <- lmer(length ~ sex + temp_2 + temp_100 + sex:temp_100 + (1|station), data = nd)
Mnl3 <- lmer(length ~ sex + temp_2 + temp_100 + (1|station), data = nd)

sjPlot::plot_model(Mnl3)

Mnl3sim <- fsim.glmm(Mnl3)
Mnl3simsum <- simsum(Mnl3sim)

#plot all three models together
ggplot(allLengthsEnv) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel2simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl2simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "blue") + 
  geom_line(data = summarize(group_by(Mnl3simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#N vs S same plot
x <- filter(allLengthsEnv, latitude >34.4)
y <- filter(allLengthsEnv, latitude <=34.4)

#north simulations
Mel2.n <- lmer(length ~  sex + temp_2 + sex:temp_2 + temp_100 + (1|station), data = filter(x, species == "EP"))
Mtl2.n <- lmer(length ~ sex + temp_2 + temp_100 + sex:temp_100 + (1|station), data = filter(x, species == "TS"))
Mnl3.n <- lmer(length ~ sex + temp_2 + temp_100 + (1|station), data = filter(x, species == "ND"))
Mel2.n.sim <- fsim.glmm(Mel2.n)
Mel2.n.simsum <- simsum(Mel2.n.sim)
Mtl2.n.sim <- fsim.glmm(Mtl2.n)
Mtl2.n.simsum <- simsum(Mtl2.n.sim)
Mnl3.n.sim <- fsim.glmm(Mnl3.n)
Mnl3.n.simsum <- simsum(Mnl3.n.sim)

n <- ggplot(x) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel2.n.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl2.n.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "blue") + 
  geom_line(data = summarize(group_by(Mnl3.n.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
  labs(x = "Sea Surface Temperature (C)", y = "Length (mm)") +
  theme(text = element_text(size = 20)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#south simulations
Mel2.s <- lmer(length ~  sex + temp_2 + sex:temp_2 + temp_100 + (1|station), data = filter(y, species == "EP"))
Mtl2.s <- lmer(length ~ sex + temp_2 + temp_100 + sex:temp_100 + (1|station), data = filter(y, species == "TS"))
Mnl3.s <- lmer(length ~ sex + temp_2 + temp_100 + (1|station), data = filter(y, species == "ND"))
Mel2.s.sim <- fsim.glmm(Mel2.s)
Mel2.s.simsum <- simsum(Mel2.s.sim)
Mtl2.s.sim <- fsim.glmm(Mtl2.s)
Mtl2.s.simsum <- simsum(Mtl2.s.sim)
Mnl3.s.sim <- fsim.glmm(Mnl3.s)
Mnl3.s.simsum <- simsum(Mnl3.s.sim)

s <- ggplot(y) + 
  geom_point(aes(x = temp_2, y = length, color = species), alpha = 0.4) + 
  geom_line(data = summarize(group_by(Mel2.s.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "red") + 
  geom_line(data = summarize(group_by(Mtl2.s.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "blue") +
  geom_line(data = summarize(group_by(Mnl3.s.simsum, temp_2), sim.mean = mean(sim.mean)), aes(x= temp_2, y = sim.mean), color = "green") + 
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

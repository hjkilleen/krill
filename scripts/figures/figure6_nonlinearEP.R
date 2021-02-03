#Script to identify and plot nonlinear models for length~temperature
# Wed Feb  3 12:22:58 2021 ------------------------------

#https://www.statforbiology.com/nonlinearregression/usefulequations#asymptotic_regression_model
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/SSasymp

#LIBRARIES & SOURCES
#====
library(nlme)
library(tidyverse)
library(ggpubr)
library(kableExtra)
load("data/allLengthsEnvEP.rda")
#====

#SETUP
#====
#Self-starting values for regression models
Asym <- 4#horizontal asymptote
r0 <- 0#respone when scaled temp_2 is 0
lrc <- -0.5#natural logarithm of the rate constant

epg <- summarize(group_by_at(ep, vars(sites, temp_2)), length = mean(length))#group data by sites and SST value for plot
#====

#MODELING 
#====
#Model testing
ep1 <- lm(length~temp_2, ep)#linear
ep2 <- nls(length ~ SSasymp(temp_2, Asym, r0, lrc), data = ep)#monomolecular growth/von Bertalanffy
AIC(ep1, ep2)
#====

#PLOTTING NONLINEAR MODEL
#====
epg2 <- nls(length ~ SSasymp(temp_2, Asym, r0, lrc), data = epg)
a <- ggplot(augment(ep2))+ 
  geom_line(aes(x = (temp_2*attr(ep$temp_2, "scaled:scale")+attr(ep$temp_2, "scaled:center")), y = (.fitted*attr(ep$length, "scaled:scale")+attr(ep$length, "scaled:center")))) + 
  geom_point(data = epg, aes(x = (temp_2*attr(ep$temp_2, "scaled:scale")+attr(ep$temp_2, "scaled:center")), y = (length*attr(ep$length, "scaled:scale")+attr(ep$length, "scaled:center")))) +
  labs(x = "Sea Surface Temperature (C)", 
       y = "Length (mm)", 
       title = quote("E. pacifica asymptotic length model" ~ {L[phi]('T') == phi[1] + (phi[2]-phi[1])*~e^{-e^{phi[3]}*~'T'}}), 
       subtitle = (quote(list(L == "Length", 'T' == "Temperature", phi[1] == "Asymptote", phi[2] == "Intercept", phi[3] == "Rate constant")))) + 
  theme_classic(base_size = 15)
a
#====

#SUMMARY TABLE
#====
#Model contrasts
b <- AIC(ep1, ep2)
row.names(b) <- c("Linear model", "Asymptotic model")
b %>% 
  kbl() %>% 
  kable_classic()
#====
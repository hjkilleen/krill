#Figure 7
#Nonlinear temperature effects on E. pacifica

# Wed Feb  3 12:22:58 2021 ------------------------------

#https://www.statforbiology.com/nonlinearregression/usefulequations#asymptotic_regression_model
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/SSasymp

#LIBRARIES & SOURCES
#====
library(nlme)
library(tidyverse)
library(ggpubr)
library(kableExtra)
library(broom)
library(mdthemes)
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
lab1 <- expression(~phi[1] == 0.825)#annotation labels
ggplot(augment(ep2))+ 
  geom_line(aes(x = (temp_2*attr(ep$temp_2, "scaled:scale")+attr(ep$temp_2, "scaled:center")), y = (.fitted*attr(ep$length, "scaled:scale")+attr(ep$length, "scaled:center")))) + 
  geom_point(data = epg, aes(x = (temp_2*attr(ep$temp_2, "scaled:scale")+attr(ep$temp_2, "scaled:center")), y = (length*attr(ep$length, "scaled:scale")+attr(ep$length, "scaled:center")))) +
  annotate("text", x = 15, y = 19.5, label = "\u03d5[1] = 0.825***", size = 5, hjust = 0) + 
  annotate("text", x = 15, y = 19, label = "\u03d5[2] = 0.026*", size = 5, hjust = 0) + 
  annotate("text", x = 15, y = 18.5, label = "\u03d5[3] = -1.299***", size = 5, hjust = 0) + 
  labs(x = "SST (°C)", 
       y = "Length (mm)", 
       title = "E. pacifica asymptotic length model", 
       subtitle = (quote({L[phi]('T') == phi[1] + (phi[2]-phi[1])*~e^{-e^{phi[3]}*~'T'}}))) +
  theme_classic(base_size = 20) + 
  ggsave("figures/manuscript/figure7_nonlinear.jpeg", width =6.5, height = 5, dpi = 400)

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
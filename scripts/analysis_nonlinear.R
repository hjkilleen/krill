#Script to identify and plot nonlinear models for length~temperature
# Wed Feb  3 12:22:58 2021 ------------------------------

#https://www.statforbiology.com/nonlinearregression/usefulequations#asymptotic_regression_model
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/SSasymp

#LIBRARIES & SOURCES
#====
library(nlme)
library(broom)
library(Amelia)
library(mlbench)
library(splines)
load("data/allLengthsEnv.rda")
load("data/allLengthsEnvEP.rda")
load("data/allLengthsEnvND.rda")
load("data/allLengthsEnvTS.rda")
missmap(nd, col=c("blue", "red"), legend=FALSE)
#====

#SETUP
#====
Asym <- 4
r0 <- .24
lrc <- -1
b <- .5
epg <- summarize(group_by_at(ep, vars(sites, temp_2)), length = mean(length))
ndg <- summarize(group_by_at(nd, vars(sites, temp_2, temp_100)), length = mean(length))

#Euphausia pacifica
#Model testing
ep1 <- lm(length~temp_2, ep)#linear
ep2 <- nls(length ~ SSasymp(temp_2, Asym, r0, lrc), data = ep)#monomolecular growth/von Bertalanffy
ep3 <- nls(length ~ SSgompertz(temp_2, Asym, r0, b), data = ep)#Gompertz
AIC(ep1, ep2, ep3)

#Final model
epg2 <- nls(length ~ SSasymp(temp_2, Asym, r0, lrc), data = epg)
a <- ggplot(augment(epg2), aes(temp_2, length))+ 
  geom_point() +
  geom_line(aes(y = .fitted)) + 
  labs(x = "Sea Surface Temperature (C)", 
       y = "Length (mm)", 
       title = quote("Asymptotic length model " ~ {L[phi]('T') == phi[1] + (phi[2]-phi[1])*~e^{-e^{phi[3]}*~'T'}}), 
       subtitle = (quote(list(L == "Length", 'T' == "Temperature", phi[1] == "Asymptote", phi[2] == "Intercept", phi[3] == "Rate constant")))) + 
  annotate("text", x = 9.75, y = 23.5, label = "E. pacifica", fontface = "bold") + 
  annotate("text", x = 17, y = 20, label = quote(phi[1] == "22.5***"), hjust = 0) + 
  annotate("text", x = 17, y = 19.5, label = quote(phi[2] == "10.06"), hjust = 0) + 
  annotate("text", x = 17, y = 19, label = quote(phi[3] == "-1.72"), hjust = 0)


#Nematocelis difficilis
#Set Up
Asym <- 2
r0 <- 0
lrc <- -1
b <- .5
#Model testing
nd1 <- lm(scale(length)~scale(temp_100), nd)#linear
nd2 <- nls(length ~ SSasymp(temp_100, Asym, r0, lrc), data = nd)#monomolecular growth/von Bertalanffy
nd3 <- nls(length ~ SSgompertz(temp_2, Asym, r0, b), data = nd)#Gompertz
AIC(nd1, nd2, nd3)

#Final model
ndg2 <- nls(length ~ SSasymp(temp_2, Asym, r0, lrc), data = ndg)
b <- ggplot(augment(ndg2), aes(temp_2, length))+ 
  geom_point() +
  geom_line(aes(y = .fitted)) + 
  labs(x = "Sea Surface Temperature (C)", 
       y = "Length (mm)") + 
  annotate("text", x = 10.75, y = 24.5, label = "N. difficilis", fontface = "bold") + 
  annotate("text", x = 17, y = 21, label = quote(phi[1] == "22.85**"), hjust = 0) + 
  annotate("text", x = 17, y = 20.75, label = quote(phi[2] == "-14.51"), hjust = 0) + 
  annotate("text", x = 17, y = 20.5, label = quote(phi[3] == "-2.27"), hjust = 0)

#Model contrasts
a <- AIC(ep1, ep2, ep3)
row.names(a) <- c("EP linear", "EP asymptote", "EP Gompertz")
b <- AIC(nd1, nd2, nd3)
row.names(b) <- c("ND linear", "ND asymptote", "ND Gompertz")
c <- rbind(a, b)
formattable::formattable(c)

#Final plot
gridExtra::grid.arrange(a, b)
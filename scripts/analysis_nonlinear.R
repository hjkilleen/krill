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

#Euphausia pacifica
#Set up
Asym <- 4
r0 <- .24
lrc <- -1
b <- .5
epg <- summarize(group_by_at(ep, vars(sites, temp_2)), length = mean(length))
ndg <- summarize(group_by_at(nd, vars(sites, temp_2, temp_100)), length = mean(length))

#Model testing
ep1 <- lm(length~temp_2, ep)#linear
ep2 <- nls(length ~ SSasymp(temp_2, Asym, r0, lrc), data = ep)#monomolecular growth/von Bertalanffy
ep3 <- nls(length ~ SSgompertz(temp_2, Asym, r0, b), data = ep)#Gompertz, singular gradient
AIC(ep1, ep2)

#Thysanoessa spinifera
#Set up
Asym <- 4
r0 <- .24
lrc <- -1
b <- .5
tsg <- summarize(group_by_at(ts, vars(sites, temp_2)), length = mean(length))
tsg <- summarize(group_by_at(ts, vars(sites, temp_2, temp_100)), length = mean(length))

#Model testing
ts1 <- lm(length~temp_2, ts)#linear
ts2 <- nls(length ~ SSasymp(temp_2, Asym, r0, lrc), data = ts)#monomolecular growth/von Bertalanffy
ts3 <- nls(length ~ SSgompertz(temp_2, Asym, r0, b), data = ts)#Gompertz, singular gradient
AIC(ts1, ts2)


#Nematocelis difficilis
#Set Up
Asym <- 2
r0 <- .2
lrc <- -1
b <- .5
#Model testing
nd1 <- lm(scale(length)~scale(temp_100), nd)#linear
nd2 <- nls(length ~ SSasymp(temp_100, Asym, r0, lrc), data = nd)#monomolecular growth/von Bertalanffy, singular gradient
AIC(nd1, nd2)

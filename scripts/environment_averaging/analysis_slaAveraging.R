#sla averaging
# Wed Nov 11 10:20:51 2020 ------------------------------

#LIBRARIES & SOURCES
library(readr)
library(lubridate)
library(reshape2)
library(stringr)
source("scripts/functions/length_frequency.R")

#set up 
epn <- summarise(group_by_at(ep, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
epn$sla <- rep(NA, nrow(epn))
env$date <- as.POSIXct(env$time64)
env$year <- as.character(substring(env$date, 1, 4))
env <- filter(env, dtime != 0)#get rid of zero day (UTC correction)

#EP length~sla relationship
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epn))){
    epn$sla[i] <- get.sla(epn$station[i], epn$year[i], n)
    m <- lm(length~sla, data = epn)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epdata <- as.data.frame(do.call(rbind, datalist))
names(epdata) <- c("n", "r")
ggplot(epdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor Sea Level Anomaly") +
  ggsave("output/slaAveraging.jpg")
rm(datalist)
#Highest R values occur for sla on day prior to capture. 

#Get values (all NAs are too close to shore, no AVISO data available)

for(i in seq(1:nrow(epn))){
  epn$sla[i] <- get.sla(epn$station[i], epn$year[i], 1)
}

#Best fit plot
ggplot(epn, aes(x = sla, y = length)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  ggtitle("Best sla fit") + 
  ggsave("output/slaAveragingBestFit.jpg")

#Fit just for the north
epnN <- filter(epn, latitude > 36)
epnN$sla <- rep(NA, nrow(epnN))
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epnN))){
    epnN$sla[i] <- get.sla(epnN$station[i], epnN$year[i], n)
    m <- lm(length~sla, data = epnN)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epNdata <- as.data.frame(do.call(rbind, datalist))
names(epNdata) <- c("n", "r")
ggplot(epNdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor Sea Level Anomaly NORTH") +
  ggsave("output/slaAveragingNorth.jpg")
rm(datalist)
#Fit in the North is slightly better when averaged over 3 days prior to capture, but not by much. 
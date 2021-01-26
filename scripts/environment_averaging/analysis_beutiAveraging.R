#BEUTI data
# Tue Nov 10 16:31:13 2020 ------------------------------

#LIBRARIES & SOURCES
library(readr)
library(lubridate)
library(reshape2)
library(stringr)
source("scripts/functions/length_frequency.R")

#set up 
beuti <- read_csv("data/BEUTI_daily.csv")#load beuti data, PST day
beuti <- filter(beuti, year>2010, year <2019)
beuti$date <- ymd(paste(beuti$year, beuti$month, beuti$day, sep = "-"))
beuti <- melt(beuti, id.vars = c("year", "month", "day", "date"))#long form data
beuti <- rename(beuti, beuti = value)
beuti$latitude.round <- as.numeric(str_sub(as.character(beuti$variable), 1, 2))
str(beuti)

epn <- summarise(group_by_at(ep, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
epn$beuti <- rep(NA, nrow(epn))
epn$latitude.round <- round(epn$latitude)

#EP length~beuti relationship
get.beuti(110, 2011, 2)
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epn))){
    epn$beuti[i] <- get.beuti(epn$station[i], epn$year[i], n)
    m <- lm(length~beuti, data = epn)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epdata <- as.data.frame(do.call(rbind, datalist))
names(epdata) <- c("n", "r")
ggplot(epdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor BEUTI index") +
  ggsave("output/beutiAveraging.jpg")
rm(datalist)

#BEUTI has highest explanatory power when averaged over the 13 days prior to collection R~0.04
for(i in seq(1:nrow(epn))){
  epn$beuti[i] <- get.beuti(epn$station[i], epn$year[i], 13)
}

#Best fit plot
ggplot(epn, aes(x = beuti, y = length)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) + 
  ggtitle("Best BEUTI fit") + 
  ggsave("output/beutiAveragingBestFit.jpg")
#looks like it might be more of a logistic fit than linear

#Fit just for the north
epnN <- filter(epn, latitude > 36)
epnN$beuti <- rep(NA, nrow(epnN))
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epnN))){
    epnN$beuti[i] <- get.beuti(epnN$station[i], epnN$year[i], n)
    m <- lm(length~beuti, data = epnN)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epNdata <- as.data.frame(do.call(rbind, datalist))
names(epNdata) <- c("n", "r")
ggplot(epNdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor BEUTI index NORTH") +
  ggsave("output/beutiAveragingNorth.jpg")
rm(datalist)

#Best fit plot NORTH
for(i in seq(1:nrow(epnN))){
  epnN$beuti[i] <- get.beuti(epnN$station[i], epnN$year[i], 13)
}
ggplot(epnN, aes(x = beuti, y = length)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) + 
  ggtitle("Best BEUTI fit") + 
  ggsave("output/beutiAveragingBestFitNORTH.jpg")

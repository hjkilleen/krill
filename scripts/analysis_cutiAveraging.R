#CUTI data
# Mon Nov 16 11:15:52 2020 ------------------------------

#LIBRARIES & SOURCES
library(readr)
library(lubridate)
library(reshape2)
library(stringr)
source("scripts/functions/length_frequency.R")

#set up 
cuti <- read_csv("data/CUTI_daily.csv")#load cuti data, PST day
cuti <- filter(cuti, year>2010, year <2019)
cuti$date <- ymd(paste(cuti$year, cuti$month, cuti$day, sep = "-"))
cuti <- melt(cuti, id.vars = c("year", "month", "day", "date"))#long form data
cuti <- rename(cuti, cuti = value)
cuti$latitude.round <- as.numeric(str_sub(as.character(cuti$variable), 1, 2))
str(cuti)

epn <- summarise(group_by_at(ep, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
epn$cuti <- rep(NA, nrow(epn))
epn$latitude.round <- round(epn$latitude)

#EP length~cuti relationship
get.cuti(110, 2011, 2)
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epn))){
    epn$cuti[i] <- get.cuti(epn$station[i], epn$year[i], n)
    m <- lm(length~cuti, data = epn)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epdata <- as.data.frame(do.call(rbind, datalist))
names(epdata) <- c("n", "r")
ggplot(epdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor cuti index") +
  ggsave("output/cutiAveraging.jpg")
rm(datalist)

#cuti has highest explanatory power when averaged over the 13 days prior to collection R~0.04
for(i in seq(1:nrow(epn))){
  epn$cuti[i] <- get.cuti(epn$station[i], epn$year[i], 13)
}

#Best fit plot
ggplot(epn, aes(x = cuti, y = length)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) + 
  ggtitle("Best cuti fit") + 
  ggsave("output/cutiAveragingBestFit.jpg")
#looks like it might be more of a logistic fit than linear

#Fit just for the north
epnN <- filter(epn, latitude > 36)
epnN$cuti <- rep(NA, nrow(epnN))
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epnN))){
    epnN$cuti[i] <- get.cuti(epnN$station[i], epnN$year[i], n)
    m <- lm(length~cuti, data = epnN)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epNdata <- as.data.frame(do.call(rbind, datalist))
names(epNdata) <- c("n", "r")
ggplot(epNdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor cuti index NORTH") +
  ggsave("output/cutiAveragingNorth.jpg")
rm(datalist)

#Best fit plot NORTH
for(i in seq(1:nrow(epnN))){
  epnN$cuti[i] <- get.cuti(epnN$station[i], epnN$year[i], 13)
}
ggplot(epnN, aes(x = cuti, y = length)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) + 
  ggtitle("Best cuti fit") + 
  ggsave("output/cutiAveragingBestFitNORTH.jpg")

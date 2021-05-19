#sst_sd averaging
# Wed Nov 11 09:46:59 2020 ------------------------------

#LIBRARIES & SOURCES
library(readr)
library(lubridate)
library(reshape2)
library(stringr)
source("scripts/functions/length_frequency.R")

#set up 

epn <- summarise(group_by_at(ep, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
epn$sst_sd <- rep(NA, nrow(epn))

#EP length~sst_sd relationship
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epn))){
    epn$sst_sd[i] <- get.sd(epn$station[i], epn$year[i], n)
    m <- lm(length~sst_sd, data = epn)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epdata <- as.data.frame(do.call(rbind, datalist))
names(epdata) <- c("n", "r")
c <- ggplot(epdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("SST Std. Dev.") +
  theme_classic(base_size = 20) +
  ggsave("output/sst_sdAveraging.jpg")
save(c, file = "output/sstSdAveraging.rda")
rm(datalist)

#SST_SD has highest explanatory power when averaged over the 17 days prior to collection R~0.045
for(i in seq(1:nrow(epn))){
  epn$sst_sd[i] <- get.sd(epn$station[i], epn$year[i], 17)
}

#Best fit plot
ggplot(epn, aes(x = sst_sd, y = length)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  ggtitle("Best SST Std. Dev fit") + 
  ggsave("output/sst_sdAveragingBestFit.jpg")

#Fit just for the north
epnN <- filter(epn, latitude > 36)
epnN$sst_sd <- rep(NA, nrow(epnN))
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epnN))){
    epnN$sst_sd[i] <- get.sd(epnN$station[i], epnN$year[i], n)
    m <- lm(length~sst_sd, data = epnN)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epNdata <- as.data.frame(do.call(rbind, datalist))
names(epNdata) <- c("n", "r")
ggplot(epNdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor SST Std. Dev. NORTH") +
  ggsave("output/ssd_sdAveragingNorth.jpg")
rm(datalist)
#Best fit with averaging over 17 days with R~0.08

#Best fit plot NORTH
for(i in seq(1:nrow(epnN))){
  epnN$sst_sd[i] <- get.sd(epnN$station[i], epnN$year[i], 17)
}
ggplot(epnN, aes(x = sst_sd, y = length)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle("Best SST Std. Dev. fit") + 
  ggsave("output/sst_sdAveragingBestFitNORTH.jpg")

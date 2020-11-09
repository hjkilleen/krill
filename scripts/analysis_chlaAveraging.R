# Thu Oct  8 16:47:40 2020 ------------------------------
#Script to derive maximum r2 value for chlorophyll length relationship

#LIBRARIES & SOURCES
source("scripts/functions/length_frequency.R")

#set up 
epn <- summarise(group_by_at(ep, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
epn$chla <- rep(NA, nrow(epn))
env$date <- as.POSIXct(env$time64)
env$year <- as.character(substring(env$date, 1, 4))
env <- filter(env, dtime != 0)#get rid of zero day (UTC correction)

#==========
#See how well analyze_sst aligns with ROMS/TOMS temp_2
d <- env
d$date <- as_date(d$time64)
d$temp <- d$analysed_sst-273.15#convert temperature to Celsius
d1 <- filter(d, dtime == 0)
d2 <- filter(d, dtime == -1)
d1$roms <- rep(NA, nrow(d1))#ROMS day x = sattelite day x
d2$roms <- rep(NA, nrow(d2))#ROMS day x = sattelite day x-1
for (i in seq(1:nrow(d1))) {#get roms temp for d1
  d1$roms[i] <- filter(waterTemp, date == d1$date[i], station == d1$station[i])$temp_2
}
for (i in seq(1:nrow(d2))) {#get roms temp for d2
  d2$roms[i] <- filter(waterTemp, date == d2$date[i], station == d2$station[i])$temp_2
}
#Plot d1 and d2 roms vs satellite sst
plot(d1$temp, d1$roms)
plot(d2$temp, d2$roms)
m1 <- lm(temp~roms, d1)
m2 <- lm(temp~roms, d2)
summary(m1)
summary(m2)
#pretty strong ~85% correlation, a bit stronger (1-2%) for d2. So ROMS day x = sattelite day x-1. Will refrain from using analyse_sst in the full model because it is roughly similar to ROMS data, but presents issues with inconsistent spatial resolution across the domain. 
#========

#EP length~chlorophyll relationship
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epn))){
    epn$chla[i] <- get.chla(epn$station[i], epn$year[i], n)
    m <- lm(length~chla, data = epn)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epdata <- as.data.frame(do.call(rbind, datalist))
names(epdata) <- c("n", "r")
ggplot(epdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor Log chlorophyll_a mg/m3") +
  ggsave("output/chlaAveraging.jpg")
rm(datalist)

#Chlorophyll values chose for each station/date based on mean values over the prior three days. If no data is available, the values are averaged over past 8 days, and then over 13 days. If station/date still does not have a value, then NA is accepted. 
for(i in seq(1:nrow(epn))){
  if(get.chla(epn$station[i], epn$year[i], 3) != "NaN"){
    epn$chla[i] <- get.chla(epn$station[i], epn$year[i], 3)
  } else if(get.chla(epn$station[i], epn$year[i], 8) != "NaN"){
    epn$chla[i] <- get.chla(epn$station[i], epn$year[i], 8)
  } else epn$chla[i] <- get.chla(epn$station[i], epn$year[i], 13)
}

#Best fit plot
ggplot(epn, aes(x = chla, y = length)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  ggtitle("Best chla fit") + 
  ggsave("output/chlaAveraging.jpg")

#Fit just for the north
epnN <- filter(epn, latitude > 36)
epnN$chla <- rep(NA, nrow(epnN))
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epnN))){
    epnN$chla[i] <- get.chla(epnN$station[i], epnN$year[i], n)
    m <- lm(length~chla, data = epnN)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epNdata <- as.data.frame(do.call(rbind, datalist))
names(epNdata) <- c("n", "r")
ggplot(epNdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor Log chlorophyll mg/m3 NORTH") +
  ggsave("output/chlaAveragingNorth.jpg")
rm(datalist)

# Thu Oct  8 16:47:40 2020 ------------------------------
#Script to derive maximum r2 value for chlorophyll length relationship

#set up 
epn <- summarise(group_by_at(ep, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
epn$chla <- rep(NA, nrow(epn))
env$date <- as.POSIXct(env$time64)
env$year <- as.factor(substring(env$date, 1, 4))
env <- filter(env, dtime != 0)#get rid of zero day (UTC correction)

#get chlA functions
get.chla <- function(x, y, z) {
  end.date <- 0
  start.date <- end.date-z
  chla_mean <- mean(filter(env, station == x, year == y, dtime >= start.date, dtime <= end.date)$chlor_a, na.rm = TRUE)
  chla_log <- log(chla_mean)#log transform chlorophyll to make distribution normal
  chla_log
}
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

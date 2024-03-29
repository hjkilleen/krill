# Thu Oct  8 16:47:40 2020 ------------------------------
#Script to derive maximum r2 value for temperature length relationship
load("data/allLengthsEnvEP.rda")
#set up 
epn <- summarise(group_by_at(ep, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
epn$temp_2 <- rep(NA, nrow(epn))
epn$temp_100 <- rep(NA, nrow(epn))
ggplot(epn, aes(x = latitude, y = length)) + 
  geom_point()

#alter get temp functions
get.temp.2t <- function(x, y, z) {
  end.date <- filter(epn, station == x, year == y)$date[1]
  start.date <- end.date-z
  temp_2_mean <- mean(filter(waterTemp, station == x, year == y, date >= start.date, date <= end.date)$temp_2)
  temp_2_mean
}
get.temp.100t <- function(x, y, z) {
  end.date <- filter(epn, station == x, year == y)$date[1]
  start.date <- end.date-z
  temp_100_mean <- mean(filter(waterTemp, station == x, year == y, date >= start.date, date <= end.date)$temp_100)
  temp_100_mean
}

#EP surface relationship
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epn))){
  epn$temp_2[i] <- get.temp.2t(epn$station[i], epn$year[i], n)
  m <- lm(length~temp_2, data = epn)
  r <- summary(m)$r.squared
  datalist[[n]] <- c(n, r)
}
}
epdata <- as.data.frame(do.call(rbind, datalist))
names(epdata) <- c("n", "r")
a <- ggplot(epdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("SST") +
  theme_classic(base_size = 20) +
  ggsave("output/temp_2Averaging.jpg")
save(a, file = "output/sstAveraging.rda")
rm(datalist)
#EP at depth relationship
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epn))){
    epn$temp_100[i] <- get.temp.100t(epn$station[i], epn$year[i], n)
    m <- lm(length~temp_100, data = epn)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epdata <- as.data.frame(do.call(rbind, datalist))
names(epdata) <- c("n", "r")
b <- ggplot(epdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("100m temperature") +
  theme_classic(base_size = 20) +
  ggsave("output/temp_100Averaging.jpg")
save(b, file = "output/temp100Averaging.rda")
rm(datalist)
#plots for best fits
n.a <- 2
n.b <- 5
for(i in seq(1:nrow(epn))){
  epn$temp_2[i] <- get.temp.2t(epn$station[i], epn$year[i], n.a)
  epn$temp_100[i] <- get.temp.100t(epn$station[i], epn$year[i], n.b)
}
ggplot(epn, aes(x = temp_2, y = length)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  ggtitle("Best temp_2 fit") + 
  ggsave("output/temp_2Averaging_BestFit.jpg")

ggplot(epn, aes(x = temp_100, y = length)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  ggtitle("Best temp_100 fit") + 
  ggsave("output/temp_100Averaging_BestFit.jpg")

#Fit just for the north
epnN <- filter(epn, latitude > 34.4)
epnN$temp_2 <- rep(NA, nrow(epnN))
epnN$temp_100 <- rep(NA, nrow(epnN))
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(epnN))){
    epnN$temp_2[i] <- get.temp.2t(epnN$station[i], epnN$year[i], n)
    m <- lm(length~temp_2, data = epnN)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
epNdata <- as.data.frame(do.call(rbind, datalist))
names(epNdata) <- c("n", "r")
ggplot(epNdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor surface temperature NORTH") +
  ggsave("output/temp_2AveragingNorth.jpg")
rm(datalist)

ggplot(epnN, aes(x = temp_2, y = length)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  ggtitle("Best temp_2 fit NORTH") + 
  ggsave("output/temp_2Averaging_BestFitNorth.jpg")

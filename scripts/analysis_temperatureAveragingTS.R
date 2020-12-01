# Mon Nov 30 12:50:19 2020 ------------------------------

#TS Temperature Averaging
#set up 
tsn <- summarise(group_by_at(ts, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
tsn$temp_2 <- rep(NA, nrow(tsn))
tsn$temp_100 <- rep(NA, nrow(tsn))
ggplot(tsn, aes(x = latitude, y = length)) + 
  geom_point()

#alter get temp functions
get.temp.2t <- function(x, y, z) {
  end.date <- filter(tsn, station == x, year == y)$date[1]
  start.date <- end.date-z
  temp_2_mean <- mean(filter(waterTemp, station == x, year == y, date >= start.date, date <= end.date)$temp_2)
  temp_2_mean
}
get.temp.100t <- function(x, y, z) {
  end.date <- filter(tsn, station == x, year == y)$date[1]
  start.date <- end.date-z
  temp_100_mean <- mean(filter(waterTemp, station == x, year == y, date >= start.date, date <= end.date)$temp_100)
  temp_100_mean
}

#TS surface relationship
datalist = list()
for(i in seq(1:30)){
  n <- i
  for(i in seq(1:nrow(tsn))){
    tsn$temp_2[i] <- get.temp.2t(tsn$station[i], tsn$year[i], n)
    m <- lm(length~temp_2, data = tsn)
    r <- summary(m)$r.squared
    datalist[[n]] <- c(n, r)
  }
}
tsdata <- as.data.frame(do.call(rbind, datalist))
names(tsdata) <- c("n", "r")
ggplot(tsdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor surface temperature") +
  ggsave("output/temp_2Averaging.jpg")
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
ggplot(epdata, aes(x = n, y = r)) + 
  geom_point() + 
  ggtitle("Days averaged vs R squared\nfor 100m temperature") +
  ggsave("output/temp_100Averaging.jpg")
# Mon Dec  7 14:18:53 2020 ------------------------------
#Script to determine the number of krill necessary for length frequency analysis

#set up 
load("data/allLengthsEnv.rda")
aLE_tally <- add_tally(group_by_at(allLengthsEnv, vars(year, species, station)), name = "LFcounts")
aLE_tally <- summarize(group_by_at(aLE_tally, vars(year, species, station)), LFcounts = mean(LFcounts))
#top counts
epTop <- filter(allLengthsEnv, station == 442, year == 2016, species == "EP")#n=402
tsTop <- filter(allLengthsEnv, station == 131, year == 2013, species == "TS")#n=225
ndTop <- filter(allLengthsEnv, station == 134, year == 2017, species == "ND")#n=152

#EP Sampling
datalist = list()
for(n in seq(1:402)){
  epx <- ep[sample(nrow(epTop), n),]
  Mean <- mean(epx$length)
  Stddev <- sd(epx$length)
  datalist[[n]] <- c(n, Mean, Stddev)
}
epdata <- as.data.frame(do.call(rbind, datalist))
names(epdata) <- c("n", "mean", "sd")
epx.mean.lm <- lm(mean~n, data = epdata)
epx.mean.res <- resid(epx.mean.lm)
plot(epdata$n, epx.mean.res)
ggplot(epdata, aes(x = n, y = mean)) + 
  geom_point() + 
  ggtitle("Mean EP length by n") +
  ggsave("output/epSampling.jpg")
ggplot(epdata, aes(x = n, y = sd)) + 
  geom_point() + 
  ggtitle("SD of EP length by n") +
  ggsave("output/epSamplingSD.jpg")
rm(datalist)
#At least 45 required

#TS Sampling
datalist = list()
for(n in seq(1:225)){
  tsx <- ts[sample(nrow(tsTop), n),]
  Mean <- mean(tsx$length)
  Stddev <- sd(tsx$length)
  datalist[[n]] <- c(n, Mean, Stddev)
}
tsdata <- as.data.frame(do.call(rbind, datalist))
names(tsdata) <- c("n", "mean", "sd")
tsx.mean.lm <- lm(mean~n, data = tsdata)
tsx.mean.res <- resid(tsx.mean.lm)
plot(tsdata$n, tsx.mean.res)
ggplot(tsdata, aes(x = n, y = mean)) + 
  geom_point() + 
  ggtitle("Mean TS length by n") +
  ggsave("output/tsSampling.jpg")
ggplot(tsdata, aes(x = n, y = sd)) + 
  geom_point() + 
  ggtitle("SD of TS length by n") +
  ggsave("output/tsSamplingSD.jpg")
rm(datalist)
#At least 45 required

#ND Sampling
datalist = list()
for(n in seq(1:152)){
  ndx <- nd[sample(nrow(ndTop), n),]
  Mean <- mean(ndx$length)
  Stddev <- sd(ndx$length)
  datalist[[n]] <- c(n, Mean, Stddev)
}
nddata <- as.data.frame(do.call(rbind, datalist))
names(nddata) <- c("n", "mean", "sd")
ndx.mean.lm <- lm(mean~n, data = nddata)
ndx.mean.res <- resid(ndx.mean.lm)
plot(nddata$n, ndx.mean.res)
ggplot(nddata, aes(x = n, y = mean)) + 
  geom_point() + 
  ggtitle("Mean ND length by n") +
  ggsave("output/ndSampling.jpg")
ggplot(nddata, aes(x = n, y = sd)) + 
  geom_point() + 
  ggtitle("SD of ND length by n") +
  ggsave("output/ndSamplingSD.jpg")
rm(datalist)
#At least 45 reqired
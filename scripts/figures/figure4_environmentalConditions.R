#FIGURE 4 - ENVIRONMENTAL CONDITIONS

#Script to produce figure describing environmental conditions throughout the study period
# Wed Feb  3 13:09:04 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(zoo)
library(lubridate)
source("scripts/data_load.R")
source("scripts/functions/length_frequency.R")
load("data/allLengthsEnv.rda")
load("data/metadata.rda")
#====

#SETUP
#====
sentinels <- c(453, 454, 167, 171, 139, 152, 132, 134, 124, 127, 114, 110, 442, 445, 493, 495, 422, 425, 411, 414, 481, 402)#stations used to get domain-wide averages
coreStations <- unique(filter(allLengthsEnv, region == "north_central")$station)#create vector of all core station IDs
stations <- summarize(group_by_at(allLengthsEnv, vars(station, latitude.round)))

#Create CUTI data frames
cuti <- filter(cuti, year>=2011, year <2019)
cuti$date <- ymd(paste(cuti$year, cuti$month, cuti$day, sep = "-"))
cuti <- melt(cuti, id.vars = c("year", "month", "day", "date"))#long form data
cuti <- rename(cuti, cuti = value)
cuti$latitude.round <- as.numeric(str_sub(as.character(cuti$variable), 1, 2))
cuti <- left_join(cuti, stations)
cuti.core <- filter(cuti, station %in% coreStations)#create core df
cuti <- filter(cuti, station %in% sentinels)#full df
cuti.mean <- mean(cuti$cuti)#get mean value for the whole CCE
cuti <- dcast(cuti, date ~ station, value.var = "cuti")#reshape to wide-format data table
cuti.core <- dcast(cuti.core, date ~ station, value.var = "cuti")
cuti$cuti_mean <- rowMeans(cuti[2:23])#take mean value across all stations
cuti.core$cuti_mean <- rowMeans(cuti.core[2:16])

#Create water temperature data frames
urls <- read_csv("data/urls.csv")
mydir = "data/roms_temperatures/"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
dat_csv = plyr::ldply(myfiles, read_csv)
waterTemp <- left_join(dat_csv, urls[,1:4], by = c("lat", "lon"))
waterTemp$date <- as.Date(paste(waterTemp$year, waterTemp$month, waterTemp$day, sep = "/"))#add date column
sst <- filter(waterTemp, station %in% sentinels)#filter to sentinel stations
sst.mean <- mean(sst$temp_2)#get mean SST for the whole CCE
sub.mean <- mean(sst$temp_100, na.rm = TRUE)#get mean subsurface temp for the whole CCE
sst.sd <- sst[,c(2:8, 11, 13)]#create sst.sd df
sst.sd$year.station <- paste(sst.sd$year, sst.sd$station, sep = ".")#make year.station ID
sst.sd.list <- split(sst.sd, f = sst.sd$year.station)#split df into a list by year.station
for(i in seq(1:length(sst.sd.list))){
  sst.sd.list[[i]]$sst.sd <- c(rep(NA, 16), rollapply(data = sst.sd.list[[i]]$temp_2, 17, sd))
}#get sst sd for year station in each year
sst.sd <- bind_rows(sst.sd.list)#bind back into a single dataframe
sst.sd.mean <- mean(sst.sd$sst.sd, na.rm = TRUE)# get global sst.sd mean
sst.sd <- dcast(sst.sd, date ~ station, value.var = "sst.sd")#reshape to wide-format data table
sst.sd$sst.sd_mean <- rowMeans(sst.sd[2:23], na.rm = TRUE)#get daily means for sst sd
sst <- dcast(sst, date ~ station, value.var = "temp_2")#reshape to wide-format data table
sst$sst_mean <- rowMeans(sst[2:23], na.rm = TRUE)#get daily means for sst
sub <- filter(waterTemp, station %in% sentinels)#filter to sentinel stations
sub <- dcast(sub, date ~ station, value.var = "temp_100")#reshape to wide-format data table
sub$sub_mean <- rowMeans(sub[2:23], na.rm = TRUE)#get daily means for subsurface temp
waterTemp.core <- filter(waterTemp, station %in% coreStations)#core data
sst.core <- dcast(waterTemp.core, date ~ station, value.var = "temp_2")#reshape to wide-format data table
sst.core$sst_mean <- rowMeans(sst.core[2:16], na.rm = TRUE)
sub.core <- dcast(waterTemp.core, date ~ station, value.var = "temp_100")#reshape to wide-format data table
sub.core$sub_mean <- rowMeans(sub.core[2:16], na.rm = TRUE)

#Create MOCI data frames
moci <- rename(moci, year = Year)
moci <- filter(moci, year>=2011, year <=2018)
moci$moci_mean <- rowMeans(moci[4:6])
moci.core <- moci[,c(1:3,5)]#just central time series
colnames(moci.core)[4] <- "moci_mean"
moci.mean <- mean(moci$moci_mean)#get mean MOCI for whole CCE

#Create Chlorophyll data frame
mydir = "data/chla/"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
data2=lapply(myfiles, read_csv)
for (i in 1:length(data2)){data2[[i]]<-cbind(data2[[i]],myfiles[i])}
dat_csv <- do.call("rbind", data2) #merge chla data from individual station datasets
dat_csv$date <- as_datetime(dat_csv$UTC_time, format = "%d-%b-%Y %H:%M:%S")#convert time to date
dat_csv$station <- as.numeric(gsub("\\D", "", dat_csv$`myfiles[i]`))#extract station ID
names(dat_csv) <- c("date_str", "latitude", "longitude", "chla", "file", "date", "station")#rename variables
allChla <- filter(dat_csv, station %in% sentinels)
allChla$chla <- log(allChla$chla)#log transform
chla.mean <- mean(allChla$chla, na.rm = TRUE)#get chla values for whole CCE
allChla <- dcast(allChla, date ~ station, value.var = "chla")#reshape to wide-format data table
allChla$chla_mean <- rowMeans(allChla[2:23], na.rm = TRUE)

#Cruise highlights
cruises <- data.frame(start.x = c("2011-05-01", "2012-05-01", "2013-05-01", "2015-05-01", "2016-05-01", "2017-05-01", "2018-05-01"),
                      end.x = c("2011-07-01", "2012-07-01", "2013-07-01", "2015-07-01", "2016-07-01", "2017-07-01", "2018-07-01"),
                      min.y = rep("-Inf", 7),
                      max.y = rep("Inf", 7))
cruises$start.x <- as.Date(cruises$start.x)
cruises$end.x <- as.Date(cruises$end.x)
cruises$min.y <- as.numeric(as.character(cruises$min.y))
cruises$max.y <- as.numeric(as.character(cruises$max.y))
#====

#PLOTTING
#====
#Full Study Domain
#CUTI anomaly time series
cf <- ggplot(cuti, aes(x = date, y = cuti_mean-cuti.mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(cuti_mean-cuti.mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  ylim(-1, 2.5) +
  labs(y = "CUTI\nanomaly") +
  theme_classic(base_size = 20) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))

#CUTI station conditions boxplot
df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), cuti = (mean(cuti)*attr(allLengthsEnv$cuti, "scaled:scale")+attr(allLengthsEnv$cuti, "scaled:center")))
cuti.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = cuti)) +
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

#Chlorophyll time series
ave <- allChla
cruises$start.x <- as.POSIXct(cruises$start.x)
cruises$end.x <- as.POSIXct(cruises$end.x)
ylab <- expression(paste("Chl-a\nanomaly log mg ", m^-3))
ave$ave2 <- c(rep(NA, 29), rollapply(allChla$chla_mean-chla.mean, 30, mean, na.rm = TRUE))
chf <- ggplot(allChla, aes(x = date, y = chla_mean-chla.mean)) + 
  geom_line(color = "grey") + 
  geom_line(data = ave, aes(y = ave2), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  ylim(-2, 1.5) +
  labs(y = ylab) +
  theme_classic(base_size = 20) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 15, hjust = 0.5))
cruises$start.x <- as.Date(cruises$start.x)
cruises$end.x <- as.Date(cruises$end.x)

#Chlorophyll station conditions boxplot
df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), chla = (mean(chla)*attr(allLengthsEnv$chla, "scaled:scale")+attr(allLengthsEnv$chla, "scaled:center")))
chla.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = chla))+
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

#SST time series
ylab <- "SST\nanomaly (°C)"
sstf <- ggplot(sst, aes(x = date, y = sst_mean-sst.mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sst_mean-sst.mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  labs(y = ylab) +
  theme_classic(base_size = 20) +
  theme(axis.title.x = element_blank())

#SST station conditions boxplot
df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), temp_2 = (mean(temp_2)*attr(allLengthsEnv$temp_2, "scaled:scale")+attr(allLengthsEnv$temp_2, "scaled:center")))
temp_2.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = temp_2)) +
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

#SST SD time series
ylab <- "SST SD\nanomaly"
sst.sdf <- ggplot(sst.sd, aes(x = date, y = sst.sd_mean-sst.sd.mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sst.sd_mean-sst.sd.mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  labs(y = ylab) +
  theme_classic(base_size = 20) +
  theme(axis.title.x = element_blank())

#SST SD station conditions boxplot
df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), sst_sd = (mean(sst_sd)*attr(allLengthsEnv$sst_sd, "scaled:scale")+attr(allLengthsEnv$sst_sd, "scaled:center")))
sst_sd.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = sst_sd)) +
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

#Subsurface time series
ylab <- "Subsurface\ntemperature anomaly (°C)"
subf <- ggplot(sub, aes(x = date, y = sub_mean-sub.mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sub_mean-sub.mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  labs(y = ylab) +
  theme_classic(base_size = 20) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15))

#Subsurface station conditions boxplot
df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), temp_100 = (mean(temp_100)*attr(allLengthsEnv$temp_100, "scaled:scale")+attr(allLengthsEnv$temp_100, "scaled:center")))
temp_100.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = temp_100)) +
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

#MOCI time series
mocif <- ggplot(moci, aes(x = time, y = moci_mean)) +
  geom_line() + 
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  ylim(-10, 12) + 
  labs(x = "Date", y = "MOCI") + 
  theme_classic(base_size = 20)

#MOCI station conditions boxplot
df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), moci_spring = (mean(moci_spring)*attr(allLengthsEnv$moci_spring, "scaled:scale")+attr(allLengthsEnv$moci_spring, "scaled:center")))
moci.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = moci_spring))+
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

#====

#MERGE PLOTS
#====
jpeg("figures/manuscript/figure4_environmentalConditions.jpeg", units = "in", width = 12, height = 16, res = 400)
ggarrange(cf, NULL, cuti.sum, sstf, NULL, temp_2.sum, subf, NULL, temp_100.sum, sst.sdf, NULL, sst_sd.sum, chf, NULL, chla.sum, mocif, NULL, moci.sum, ncol = 3, nrow = 6, align = "hv", labels = c("A", "", "B", "C", "", "D", "E", "", "F", "G", "", "H", "I", "", "J", "K", "", "L"), font.label = list(size = 20), widths = c(1, -.1, 1))#arrange plots into multipanel grid, vertically and horizontally aligned
dev.off()
#====
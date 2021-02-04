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
source("scripts/data_load.R")
load("data/allLengthsEnv.rda")
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
sst <- dcast(sst, date ~ station, value.var = "temp_2")#reshape to wide-format data table
sst$sst_mean <- rowMeans(sst[2:23], na.rm = TRUE)
sub <- filter(waterTemp, station %in% sentinels)#filter to sentinel stations
sub <- dcast(sub, date ~ station, value.var = "temp_100")#reshape to wide-format data table
sub$sub_mean <- rowMeans(sub[2:23], na.rm = TRUE)
waterTemp.core <- filter(waterTemp, station %in% coreStations)#core data
sst.core <- dcast(waterTemp.core, date ~ station, value.var = "temp_2")#reshape to wide-format data table
sst.core$sst_mean <- rowMeans(sst.core[2:16], na.rm = TRUE)
sub.core <- dcast(waterTemp.core, date ~ station, value.var = "temp_100")#reshape to wide-format data table
sub.core$sub_mean <- rowMeans(sub.core[2:16], na.rm = TRUE)

#Create MOCI data frames
moci <- rename(moci, year = Year)
moci.core <- filter(moci, station %in% coreStations) %>% 
  mutate(moci_mean = rowMeans(.[x:x]))

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
#CUTI time series
cf <- ggplot(cuti, aes(x = date, y = cuti_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(cuti_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "red", alpha = 0.3) +
  ylim(-1, 2.5) +
  labs(y = "CUTI") +
  theme_classic(base_size = 15) +
  theme(axis.title.x = element_blank())
#SST time series
ylab <- "SST (°C)"
sstf <- ggplot(sst, aes(x = date, y = sst_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sst_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "red", alpha = 0.3) +
  #ylim(-1, 2.5) +
  labs(y = ylab) +
  theme_classic(base_size = 15) +
  theme(axis.title.x = element_blank())
#Subsurface time series
ylab <- "Subsurface\ntemperature (°C)"
subf <- ggplot(sub, aes(x = date, y = sub_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sub_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "red", alpha = 0.3) +
  #ylim(-1, 2.5) +
  labs(y = ylab) +
  theme_classic(base_size = 15) +
  theme(axis.title.x = element_blank())

#Core Region
cc <- ggplot(cuti.core, aes(x = date, y = cuti_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(cuti_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "red", alpha = 0.3) +
  ylim(-1, 2.5) +
  labs(y = "CUTI") +
  theme_classic(base_size = 15) +
  theme(axis.title.x = element_blank())
#SST time series
ylab <- "SST (°C)"
sstc <- ggplot(sst.core, aes(x = date, y = sst_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sst_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "red", alpha = 0.3) +
  #ylim(-1, 2.5) +
  labs(y = ylab) +
  theme_classic(base_size = 15) +
  theme(axis.title.x = element_blank())
#Subsurface time series
ylab <- "Subsurface\ntemperature (°C)"
subc <- ggplot(sub.core, aes(x = date, y = sub_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sub_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "red", alpha = 0.3) +
  #ylim(-1, 2.5) +
  labs(y = ylab) +
  theme_classic(base_size = 15) +
  theme(axis.title.x = element_blank())
#MOCI time series
mc <- ggplot(moci, aes(x = date, y = moci_mean)) +
  geom_line() + 
  labs(y = ylab) +
  theme_classic() +
  theme(axis.title.x = element_blank())
#====

#MERGE PLOTS
#====
ggarrange(cf, cc, sstf, sstc, subf, subc, mf, mc, ncol = 2, nrow = 4, align = "hv", labels = c("A", "E", "B", "F", "C", "G", "D", "H"))#arrange plots into multipanel grid, vertically and horizontally aligned
#====
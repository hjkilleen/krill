# Fri Nov  6 14:23:48 2020 ------------------------------
library(dplyr)
library(stringr)
library(readr)
load("data/allLengths.rda")
source("scripts/functions/length_frequency.R")

#Get data from CenCOOS servers using the virtual sensor tool. 
#Data are from ROMS/TOMS nowcast (10km), May1-June15 average at 2m and 100m depths. Station locations are taken from the RF_Euphausidae station_lat and _lon. 
#Note that I shifted station 132 and 481 location ~2 km onshore. The original location (-122.65, -117.7500 respectively) was on a grid boundary and returned an error. I also moved station 183 ~1 km offshore as the nearshore location (-123.2333) was outside the model boundary.
#I do not have station information for station 166, 421, and 118. These need to be added to urls and ROMS data from the most recent RF dataset (or ask Keith). 
stations <- read.csv("data/stationMetadata.csv")
station <- summarize(group_by(stations, station), lat = mean(station_latitude), lon = mean(station_longitude), depth = mean(bottom_depth))
urls <- read.csv("data/urls.csv")

#UNCOMMENT BELOW TO ADD NEW ROMS DATA FOR NEW STATIONS.

# for(i in seq(1:nrow(urls))) {
#   #download file and read as csv
#   temporary <- tempfile()
#   download.file(as.character(urls$url[27]),temporary)
#   tempData <- read.csv(unz(temporary, "temp.csv"))
#   unlink(temporary)
#   #rename data
#   tempNames <- names(tempData[,6:ncol(tempData)])
#   tempNames <- str_extract(tempNames[], "(\\d)+")
#   metaNames <- c("x", "y", "lat", "lon", "datetime")
#   allNames <- c(metaNames, tempNames)
#   names(tempData) <- allNames
#   #dates
#   tempData$date <-as.Date(str_extract(as.character(tempData$datetime), "[^T]+"))
#   tempData <- mutate(tempData, year = lubridate::year(date),
#                      month = lubridate::month(date),
#                      day = lubridate::day(date))
#   tempData$monthDay <- paste(tempData$month, tempData$day, sep = ".")
#   #filtering
#   if("100" %in% names(tempData)){
#     temps <- select(tempData, lat, lon, year, month, day, monthDay, "2", "100")
#     names(temps) <- c("lat", "lon", "year", "month", "day", "monthDay", "temp_2", "temp_100")
#   } else {
#     temps <- select(tempData, lat, lon, year, month, day, monthDay, "2")
#     names(temps) <- c("lat", "lon", "year", "month", "day", "monthDay", "temp_2")
#   }
#   temps$lat_lon <- paste(temps$lat, temps$lon, sep = "_")
#   temps <- filter(temps, year >= 2011, year <=2018)
#   temps <- filter(temps, year != 2014)
#   temps <- filter(temps, month >=4, month<8)
#   #convert temp to Celsius
#   temps$temp_2 <- (temps$temp_2-32)/1.8
#   if("temp_100" %in% names(temps)){
#     temps$temp_100 <- (temps$temp_100-32)/1.8
#   }
#   #write .csv
#   write.csv(temps, file = paste("data/roms_temperatures/", urls$station[27], "_temp.csv", sep = ""))
# }

#Load csv files
mydir = "data/roms_temperatures/"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
dat_csv = plyr::ldply(myfiles, read_csv)

#Create water temperature data frame
waterTemp <- left_join(dat_csv, urls[,1:4], by = c("lat", "lon"))
waterTemp$date <- as.Date(paste(waterTemp$year, waterTemp$month, waterTemp$day, sep = "/"))

# filter and average across dates prior to sample
a <- as.data.frame(summarize(group_by_at(allLengths, vars(station, year)), temp_2 = NA, temp_100 = NA))
for(i in seq(1:nrow(a))) {
  a$temp_2[i] <- get.temp.2(a$station[i], a$year[i], 2)
  a$temp_100[i] <- get.temp.100(a$station[i], a$year[i], 2)
}

#add sst_sd variable based on prior
a$sst_sd <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))) {
  a$sst_sd[i] <- get.sd(a$station[i], a$year[i], 13)
}

#merge new columns with allLengths in a novel df
allLengthsEnv <- left_join(select(allLengths, station, species, sex, length, year, sites, region, latitude, shore, date), a)

#Save length + environment dataset
save(allLengthsEnv, file = "data/allLengthsEnv.rda")

ep <- filter(allLengthsEnv, species == "EP")
ts <- filter(allLengthsEnv, species == "TS")
nd <- filter(allLengthsEnv, species == "ND")

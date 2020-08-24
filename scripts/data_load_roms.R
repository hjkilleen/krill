# Tue Jun  9 16:50:59 2020 ------------------------------
library(dplyr)
library(stringr)
library(readr)
load("data/allLengths.rda")

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
#   download.file(as.character(urls$url[i]),temporary)
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
#   dates <- c("5.1", "5.2", "5.3", "5.4", "5.5", "5.6", "5.7", "5.8", "5.9", "5.10", "5.11", "5.12", "5.13", "5.14", "5.15", "5.16", "5.17", "5.18", "5.19", "5.20", "5.21", "5.22", "5.23", "5.24", "5.25", "5.26", "5.27", "5.28", "5.29", "5.30", "5.31", "6.1", "6.2", "6.3", "6.4", "6.5", "6.6", "6.7", "6.8", "6.9", "6.10", "6.11", "6.12", "6.13", "6.14", "6.15")
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
#   temps <- filter(temps, monthDay %in% dates)
#   #convert temp to Celsius
#   temps$temp_2 <- (temps$temp_2-32)/1.8
#   if("temp_100" %in% names(temps)){
#     temps$temp_100 <- (temps$temp_100-32)/1.8
#   }
#   #grouping
#   if("temp_100" %in% names(temps)){
#     temp <- summarize(group_by(temps, year), lat = mean(lat), lon = mean(lon), temp_2 = mean(temp_2), temp_100 = mean(temp_100))
#   } else {
#     temp <- summarize(group_by(temps, year), lat = mean(lat), lon = mean(lon), temp_2 = mean(temp_2))
#   }
#   #write .csv
#   write.csv(temp, file = paste("data/roms_temperatures/", urls$station[i], "_temp.csv", sep = ""))
# }

#Load csv files
mydir = "data/roms_temperatures/"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
dat_csv = plyr::ldply(myfiles, read_csv)

#merge with krill length data
waterTemp <- left_join(dat_csv, urls[,1:4], by = c("lat", "lon"))
waterTemp <- select(waterTemp, year, station, temp_2, temp_100)
allLengths$year <- as.numeric(allLengths$year)
allLengthsEnv <- left_join(allLengths, waterTemp)
save(allLengthsEnv, file = "data/allLengthsEnv.rda")

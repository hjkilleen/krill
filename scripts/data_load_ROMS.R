#Load ROMS data
#Script to scrape 10 km resolution ROMS SST and subsurface temperature using CenCOOS virtual sensor tool at https://data.cencoos.org/#module-metadata/0ac0326a-a2c6-11e2-9dc2-00219bfe5678
# Fri Jan  8 14:47:56 2021 ------------------------------

#LIBRARIES
#====
library(stringr)
library(lubridate)
library(tidyverse)
#====

#LOAD DATA ACCESS POINTS
#====
#Get data from CenCOOS servers using the virtual sensor tool. 
#Data are from ROMS/TOMS nowcast (10km), May1-June15 average at 2m and 100m depths. Station locations are taken from the RF_Euphausidae station_lat and _lon.
#Note that I shifted station 132 and 481 location ~2 km onshore. The original location (-122.65, -117.7500 respectively) was on a grid boundary and returned an error. I also moved station 183 ~1 km offshore as the nearshore location (-123.2333) was outside the model boundary.
#I do not have station information for station 166, 421, and 118. These need to be added to urls and ROMS data from the most recent RF dataset (or ask Keith). 
urls <- read.csv("data/urls.csv")#URLs to ROMS virtual sensor tool to access temperature data
#====

#SCRAPE DATA
#====
for(i in seq(1:nrow(urls))) {#for loop to access URLs and tidy resulting dataframe
  temporary <- tempfile()#download file and read data
  download.file(as.character(urls$url[i]),temporary)
  tempData <- read.csv(unz(temporary, "temp.csv"))
  unlink(temporary)
  tempNames <- names(tempData[,6:ncol(tempData)])#tidy dataframe
  tempNames <- str_extract(tempNames[], "(\\d)+")
  metaNames <- c("x", "y", "lat", "lon", "datetime")
  allNames <- c(metaNames, tempNames)
  names(tempData) <- allNames
  tempData$date <-as.Date(str_extract(as.character(tempData$datetime), "[^T]+"))
  tempData <- mutate(tempData, year = lubridate::year(date),
                     month = lubridate::month(date),
                     day = lubridate::day(date))
  tempData$monthDay <- paste(tempData$month, tempData$day, sep = ".")
  if("100" %in% names(tempData)){#if dataframe has subsurface data then pull it
    temps <- select(tempData, lat, lon, year, month, day, monthDay, "2", "100")
    names(temps) <- c("lat", "lon", "year", "month", "day", "monthDay", "temp_2", "temp_100")
  } else {#if dataframe does not have subsurface data then only pull SST
    temps <- select(tempData, lat, lon, year, month, day, monthDay, "2")
    names(temps) <- c("lat", "lon", "year", "month", "day", "monthDay", "temp_2")
  }
  temps$lat_lon <- paste(temps$lat, temps$lon, sep = "_")
  temps <- filter(temps, year >= 2011, year <=2018)#only 2011-2018
  temps <- filter(temps, year != 2014)#leave out 2014
  temps <- filter(temps, month >=4, month<8)#only months April-August
  temps$temp_2 <- (temps$temp_2-32)/1.8#convert temp to Celsius
  if("temp_100" %in% names(temps)){#convert subsurface temp to Celsius
    temps$temp_100 <- (temps$temp_100-32)/1.8
  }
write.csv(temps, file = paste("data/roms_temperatures/", urls$station[i], "_temp.csv", sep = ""))#save data
}
#====
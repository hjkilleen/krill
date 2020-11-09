library(readxl)
library(stringr)
library(dplyr)
library(lubridate)

#2011-2018 sample dates and locations using RREAS online dataset
#Accessed here: https://coastwatch.pfeg.noaa.gov/erddap/tabledap/FED_Rockfish_Catch.html

#METADATA
d <- read_xlsx("data/RREASmetadata.xlsx")

d$year <- as.integer(substring(d$time, 1, 4))
d$timeUTC <- as_datetime(d$time, tz = "UTC")
d$time <- as_datetime(d$timeUTC, "America/Los_Angeles")
d$date <- as_date(d$time)
d$station <- as.integer(d$station)
d$haul <- d$haul_no

#group metadata by dates and stations, locational data will be averaged over hauls
ds <- summarize(group_by_at(d, vars(station, date)), latitude = mean(station_latitude), longitude = mean(station_longitude), bottom_depth = mean(station_bottom_depth), tdr_depth = mean(tdr_depth))

#DATA
load("data/lengths.rda")
baldo <- read.csv("data/lengthsBaldo.csv")

#2013-2018
#get dates from first 6 integers
gp <- summarize(group_by_at(lengths, vars(station, date)), station = mean(station), date = mean(date), year = mean(year))

#2011-2012
#extract haul and year from photo ID string
baldo$haul <- sub(".*H", "", baldo$Photo.ID)
baldo$haul <- as.numeric(gsub("([0-9]+).*$", "\\1", baldo$haul))
baldo$year <- as.integer(paste("20", str_extract(baldo$Photo.ID, "\\d{2}"), sep = ""))
baldo$station <- baldo$Station..
gpb <- summarize(group_by_at(baldo, vars(station, year, haul)), station = mean(station), year = mean(year), haul = mean(haul))

#Join data subsets with metadata
#2013-2018
late <- left_join(gp, ds)
#Fix dates with typos or delay issues (sample recorded in RREAS database after it has been collected from)
late[8, 4:7] <- late[10,4:7]
late[9, 4:7] <- late[10,4:7]
late[28, 4:7] <- late[29,4:7]
late[77, 4:7] <- late[76,4:7]
late[46, 4:7] <- late[45,4:7]
late[82, 4:7] <- late[83,4:7]
#this last sample appears to have been mislabeled as 170. According to the RREAS metadata sheet it should be 171
late[91, 4:7] <- late[12,4:7]
late[91,1] <- 171

#2011-2012
early <- left_join(gpb, d)
early <- select(early, year, station, date, station_latitude, station_longitude, station_bottom_depth, tdr_depth)
names(early) <- c("year", "station", "date", "latitude", "longitude", "bottom_depth", "tdr_depth")

allStationMetadata <- rbind(early, late)

#write .csv
write.csv(allStationMetadata, "data/allStationMetadata.csv")

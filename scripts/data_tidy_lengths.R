#Tidy
#This script cleans up all of the data required for analysis of krill lengths.

# Mon Dec 14 17:33:11 2020 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/data_load.R")
library(tidyverse)
library(lubridate)
#====

#TIDY 2015-2018 LENGTH DATASET
#====
#Clean up variable names
lengths <- lengths[,-11]
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengths) <- vars
rm(vars)

#Add derived columns
lengths <- mutate(lengths, length = pixels/scale)#add lengths variable multiplying pixels by scale measure
lengths <- mutate(lengths, year = as.integer(paste("20", str_extract(lengths$ID, "\\d{2}"), sep = "")))#add year
lengths <- as.data.frame(add_tally(group_by_at(lengths, vars(year, station, species, sex))))#add variable for number of observations for each station/year/species/sex
lengths$date <- as.integer(str_extract(lengths$ID, "\\d{6}"))#add date column
lengths$date <- as.Date(as.character(lengths$date), format = '%y%m%d')

#Add site-level metadata
lengths <- filter(lengths, station %in% metadata$station)#only LF analysis stations
lengths <- filter(lengths, species == "EP" | species == "TS" | species == "ND")#filter to only EP, TS, and ND
lengths <- left_join(lengths, metadata[,-1], by = c("station", "year", "date"))#add metadata for stations

#Filter out individuals that do not meet length or numbers necessary
lengths <- filter(lengths, length <50, length >10)#no unrealistically large individuals or individuals that are too small to be accurately sampled
lengths <- filter(lengths, n>=40)#n per station must be greater than 40
#====

#TIDY 2011-2013 LENGTH DATASET
#====
#Clean up variable names
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengthsBaldo) <- vars
rm(vars)

#Add derived variables
lengthsBaldo <- mutate(lengthsBaldo, length = pixels/scale)#add length variable by multiplying scale and pixels
lengthsBaldo <- mutate(lengthsBaldo, year = as.integer(paste("20", str_extract(lengthsBaldo$ID, "\\d{2}"), sep = "")))#add year
lengthsBaldo <- as.data.frame(add_tally(group_by_at(lengthsBaldo, vars(year, station, species, sex))))#add variable for number of observations for each station/year/species/sex
#add haul and date from from NMFS data
d$year <- as.integer(substring(d$time, 1, 4))
d$timeUTC <- as_datetime(d$time, tz = "UTC")
d$time <- as_datetime(d$timeUTC, "America/Los_Angeles")
d$date <- as_date(d$time)
d$station <- as.integer(d$station)
d$haul <- d$haul_no
d <- select(d, haul, station, year, date)
lengthsBaldo <- left_join(lengthsBaldo, d)

#add haul and date for legacy stations (see data/na.fixes) from Keith
d <- select(legacySites, haul, station, year, date)
legacyKrill <- filter(lengthsBaldo, station %in% c(118, 421, 166))
legacyKrill <- legacyKrill[,-c(14, 15)]
legacyKrill <- left_join(legacyKrill, d, by = c("station", "year"))
legacyKrill$date <- mdy(legacyKrill$date)
notLegacy <- filter(lengthsBaldo, !station %in% c(118, 421, 166))
lengthsBaldo <- rbind(notLegacy, legacyKrill)

#Filter out individuals that do not meet length or numbers necessary
lengthsBaldo <- filter(lengthsBaldo, length <50, length >10)#no unrealistically large individuals or individuals that are too small to be accurately sampled
lengthsBaldo <- filter(lengthsBaldo, n>=40)#n per station must be greater than 40

#Add site-level metadata
lengthsBaldo <- left_join(lengthsBaldo, select(metadata, station, date, latitude, longitude, bottom_depth, tdr_depth), by = c("station", "date"))#add station metadata
lengthsBaldo <- lengthsBaldo[,-14]#drop haul variable
#====

#MERGE & SAVE DATASETS
#====
#merge 2011-2013 dataset with 2015-2018 dataset
lengthsBaldo$year <- as.factor(lengthsBaldo$year)
allLengths <- as.data.frame(rbind(lengths, lengthsBaldo))

#Save as .RDA file
save(allLengths, file = "data/allLengths.rda")
#====
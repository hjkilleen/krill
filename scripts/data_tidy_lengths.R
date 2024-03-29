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
lengths <- as.data.frame(add_tally(group_by_at(lengths, vars(year, station, species))))#add variable for number of observations for each station/year/species
lengths$date <- as.integer(str_extract(lengths$ID, "\\d{6}"))#add date column
lengths$date <- as.Date(as.character(lengths$date), format = '%y%m%d')

#Add site-level metadata
lengths <- filter(lengths, station %in% metadata$station)#only LF analysis stations
lengths <- filter(lengths, species == "EP" | species == "TS" | species == "ND")#filter to only EP, TS, and ND

#Filter out individuals that do not meet length or numbers necessary
lengths <- filter(lengths, length <50, length >10)#no unrealistically large individuals or individuals that are too small to be accurately sampled
lengths <- filter(lengths, nn>=40)
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
lengthsBaldo <- mutate(lengthsBaldo, haul = as.integer(str_extract(lengthsBaldo$ID, "(?<=H)[0-9]*")))#add haul
lengthsBaldo <- as.data.frame(add_tally(group_by_at(lengthsBaldo, vars(year, station, species, sex))))#add variable for number of observations for each station/year/species/sex
lengthsBaldo <- as.data.frame(add_tally(group_by_at(lengthsBaldo, vars(year, station, species))))#add variable for number of observations for each station/year/species
#add haul and date from from NMFS data
d$year <- as.integer(substring(d$time, 1, 4))
d$timeUTC <- as_datetime(d$time, tz = "UTC")
d$time <- as_datetime(d$timeUTC, "America/Los_Angeles")
d$date <- as_date(d$time)
d$station <- as.integer(d$station)
d$haul <- d$haul_no
d <- dplyr::select(d, haul, station, year, date)
lengthsBaldo <- left_join(lengthsBaldo, d)

#add haul and date for legacy stations (see data/na.fixes) from Keith
d <- dplyr::select(legacySites, haul, station, year, date)
legacyKrill <- filter(lengthsBaldo, station %in% c(118, 421, 166))
legacyKrill <- legacyKrill[,-16]#drop date
legacyKrill <- left_join(legacyKrill, d, by = c("station", "year", "haul"))
legacyKrill$date <- mdy(legacyKrill$date)
notLegacy <- filter(lengthsBaldo, !station %in% c(118, 421, 166))
lengthsBaldo <- rbind(notLegacy, legacyKrill)
lengthsBaldo <- lengthsBaldo[,-13]#drop haul variable

#Filter out individuals that do not meet length or numbers necessary
lengthsBaldo <- filter(lengthsBaldo, length <50, length >10)#no unrealistically large individuals or individuals that are too small to be accurately sampled
lengthsBaldo <- filter(lengthsBaldo, nn >= 40)
#====

#MERGE & SAVE DATASETS
#====
lengthsBaldo$year <- as.factor(lengthsBaldo$year)#merge 2011-2013 dataset with 2015-2018 dataset
allLengths <- as.data.frame(rbind(lengths, lengthsBaldo))#merge all lengths

allLengths <- allLengths[,-c(1, 5:10)]#get rid of unneccessary notes

metadata$date <- mdy(metadata$date)#add regional and site identifiers and metadata
allLengths <- left_join(allLengths, dplyr::select(metadata, station, date, latitude, longitude, bottom_depth, tdr_depth), by = c("station", "date"))#add station metadata
allLengths <- left_join(allLengths, dplyr::select(regions, station, sites, region, shore), by = 'station')#merge with regional and site identifiers

save(allLengths, file = "data/allLengths.rda")#save as .RDA file
#====
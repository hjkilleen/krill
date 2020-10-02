#Tidy
#This script cleans up all of the data required for analysis of krill lengths.

# Wed Aug  5 11:42:20 2020 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/data_load.R")
library(tidyverse)
library(readxl)
library(lubridate)
#====

#TIDY LENGTH DATASET
#====
lengths <- lengths[,-11]
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengths) <- vars
rm(vars)
#add lengths variable multiplying pixels by scale measure
lengths <- mutate(lengths, length = pixels/scale)
lengths <- mutate(lengths, year = as.integer(paste("20", str_extract(lengths$ID, "\\d{2}"), sep = "")))
#add variable for number of observations for each station/year/species/sex
add_tally(group_by_at(lengths, vars(year, station, species, sex)))
#filter to only the stations we are including in the analysis
lengths <- filter(lengths, station %in% regions$station)
lengths <- left_join(lengths, regions, by = "station")
lengths <- filter(lengths, species == "EP" | species == "TS" | species == "ND")
lengths <- filter(lengths, length <50, length >10)
lengths$date <- as.integer(str_extract(lengths$ID, "\\d{6}"))
lengths$date <- as.Date(as.character(lengths$date), format = '%y%m%d')
save(lengths, file = "data/lengths.rda")

#Merge 2011 & 2012 with 2015-2018 dataset
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengthsBaldo) <- vars
rm(vars)
#add lengths variable multiplying pixels by scale measure
lengthsBaldo <- mutate(lengthsBaldo, length = pixels/scale)
lengthsBaldo <- mutate(lengthsBaldo, year = as.integer(paste("20", str_extract(lengthsBaldo$ID, "\\d{2}"), sep = "")))
#get locational information
lengthsBaldo <- left_join(lengthsBaldo, regions, by = "station")
lengthsBaldo$haul <- sub(".*H", "", lengthsBaldo$ID)
lengthsBaldo$haul <- as.numeric(gsub("([0-9]+).*$", "\\1", lengthsBaldo$haul))
#add date to 2011-2012 data
d <- read_xlsx("data/RREASmetadata.xlsx")
d$year <- as.integer(substring(d$time, 1, 4))
d$timeUTC <- as_datetime(d$time, tz = "UTC")
d$time <- as_datetime(d$timeUTC, "America/Los_Angeles")
d$date <- as_date(d$time)
d$station <- as.integer(d$station)
d$haul <- d$haul_no
d <- select(d, haul, station, year, date)
lengthsBaldo <- left_join(lengthsBaldo, d)
lengthsBaldo <- lengthsBaldo[,-17]
save(lengthsBaldo, file = "data/lengthsBaldo.rda")
#merge with 2015-2018 dataset and omit na
lengthsBaldo$year <- as.factor(lengthsBaldo$year)
allLengths <- rbind(lengths, lengthsBaldo)
#create sample ID
allLengths$sample <- seq(1:nrow(allLengths))
#filter to relevant species and sizes
allLengths <- filter(allLengths, length <50, length >10)
allLengths <- filter(allLengths, species == "EP" | species == "TS" | species == "ND")
allLengths$species <- factor(allLengths$species, levels=c("EP", "TS", "ND")) 
save(allLengths, file = "data/allLengths.rda")

#filter by species
ep <- filter(allLengths, species == "EP")
ts <- filter(allLengths, species == "TS")
nd <- filter(allLengths, species == "ND")
#=====
#TIDY METADATA
#====
metadata <- na.exclude(metadata[,-1])
save(metadata, file = "data/metadata.rda")

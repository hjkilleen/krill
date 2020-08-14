#Tidy
#This script cleans up all of the data required for analysis of krill lengths.

# Wed Aug  5 11:42:20 2020 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/data_load.R")

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
#merge with 2015-2018 dataset and omit na
lengthsBaldo$year <- as.factor(lengthsBaldo$year)
allLengths <- rbind(lengths, lengthsBaldo)
allLengths <- na.omit(allLengths)
#create sample ID
allLengths$sample <- seq(1:nrow(allLengths))
#filter to relevant species and sizes
allLengths <- filter(allLengths, length <50, length >10)
allLengths <- filter(allLengths, species == "EP" | species == "TS" | species == "ND")
save(allLengths, file = "data/allLengths.rda")

#filter by species
ep <- filter(allLengths, species == "EP")
ts <- filter(allLengths, species == "TS")
nd <- filter(allLengths, species == "ND")

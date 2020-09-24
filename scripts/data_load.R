#Load
#This script loads all the data that is required for analysis of krill lengths.

# Wed Aug  5 11:40:39 2020 ------------------------------

#LIBRARIES & SOURCES
#====
library(readr)
#====
#LOAD METADATA
#====
#Load station metadata (lats are from mean latitude of corresponding "area" in NMFS 2015 MWT dataset, except for San Mateo, which I estimated on google maps, and Monterey, which is an average value of all the MWT Monterey sites)
regions <- read_csv("data/regions.csv")

#load yearly sampling record. lists stations used in each year, varies due to incomplete sampling effort, degraded specimens, or misplaced sample.
sites2015 <- read_csv("data/sites2015.csv")
sites2016 <- read_csv("data/sites2016.csv")
sites2017 <- read_csv("data/sites2017.csv")
sites2018 <- read_csv("data/sites2018.csv")

#====
#LOAD LENGTH DATA
#====
lengths <- read_csv("data/lengths.csv")
lengthsBaldo <- read_csv("data/lengthsBaldo.csv")
metadata <- read_csv("data/allStationMetadata.csv")

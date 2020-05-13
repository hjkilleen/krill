# Wed May 13 14:20:30 2020 ------------------------------

#This script creates a krill "climatology" for Euphausia pacifica for use in testing the target strength to biomass model.

#LIBRARIES AND SOURCES-----------
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(stringr)
library(readxl)
library(lubridate)
library(e1071) #for histogram stats
library(ggridges)
library(formattable)
#You can find these scripts in the GitHub krill repository. Once you download them you can change the filepath to match their location on your computer.
source("scripts/functions/length_frequency.R")
source("scripts/functions/regions.R")

#LOAD DATA-----------
#All data files can be found in the GitHub krill repository

#load length data from 2015-2018 samples
lengths <- read.csv("data/lengths.csv")
#get rid of unused column for notes
lengths <- lengths[,-11]
#rename data variables
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengths) <- vars
#add lengths variable multiplying pixels by scale measure
lengths <- mutate(lengths, length = pixels/scale)
lengths <- mutate(lengths, year = as.integer(paste("20", str_extract(lengths$ID, "\\d{2}"), sep = "")))
#filter to only the stations we are including in the analysis (i.e. those that are listed in the "Stations in Analysis" document on FI Google Drive)
lengths <- filter(lengths, station %in% allSites)
#add in locational information
lengths <- left_join(lengths, regions, by = "station")
#omit NAs
lengths <- na.omit(lengths)
#save as an R datafile object
save(lengths, file = "data/lengths.rda")

#I've commented out all the code below, to uncomment, highlight everything and hit Command+Shift+C (Mac) or Ctrl+Shift+C (PC). The code below pulls in Baldo's data from 2011 and 2012 and merges it with the 2015-2018 data into a new dataset called 'allLengths'. If you'd like to use the 2011 & 2012 data for the krill climatology then run the code below and change 'lengths' to 'allLengths' in the Krill Climatology section.

# #load 2011&2012 data and merge with 2015-2018 dataset
# lengthsBaldo <- read.csv("data/lengthsBaldo.csv")
# vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
# names(lengthsBaldo) <- vars
# #add lengths variable multiplying pixels by scale measure
# lengthsBaldo <- mutate(lengthsBaldo, length = pixels/scale)
# lengthsBaldo <- mutate(lengthsBaldo, year = as.integer(paste("20", str_extract(lengthsBaldo$ID, "\\d{2}"), sep = "")))
# #get locational information
# lengthsBaldo <- left_join(lengthsBaldo, regions, by = "station")
# 
# #merge Baldo's 2011/2012 data with 2015-2018 dataset and omit NAs
# lengthsBaldo$year <- as.factor(lengthsBaldo$year)
# allLengths <- rbind(lengths, lengthsBaldo)
# allLengths <- na.omit(allLengths)


#DATA FILTERING--------------
#get rid of very small (poor net sampling) and very large krill.
lengths <- filter(lengths, length <35, length >10)
#filter by species
ep <- filter(lengths, species == "EP")

#If you would like a TS or ND climatology, the uncomment and run the code below and replace 'ep' with 'ts' or 'nd' in the Krill Climatology section
# ts <- filter(lengths, species == "TS")
# nd <- filter(lengths, species == "ND")

#KRILL CLIMATOLOGY------------
#specify bins, square bracket means inclusive. Larger bins would need to be changed for TS, but would likely be fine for ND
bins <- c("[10-11)", "[11-12)", "[12-13)", "[13-14)", "[14-15)", "[15-16)", "[16-17)", "[17-18)", "[18-19)", "[19-20)", "[20-21)", "[21-22)", "[22-23)", "[23-24)", "[24-25)", "[25-26)", "[26-27)", "[27-28)", "[28-29)", "[29-30)", "[30-31)", "[31-32)", "[32-33)", "[33-34)", "[34-35)")
breaks <- seq(10, 35)
#add bin as a new factor in EP dataset
ep$bin <- cut(ep$length, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = bins)
#add a tally for number of krill in each bin for each region/shore combination
binned <- add_tally(group_by_at(ep, vars(region, shore, bin)))
#select only relevant variables 
binned <- select(binned, region, shore, bin, n)
#reshape dataframe to make bins into variables with values as counts
binned <- dcast(binned, region + shore~bin, fun.aggregate = mean, value.var = "n")
#add a column for total krill, change the column indexing below if adding more bins
binned <- binned %>%
  mutate(total = select(., 3:23) %>% rowSums(na.rm = TRUE))
#turn bin counts into proportions of total counts
binned[, c(-1, -2, -23)] <- lapply( binned[ , c(-1, -2, -23)], function(x) x/binned$total)

#save file as .csv
write.csv(binned, file = "data/epClimatology.csv")

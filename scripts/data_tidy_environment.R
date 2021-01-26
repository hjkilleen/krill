#Tidy ROMS data
# Fri Jan  8 15:24:30 2021 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/data_tidy_lengths.R")
library(dplyr)
library(stringr)
library(readr)
#====

#LOAD 
#====
#Load csv files with ROMS temperatures
urls <- read_csv("data/urls.csv")
mydir = "data/roms_temperatures/"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
dat_csv = plyr::ldply(myfiles, read_csv)
#====

#TIDY
#====
#Create water temperature data frame
waterTemp <- left_join(dat_csv, urls[,1:4], by = c("lat", "lon"))
waterTemp$date <- as.Date(paste(waterTemp$year, waterTemp$month, waterTemp$day, sep = "/"))#add date column

#BEUTI data
beuti <- filter(beuti, year>2010, year <2019)
beuti$date <- ymd(paste(beuti$year, beuti$month, beuti$day, sep = "-"))
beuti <- melt(beuti, id.vars = c("year", "month", "day", "date"))#long form data
beuti <- rename(beuti, beuti = value)
beuti$latitude.round <- as.numeric(str_sub(as.character(beuti$variable), 1, 2))
#====


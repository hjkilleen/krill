library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(maps)
library(readxl)
library(lubridate)
source("scripts/functions/regions.R")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

ggplot(data = coast) +
  geom_sf()

#Station maps for each year
#=======
#read in RF dataset for Euphausia
rf <- read_xlsx("../../PhD/Research/Krill/RF_Survey_Data_EUPHAUSIDAE.xlsx")
rf$time <- as_datetime(rf$time)
#all stations from all years
rfYear <- filter(rf, station %in% allSites)
rfYear <- select(rfYear, station, station_longitude, station_latitude, area, bottom_depth)
write_csv(rfYear, "data/stationMetadata.csv")
#Pull out position information for relevant stations
#for 2015
rfYear <- filter(rf, station %in% allSites2015)
jpeg("figures/stationMaps/2015.jpg")
mapStations(summarize(
  group_by(rfYear, station),
  lat = mean(latitude),
  lon = mean(longitude)
), j = 25)
dev.off()

#for 2016
rfYear <- filter(rf, station %in% allSites2016)
jpeg("figures/stationMaps/2016.jpg")
mapStations(summarize(
  group_by(rfYear, station),
  lat = mean(latitude),
  lon = mean(longitude)
), j = 25)
dev.off()

#for 2017
rfYear <- filter(rf, station %in% allSites2017)
jpeg("figures/stationMaps/2017.jpg")
mapStations(summarize(
  group_by(rfYear, station),
  lat = mean(latitude),
  lon = mean(longitude)
), j = 25)
dev.off()

#for 2018
rfYear <- filter(rf, station %in% allSites2018)
jpeg("figures/stationMaps/2018.jpg")
mapStations(summarize(
  group_by(rfYear, station),
  lat = mean(latitude),
  lon = mean(longitude)
), j = 25)
dev.off()
#=======

#SUPER NORTH Map
#=======
#Map out northern CA stations to see whether or not it's worth adding a new line north of Fort Ross
#get relevant stations
rf <- read_xlsx("../../PhD/Research/Krill/RF_Survey_Data_EUPHAUSIDAE.xlsx")
norCal <- c("False Cape", "Flint Rock Head", "Trinidad Head")
nrf <- filter(rf, area %in% norCal)
#get observations from 2015
nrf$time <- as_datetime(nrf$time)
nrf <- filter(nrf, time > as.POSIXct("2015-01-01"))

#plot out station locations
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = states, fill = NA) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(data = nrf, aes(x = station_longitude, y = station_latitude), size = 2, 
             shape = 23, fill = "darkred") +
  geom_text(data= nrf, aes(x = station_longitude, y = station_latitude, label = station),  hjust = 0.5, vjust = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-125.5, -124.0), ylim = c(40.0, 42.30), expand = FALSE)
#========





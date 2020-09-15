library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sp)
library(maps)
library(readxl)
library(lubridate)
library(geosphere)

#General mapping objects
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
CAfromstates <- states %>%
  subset(., states$ID == "california")

#ggplot(data = coast) +
  #geom_sf()

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
allSites2015 <- c(sites2015$onshore, sites2015$offshore)
jpeg("figures/stationMaps/2015.jpg")
mapStations(summarize(
  group_by(filter(rf, station %in% na.exclude(allSites2015)), station),
  lat = mean(latitude),
  lon = mean(longitude)
), font_size = 25)
dev.off()

#for 2016
jpeg("figures/stationMaps/2016.jpg")
mapStations(summarize(
  group_by(filter(rf, station %in% allSites2016), station),
  lat = mean(latitude),
  lon = mean(longitude)
), font_size = 25)
dev.off()

#for 2017
jpeg("figures/stationMaps/2017.jpg")
mapStations(summarize(
  group_by(filter(rf, station %in% allSites2017), station),
  lat = mean(latitude),
  lon = mean(longitude)
), font_size = 25)
dev.off()

#for 2018
jpeg("figures/stationMaps/2018.jpg")
mapStations(summarize(
  group_by(filter(rf, station %in% allSites2018), station),
  lat = mean(latitude),
  lon = mean(longitude)
), font_size = 25)
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

#Calculate the distance between stations and the coast in meters https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/
#==========
#get station data and convert to sf
urls <- read.csv("data/urls.csv")
stations.sf <- urls %>% st_as_sf(coords = c('lon','lat')) %>% 
  st_set_crs(4326)
#transform CA polygon to a line
ca <- st_cast(CAfromstates, "MULTILINESTRING")
#calculate the distance between the set of points and the line
dist <- st_distance(ca, stations.sf)
#combine distances with stations
df <- data.frame(dist = as.vector(dist)/1000,
                 st_coordinates(stations.sf))
df <- left_join(df, urls[,1:4], by = c("X" = "lon", "Y" = "lat"))
df <- left_join(df, regions)
distFromShore <- summarize(group_by_at(df, vars(station, shore, sites)), dist = mean(dist))
regions <- left_join(regions, distFromShore)
rm(distFromShore)

# 
# col_dist <- RColorBrewer::brewer.pal(9, "YlGnBu")
# 
# 
# ggplot(df, aes(X, Y, fill = dist))+ #variables
#   geom_tile()+ #geometry
#   scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
#   labs(fill = "Distance (km)")+ #legend name
#   theme_void()+ #map theme
#   theme(legend.position = "bottom") #legend position

#plot map of stations colored by temperature
for(i in 2015:2018){
ggplot(data = world) +
  geom_sf(color = "black", fill = "bisque2") +
  geom_sf(data = states, fill = NA) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(data = filter(allLengthsRecentEnv, year.x == i), aes(x = latitude, y = longitude, color = temp_2, size = 2, 
             shape = 23, fill = "darkred") +
  geom_label(data = i, aes(x = lon, y = lat, label = station), nudge_y = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-125.5, -116.75), ylim = c(32.0, 42.30), expand = FALSE) + 
  theme(text = element_text(size = font_size))  
}
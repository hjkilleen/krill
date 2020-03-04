library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(maps)
library(readxl)
library(lubridate)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

ggplot(data = coast) +
  geom_sf()

#Pull in station location information from rockfish survey master datajet and join with length frequency data. Then find list of unique values to create station location data frame for use in maps. 
rf <- read_xlsx("../../PhD/Research/Krill/RF_Survey_Data_EUPHAUSIDAE.xlsx")
rf$time <- as_datetime(rf$time)
rf15 <- filter(rf, time > as.POSIXct("2015-01-01"))
rf15 <- filter(rf15, time < as.POSIXct("2016-01-01"))
lengths15 <- left_join(lengths15, rf15, by = "station")
lengths15 <- select(lengths15, "station", "station_latitude", "station_longitude", "station_bottom_depth", "bottom_depth", "species", "sex", "dish", "length", "year", "measured_by", "incomplete")
ll <- select(lengths15, "station", "station_latitude", "station_longitude")
ll <- distinct(ll)
ll <- filter(ll, station_latitude!="NA")

#Plot map of the CCE with stations we looked at for length frequency in 2015
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = states, fill = NA) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(data = ll, aes(x = station_longitude, y = station_latitude), size = 2, 
             shape = 23, fill = "darkred") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-125.5, -116.75), ylim = c(32.0, 42.30), expand = FALSE)

#What about the missing areas and the stations 358, 421, 426, 427, and 477 that don't appear to have station coordinates, or to have been sampled during 2015 according to the RF metadata?
#ALSO, what about the other stations on Jeff's priority list that don't appear to have been measured?

#let's look in the krill counts, to see whether or not we ever looked through these stations (2015)
cts <- read_xlsx("data/Krillcounts.xlsx",2)
cts <- cts[,1:10]
names <- c("year", "id", "sample", "haul", "station", "species", "split", "male", "female", "total")
names(cts) <- names

cts15 <- filter(cts, year == 2015)
View(unique(cts15$station))

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

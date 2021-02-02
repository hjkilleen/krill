#Figure 1
#Map of study area with inset
# Wed Jan 27 13:58:20 2021 ------------------------------

#LIBRARIES
#====
library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sp)
library(maps)
library(geosphere)
library(cowplot)
load("data/metadata.rda")
#====

#SETUP
#====
regions <- read_csv("data/regions.csv")#add regions for merging shore identifiers

boundaries <- data.frame(x1 = c(-125.5, -125.5, -125.5), y1 = c(34.448, 36.306, 37.948), x2 = c(-120.472, -121.901, -122.785), y2 = c(34.448, 36.306, 37.948))#create gridpoints for regional boundaries

states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))#state lines and coast
CAfromstates <- states %>%
  subset(., states$ID == "california")

#combine region and metadata 
md <- left_join(metadata, regions, by = 'station')
#=====

#MAPPING
#====
#Large Map
map <- ggplot(data = states) +
  geom_sf(fill = "white") +
  geom_point(data = md, aes(x = longitude,y = latitude.x, shape = shore), size = 2) +
  coord_sf(
    xlim = c(-125.5,-116.75),
    ylim = c(32.0, 39.5),
    expand = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  scale_shape_discrete(labels = c("Offshore", "Onshore")) +
  geom_segment(data = boundaries, aes(x = x1, y = y1, xend = x2, yend = y2)) + 
  annotate("text", x = -124.75, y = 33.75, label = "South", size = 5) + 
  annotate("text", x = -124.75, y = 35.25, label = "Central", size = 5) + 
  annotate("text", x = -124.75, y = 37, label = "North\nCentral", size = 5) + 
  annotate("text", x = -124.75, y = 38.75, label = "North", size = 5) +
  theme(text = element_text(size = 25)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.77, 0.6),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "NA")) + 
  guides(shape = guide_legend(override.aes = list(size = 5)))

#Inset Map
inset <- ggplot(data = states) +
  geom_sf(fill = "beige") +
  coord_sf(
    xlim = c(-130,-66),
    ylim = c(24.5, 50),
    expand = FALSE
  ) + 
  geom_rect(aes(xmax = -116.75, xmin = -125.5, ymin = 32, ymax = 39.5), color = "red", alpha = 0) +
  theme_void()

#Full Map
ggdraw() + 
  draw_plot(map) + 
  draw_plot(inset, x = 0.6, y = 0.7, height = .3, width = 0.3)

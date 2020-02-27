#Krill Length Frequency Exploratory Analysis 
#setup script

# Thu Jan 30 15:51:53 2020 ------------------------------

#LIBRARIES AND SOURCES
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(readxl)
library(lubridate)
source("scripts/functions/length_frequency.R")
source("scripts/functions/regions.R")

#LOAD DATA
lengths <- read.csv("data/lengths.csv")
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengths) <- vars
#add lengths variable multiplying pixels by scale measure
lengths <- mutate(lengths, length = pixels/scale)
lengths <- mutate(lengths, year = as.integer(paste("20", str_extract(lengths$ID, "\\d{2}"), sep = "")))
save(lengths, file = "data/lengths.rda")

#Cross-shelf transects histograms for comparison
#==========
#2015 only
lengths15 <- filter(lengths, year == 2015)
#EP, TS, ND only
lengths15$species<-as.character(lengths15$species)
lengths15 <- filter(lengths15, species %in% c('EP', 'TS', 'ND'))
lengths15$species<-as.factor(lengths15$species)
#2015 San Miguel line only
sm15 <- filter(lengths15, station %in% (421:425))
gs <- sm15 %>% 
  group_by_at(vars(station, species))
gl <- group_split(gs)
pl <- lapply(gl, plotHist)
#plot histograms in a grid
n <- length(pl)
sm15p <- do.call("grid.arrange", c(pl, ncol=3, top = "2015 San Miguel Line"))
ggsave("figures/crossShelfLines_hist/2015_sanMiguel.pdf", sm15p, device = "pdf")

#2015 Delgada line only
d15 <- filter(lengths15, station %in% (471:474))
gs <- d15 %>% 
  group_by_at(vars(station, species))
gl <- group_split(gs)
pl <- lapply(gl, plotHist)
#plot histograms in a grid
n <- length(pl)
d15p <- do.call("grid.arrange", c(pl, ncol=2, top = "2015 Delgada Line"))
ggsave("figures/crossShelfLines_hist/2015_delgada.pdf", d15p, device = "pdf")

#2015 Fort Ross line only
fr15 <- filter(lengths15, station %in% (453:455))
gs <- fr15 %>% 
  group_by_at(vars(station, species)) 
gl <- group_split(gs)
pl <- lapply(gl, plotHist)
#plot histograms in a grid
n <- length(pl)
fr15p <- do.call("grid.arrange", c(pl, ncol=2, top = "2015 Fort Ross Line"))
ggsave("figures/crossShelfLines_hist/2015_fortRoss.pdf", fr15p, device = "pdf")
#==========

#Create regional histograms 2015-2018 as available
#=========
#drop all species that are not Ep, Ts, and Nd
topThree <- c("EP", "TS", "ND")
lengthss <- filter(lengths, species %in% topThree)
#drop all species&station combinations with less than 40 observations
tally <- as.data.frame(group_by_at(lengthss, vars(station, species, year)) %>% tally())
tally <- filter(tally, n>=40)
tally <- mutate(tally, id = paste(station, species, year, sep = ""))
lengthss <- mutate(lengthss, id = paste(station, species, year, sep = ""))
lengthss <- filter(lengthss, id %in% tally$id)

#Fort Ross line histograms
d <- filter(lengthss, station %in% FortRoss)
x <- histGrid(d, "Fort Ross")
ggsave("figures/regions_hist/FortRoss.pdf", x, device = "pdf")
#Point Reyes line histograms
d <- filter(lengthss, station %in% PointReyes)
x <- histGrid(d, "Point Reyes")
ggsave("figures/regions_hist/PointReyes.pdf", x, device = "pdf")
#Gulf of the Farallons line histograms
d <- filter(lengthss, station %in% GulfFarallons)
x <- histGrid(d, "Gulf of the Farallons")
ggsave("figures/regions_hist/GoF.pdf", x, device = "pdf")
#San Mateo line histograms
d <- filter(lengthss, station %in% SanMateo)
x <- histGrid(d, "San Mateo")
ggsave("figures/regions_hist/SanMateo.pdf", x, device = "pdf")
#Davenport line histograms
d <- filter(lengthss, station %in% Davenport)
x <- histGrid(d, "Davenport")
ggsave("figures/regions_hist/Davenport.pdf", x, device = "pdf")
#Monterey Bay line histograms
d <- filter(lengthss, station %in% MontereyBay)
x <- histGrid(d, "Monterey Bay")
ggsave("figures/regions_hist/MontereyBay.pdf", x, device = "pdf")
#Point Sur line histograms
d <- filter(lengthss, station %in% PointSur)
x <- histGrid(d, "Point Sur")
ggsave("figures/regions_hist/PointSur.pdf", x, device = "pdf")
#Piedras Blancas line histograms
d <- filter(lengthss, station %in% PiedrasBlancas)
x <- histGrid(d, "Piedras Blancas")
ggsave("figures/regions_hist/PiedrasBlancas.pdf", x, device = "pdf")
#Morro Bay line histograms
d <- filter(lengthss, station %in% MorroBay)
x <- histGrid(d, "Morro Bay")
ggsave("figures/regions_hist/MorroBay.pdf", x, device = "pdf")
#Point Conception line histograms
d <- filter(lengthss, station %in% PointConception)
x <- histGrid(d, "Point Conception")
ggsave("figures/regions_hist/PointConception.pdf", x, device = "pdf")
#Santa Barbars line histograms
d <- filter(lengthss, station %in% SantaBarbara)
x <- histGrid(d, "Santa Barbara")
ggsave("figures/regions_hist/SantaBarbara.pdf", x, device = "pdf")
#Mid Channel Islands line histograms
d <- filter(lengthss, station %in% MidChannelIslands)
x <- histGrid(d, "Mid Channel Islands")
ggsave("figures/regions_hist/MidChannelIslands.pdf", x, device = "pdf")
#Southern CA line histograms
d <- filter(lengthss, station %in% SouthernCA)
x <- histGrid(d, "Southern CA")
ggsave("figures/regions_hist/SouthernCA.pdf", x, device = "pdf")
#=========

#Create dataframe with histogram summary statistics 2015-2018 as available
#======
#subset full lengths dataset to the priority stations
onshore <- filter(lengthss, station %in% regions$onshore)
offshore <- filter(lengthss, station %in% regions$offshore)
subLengths <- full_join(onshore, offshore)

#
#======
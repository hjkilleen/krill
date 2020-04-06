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
library(e1071) #for histogram stats
library(ggridges)
library(formattable)
source("scripts/functions/length_frequency.R")
source("scripts/functions/regions.R")

#LOAD DATA
lengths <- read.csv("data/lengths.csv")
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengths) <- vars
#add lengths variable multiplying pixels by scale measure
lengths <- mutate(lengths, length = pixels/scale)
lengths <- mutate(lengths, year = as.integer(paste("20", str_extract(lengths$ID, "\\d{2}"), sep = "")))
#filter to only the stations we are including in the analysis
lengths <- filter(lengths, station %in% allSites)
lengths <- left_join(lengths, regions, by = "station")
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
#drop all species that are not TS, Ts, and Nd
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
#Point ConcTStion line histograms
d <- filter(lengthss, station %in% PointConcTStion)
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
lfStats <- summarize(group_by_at(lengths, vars(species, year)), med = median(length), sd = sd(length), skew = skewness(length), kurtosis = kurtosis(length))
formattable(lfStats)
#look at the same data by species
epStats <- filter(lfStats, species == "EP")
tsStats <- filter(lfStats, species == "TS")
ndStats <- filter(lfStats, species == "ND")
#======

#Create plots for body size across years, regions, and species
#======
#All species, all time, all regions
jpeg("figures/bodySize/allSppAllYrsAllReg.jpg")
plotHist(lengths, "All species, all years, all regions")
dev.off()
#All species, all time, by region
p1 <-plotHist(filter(lengths, region == "north"), "North Region")
p2 <-plotHist(filter(lengths, region == "north_central"), "North-Central Region")
p3 <-plotHist(filter(lengths, region == "central"), "Central Region")
p4 <-plotHist(filter(lengths, region == "south"), "South Region")
jpeg("figures/bodySize/allSppAllYrsByReg.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/allSppAllYrsByReg_boxplot.jpg")
boxplot(length~region, lengths)
dev.off()
#All species, by time, all regions
p1 <-plotHist(filter(lengths, year == 2015), "2015")
p2 <-plotHist(filter(lengths, year == 2016), "2016")
p3 <-plotHist(filter(lengths, year == 2017), "2017")
p4 <-plotHist(filter(lengths, year == 2018), "2018")
jpeg("figures/bodySize/allSppByYrsAllReg.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/allSppByYrsAllReg_boxplot.jpg")
boxplot(length~year, lengths)
dev.off()
#All species, by time by region
#North
p1 <-plotHist(filter(lengths, region == "north", year == 2015), "North Region 2015")
p2 <-plotHist(filter(lengths, region == "north", year == 2016), "North Region 2016")
p3 <-plotHist(filter(lengths, region == "north", year == 2017), "North Region 2017")
p4 <-plotHist(filter(lengths, region == "north", year == 2018), "North Region 2018")
jpeg("figures/bodySize/allSppByYrsNorth.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/allSppByYrsNorth_boxplot.jpg")
boxplot(length~year, filter(lengths, region == "north"))
dev.off()
#North Central
p1 <-plotHist(filter(lengths, region == "north_central", year == 2015), "North-Central Region")
p2 <-plotHist(filter(lengths, region == "north_central", year == 2016), "North-Central Region 2016")
p3 <-plotHist(filter(lengths, region == "north_central", year == 2017), "North-Central Region 2017")
p4 <-plotHist(filter(lengths, region == "north_central", year == 2018), "North-Central Region 2018")
jpeg("figures/bodySize/allSppByYrsNorthCentral.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/allSppByYrsNorthCentral_boxplot.jpg")
boxplot(length~year, filter(lengths, region == "north_central"))
dev.off()
#Central
p1 <-plotHist(filter(lengths, region == "central", year == 2015), "Central Region 2015")
p2 <-plotHist(filter(lengths, region == "central", year == 2016), "Central Region 2016")
p3 <-plotHist(filter(lengths, region == "central", year == 2017), "Central Region 2017")
p4 <-plotHist(filter(lengths, region == "central", year == 2018), "Central Region 2018")
jpeg("figures/bodySize/allSppByYrsCentral.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/allSppByYrsCentral_boxplot.jpg")
boxplot(length~year, filter(lengths, region == "central"))
dev.off()
#South
p1 <-plotHist(filter(lengths, region == "south", year == 2015), "South Region 2015")
p2 <-plotHist(filter(lengths, region == "south", year == 2016), "South Region 2016")
p3 <-plotHist(filter(lengths, region == "south", year == 2017), "South Region 2017")
p4 <-plotHist(filter(lengths, region == "south", year == 2018), "South Region 2018")
jpeg("figures/bodySize/allSppByYrsSouth.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/allSppByYrsSouth_boxplot.jpg")
boxplot(length~year, filter(lengths, region == "south"))
dev.off()

#filter to EP only
ep <- filter(lengths, species == "EP")
#EP, all time, all regions
jpeg("figures/bodySize/EP/EPAllYrsAllReg.jpg")
plotHist(ep, "E. pacifica, all years, all regions")
dev.off()
#EP, all time, by region
p1 <-plotHist(filter(ep, region == "north"), "EP North Region")
p2 <-plotHist(filter(ep, region == "north_central"), "EP North-Central Region")
p3 <-plotHist(filter(ep, region == "central"), "EP Central Region")
p4 <-plotHist(filter(ep, region == "south"), "EP South Region")
jpeg("figures/bodySize/EP/EPAllYrsByReg.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/EP/EPAllYrsByReg_boxplot.jpg")
boxplot(length~region, ep)
dev.off()
#EP, by time, all regions
p1 <-plotHist(filter(ep, year == 2015), "EP 2015")
p2 <-plotHist(filter(ep, year == 2016), "EP 2016")
p3 <-plotHist(filter(ep, year == 2017), "EP 2017")
p4 <-plotHist(filter(ep, year == 2018), "EP 2018")
jpeg("figures/bodySize/EP/EPByYrsAllReg.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/EP/EPByYrsAllReg_boxplot.jpg")
boxplot(length~year, ep)
dev.off()
#EP, by time by region
#North
p1 <-plotHist(filter(ep, region == "north", year == 2015), "EP North Region 2015")
p2 <-plotHist(filter(ep, region == "north", year == 2016), "EP North Region 2016")
p3 <-plotHist(filter(ep, region == "north", year == 2017), "EP North Region 2017")
p4 <-plotHist(filter(ep, region == "north", year == 2018), "EP North Region 2018")
jpeg("figures/bodySize/EP/EPByYrsNorth.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/EP/EPByYrsNorth_boxplot.jpg")
boxplot(length~year, filter(ep, region == "north"))
dev.off()
#North Central
p1 <-plotHist(filter(ep, region == "north_central", year == 2015), "EP North-Central Region")
p2 <-plotHist(filter(ep, region == "north_central", year == 2016), "EP North-Central Region 2016")
p3 <-plotHist(filter(ep, region == "north_central", year == 2017), "EP North-Central Region 2017")
p4 <-plotHist(filter(ep, region == "north_central", year == 2018), "EP North-Central Region 2018")
jpeg("figures/bodySize/EP/EPByYrsNorthCentral.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/EP/EPByYrsNorthCentral_boxplot.jpg")
boxplot(length~year, filter(ep, region == "north_central"))
dev.off()
#Central
p1 <-plotHist(filter(ep, region == "central", year == 2015), "EP Central Region 2015")
p2 <-plotHist(filter(ep, region == "central", year == 2016), "EP Central Region 2016")
p3 <-plotHist(filter(ep, region == "central", year == 2017), "EP Central Region 2017")
p4 <-plotHist(filter(ep, region == "central", year == 2018), "EP Central Region 2018")
jpeg("figures/bodySize/EP/EPByYrsCentral.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/EP/EPByYrsCentral_boxplot.jpg")
boxplot(length~year, filter(ep, region == "central"))
dev.off()
#South
p1 <-plotHist(filter(ep, region == "south", year == 2015), "EP South Region 2015")
p2 <-plotHist(filter(ep, region == "south", year == 2016), "EP South Region 2016")
p3 <-plotHist(filter(ep, region == "south", year == 2017), "EP South Region 2017")
p4 <-plotHist(filter(ep, region == "south", year == 2018), "EP South Region 2018")
jpeg("figures/bodySize/EP/EPByYrsSouth.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/EP/EPByYrsSouth_boxplot.jpg")
boxplot(length~year, filter(ep, region == "south"))
dev.off()

#filter to TS only
ts <- filter(lengths, species =="TS")
#TS, all time, all regions
jpeg("figures/bodySize/TS/tsAllYrsAllReg.jpg")
plotHist(ts, "T. spinifera, all years, all regions")
dev.off()
#TS, all time, by region
p1 <-plotHist(filter(ts, region == "north"), "TS North Region")
p2 <-plotHist(filter(ts, region == "north_central"), "TS North-Central Region")
p3 <-plotHist(filter(ts, region == "central"), "TS Central Region")
p4 <-plotHist(filter(ts, region == "south"), "TS South Region")
jpeg("figures/bodySize/TS/tsAllYrsByReg.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/TS/tsAllYrsByReg_boxplot.jpg")
boxplot(length~region, ts)
dev.off()
#TS, by time, all regions
p1 <-plotHist(filter(ts, year == 2015), "TS 2015")
p2 <-plotHist(filter(ts, year == 2016), "TS 2016")
p3 <-plotHist(filter(ts, year == 2017), "TS 2017")
p4 <-plotHist(filter(ts, year == 2018), "TS 2018")
jpeg("figures/bodySize/TS/tsByYrsAllReg.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/TS/tsByYrsAllReg_boxplot.jpg")
boxplot(length~year, ts)
dev.off()
#TS, by time by region
#North
p1 <-plotHist(filter(ts, region == "north", year == 2015), "TS North Region 2015")
p2 <-plotHist(filter(ts, region == "north", year == 2016), "TS North Region 2016")
p3 <-plotHist(filter(ts, region == "north", year == 2017), "TS North Region 2017")
p4 <-plotHist(filter(ts, region == "north", year == 2018), "TS North Region 2018")
jpeg("figures/bodySize/TS/tsByYrsNorth.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/TS/tsByYrsNorth_boxplot.jpg")
boxplot(length~year, filter(ts, region == "north"))
dev.off()
#North Central
p1 <-plotHist(filter(ts, region == "north_central", year == 2015), "TS North-Central Region")
p2 <-plotHist(filter(ts, region == "north_central", year == 2016), "TS North-Central Region 2016")
p3 <-plotHist(filter(ts, region == "north_central", year == 2017), "TS North-Central Region 2017")
p4 <-plotHist(filter(ts, region == "north_central", year == 2018), "TS North-Central Region 2018")
jpeg("figures/bodySize/TS/tsByYrsNorthCentral.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/TS/tsByYrsNorthCentral_boxplot.jpg")
boxplot(length~year, filter(ts, region == "north_central"))
dev.off()
#Central
p1 <-plotHist(filter(ts, region == "central", year == 2015), "TS Central Region 2015")
p2 <-plotHist(filter(ts, region == "central", year == 2016), "TS Central Region 2016")
p3 <-plotHist(filter(ts, region == "central", year == 2017), "TS Central Region 2017")
p4 <-plotHist(filter(ts, region == "central", year == 2018), "TS Central Region 2018")
jpeg("figures/bodySize/TS/tsByYrsCentral.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/TS/tsByYrsCentral_boxplot.jpg")
boxplot(length~year, filter(ts, region == "central"))
dev.off()
#South
p1 <-plotHist(filter(ts, region == "south", year == 2015), "TS South Region 2015")
p2 <-plotHist(filter(ts, region == "south", year == 2016), "TS South Region 2016")
p3 <-plotHist(filter(ts, region == "south", year == 2017), "TS South Region 2017")
p4 <-plotHist(filter(ts, region == "south", year == 2018), "TS South Region 2018")
jpeg("figures/bodySize/TS/tsByYrsSouth.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/TS/tsByYrsSouth_boxplot.jpg")
boxplot(length~year, filter(ts, region == "south"))
dev.off()

#filter to ND only
nd <- filter(lengths, species =="ND")
#ND, all time, all regions
jpeg("figures/bodySize/ND/ndAllYrsAllReg.jpg")
plotHist(nd, "N.difficilis, all years, all regions")
dev.off()
#ND, all time, by region
p1 <-plotHist(filter(nd, region == "north"), "ND North Region")
p2 <-plotHist(filter(nd, region == "north_central"), "ND North-Central Region")
p3 <-plotHist(filter(nd, region == "central"), "ND Central Region")
p4 <-plotHist(filter(nd, region == "south"), "ND South Region")
jpeg("figures/bodySize/ND/ndAllYrsByReg.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/ND/ndAllYrsByReg_boxplot.jpg")
boxplot(length~region, nd)
dev.off()
#ND, by time, all regions
p1 <-plotHist(filter(nd, year == 2015), "ND 2015")
p2 <-plotHist(filter(nd, year == 2016), "ND 2016")
p3 <-plotHist(filter(nd, year == 2017), "ND 2017")
p4 <-plotHist(filter(nd, year == 2018), "ND 2018")
jpeg("figures/bodySize/ND/ndByYrsAllReg.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/ND/ndByYrsAllReg_boxplot.jpg")
boxplot(length~year, nd)
dev.off()
#ND, by time by region
#North
p1 <-plotHist(filter(nd, region == "north", year == 2015), "ND North Region 2015")
p2 <-plotHist(filter(nd, region == "north", year == 2016), "ND North Region 2016")
p3 <-plotHist(filter(nd, region == "north", year == 2017), "ND North Region 2017")
p4 <-plotHist(filter(nd, region == "north", year == 2018), "ND North Region 2018")
jpeg("figures/bodySize/ND/ndByYrsNorth.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/ND/ndByYrsNorth_boxplot.jpg")
boxplot(length~year, filter(nd, region == "north"))
dev.off()
#North Central
p1 <-plotHist(filter(nd, region == "north_central", year == 2015), "ND North-Central Region")
p2 <-plotHist(filter(nd, region == "north_central", year == 2016), "ND North-Central Region 2016")
p3 <-plotHist(filter(nd, region == "north_central", year == 2017), "ND North-Central Region 2017")
p4 <-plotHist(filter(nd, region == "north_central", year == 2018), "ND North-Central Region 2018")
jpeg("figures/bodySize/ND/ndByYrsNorthCentral.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/ND/ndByYrsNorthCentral_boxplot.jpg")
boxplot(length~year, filter(nd, region == "north_central"))
dev.off()
#Central
p1 <-plotHist(filter(nd, region == "central", year == 2015), "ND Central Region 2015")
p2 <-plotHist(filter(nd, region == "central", year == 2016), "ND Central Region 2016")
p3 <-plotHist(filter(nd, region == "central", year == 2017), "ND Central Region 2017")
p4 <-plotHist(filter(nd, region == "central", year == 2018), "ND Central Region 2018")
jpeg("figures/bodySize/ND/ndByYrsCentral.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/ND/ndByYrsCentral_boxplot.jpg")
boxplot(length~year, filter(nd, region == "central"))
dev.off()
#South
p1 <-plotHist(filter(nd, region == "south", year == 2015), "ND South Region 2015")
p2 <-plotHist(filter(nd, region == "south", year == 2016), "ND South Region 2016")
p3 <-plotHist(filter(nd, region == "south", year == 2017), "ND South Region 2017")
p4 <-plotHist(filter(nd, region == "south", year == 2018), "ND South Region 2018")
jpeg("figures/bodySize/ND/ndByYrsSouth.jpg")
grid.arrange(p1, p2, p3, p4, nrow = 4)
dev.off()
#boxplot
jpeg("figures/bodySize/ND/ndByYrsSouth_boxplot.jpg")
boxplot(length~year, filter(nd, region == "south"))
dev.off()
#======

#Create waterfall plot for 2016 EP across all latitudes
#========
ep16 <- filter(lengths, year == 2016, species == "EP")
ggplot(ep16, aes(x = length, y = latitude, group = latitude)) +
  geom_density_ridges()
ggsave("figures/bodySize/ridges/2016_ep.jpeg")
#========
#Waterfall plots for all years and all species
five <- filter(lengths, year == 2015)
six <- filter(lengths, year == 2016)
seven <- filter(lengths, year == 2017)
eight <- filter(lengths, year == 2018)

#EP 2015-2017 whole coast 
a <- rbind(five, six, seven)
a <- filter(a, species == "EP")
a$lat.round <- round(a$latitude, 1)
a$lat.fac <- as.factor(a$lat.round)
a$year <- as.factor(a$year)

ggplot(a, aes(x = length, y = lat.fac, group = paste(lat.fac, year), fill = year, point_color = year)) + 
  geom_density_ridges(scale = 0.9, rel_min_height = .01) + 
  xlim(10, 30) +
  scale_fill_manual(values = c("#ff000080", "#00ff0080", "#0000ff80"), labels = c("2015", "2016", "2017")) +
  labs(x = "Length (mm)", y = "Latitude", title = "Eupahusia pacifica Body Length by Latitude and Year") + 
  geom_hline(yintercept = c(4, 6, 10), color = "black", alpha = .5) + 
  ggsave("figures/bodySize/latitude/EP_CA_15.16.17.jpg", width = 6, height = 10)

#Companion table for median, mean, and mode
epTab15 <- summarize(group_by(filter(a, year == 2015), lat.fac), median15 = median(length))
epTab15 <- distinct(left_join(epTab15, select(a, lat.fac, sites)))
epTab16 <- summarize(group_by(filter(a, year == 2016), lat.fac), median16 = median(length))
epTab16 <- distinct(left_join(epTab16, select(a, lat.fac, sites)))
epTab17 <- summarize(group_by(filter(a, year == 2017), lat.fac), median17 = median(length))
epTab17 <- distinct(left_join(epTab17, select(a, lat.fac, sites)))
merge(epTab15, epTab16, by = sites)
formattable(epTab)

#TS 2015-2016 whole coast
b <- rbind(five, six, seven)
b <- filter(b, species == "TS")
b$lat.round <- round(b$latitude, 1)
b$lat.fac <- as.factor(b$lat.round)
b$year <- as.factor(b$year)

ggplot(b, aes(x = length, y = lat.fac, group = paste(lat.fac, year), fill = year, point_color = year)) + 
  geom_density_ridges(scale = 0.9, rel_min_height = .01) + 
  scale_fill_manual(values = c("#ff000080", "#00ff0080", "#0000ff80"), labels = c("2015", "2016", "2017")) +
  labs(x = "Length (mm)", y = "Latitude", title = "Thysanoessa spinifera Body Length by Latitude and Year") + 
  geom_hline(yintercept = c(4, 6, 10), color = "black", alpha = .5) + 
  ggsave("figures/bodySize/latitude/TS_CA_15.16.17.jpg", width = 6, height = 10)

#SoCal 2015-2017 stacked
c <- rbind(five, six, seven)
c <- filter(c, latitude < 36, species == "ND")
ggplot(c, aes(y = latitude)) + 
  geom_density_ridges(aes(x = length, fill = paste(latitude, year)), scale = 0.8, rel_min_height = .01) +
  scale_fill_cyclical(values = c("#ff000080", "#00ff0080", "#0000ff80"), name = "Year", guide = "legend", labels = c("2015", "2016", "2017")) +
  labs(x = "Length (mm)" , y = "Latitude", title = "Nematocelis difficilis body lengths") + 
  ggsave("figures/bodySize/latitude/ND_So_15.16.17.jpg")

#Central region comparisons to pre-blob data RUN AGAIN AFTER PUTTING IN 2017 AND 2018 DATA
#=========
#load data
lengthsBaldo <- read.csv("data/lengthsBaldo.csv")
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengthsBaldo) <- vars
#add lengths variable multiplying pixels by scale measure
lengthsBaldo <- mutate(lengthsBaldo, length = pixels/scale)
lengthsBaldo <- mutate(lengthsBaldo, year = as.integer(paste("20", str_extract(lengthsBaldo$ID, "\\d{2}"), sep = "")))
#get locational information
lengthsBaldo <- left_join(lengthsBaldo, regions, by = "station")
#merge with 2015-2018 dataset
allLengths <- rbind(lengths, lengthsBaldo)

#summary statistics
stats <- summarize(group_by_at(filter(allLengths, region == "north_central"), vars(species, year, region)), med = median(length), sd = sd(length), skew = skewness(length), kurtosis = kurtosis(length))
formattable(stats)

#boxplots
#All species
jpeg("figures/bodySize/time/NorthCentral.jpg")
boxplot(length~year, filter(allLengths, region == "north_central"))
dev.off()
#EP only
jpeg("figures/bodySize/time/NorthCentralEP.jpg")
boxplot(length~year, filter(allLengths, region == "north_central", species == "EP"))
dev.off()
jpeg("figures/bodySize/time/NorthEP.jpg")
boxplot(length~year, filter(allLengths, region == "north", species == "EP"))
dev.off()

#TS only
jpeg("figures/bodySize/time/NorthCentralTS.jpg")
boxplot(length~year, filter(allLengths, region == "north_central", species == "TS"))
dev.off()
jpeg("figures/bodySize/time/NorthTS.jpg")
boxplot(length~year, filter(allLengths, region == "north", species == "TS"))
dev.off()

#ND only
jpeg("figures/bodySize/time/NorthCentralND.jpg")
boxplot(length~year, filter(allLengths, region == "north_central", species == "ND"))
dev.off()
jpeg("figures/bodySize/time/NorthNS.jpg")
boxplot(length~year, filter(allLengths, region == "north", species == "ND"))
dev.off()
#=========

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

#LOAD DATA
lengths <- read.csv("data/lengths.csv")
vars <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by")
names(lengths) <- vars
#add lengths variable multiplying pixels by scale measure
lengths <- mutate(lengths, length = pixels/scale)
lengths <- mutate(lengths, year = as.integer(paste("20", str_extract(lengths$ID, "\\d{2}"), sep = "")))
save(lengths, file = "data/lengths.rda")

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

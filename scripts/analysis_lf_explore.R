#Krill Length Frequency Exploratory Analysis 
#setup script

# Thu Jan 30 15:51:53 2020 ------------------------------

#LIBRARIES AND SOURCES
library(dplyr)
library(ggplot2)
library(gridExtra)
source("scripts/functions/length_frequency.R")

#LOAD DATA
lengths <- read.csv("data/lengths.csv")
#add lengths variable multiplying pixels by scale measure
lengths <- mutate(lengths, length = pixels/scale)
lengths <- mutate(lengths, year = as.integer(paste("20", str_extract(lengths$ID, "\\d{2}"), sep = "")))
save(lengths, file = "data/lengths.rda")
n <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by", "length")
names(lengths) <- n

#2015 only
lengths15 <- filter(lengths, year == 2015)

#2015 San Miguel line only
sm15 <- filter(lengths15, station %in% (421:425))
gs <- sm15 %>% 
  group_by(station) 
gl <- group_split(gs)
pl <- lapply(gl, plotHist)
#plot histograms in a grid
n <- length(pl)
soCal2015 <- do.call("grid.arrange", c(pl, ncol=1, top = "2015 San Miguel Line"))
ggsave("figures/crossShelfLines_hist/2015_sanMiguel.pdf", soCal2015, device = "pdf")

#2015 San Nicolas line only
sn15 <- filter(lengths15, station %in% (411:414))
gs <- sm15 %>% 
  group_by(station) 
gl <- group_split(gs)
pl <- lapply(gl, plotHist)
#plot histograms in a grid
n <- length(pl)
soCal2015 <- do.call("grid.arrange", c(pl, ncol=1, top = "2015 San Miguel Line"))
ggsave("figures/crossShelfLines_hist/2015_sanMiguel.pdf", soCal2015, device = "pdf")

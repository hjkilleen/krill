#Krill Length Frequency Exploratory Analysis 
#setup script

# Thu Jan 30 15:51:53 2020 ------------------------------

#LIBRARIES AND SOURCES
library(dplyr)
library(ggplot2)
library(gridExtra)

#LOAD DATA
lengths <- read.csv("data/lengths.csv")
#add lengths variable multiplying pixels by scale measure
lengths <- mutate(lengths, length = pixels/scale)
save(lengths, file = "data/lengths.rda")
n <- c("ID", "station", "species", "sex", "dish", "scale", "pixels", "notes", "incomplete", "measured_by", "length")
names(lengths) <- n

#2015 SoCal line only
lengths15 <- filter(lengths, station %in% (421:425))
gs <- lengths15 %>% 
  group_by(station) 
gl <- group_split(gs)
pl <- lapply(gl, plotHist)
#plot histograms in a grid
n <- length(pl)
soCal2015 <- do.call("grid.arrange", c(pl, ncol=1, top = "2015 Point Conception Line"))
ggsave("figures/crossShelfLines_hist/2015_420.pdf", soCal2015, device = "pdf")

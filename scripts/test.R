setwd("../krill/")
library(readr)
library(ggplot2)
krill <- read_csv("data/LengthFrequencyMaster.csv")
krill <- krill[,1:10]
str(krill)

st <- as.vector(unique(krill$station))

#generate histograms by station
for (i in st) {
    t <- filter(krill, station == i) %>% 
    ggplot(aes(length)) + 
    geom_histogram() +
    ggtitle(i)
    plot(t)
}




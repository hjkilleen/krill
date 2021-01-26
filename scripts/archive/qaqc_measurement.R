#Measurement QC
qc <- read.csv("data/lengths.csv")
library(tidyverse)
qc <- mutate(qc, length = Krill.length/Scale.length)
qc <- filter(qc, length < 100)
library(ggplot2)
ggplot(qc) +
  geom_histogram(aes(length, y = ..density..)) +
  facet_wrap(qc$Measured.by)

#Jeffrey and Ava have long left tails
as <- filter(qc, Measured.by == "Ava Smith")
ggplot(as) + 
  geom_point(aes(Station.., y = length, color = Species))
#Both showed normal distribution of measurements by station. 
#I double checked their measurement technique. JY was fine. AS photos are missing.

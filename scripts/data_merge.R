#Merge krill lengths and environmental data into one dataset
# Fri Dec 11 16:15:42 2020 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/data_load.R")
#====

#Filter by species and save as .RDA
ep <- filter(allLengths, species == "EP")
ts <- filter(allLengths, species == "TS")
nd <- filter(allLengths, species == "ND")
save(ep, file = "data/allLengthsEP.rda")
save(ts, file = "data/allLengthsTS.rda")
save(nd, file = "data/allLengthsND.rda")
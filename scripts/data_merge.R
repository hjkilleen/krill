#Merge krill lengths and environmental data into one dataset
# Mon Jan 25 16:57:29 2021 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/data_tidy_environment.R")
source("scripts/data_tidy_satellite.R")
source("scripts/functions/length_frequency.R")
load("data/allLengths.rda")
#====

#SET UP
#====
allLengths$latitude.round <- round(allLengths$latitude, 0)#rounded latitude variable to index with CUIT/BEUTI
a <- as.data.frame(summarize(group_by_at(allLengths, vars(station, year, latitude, latitude.round)), temp_2 = NA, temp_100 = NA))#list of stations and years
#====

#TEMPERATURE
#====
#Get SST and subsurface temperatures, average across 2 days prior to sample
for(i in seq(1:nrow(a))) {#get SST and subsurface temperatures for each list element
  a$temp_2[i] <- get.temp.2(a$station[i], a$year[i], 2)
  a$temp_100[i] <- get.temp.100(a$station[i], a$year[i], 2)
}

#Get SST variance variable based on prior two weeks
a$sst_sd <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))) {
  a$sst_sd[i] <- get.sd(a$station[i], a$year[i], 17)
}
#====

#CHLOROPHYLL
#====
#Get chlorophyll variable based on prior 3, 8, 13 days depending on availability
a$chla <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))){
  if(get.chla(a$station[i], a$year[i], 3) != "NaN"){
    a$chla[i] <- get.chla(a$station[i], a$year[i], 3)
  } else if(get.chla(a$station[i], a$year[i], 8) != "NaN"){
    a$chla[i] <- get.chla(a$station[i], a$year[i], 8)
  } else a$chla[i] <- get.chla(a$station[i], a$year[i], 13)
}
#====

#UPWELLING
#====
#Get BEUTI averaged over past 13 days, PST day
a$beuti <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))){
  a$beuti[i] <- get.beuti(a$station[i], a$year[i], 13)
}

#Get CUTI averaged over past 13 days, PST day
a$cuti <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))){
  a$cuti[i] <- get.cuti(a$station[i], a$year[i], 13)
}
#====

#OTHER
#====
#Get SLA averaged over past 13 days
a$sla <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))){
  a$sla[i] <- get.sla(a$station[i], a$year[i], 1)
}

#Get spring MOCI
a$year <- as.numeric(a$year)#year as numeric for indexing with MOCI

aN <- filter(a, latitude >=38)#create regional dataframes to match with MOCI data structure (divided into N, C, and S CA)
aC <- filter(a, latitude >=34.5, latitude <38)
aS <- filter(a, latitude >=32, latitude <34.5)

krillList <- list(aN, aC, aS)
mociList <- list(moci[,1:4], moci[,c(1:3, 5)], moci[,c(1:3,6)])

for(i in 1:3){#get spring and winter moci values for each region
  b <- filter(mociList[[i]], Season == "AMJ")
  krillList[[i]] <- left_join(krillList[[i]], b, by = "year")
  krillList[[i]] <- rename(krillList[[i]], moci_spring = names(krillList[[i]])[14])
  krillList[[i]] <- krillList[[i]][,-c(12:13)]
}

a <- bind_rows(krillList)#bind all regions into one df
a$year <- as.character(a$year)
#====

#MERGE
#====
allLengthsEnv <- left_join(select(allLengths, station, date, year, species, sex, latitude, longitude, length), a)#merge new columns with allLengths in a novel df
allLengthsEnv <- left_join(allLengthsEnv, select(regions, station, sites, region, shore), by = 'station')#merge with regional and site identifiers

#SCALE PARAMETERS
#====
#Scale parameters
allLengthsEnv$temp_2 <- scale(allLengthsEnv$temp_2)
allLengthsEnv$temp_100 <- scale(allLengthsEnv$temp_100)
allLengthsEnv$sst_sd <- scale(allLengthsEnv$sst_sd)
allLengthsEnv$chla <- scale(allLengthsEnv$chla)
allLengthsEnv$beuti <- scale(allLengthsEnv$beuti)
allLengthsEnv$moci_spring <- scale(allLengthsEnv$moci_spring)
allLengthsEnv$sla <- scale(allLengthsEnv$sla)
allLengthsEnv$cuti <- scale(allLengthsEnv$cuti)
#====

#SAVE
#====
#save datafile
save(allLengthsEnv, file = "data/allLengthsEnv.rda")
#Filter by species and save as .RDA
ep <- filter(allLengthsEnv, species == "EP")
ts <- filter(allLengthsEnv, species == "TS")
nd <- filter(allLengthsEnv, species == "ND")
save(ep, file = "data/allLengthsEnvEP.rda")
save(ts, file = "data/allLengthsEnvTS.rda")
save(nd, file = "data/allLengthsEnvND.rda")
#====
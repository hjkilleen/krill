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

climatology <- summarize(group_by(env, station), chl = mean(chlor_a, na.rm = TRUE))#climatology for chlorophyll data, impute missing values
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
#Get chlorophyll variable based on prior 27 days depending on availability
a$chla <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))){
  if(get.chla(a$station[i], a$year[i], 27) != "NaN"){
    a$chla[i] <- get.chla(a$station[i], a$year[i], 27)
  } else a$chla[i] <- filter(climatology, station == a$station[i])$chl
}
#====

#UPWELLING
#====
#Get CUTI averaged over past 13 days, PST day
a$cuti <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))){
  a$cuti[i] <- get.cuti(a$station[i], a$year[i], 9)
}
#====

#MOCI
#====
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
  krillList[[i]] <- rename(krillList[[i]], moci_spring = names(krillList[[i]])[12])
  krillList[[i]] <- krillList[[i]][,-c(10:11)]
}

a <- bind_rows(krillList)#bind all regions into one df
a$year <- as.character(a$year)
#====

#MERGE
#====
allLengthsEnv <- left_join(allLengths, a)#merge new columns with allLengths in a novel df

#SCALE PARAMETERS
#====
#Filter by species
ep <- filter(allLengthsEnv, species == "EP")
ts <- filter(allLengthsEnv, species == "TS")
nd <- filter(allLengthsEnv, species == "ND")

#Scale parameters for allLengthsEnv
allLengthsEnv$temp_2 <- scale(allLengthsEnv$temp_2)
allLengthsEnv$temp_100 <- scale(allLengthsEnv$temp_100)
allLengthsEnv$sst_sd <- scale(allLengthsEnv$sst_sd)
allLengthsEnv$chla <- scale(allLengthsEnv$chla)
allLengthsEnv$moci_spring <- scale(allLengthsEnv$moci_spring)
allLengthsEnv$cuti <- scale(allLengthsEnv$cuti)
#Scale lengths
allLengthsEnv$length.unscaled <- allLengthsEnv$length
allLengthsEnv$length <- scale(allLengthsEnv$length)
#Save scale parameters to transform lengths for plotting
aleScale <- allLengthsEnv$length
save(aleScale, file = "output/aleScale.rda")

#Scale parameters for EP
ep$temp_2 <- scale(ep$temp_2)
ep$temp_100 <- scale(ep$temp_100)
ep$sst_sd <- scale(ep$sst_sd)
ep$chla <- scale(ep$chla)
ep$moci_spring <- scale(ep$moci_spring)
ep$cuti <- scale(ep$cuti)
#Scale lengths
ep$length <- scale(ep$length)
#Save scale parameters to transform lengths for plotting
epScale <- ep$length
save(epScale, file = "output/epScale.rda")

#Scale parameters for TS
ts$temp_2 <- scale(ts$temp_2)
ts$temp_100 <- scale(ts$temp_100)
ts$sst_sd <- scale(ts$sst_sd)
ts$chla <- scale(ts$chla)
ts$moci_spring <- scale(ts$moci_spring)
ts$cuti <- scale(ts$cuti)
#Scale lengths
ts$length <- scale(ts$length)
#Save scale parameters to transform lengths for plotting
tsScale <- ts$length
save(tsScale, file = "output/tsScale.rda")

#Scale parameters for ND
nd$temp_2 <- scale(nd$temp_2)
nd$temp_100 <- scale(nd$temp_100)
nd$sst_sd <- scale(nd$sst_sd)
nd$chla <- scale(nd$chla)
nd$moci_spring <- scale(nd$moci_spring)
nd$cuti <- scale(nd$cuti)
#Scale lengths
nd$length <- scale(nd$length)
#Save scale parameters to transform lengths for plotting
ndScale <- nd$length
save(ndScale, file = "output/ndScale.rda")
#====

#SAVE
#====
#save datafiles
save(allLengthsEnv, file = "data/allLengthsEnv.rda")
save(ep, file = "data/allLengthsEnvEP.rda")
save(ts, file = "data/allLengthsEnvTS.rda")
save(nd, file = "data/allLengthsEnvND.rda")
#====


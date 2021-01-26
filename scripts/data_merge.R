#Merge krill lengths and environmental data into one dataset
# Fri Dec 11 16:15:42 2020 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/data_tidy_environment.R")
source("scripts/data_tidy_satellite.R")
source("scripts/functions/length_frequency.R")
#====

#SET UP
#====
allLengths$latitude.round <- round(allLengths$latitude, 0)#rounded latitude variable to index with CUIT/BEUTI
a <- as.data.frame(summarize(group_by_at(allLengths, vars(station, year, latitude.round)), temp_2 = NA, temp_100 = NA))#list of stations and years
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


a <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(station, year, latitude.round)), beuti = NA))

for(i in seq(1:nrow(a))){
  a$beuti[i] <- get.beuti(a$station[i], a$year[i], 13)
}

allLengthsEnv <- left_join(allLengthsEnv, a)

#add cuti averaged over past 13 days
#load cuti data, PST day
cuti <- filter(cuti, year>2010, year <2019)
cuti$date <- ymd(paste(cuti$year, cuti$month, cuti$day, sep = "-"))
cuti <- melt(cuti, id.vars = c("year", "month", "day", "date"))#long form data
cuti <- rename(cuti, cuti = value)
cuti$latitude.round <- as.numeric(str_sub(as.character(cuti$variable), 1, 2))

a <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(station, year, latitude.round)), cuti = NA))

for(i in seq(1:nrow(a))){
  a$cuti[i] <- get.cuti(a$station[i], a$year[i], 13)
}

allLengthsEnv <- left_join(allLengthsEnv, a)

#add sla averaged over past 13 days
a <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(station, year, latitude.round)), sla = NA))

for(i in seq(1:nrow(a))){
  a$sla[i] <- get.sla(a$station[i], a$year[i], 1)
}

allLengthsEnv <- left_join(allLengthsEnv, a)

#add spring moci
moci <- read_csv("data/CaliforniaMOCI_JFM1991-JAS2020.csv")#load MOCI data
moci <- rename(moci, year = Year)

a <- summarise(group_by_at(allLengthsEnv, vars(station, date, year, latitude)))
a$year <- as.numeric(a$year)

aN <- filter(a, latitude >=38)
aC <- filter(a, latitude >=34.5, latitude <38)
aS <- filter(a, latitude >=32, latitude <34.5)

krillList <- list(aN, aC, aS)
mociList <- list(moci[,1:4], moci[,c(1:3, 5)], moci[,c(1:3,6)])

for(i in 1:3){#get spring and winter moci values for each region
  b <- filter(mociList[[i]], Season == "AMJ")
  krillList[[i]] <- left_join(krillList[[i]], b, by = "year")
  krillList[[i]] <- rename(krillList[[i]], moci_spring = names(krillList[[i]])[7])
  krillList[[i]] <- select(krillList[[i]], station, date, year, latitude, moci_spring)
}

a <- bind_rows(krillList)#bind all regions into one df
a$year <- as.character(a$year)
allLengthsEnv <- left_join(allLengthsEnv, a)

#Create temp^2 variable
allLengthsEnv$temp_2nl <- allLengthsEnv$temp_2^2


#merge new columns with allLengths in a novel df
allLengthsEnv <- left_join(select(allLengths, station, species, sex, length, year, sites, region, latitude, shore, date), a)

#Scale parameters
allLengthsEnv$temp_2_z <- scale(allLengthsEnv$temp_2)
allLengthsEnv$temp_100_z <- scale(allLengthsEnv$temp_100)
allLengthsEnv$temp_2nl_z <- scale(allLengthsEnv$temp_2nl)
allLengthsEnv$sst_sd_z <- scale(allLengthsEnv$sst_sd)
allLengthsEnv$chla_z <- scale(allLengthsEnv$chla)
allLengthsEnv$beuti_z <- scale(allLengthsEnv$beuti)
allLengthsEnv$moci_spring_z <- scale(allLengthsEnv$moci_spring)
allLengthsEnv$sla_z <- scale(allLengthsEnv$sla)
allLengthsEnv$cuti_z <- scale(allLengthsEnv$cuti)

#save datafile

save(allLengthsEnv, file = "data/allLengthsEnv.rda")
ep <- filter(allLengthsEnv, species == "EP")
ts <- filter(allLengthsEnv, species == "TS")
nd <- filter(allLengthsEnv, species == "ND")
#Save length + environment dataset
save(allLengthsEnv, file = "data/allLengthsEnv.rda")
#Filter by species and save as .RDA
ep <- filter(allLengths, species == "EP")
ts <- filter(allLengths, species == "TS")
nd <- filter(allLengths, species == "ND")
save(ep, file = "data/allLengthsEP.rda")
save(ts, file = "data/allLengthsTS.rda")
save(nd, file = "data/allLengthsND.rda")
#FIX NA VALUES WHERE APPROPRIATE

#Script to work through rows with NA values for metadata or environmental predictors
#Begin with date and work to the right through columns of allLengthEnv columns

# Tue Jan 26 10:02:17 2021 ------------------------------

#LIBRARIES & SOURCES
#====
load("data/allLengthsEnv.rda")
#====

#SET UP
#====
ale.na <- allLengthsEnv[!complete.cases(allLengthsEnv[,]),]
str(ale.na)
#6,508 incomplete cases
View(ale.na)
#=====

#MISSING DATES
#====
#118(2011), 421(2012), 166(2011) missing everything from date onwards
#ASKED Keith Sakuma
# Fri Nov  6 10:21:59 2020 ------------------------------
#Stations are legacy, Keith sent metadata for these stations, added manually to URLs and metadata dfs. 
#====

#MISSING LAT LON
#====
x <- allLengths[is.na(allLengths$latitude),]
x <- summarize(group_by_at(x, vars(station, year)))
View(x)
#stations were missing in metadata file, added manually
#====

#MISSING SST
#====
#118, 421, 166
#load legacy ROMS data using positions provided by Keith
#=====

#MISSING SUBSURFACE TEMP
#====
#There were three stations with depth>100m that had missing subsurface temp values, 422, 421, and 453. All three were in areas of deep nearshore shelf, too close to shore for ROMS resolution. 
#====

#MISSING CHLOROPHYLL & SEA LEVEL ANOMALY
#====
#114, 167, 421, 170, 165 all had missing chla and sla values in various years. This was because no data was available within 13 days of sampling. 
#====



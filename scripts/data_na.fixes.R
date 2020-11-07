load("data/allLengthsEnv.rda")

ale.na <- allLengthsEnv[,-12][!complete.cases(allLengthsEnv[,-12]),]
str(ale.na)

#1,437 incomplete cases (not including temp_100 which has a real reason to be NA)

View(ale.na)

#PROBLEMS

#118(2011), 421(2012), 166(2011) missing everything from site onwards
View(filter(allLengthsEnv, station == 421, year ==2012))
#ASKED Keith
# Fri Nov  6 10:21:59 2020 ------------------------------

#110(2012), 454(2011)  missing date and chla
load("data/allLengths.rda")
#110(2012) = 2012-05-08
View(filter(allLengthsEnv, station == 454, year ==2011))
#drop haul 8, plenty of measured krill from haul 9
#454(2011) = 2011-05-04
#drop these (2) because they are apparently from haul 19, whereas all others are from haul 6
#problem solved by rerunning code
#fixed

#156(2012), 483(2012) missing date, chla, and temp_2/sst_sd
#problem solved by rerunning code...
#fixed

#453(2011, 2015-2017) missing temp_2 and sst_sd
View(filter(waterTemp, station == 453))
#might not be downloading data from server properly? tested this with virtual sensor URL
#fixed

#482(2018) missing chla
View(filter(allLengths, station == 482))
#looks like this data wasn't acquired, ask Chelle, date = 2018-05-08
# Fri Nov  6 14:38:39 2020 ------------------------------

#165(2012) chla is Nan
View(filter(env, station == 165))
#data was unavailable in the dataset
#no action required. 



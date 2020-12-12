#Load
#This script loads all the data that is required for analysis of krill lengths.
#All dates and times are in PDT

# Fri Dec 11 14:11:23 2020 ------------------------------

#LIBRARIES & SOURCES
#====
library(readr)
#====
#LOAD METADATA
#====
#Load station metadata (lats are from mean latitude of corresponding "area" in NMFS 2015 MWT dataset, except for San Mateo, which I estimated on google maps, and Monterey, which is an average value of all the MWT Monterey sites)
regions <- read_csv("data/regions.csv")
stations <- read.csv("data/stationMetadata.csv")


#====
#LOAD LENGTH DATA
#====
lengths <- read_csv("data/lengths.csv")
lengthsBaldo <- read_csv("data/lengthsBaldo.csv")
metadata <- read_csv("data/allStationMetadata.csv")

#====
#LOAD PHYSICAL DATA
#====
env <- read.csv("data/zoo_selgroups_HadSST_relabundance_5aug2019_plumchrusV_4regions_final_satsstall.csv")#remote sensing data
beuti <- read_csv("data/BEUTI_daily.csv")#Mike Jacox BEUTI data
cuti <- read_csv("data/cuti_daily.csv")#Mike Jacox CUTI data
moci <- read_csv("data/CaliforniaMOCI_JFM1991-JAS2020.csv")#Farallon Institute MOCI data



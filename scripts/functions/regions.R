#List of sites and corresponding stations
FortRoss <- c(183, 453, 454)
PointReyes <- c(166, 167, 170, 171)
GulfFarallons <- c(138, 139, 152, 156)
SanMateo <- c(131, 132, 134)
Davenport <- c(124, 127)
MontereyBay <- c(114, 112, 110, 117)
PiedrasBlancas <- c(442, 445)
MorroBay <- c(493, 495)
SantaBarbara <- c(422, 425)
MidChannelIslands <- c(411, 412, 413, 414)
SouthernCA <- c(481, 482, 402)

#All sites
allSites <- c(FortRoss, PointReyes, GulfFarallons, SanMateo, Davenport, MontereyBay, PiedrasBlancas, MorroBay, SantaBarbara, MidChannelIslands, SouthernCA)

#As a dataframe (lats are from mean latitude of corresponding "area" in MWT dataset, except for San Mateo, which I estimated on google maps, and Monterey, which is an average value of all the MWT Monterey sites)
regions <- data.frame(
  station = allSites,
  sites = c("FortRoss", "FortRoss", "FortRoss", "FortRoss", "PointReyes", "PointReyes", "PointReyes", "GulfFarallons", "GulfFarallons", "GulfFarallons", "GulfFarallons", "SanMateo", "SanMateo", "SanMateo", "Davenport", "Davenport", "MontereyBay", "MontereyBay", "MontereyBay", "MontereyBay", "PiedrasBlancas", "PiedrasBlancas", "MorroBay", "MorroBay", "MorroBay", "SantaBarbara", "SantaBarbara", "MidChannelIslands", "MidChannelIslands", "MidChannelIslands", "MidChannelIslands", "SouthernCA", "SouthernCA", "SouthernCA"),
  region = c("north", "north", "north", "north", "north", "north", "north", "north_central", "north_central", "north_central", "north_central", "north_central", "north_central", "north_central", "north_central", "north_central", "north_central", "north_central", "north_central", "north_central", "central", "central", "central", "central", "central", "south", "south", "south", "south", "south", "south", "south", "south", "south"),
  latitude = c(38.4667, 38.4667, 38.4667, 38.4667, 38.1667, 38.1667, 38.1667, 37.7061277456647, 37.7061277456647, 37.7061277456647, 37.7061277456647, 37.5137, 37.5137, 37.5137, 36.9833, 36.9833, 36.65131, 36.65131, 36.65131, 36.65131, 35.7033, 35.7033, 35, 35, 35, 34.15, 34.15, 33.54717, 33.54717, 33.54717, 33.54717, 32.87108, 32.87108, 32.87108)
)
regions$region <- factor(regions$region, levels = c("north", "north_central", "central", "south"))

#For each year 
sites2015 <- data.frame(
  site = c("FortRoss", "PiedrasBlancas", "MorroBay", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(453, 442, NA, 422, 411, 481),
  offshore = c(454, NA, 495, 425, 413, 402),
  region = c("north", "central", "central", "south", "south", "south")
)
allSites2015 <- c(sites2015$onshore, sites2015$offshore)

sites2016 <- data.frame(
  site = c("FortRoss", "PointReyes", "GulfFarallons", "SanMateo", "Davenport", "MontereyBay", "PiedrasBlancas", "MorroBay", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(453, 167, 138, 132, 124, 112, 442, 493, 422, 411, 482),
  offshore = c(454, 170, 152, 134, 127, 110, 445, 495, 425, 414, 402),
  region = c("north", "north", "northCentral", "northCentral", "northCentral", "northCentral", "central", "central", "south", "south", "south")
)
allSites2016 <- c(sites2016$onshore, sites2016$offshore)

sites2017 <- data.frame(
  site = c("FortRoss", "PointReyes", "GulfFarallons", "SanMateo", "Davenport", "MontereyBay", "PiedrasBlancas", "MorroBay", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(453, 167, 138, 132, 124, 112, 442, NA, 422, 412, 482),
  offshore = c(NA, 171, 156, 134, 127, 117, 445, 495, 425, NA, 402),
  region = c("north", "north", "northCentral", "northCentral", "northCentral", "northCentral", "central", "central", "south", "south", "south")
)
allSites2017 <- c(sites2017$onshore, sites2017$offshore)

sites2018 <- data.frame(
  site = c("FortRoss", "PointReyes", "GulfFarallons", "SanMateo", "Davenport", "MontereyBay", "PiedrasBlancas", "MorroBay", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(183, 167, 139, 132, 124, 114, 442, NA, 422, 411, 481),
  offshore = c(454, 170, 156, 134, 127, 110, 445, 495, 425, 413, 402),
  region = c("north", "north", "northCentral", "northCentral", "northCentral", "northCentral", "central", "central", "south", "south", "south")
)
allSites2018 <- c(sites2018$onshore, sites2018$offshore)


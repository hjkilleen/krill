#List of sites and corresponding stations
FortRoss <- c(183, 452, 453, 454)
PointReyes <- c(166, 167, 170)
GulfFarallons <- c(138, 139, 152, 156)
SanMateo <- c(131, 132, 134)
Davenport <- c(124, 127)
MontereyBay <- c(114, 112, 110, 117)
PointSur <- c(102, 105)
PiedrasBlancas <- c(442, 445)
MorroBay <- c(492, 493, 495)
PointConception <- c(433, 435)
SantaBarbara <- c(422, 425)
MidChannelIslands <- c(411, 412, 413, 414)
SouthernCA <- c(481, 482, 402)

#As a dataframe 
sites2015 <- data.frame(
  site = c("FortRoss", "PiedrasBlancas", "MorroBay", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(453, 442, NA, 422, 411, 481),
  offshore = c(454, NA, 495, 425, 413, 402),
  region = c("north", "central", "central", "south", "south", "south")
)

sites2016 <- data.frame(
  site = c("FortRoss", "PointReyes", "GulfFarallons", "SanMateo", "Davenport", "MontereyBay", "PiedrasBlancas", "MorroBay", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(453, 167, 138, 132, 124, 112, 442, 493, 422, 411, 482),
  offshore = c(454, 170, 152, 134, 127, 110, 445, 495, 425, 414, 402),
  region = c("north", "north", "northCentral", "northCentral", "northCentral", "northCentral", "central", "central", "south", "south", "south")
)

sites2017 <- data.frame(
  site = c("FortRoss", "PointReyes", "GulfFarallons", "SanMateo", "Davenport", "MontereyBay", "PiedrasBlancas", "MorroBay", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(453, 167, 138, 132, 124, 112, 442, NA, 422, 412, 482),
  offshore = c(NA, 171, 156, 134, 127, 117, 445, 495, 425, NA, 402),
  region = c("north", "north", "northCentral", "northCentral", "northCentral", "northCentral", "central", "central", "south", "south", "south")
)

sites2018 <- data.frame(
  site = c("FortRoss", "PointReyes", "GulfFarallons", "SanMateo", "Davenport", "MontereyBay", "PiedrasBlancas", "MorroBay", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(183, 167, 139, 132, 124, 114, 442, NA, 422, 411, 481),
  offshore = c(454, 170, 156, 134, 127, 110, 445, 495, 425, 413, 402),
  region = c("north", "north", "northCentral", "northCentral", "northCentral", "northCentral", "central", "central", "south", "south", "south")
)

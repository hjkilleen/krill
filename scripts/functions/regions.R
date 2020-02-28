#List of sites and corresponding stations
FortRoss <- c(452, 454)
PointReyes <- c(166, 170)
GulfFarallons <- c(139, 156)
SanMateo <- c(132, 134)
Davenport <- c(124, 127)
MontereyBay <- c(114, 110, 117)
PointSur <- c(102, 105)
PiedrasBlancas <- c(442, 445)
MorroBay <- c(492, 495)
PointConception <- c(433, 435)
SantaBarbara <- c(422, 425)
MidChannelIslands <- c(411, 414)
SouthernCA <- c(482, 402)

#As a dataframe 
sites <- data.frame(
  site = c("FortRoss", "PointReyes", "GulfFarallons", "SanMateo", "Davenport", "MontereyBay", "PointSur", "PiedrasBlancas", "MorroBay", "PointConception", "SantaBarbara", "MidChannelIslands", "SouthernCA"),
  onshore = c(452, 166, 139, 132, 124, 114, 102, 442, 492, 433, 422, 411, 482),
  offshore = c(454, 170, 156, 134, 127, 110, 105, 445, 495, 435, 425, 414, 402),
  region = c("north", "north", "northCentral", "northCentral", "northCentral", "northCentral", "northCentral", "central", "central", "central", "south", "south", "south")
)

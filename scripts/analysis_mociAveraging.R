#MOCI data
# Wed Nov 11 11:22:28 2020 ------------------------------

#LIBRARIES & SOURCES
library(readr)
library(lubridate)
library(reshape2)
library(stringr)
source("scripts/functions/length_frequency.R")

#set up 
moci <- read_csv("data/CaliforniaMOCI_JFM1991-JAS2020.csv")#load MOCI data
moci <- rename(moci, year = Year)
str(moci)

epn <- summarise(group_by_at(ep, vars(station, date, year, latitude)), length=mean(as.numeric(length)))
epn$year <- as.numeric(epn$year)

epnN <- filter(epn, latitude >=38)
epnC <- filter(epn, latitude >=34.5, latitude <38)
epnS <- filter(epn, latitude >=32, latitude <34.5)

epList <- list(epnN, epnC, epnS)
mociList <- list(moci[,1:4], moci[,c(1:3, 5)], moci[,c(1:3,6)])

for(i in 1:3){#get spring and winter moci values for each region
  a <- filter(mociList[[i]], Season == "JFM")
  epList[[i]] <- left_join(epList[[i]], a, by = "year")
  epList[[i]] <- rename(epList[[i]], moci_winter = names(epList[[i]])[8])
  a <- filter(mociList[[i]], Season == "AMJ")
  epList[[i]] <- left_join(epList[[i]], a, by = "year")
  epList[[i]] <- rename(epList[[i]], moci_spring = names(epList[[i]])[11])
  epList[[i]] <- select(epList[[i]], station, date, year, latitude, length, moci_winter, moci_spring)
}

epn <- bind_rows(epList)#bind all regions into one df
epn$moci_ave <- (epn$moci_winter+epn$moci_spring)/2

#EP length~moci relationship
winter.mod <- lm(length~moci_winter, epn)
spring.mod <- lm(length~moci_spring, epn)
ave.mod <- lm(length~moci_ave, epn)

summary(winter.mod)
summary(spring.mod)
summary(ave.mod)
#Spring moci has the highest predictive power

#Best fit plot
ggplot(epn, aes(x = moci_spring, y = length)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle("Best MOCI fit") + 
  ggsave("output/mociAveragingBestFit.jpg")

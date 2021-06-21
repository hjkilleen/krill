#Plot of SST std. vs. CUTI std.
# Wed Jun  2 11:42:44 2021 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/data_tidy_environment.R")
library(lubridate)
library(tidyverse)
library(reshape2)
library(ggpmisc)
load("data/allLengthsEnv.rda")
#====

#SET UP
#====
a <- as.data.frame(summarize(group_by_at(allLengthsEnv, vars(station, year, latitude, latitude.round))))#list of stations and years
sentinels <- c(453, 454, 167, 171, 139, 152, 132, 134, 124, 127, 114, 110, 442, 445, 493, 495, 422, 425, 411, 414, 481, 402)#stations used to get domain-wide averages
a <- filter(a, station %in% sentinels)

get.cuti.sd <- function(x, y, z) {
  end.date <- filter(allLengthsEnv, station == x, year == y)$date[1]
  lat <- filter(allLengthsEnv, station == x, year == y)$latitude.round[1]
  start.date <- end.date-z
  cuti.val <- sd(filter(cuti, latitude.round == lat, year == y, date >= start.date, date <= end.date)$cuti)
  cuti.val
}

a$cuti.sd <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))){
  a$cuti.sd[i] <- get.cuti.sd(a$station[i], a$year[i], 30)
}

x <- summarize(group_by_at(env, vars(time64, station)), chla.sd = sd(chlor_a, na.rm = TRUE))#create chla dataframe
x$year <- as.character(paste("20", stringr::str_sub(x$time64, -2, -1), sep = ""))
a$chla.sd <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))){
  a$chla.sd[i] <- filter(x, station == a$station[i], year == a$year[i])$chla.sd
}

get.sst.sd <- function(x, y, z) {
  end.date <- filter(allLengthsEnv, station == x, year == y)$date[1]
  start.date <- end.date-z
  sst_sd <- sd(filter(waterTemp, station == x, year == y, date >= start.date, date <= end.date)$temp_2)
  sst_sd
}

a$sst.sd <- rep(NA, nrow(a))
for(i in seq(1:nrow(a))) {
  a$sst.sd[i] <- get.sst.sd(a$station[i], a$year[i], 30)
}
#====

#PLOT
#====
ggplot(a, aes(x = cuti.sd, y = sst.sd))+
  geom_point(aes(color = year), size = 2, alpha = 0.5) + 
  geom_smooth(method = 'lm', color = "black") +
  stat_poly_eq(formula = a$cuti.sd ~ a$sst.sd, aes(x = cuti.sd, y = sst.sd, label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
  scale_color_brewer(palette = "Dark2") + 
  labs(x = "CUTI Std. Dev.", y = "SST Std. Dev.", color = "Year") + 
  theme_classic(base_size = 20) +
  ggsave("figures/manuscript/S12_upwellingIntermittence.jpg", dpi = 300)

ggplot(a, aes(x = chla.sd, y = sst.sd))+
  geom_point(aes(color = year), size = 2, alpha = 0.5) + 
  geom_smooth(method = 'lm', color = "black") +
  stat_poly_eq(formula = a$chla.sd ~ a$sst.sd, aes(x = chla.sd, y = sst.sd, label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
  scale_color_brewer(palette = "Dark2") + 
  labs(x = "Chl-a Std. Dev.", y = "SST Std. Dev.", color = "Year") + 
  theme_classic(base_size = 20)
#====

#MODEL
#====
mod <- lm(cuti.sd ~ sst.sd, a)
summary(mod)
#====

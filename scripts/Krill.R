# Sun May 12 21:02:38 2019 ------------------------------

#dependencies
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

#Krill Reporting Plots

#load Krill Counts

krill <- read_xlsx("../CODING/krill/data/Krillcounts.xlsx", sheet = 2)
str(krill)

#load station list

stations <- read.csv(file = '../CODING/krill/data/RF_survey_data.csv', header = TRUE)
stations <- stations[2:nrow(stations), ]
stations$latitude <- as.numeric(as.character(stations$latitude))
stations$longitude <- as.numeric(as.character(stations$longitude))
stations <- stations %>% dplyr::filter(common_name == "KRILL, TOTAL")
# stations$time[which(is.na(stations$time) == FALSE)] <- as.POSIXct(stations$time[which(is.na(stations$time) == FALSE)])
stations$time <- as.POSIXct(stations$time)

#samples processing progress
NROW(unique(krill$Sample))


# This matching should be used if we have only the lat/lons from one year and want to estimate lat/lon of the stations (by taking the mean lat/lon of the hauls from that year at that station)
stations <- stations %>% select(station, latitude, longitude) %>% group_by(station) %>% summarize_all(.funs = mean, na.rm = TRUE)
krill <- left_join(krill, stations %>% select(latitude, longitude, station),
                   by = c("Station" = "station"))

krill %>% dplyr::filter(is.na(latitude)) %>% select(Station) %>% arrange(Station)
# This matching should be used once we have the "stations" file that includes the lat/lon for every single haul across every single year that are represented in the "krill" file. 
# krill <- left_join(krill, stations %>% select(latitude, longitude, time, cruise, haul_no, station) %>% mutate(Year = as.numeric(format(time, "%Y"))) %>% select(-time),
# 	by = c("Year", "Cruise" = "cruise", "Haul" = "haul_no", "Station" = "station"))

# QA Check
krill %>% group_by(Year) %>% select(Year, Station, latitude) %>% summarize(n_Stations = length(unique(Station)), n_Lats = length(unique(latitude)))
# ===================================================================================================
# Plot Krill Distributions
# ===================================================================================================
pdf('../CODING/krill/figures//Krill_totals_overall.pdf', width = 11, height = 8.5)
for(i in 1:length(unique(krill$Species))){
  plot.i <- ggplot(krill %>% dplyr::filter(Species == unique(krill$Species)[i])) +
    geom_point(aes(x = longitude, y = latitude, size = Total), fill = "grey90", color = "black", shape = 21) +
    geom_text_repel(data = krill %>% dplyr::filter(Species == unique(krill$Species)[i]), aes(x = longitude - 1, y = latitude, label = round(Sex.Ratio,1)), size = 2, angle = 0, direction = "x", min.segment.length = unit(10, "lines")) +
    geom_point(data = Wseaboard.df[runif(20000, min = 1, max = nrow(Wseaboard.df)),] %>% dplyr::filter(lat < 42.5), aes(x = long, y = lat), size = 0.1) + # map
    geom_point(data = data.frame(long = seq(-117, -126, length.out = 10), lat = seq(32.5, 42.5, length.out = 10)), aes(x = long, y = lat), size = 0.001, color = "white") + # fake map to manipulate borders
    facet_wrap(Year ~., nrow = 1) +
    ggtitle(paste0(unique(krill$Species)[i])) + ylab("latitude") +
    coord_equal() + theme_bw() + guides(size = guide_legend(title = "Density (per haul)")) +
    theme(legend.position = c(0.9, 0.7), legend.background = element_rect(size = 0.1, color = "black"), plot.title = element_text(margin = margin(t = 10)))
  print(plot.i)
}
dev.off()


pdf('/Users/connor/Documents/Graduate School/Dibble_Research/Krill/Figures/Krill_totals_by_sex.pdf', width = 11, height = 8.5)
for(i in 1:length(unique(krill$Species))){
  plot.i <- ggplot(krill %>% dplyr::filter(Species == unique(krill$Species)[i]) %>% select(Year, Species, Haul, Station, latitude, longitude, Males.Total, Females.Total, Sex.Ratio) %>% melt(id.vars = c("Year", "Haul", "Species", "Sex.Ratio", "Station", "latitude", "longitude"))) +
    geom_point(aes(x = longitude, y = jitter(latitude, amount = 0.05), size = value, alpha = 0.8, fill = variable), color = 'black', shape = 21) + # krill
    geom_text_repel(data = krill %>% dplyr::filter(Species == unique(krill$Species)[i]), aes(x = longitude - 1, y = latitude, label = round(Sex.Ratio,1)), size = 2, angle = 0, direction = "x", min.segment.length = unit(10, "lines")) +
    # geom_text(aes(x = longitude - 1, y = latitude, label = round(Sex.Ratio,1)), size = 2, angle = 45) +
    geom_point(data = Wseaboard.df[runif(20000, min = 1, max = nrow(Wseaboard.df)),] %>% dplyr::filter(lat < 42.5), aes(x = long, y = lat), size = 0.1) + # map
    geom_point(data = data.frame(long = seq(-117, -126, length.out = 10), lat = seq(32.5, 42.5, length.out = 10)), aes(x = long, y = lat), size = 0.001, color = "white") + # fake map to manipulate borders
    facet_wrap(Year ~., nrow = 1) +
    ggtitle(paste0(unique(krill$Species)[i])) + ylab("latitude") +
    coord_equal() + theme_bw() + guides(size = guide_legend(title = "Density (per haul)"), fill = guide_legend(title = "Sex"), alpha = FALSE) +
    scale_fill_viridis(discrete = TRUE) +
    theme(legend.position = c(0.9, 0.68), legend.background = element_rect(size = 0.1, color = "black"), plot.title = element_text(margin = margin(t = 10)))
  print(plot.i)
}
dev.off()
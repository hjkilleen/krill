#Not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#Plot histograms from length frequency data
plotHist <- function(j, i = paste(j$station[1], j$species[1], j$year, sep = " ")) {
  ggplot(j, aes(x = length, color = sex)) + 
    geom_histogram(fill = "white", alpha = 0.5, position = "identity") +
    ggtitle(i) + 
    xlim(5, 35) +
    theme(plot.title = element_text(hjust = 0.5))
}

#Plot a grid of histograms
histGrid <- function(i, j) {
  d <- group_by_at(i, vars(station, species, year))
  dl <- group_split(d)
  pd <- lapply(dl, plotHist)
  plots <- do.call("grid.arrange", c(pd, ncol = 2, top = print(j)))
}

#Determine number of individuals for a given station, species, and sex in all years
numSamp <- function(i, data = lengths) {
  s <- filter(lengths, station == i)
  if(nrow(s) == 0) stop('no observations for this station')
  st <- group_by_at(s, vars(year, species, sex))
  result <- as.data.frame(summarize(st, n=n()))
  result <- result[order(result$year, result$species, result$sex),]
  result
  }

#Plot a map of CA with station locactions
mapStations <- function(i, font_size = 16) {
  ggplot(data = world) +
    geom_sf(color = "black", fill = "bisque2") +
    geom_sf(data = states, fill = NA) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    geom_point(data = i, aes(x = lon, y = lat), size = 2, 
               shape = 23, fill = "darkred") +
    geom_label(data = i, aes(x = lon, y = lat, label = station), nudge_y = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-125.5, -116.75), ylim = c(32.0, 42.30), expand = FALSE) + 
    theme(text = element_text(size = font_size))    
}

#Calculate the coefficient of variation as a percent value
cv <- function(i) {
  (sd(i, na.rm = TRUE)/mean(i, na.rm = TRUE))*100
}

#Get temperature data over recent intervals
get.temp.2 <- function(x, y, z) {
  end.date <- filter(allLengthsEnv, station == x, year == y)$date[1]
  start.date <- end.date-z
  temp_2_mean <- mean(dplyr::filter(waterTemp, station == x, year == y, date >= start.date, date <= end.date)$temp_2)
  temp_2_mean
}
get.temp.100 <- function(x, y, z) {
  end.date <- filter(allLengthsEnv, station == x, year == y)$date[1]
  start.date <- end.date-z
  temp_100_mean <- mean(dplyr::filter(waterTemp, station == x, year == y, date >= start.date, date <= end.date)$temp_100)
  temp_100_mean
}

#Get SST standard deviation
get.sd <- function(x, y, z) {
  end.date <- filter(allLengthsEnv, station == x, year == y)$date[1]
  start.date <- end.date-z
  sst_sd <- sd(filter(waterTemp, station == x, year == y, date >= start.date, date <= end.date)$temp_2)
  sst_sd
}

#Get chlA
get.chla <- function(x, y, z) {
  end.date <- 0
  start.date <- end.date-z
  chla_mean <- mean(filter(env, station == x, year == y, dtime >= start.date, dtime <= end.date)$chlor_a, na.rm = TRUE)
  chla_log <- log(chla_mean)#log transform chlorophyll to make distribution normal
  chla_log
}

#Get beuti 
get.beuti <- function(x, y, z) {
  end.date <- filter(allLengthsEnv, station == x, year == y)$date[1]
  lat <- filter(allLengthsEnv, station == x, year == y)$latitude.round[1]
  start.date <- end.date-z
  beuti.val <- mean(filter(beuti, latitude.round == lat, year == y, date >= start.date, date <= end.date)$beuti)
  beuti.val
}

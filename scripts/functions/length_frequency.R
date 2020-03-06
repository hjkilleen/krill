#Plot histograms from length frequency data
plotHist <- function(j) {
  ggplot(j, aes(x = length, color = sex)) + 
    geom_histogram(fill = "white", alpha = 0.5, position = "identity") +
    ggtitle(paste(j$station[1], j$species[1], j$year, sep = " ")) + 
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

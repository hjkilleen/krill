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

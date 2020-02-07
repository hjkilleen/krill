#Plot histograms from length frequency data
plotHist <- function(j) {
  ggplot(j, aes(x = length, color = sex)) + 
    geom_histogram(fill = "white", alpha = 0.5, position = "identity") +
    ggtitle(paste(j$station[1], j$species[1], sep = " ")) + 
    xlim(5, 35) +
    theme(plot.title = element_text(hjust = 0.5))
}

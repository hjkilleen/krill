#Supplemental figure 10
#SST Variance by Site

#LBRARIES & SOURCES
#====
library(ggplot2)
library(tidyverse)
load("data/allLengthsEnv.rda")
#====

#PLOT
#====
df <- summarize(group_by_at(allLengthsEnv, vars(year, region)), sst_sd = mean(sst_sd))
df$region <- factor(df$region, levels = c("north", "north_central", "central", "south"))

df2 <- summarize(group_by(allLengthsEnv, year), sst_sd = mean(sst_sd))

ggplot(df, aes(x = year, y = (sst_sd*attr(allLengthsEnv$sst_sd, "scaled:scale")+attr(allLengthsEnv$sst_sd, "scaled:center")))) + 
  geom_line(data = df2, aes(group = 1), size = 2) + 
  geom_point(aes(shape = region), size = 4, color = "darkgrey") +
  scale_color_brewer(palette = "Accent", labels = c("North", "North Central", "Central", "South")) + 
  labs(y = "SST Variance", color = "Region") + 
  theme_classic(base_size = 20) +
  theme(legend.position = "top", legend.direction = "horizontal", legend.text = element_text(size = 15), legend.title = element_text(size = 20), axis.title.x = element_blank())
#====
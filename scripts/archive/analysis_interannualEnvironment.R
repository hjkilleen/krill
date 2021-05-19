str(allLengthsEnv)
library(tidyverse)
load("data/allLengthsEnv.rda")
df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), temp_2 = (mean(temp_2)*attr(allLengthsEnv$temp_2, "scaled:scale")+attr(allLengthsEnv$temp_2, "scaled:center")))
temp_2.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = temp_2)) +
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), temp_100 = (mean(temp_100)*attr(allLengthsEnv$temp_100, "scaled:scale")+attr(allLengthsEnv$temp_100, "scaled:center")))
temp_100.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = temp_100)) +
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), sst_sd = mean(sst_sd))
c <- ggplot(df) + 
  geom_point(aes(x = year, y = sst_sd, color =sites))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), chla = (mean(chla)*attr(allLengthsEnv$chla, "scaled:scale")+attr(allLengthsEnv$chla, "scaled:center")))
chla.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = chla))+
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), moci_spring = (mean(moci_spring)*attr(allLengthsEnv$moci_spring, "scaled:scale")+attr(allLengthsEnv$moci_spring, "scaled:center")))
moci.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = moci_spring))+
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites, region)), cuti = (mean(cuti)*attr(allLengthsEnv$cuti, "scaled:scale")+attr(allLengthsEnv$cuti, "scaled:center")))
cuti.sum <- ggplot(df) + 
  geom_boxplot(aes(x = year, y = cuti)) +
  labs(x = "Year") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank(), plot.margin=unit(c(1,1,1,-2), "cm"), axis.text.x = element_text(size = 15))

gridExtra::grid.arrange(a, b, c, d, g, h, ncol = 2, nrow = 4)

i <- sjPlot::plot_model(Mnl5)
j <- sjPlot::plot_model(Mtl2)
k <- sjPlot::plot_model(Mel2)
gridExtra::grid.arrange(i, j, k, ncol = 1, nrow = 3)

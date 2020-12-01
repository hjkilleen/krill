str(allLengthsEnv)

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), temp_2 = mean(temp_2))
a <- ggplot(df) + 
  geom_point(aes(x = year, y = temp_2, color =sites))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), temp_100 = mean(temp_100))
b <- ggplot(df) + 
  geom_point(aes(x = year, y = temp_100, color =sites))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), sst_sd = mean(sst_sd))
c <- ggplot(df) + 
  geom_point(aes(x = year, y = sst_sd, color =sites))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), chla = mean(chla))
d <- ggplot(df) + 
  geom_point(aes(x = year, y = chla, color =sites))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), beuti = mean(beuti))
e <- ggplot(df) + 
  geom_point(aes(x = year, y = beuti, color =sites))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), sla = mean(sla))
f <- ggplot(df) + 
  geom_point(aes(x = year, y = sla, color =sites))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), moci_spring = mean(moci_spring))
g <- ggplot(df) + 
  geom_point(aes(x = year, y = moci_spring, color =sites))

df <- summarize(group_by_at(allLengthsEnv, vars(year, sites)), cuti = mean(cuti))
h <- ggplot(df) + 
  geom_point(aes(x = year, y = cuti, color =sites))

gridExtra::grid.arrange(a, b, c, d, e, f, g, h, ncol = 2, nrow = 4)

i <- sjPlot::plot_model(Mnl5)
j <- sjPlot::plot_model(Mtl2)
k <- sjPlot::plot_model(Mel2)
gridExtra::grid.arrange(i, j, k, ncol = 1, nrow = 3)

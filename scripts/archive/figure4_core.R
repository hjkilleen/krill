#Core region plots removed from Figure 4

#Core Region
cc <- ggplot(cuti.core, aes(x = date, y = cuti_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(cuti_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  ylim(-1, 2.5) +
  labs(y = "CUTI", title = "North Central Region") +
  theme_classic(base_size = 20) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
#SST time series
ylab <- "SST (°C)"
sstc <- ggplot(sst.core, aes(x = date, y = sst_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sst_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  ylim(10, 18.5) +
  labs(y = ylab) +
  theme_classic(base_size = 20) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
#Subsurface time series
ylab <- "Subsurface\ntemperature (°C)"
subc <- ggplot(sub.core, aes(x = date, y = sub_mean)) +
  geom_line(color = "grey") + 
  geom_line(aes(y = rollmean(sub_mean, 30, na.pad = TRUE)), color = "black") +
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  ylim(7, 12) +
  labs(y = ylab) +
  theme_classic(base_size = 20) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15))
#MOCI time series
mocic <- ggplot(moci.core, aes(x = time, y = moci_mean)) +
  geom_line() + 
  geom_rect(data = cruises, inherit.aes = FALSE, aes(xmin = start.x, ymin = min.y, xmax = end.x, ymax = max.y), fill = "blue", alpha = 0.3) +
  ylim(-10, 12) +
  labs(x = "Date", y = "MOCI") + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank())
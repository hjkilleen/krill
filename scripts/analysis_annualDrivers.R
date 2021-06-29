ic.noSex$year <- row.names(ic.noSex)

cols <-c("#E69F00", "#009E73", "#56B4E9", "#D55E00", "#0072B2", "#F0E442", "#CC79A7")#color palette

#Euphausia pacfica
epsum <- summarize(group_by_at(ep, vars(station, year)), temp_2 = mean(temp_2), chla = mean(chla), cuti = mean(cuti))
epsum <- left_join(epsum, ic.noSex[,c(1,4)])
epsum$est <- epsum$`E. pacifica`

summary(lm(est~temp_2 + chla + cuti, epsum))

ep.sst.plot <- ggplot(epsum, aes(x = temp_2*attr(ep$temp_2, "scaled:scale") + attr(ep$temp_2, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = "\n\nmean length (mm)", title = "SST (°C)") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

ep.cuti.plot <- ggplot(epsum, aes(x = cuti*attr(ep$cuti, "scaled:scale") + attr(ep$cuti, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = " ", title = "CUTI") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

ep.chla.plot <- ggplot(epsum, aes(x = chla*attr(ep$chla, "scaled:scale") + attr(ep$chla, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = " ", title = expression(paste("Chl-a (log mg ", m^-3, ")"))) + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Thysanoessa spinifera
tssum <- summarize(group_by_at(ts, vars(station, year)), temp_2 = mean(temp_2), chla = mean(chla), cuti = mean(cuti))
tssum <- left_join(tssum, ic.noSex[,c(2,4)])
tssum$est <- tssum$`T. spinifera`

summary(lm(est~temp_2 + chla + cuti, tssum))

ts.sst.plot <- ggplot(tssum, aes(x = temp_2*attr(ts$temp_2, "scaled:scale") + attr(ts$temp_2, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = "\n\nmean length (mm)", title = " ") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

ts.cuti.plot <- ggplot(tssum, aes(x = cuti*attr(ts$cuti, "scaled:scale") + attr(ts$cuti, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = " ", title = " ") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

ts.chla.plot <- ggplot(tssum, aes(x = chla*attr(ts$chla, "scaled:scale") + attr(ts$chla, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = " ") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Nematoscelis difficilis
ndsum <- summarize(group_by_at(nd, vars(station, year)), temp_2 = mean(temp_2), chla = mean(chla), cuti = mean(cuti))
ndsum <- left_join(ndsum, ic.noSex[4:7,c(3,4)])
ndsum$est <- ndsum$`N. difficilis`

summary(lm(est~temp_2 + chla + cuti, ndsum))

nd.sst.plot <- ggplot(ndsum, aes(x = temp_2*attr(nd$temp_2, "scaled:scale") + attr(nd$temp_2, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = "\n\nmean length (mm)") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

nd.cuti.plot <- ggplot(ndsum, aes(x = cuti*attr(nd$cuti, "scaled:scale") + attr(nd$cuti, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = " ") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

nd.chla.plot <- ggplot(ndsum, aes(x = chla*attr(nd$chla, "scaled:scale") + attr(nd$chla, "scaled:center"), y = est, group = year)) + 
  geom_boxplot(aes(color = year)) + 
  scale_color_manual(values = cols) + 
  labs(x = " ", y = " ") + 
  theme_classic(base_size = 20) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Merge
ggarrange(ep.sst.plot, ep.cuti.plot, ep.chla.plot, ts.sst.plot, ts.cuti.plot, ts.chla.plot, nd.sst.plot, nd.cuti.plot, nd.chla.plot, ncol = 3, nrow = 3)

#AFS Figures
#Figures for presentation given at the American Fisheries Society Annual Meeting in September 2020

# Thu Aug 13 19:30:48 2020 ------------------------------

#Figure 1a - E. pacfica lengths by year
ggplot(filter(ep), aes(x = year, y = length, fill = year)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values = c("#ffffff80", "#00000080", "#ffffff50", "#ff000080", "#00ff0080", "#0000ff80", "#ffff0080"), labels = c("2011", "2012", "2013", "2015", "2016", "2017", "2018")) +
  labs(x = "Year", y = "Length (mm)", title = "E. pacifica lengths by year") + 
  theme(text = element_text(size = 20)) +
  ggsave("figures/bodySize/EP/byYear.jpg", width = 7, height = 9)

#Figure 1b - T. spinifera lengths by year
ggplot(filter(ts), aes(x = year, y = length, fill = year)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values = c("#ffffff80", "#00000080", "#ffffff50", "#ff000080", "#00ff0080", "#0000ff80", "#ffff0080"), labels = c("2011", "2012", "2013", "2015", "2016", "2017", "2018")) +
  labs(x = "Year", y = "Length (mm)", title = "T. spinifera lengths by year") + 
  theme(text = element_text(size = 20)) +
  ggsave("figures/bodySize/TS/byYear.jpg", width = 7, height = 9)

#Figure 1c - N. difficilis lengths by year
ggplot(filter(nd), aes(x = year, y = length, fill = year)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values = c("#ffffff80", "#00000080", "#ffffff50", "#ff000080", "#00ff0080", "#0000ff80", "#ffff0080"), labels = c("2011", "2012", "2013", "2015", "2016", "2017", "2018")) +
  labs(x = "Year", y = "Length (mm)", title = "N. difficilis lengths by year") + 
  theme(text = element_text(size = 20)) +
  ggsave("figures/bodySize/ND/byYear.jpg", width = 7, height = 9)

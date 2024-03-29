#Supplemental figure 2
#Environmental predictor averaging

load("output/sstAveraging.rda")
load("output/temp100Averaging.rda")
load("output/chlaAveraging.rda")
load("output/sstSdAveraging.rda")
load("output/chlaAveraging.rda")
library(ggpubr)

jpeg("figures/manuscript/S2_averaging.jpeg", width = 12, height = 10.5, units = 'in', res = 300)
ggarrange(a, b, c, d, e, ncol = 2, nrow = 3, align = "v", labels = c("A", "B", "C", "D", "E"), font.label = list(size = 20))
dev.off()

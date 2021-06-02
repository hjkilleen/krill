#Figure 6
#Environmental model random effects

# Fri Jan 29 12:40:17 2021 ------------------------------

#LIBRARIES
#====
library(tidyverse)
library(ggpmisc)
library(ggpubr)
library(gridExtra)
library(lme4)
load("output/environmentalEP.rda")
load("output/environmentalTS.rda")
load("output/environmentalND.rda")
load("output/environmentalPooled.rda")
#====

#SETUP
#====
regions <- read_csv("data/regions.csv")#load regions script

#Euphausia pacifica random effects dataframe
epGroups <- data.frame(
  station = as.numeric(row.names(ranef(epm)$station)),
  intercept = ranef(epm)$station$'(Intercept)',
  coefficient = ranef(epm)$station$'temp_2'
)
epGroups <- left_join(epGroups, regions)

#Thysanoessa spinifera random effects dataframe
tsGroups <- data.frame(
  station = as.numeric(row.names(ranef(tsm)$station)),
  intercept = ranef(tsm)$station$'(Intercept)',
  coefficient = ranef(tsm)$station$'temp_2'
)
tsGroups <- left_join(tsGroups, regions)

#Nematocelis difficilis random effects dataframe
ndGroups <- data.frame(
  station = as.numeric(row.names(ranef(ndm)$station)),
  intercept = ranef(ndm)$station$'(Intercept)'
)
ndGroups <- left_join(ndGroups, regions)
#====

#INTERCEPT PLOTS
#====
summary(lm(intercept~latitude, epGroups))#insignificant
epi <- ggplot(epGroups) +#EP intercept plot
  geom_point(aes(x = latitude, y = intercept, shape = shore), size = 4) + 
  #geom_smooth(aes(x = latitude, y = intercept), method = 'lm', se = FALSE, color = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = 0)) + 
  labs(title = "E. pacifica", x = "", y = "Intercept") + 
  scale_shape_manual(values = c(16, 17), labels = c("Offshore", "Onshore")) + 
  geom_rect(aes(xmin = 36.6, ymin = -Inf, xmax = 37.8, ymax = Inf), fill = "grey", alpha = .01) +
  theme_classic(base_size = 20) 
epi <- epi + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "italic"))

summary(lm(intercept~latitude, tsGroups))#significant, p<.05
tsi <- ggplot(tsGroups) +#TS intercept plot
  geom_point(aes(x = latitude, y = intercept, shape = shore), size = 4) + 
  #geom_smooth(aes(x = latitude, y = intercept), method = 'lm', se = FALSE, color = "blue", linetype = "dashed") +
  #stat_poly_eq(formula = tsGroups$intercept ~ tsGroups$latitude, aes(x = latitude, y = intercept, label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, eq.with.lhs = "bold(hat(L))~'='~", eq.x.rhs = "~bold(lat)", label.x.npc = "right") + 
  geom_hline(aes(yintercept = 0)) + 
  labs(title = "T. spinifera", x = "") + 
  scale_shape_manual(values = c(16, 17), labels = c("Offshore", "Onshore")) + 
  geom_rect(aes(xmin = 36.6, ymin = -Inf, xmax = 37.8, ymax = Inf), fill = "grey", alpha = .01) +
  theme_classic(base_size = 20) 
tsi <- tsi + theme(legend.position = "none", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5, face = "italic"))

summary(lm(intercept~latitude, ndGroups))#insignificant
ndi <- ggplot(ndGroups) +#ND intercept plot
  geom_point(aes(x = latitude, y = intercept, shape = shore), size = 4) + 
  #geom_smooth(aes(x = latitude, y = intercept), method = 'lm', se = FALSE, color = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = 0)) + 
  labs(title = "N. difficilis", x = "Latitude", y = "Intercept") + 
  scale_shape_manual(values = c(16, 17), labels = c("Offshore", "Onshore")) + 
  geom_rect(aes(xmin = 36.6, ymin = -Inf, xmax = 37.8, ymax = Inf), fill = "grey", alpha = .03) +
  theme_classic(base_size = 20) 
ndi <- ndi + theme(legend.position = "none", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5, face = "italic"))
#====

#SLOPE PLOTS
#====
summary(lm(coefficient~latitude, epGroups))#insignificant
eps <- ggplot(epGroups) +#EP slope plot
  geom_point(aes(x = latitude, y = coefficient, shape = shore), size = 4) + 
  #geom_smooth(aes(x = latitude, y = coefficient), method = 'lm', se = FALSE, color = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Latitude", y = "SST Slope\nCoefficient") + 
  scale_shape_manual(values = c(16, 17), labels = c("Offshore", "Onshore")) + 
  geom_rect(aes(xmin = 36.6, ymin = -Inf, xmax = 37.8, ymax = Inf), fill = "grey", alpha = .01) +
  theme_classic(base_size = 20)
eps <- eps + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.text = element_text(size = 20))

summary(lm(coefficient~latitude, tsGroups))#insignificant
tss <- ggplot(tsGroups) +#TS intercept plot
  geom_point(aes(x = latitude, y = coefficient, shape = shore), size = 4) + 
  #geom_smooth(aes(x = latitude, y = coefficient), method = 'lm', se = FALSE, color = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Latitude", y = "SST\nCoefficient") + 
  geom_rect(aes(xmin = 36.6, ymin = -Inf, xmax = 37.8, ymax = Inf), fill = "grey", alpha = .01) +
  scale_shape_manual(values = c(16, 17), labels = c("Offshore", "Onshore")) + 
  theme_classic(base_size = 20) 
tss <- tss + theme(legend.position = "none", axis.title.y = element_blank())
#====

#MERGE FIGURES
#====
get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mp.legend <- get_legend(eps)

panels <- ggarrange(epi, tsi, ndi, eps + theme(legend.position = "none"), tss, ncol = 3, nrow = 2, align = "v", labels = c("A", "C", "E", "B", "D"), font.label = list(size = 20))#multipanel plot

jpeg("figures/manuscript/figure6_randomEffects.jpeg", units = "in", width = 10, height = 7, res = 400)
grid.arrange(mp.legend, panels, heights = c(1, 10))#multipanel plot with legend
dev.off()
#====
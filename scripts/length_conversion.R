#Load data
library(readr)
conv <- read_csv("data/lengthConversion.csv")
str(conv)

#Derive
library(dplyr)
conv <- conv %>% 
  mutate(length_mm = length/scale, sl2_mm = sl2/scale)

#Linear model
ep <- lm(sl2_mm ~ length_mm, data = filter(conv, species == "EP"))
ts <- lm(sl2_mm ~ length_mm, data = filter(conv, species == "TS"))
nd <- lm(sl2_mm ~ length_mm, data = filter(conv, species == "ND"))

#Plots
library(ggplot2)
#EP
ggplot(filter(conv, species == "EP"), aes(x = length_mm, y = sl2_mm)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  annotate("text", x = 15, y = 24, label = paste(coef(ep)[1], "+", coef(ep)[2], "x", sep = " ")) + 
  labs(x = "Our measured length (mm)", y = "SL2 (mm)", title = "Euphausia pacifica")

#TS
ggplot(filter(conv, species == "TS"), aes(x = length_mm, y = sl2_mm)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  annotate("text", x = 22, y = 26, label = paste(coef(ts)[1], "+", coef(ts)[2], "x", sep = " ")) + 
  labs(x = "Our measured length (mm)", y = "SL2 (mm)", title = "Thysanoessa spinifera")
  
#ND
ggplot(filter(conv, species == "ND"), aes(x = length_mm, y = sl2_mm)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  annotate("text", x = 20, y = 24, label = paste(coef(nd)[1], "+", coef(nd)[2], "x", sep = " ")) + 
  labs(x = "Our measured length (mm)", y = "SL2 (mm)", title = "Nematocelis difficilis")

#Formulae
library(formattable)
formulae <- data.frame(
  species = c("EP", "TS", "ND"), 
  intercept = c(coef(ep)[1], coef(ts)[1], coef(nd)[1]),
  slope = c(coef(ep)[2], coef(ts)[2], coef(nd)[2])
)
formattable(formulae)

#Table 3
#Heatwave Model Summary
# Mon Jun 28 17:29:56 2021 ------------------------------

#LIBRARIES
#====
library(readxl)
library(knitr)
library(kableExtra)
library(tikzDevice)
#====

#SET UP
#====
df <- read_xlsx("data/heatwaveModelSummary_.xlsx")#load table
#====

#TABLE
#====
df %>% #knit table
  kbl() %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, "Female" = 3, "Male" = 3, "Female" = 3, "Male" = 3,"Female" = 3, "Male" = 3)) %>% 
  add_header_above(c(" " = 1, "Euphausia pacifica" = 6, "Thysanoessa spinifera" = 6, "Nematocelis difficilis" = 6)) %>% 
  row_spec(8, bold = TRUE) %>% 
  column_spec(c(1, 4, 7, 10, 13, 16, 19), border_right = TRUE)
#====
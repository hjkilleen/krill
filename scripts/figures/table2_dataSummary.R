#Table 2
#Data Summary Table
# Wed Feb 17 10:24:54 2021 ------------------------------

#LIBRARIES
#====
library(reshape2)
library(knitr)
library(kableExtra)
library(janitor)
library(tidyverse)
load("data/allLengthsEnv.rda")
#====

#SET UP
#====
allLengthsEnv$region <- factor(allLengthsEnv$region, levels = c("north", "north_central", "central", "south"))

grouped <- tally(group_by_at(allLengthsEnv, vars(year, species, sex, region)))#get tally per group without on/offshore

grouped <- as.data.frame(grouped[complete.cases(grouped),])#get rid of NA regions

grouped$name <- paste(grouped$species, grouped$sex, grouped$region, sep = ".")

grouped <- dcast(grouped, year~name, value.var = "n")

col_order <- c("year", "EP.F.north", "EP.F.north_central", "EP.F.central", "EP.F.south", "EP.M.north", "EP.M.north_central", "EP.M.central", "EP.M.south", "TS.F.north", "TS.F.north_central", "TS.F.central", "TS.F.south", "TS.M.north", "TS.M.north_central", "TS.M.central", "TS.M.south", "ND.F.north", "ND.F.north_central", "ND.F.central", "ND.F.south", "ND.M.north", "ND.M.north_central", "ND.M.central", "ND.M.south")

grouped2 <- grouped[,col_order]

grouped2[is.na(grouped2)] <- 0

grouped2$EP.F.core <- grouped2$EP.F.north+grouped2$EP.F.north_central#combine north and north central into a single "core" region to match NMFW RREAS geography
grouped2$EP.M.core <- grouped2$EP.M.north+grouped2$EP.M.north_central
grouped2$TS.F.core <- grouped2$TS.F.north+grouped2$TS.F.north_central
grouped2$TS.M.core <- grouped2$TS.M.north+grouped2$TS.M.north_central
grouped2$ND.F.core <- grouped2$ND.F.north+grouped2$ND.F.north_central
grouped2$ND.M.core <- grouped2$ND.M.north+grouped2$ND.M.north_central

grouped2 <- grouped2[,-c(2, 3, 6, 7, 10, 11, 14, 15, 18, 19, 22, 23)]#get rid of north and north_central regions

col_order <- c("year", "EP.F.core", "EP.F.central", "EP.F.south", "EP.M.core", "EP.M.central", "EP.M.south", "TS.F.core", "TS.F.central", "TS.F.south", "TS.M.core", "TS.M.central", "TS.M.south", "ND.F.core", "ND.F.central", "ND.F.south", "ND.M.core", "ND.M.central", "ND.M.south")

grouped2 <- grouped2[,col_order]

grouped2 <- as.data.frame(adorn_totals(grouped2, "row"))

names(grouped2) <- c("year", "C", "SC", "S", "C", "SC", "S", "C", "SC", "S", "C", "SC", "S", "C", "SC", "S", "C", "SC", "S")
#====

#TABLE
#====
grouped2 %>% 
  kbl() %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, "Female" = 3, "Male" = 3, "Female" = 3, "Male" = 3,"Female" = 3, "Male" = 3)) %>% 
  add_header_above(c(" " = 1, "Euphausia pacifica" = 6, "Thysanoessa spinifera" = 6, "Nematocelis difficilis" = 6)) %>% 
  row_spec(8, bold = TRUE) %>% 
  column_spec(c(1, 4, 7, 10, 13, 16, 19), border_right = TRUE)
#====
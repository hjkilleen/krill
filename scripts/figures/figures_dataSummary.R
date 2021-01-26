#Data Summary Table
# Fri Nov 20 10:59:14 2020 ------------------------------

library(reshape2)
library(sjPlot)
library(qwraps2)
library(knitr)
library(kableExtra)

allLengthsEnv$region <- factor(allLengthsEnv$region, levels = c("north", "north_central", "central", "south"))

grouped <- tally(group_by_at(allLengthsEnv, vars(year, species, sex, region)))#get tally per group without on/offshore

grouped <- as.data.frame(grouped[complete.cases(grouped),])#get rid of NA regions

grouped$name <- paste(grouped$species, grouped$sex, grouped$region, sep = ".")

grouped <- dcast(grouped, year~name, value.var = "n")

col_order <- c("year", "EP.F.north", "EP.F.north_central", "EP.F.central", "EP.F.south", "EP.M.north", "EP.M.north_central", "EP.M.central", "EP.M.south", "TS.F.north", "TS.F.north_central", "TS.F.central", "TS.F.south", "TS.M.north", "TS.M.north_central", "TS.M.central", "TS.M.south", "ND.F.north", "ND.F.north_central", "ND.F.central", "ND.F.south", "ND.M.north", "ND.M.north_central", "ND.M.central", "ND.M.south")

grouped2 <- grouped[,col_order]

grouped2[is.na(grouped2)] <- 0

names(grouped2) <- c("year", "N", "NC", "C", "S", "N", "NC", "C", "S", "N", "NC", "C", "S", "N", "NC", "C", "S", "N", "NC", "C", "S", "N", "NC", "C", "S")

grouped2 %>% 
  kbl() %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, "Female" = 4, "Male" = 4, "Female" = 4, "Male" = 4,"Female" = 4, "Male" = 4)) %>% 
  add_header_above(c(" " = 1, "Euphausia pacifica" = 8, "Thysanoessa spinifera" = 8, "Nematocelis difficilis" = 8))

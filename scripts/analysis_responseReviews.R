#This script documents analyses conducted in response to reviewer comments from ICES Journal of Marine Science

# Fri May 21 11:44:17 2021 ------------------------------

#N difficilis in 2011. Interannual model estimates do not correlate with raw data shown in Figure 2. 
ggplot(nd, aes(latitude, length, color = year))+
  geom_point() +
  geom_smooth()

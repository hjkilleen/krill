#This script documents analyses conducted in response to reviewer comments from ICES Journal of Marine Science

# Tue May 18 09:38:16 2021 ------------------------------

#LIBRARIES & SOURCES
library(tidyverse)
library(ggpubr)
load("data/allLengthsEnvEP.rda")
load("data/allLengthsEnvND.rda")
load("data/allLengthsEnvTS.rda")

#I use t-tests to compare mean adult total length of each sex for each species. Following up on Reviewer 4's recommendations, I checked for normality in the distribution of each species-sex using visual investigation of histograms and quantile-quantile plots, D'Agostino test for skewness, and a test for excess kurtosis (values between -1 and 1 considered close to normal for both D'Agostino skewness and excess kurtosis tests).

#E. pacifica
hist(filter(ep, sex =="F")$length)
ggqqplot(filter(ep, sex =="F")$length)
moments::agostino.test(filter(ep, sex =="F")$length)
moments::kurtosis(filter(ep, sex =="F")$length)-3
#Investigation of the histogram and qqplot show the distribution is close to normal, but has a slight left skew. D'Agostino test also detected this skew, but the estimated value (-0.26) was >-1. Excess kurtosis indicated the distribution was slightly leptokurtic, but the value (0.68) was <1.

hist(filter(ep, sex =="M")$length)
ggqqplot(filter(ep, sex =="M")$length)
moments::agostino.test(filter(ep, sex =="M")$length)
moments::kurtosis(filter(ep, sex =="M")$length)-3
#Investigation of the histogram and qqplot show the distribution is close to normal, but has a long tails on both sides of the distribution. D'Agostino test detected a leftward skew, but the estimated value (-0.18) was >-1. Excess kurtosis indicated the distribution was slightly leptokurtic, but the value (0.65) was <1.

#T. spinifera
hist(filter(ts, sex =="F")$length)
ggqqplot(filter(ts, sex =="F")$length)
moments::agostino.test(filter(ts, sex =="F")$length)
moments::kurtosis(filter(ts, sex =="F")$length)-3
#Investigation of the histogram and qqplot show the distribution is left skewed. D'Agostino test detected a leftward skew, but the estimated value (-0.60) was >-1. Excess kurtosis indicated the distribution was very slightly leptokurtic, but the value (0.40) was <1.

hist(filter(ts, sex =="M")$length)#left skewed, qqplot nearly normal
ggqqplot(filter(ts, sex =="M")$length)
moments::agostino.test(filter(ts, sex =="M")$length)
moments::kurtosis(filter(ts, sex =="M")$length)-3
#Investigation of the histogram and qqplot show the distribution is close to normal with a slight left skew. D'Agostino test also detected this skew, but the estimated value (-0.12) was >-1. Excess kurtosis indicated the distribution was slightly platykurtic, but the value (-0.20) was >-1.

#N. difficilis
hist(filter(nd, sex =="F")$length)#generally normal, slight left skew, qqplot normal
ggqqplot(filter(nd, sex =="F")$length)
moments::agostino.test(filter(nd, sex =="F")$length)
moments::kurtosis(filter(nd, sex =="F")$length)-3
#Investigation of the histogram and qqplot show the distribution is close to normal with a slight left skew. D'Agostino test also detected this skew, but the estimated value (-0.23) was >-1. Excess kurtosis indicated the distribution was slightly leptokurtic, but the value (0.28) was <1.


hist(filter(nd, sex =="M")$length)#generally normal, slight left skew, n low, qqnorm nearly normal with one very left skewed ind.
ggqqplot(filter(nd, sex =="M")$length)
moments::agostino.test(filter(nd, sex =="M")$length)
moments::kurtosis(filter(nd, sex =="M")$length)-3
#Investigation of the histogram and qqplot show the distribution is close to normal with a slight right skew. D'Agostino test also detected this skew, but the estimated value (0.23) was <1. Excess kurtosis indicated the distribution was slightly platykurtic, but the value (-0.19) was >-1.

#N difficilis in 2011. Interannual model estimates do not correlate with raw data shown in Figure 2. 
ggplot(nd, aes(latitude, length, color = year))+
  geom_point() +
  geom_smooth()

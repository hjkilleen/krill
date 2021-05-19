load("data/metadata.rda")
library(reshape2)
library(kableExtra)

sentinels <- c(453, 454, 167, 171, 139, 152, 132, 134, 124, 127, 114, 110, 442, 445, 493, 495, 422, 425, 411, 414, 481, 402)#stations used to get domain-wide averages

md <- metadata[,-1]

md <- dcast(md, station + latitude + longitude + bottom_depth ~ year)#mark sampled years with Xs
for(i in 5:11){
  for(j in 1:nrow(md)) {
    if(md[j,i]>0) {
      md[j,i] <-'X'
    } else {
      md[j,i] <- " "
    }
  }
}

md$figure4 <- rep(NA, nrow(md))

for(i in 1:nrow(md)) {#mark sentinel stations
  if(md[i,1] %in% sentinels) {
    md[i,12] <-'X'
  } else {
    md[i,12] <- " "
  }
}

md %>% #knit table
  kbl() %>% 
  kable_classic(lightable_options = "striped")

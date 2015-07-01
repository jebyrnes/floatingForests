###################################
# File to look at *where* has been sampled
# Jarrett Byrnes jarrett.byrnes@umb.edu
#
# Changelog
#
###################################
source("./parseZooData.R")
library(ggplot2)
library(rgdal)


##What locations have been sampled?
klocs <-  kelpzoo %>% group_by(lower_left_x, lower_left_y) %>%
  summarise(n=n()) %>% ungroup()

#Plot them all on the planet
library(maptools)
data(wrld_simpl)
plot(wrld_simpl)
matplot(as.matrix(klocs[,1]),as.matrix(klocs[,2]), pch=20, add=T)

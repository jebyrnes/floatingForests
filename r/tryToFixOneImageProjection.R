
source("./FFmethods.R")
source("./parseZooData.R")
library(ggplot2)

load("../data/oneImage_data_only_AKP00016e6.RData")
corners <- getCorrectCorners(oneImage$subject_zooniverse_id[1])

#fix the corners to the proper projection
oneImage$lower_left_x <- corners$ll[1]
oneImage$lower_left_y <- corners$ll[2]
oneImage$upper_right_x <- corners$ur[1]
oneImage$upper_right_y <- corners$ur[2]

polysData <- getSpatialPolysDataFrameForOneImage(oneImage, 
                                                 proj="+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

plot(polysData)

tileBrick <- rasterizeFFImage(oneImage[1,],
                              proj="+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


plotRGB(tileBrick)
plot(polysData, add=T)



####
#load at the real coastline
####
library(rgdal)
coasts <- readOGR("../data/gshhg-shp-2.3.4/GSHHS_shp/h", layer="GSHHS_h_L1",  useC=FALSE)

#plot the image
plotRGB(tileBrick)

#plot the cropped coastline on top
coastCrop <- crop(coasts, extent(tileBrick))
plot(coastCrop, add=T, lwd=4)

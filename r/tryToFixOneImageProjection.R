#load functions needed
source("./FFmethods.R")
#source("./parseZooData.R")
library(ggplot2)

#load dataset to work on
load("../data/oneImage_data_only_AKP00016e6.RData")
proj <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#run the correction on corners
corners <- getCorrectCorners(oneImage$subject_zooniverse_id[1], proj=proj)

#fix the corners to the proper projection
oneImage$lower_left_x <- corners$ll[1]
oneImage$lower_left_y <- corners$ll[2]
oneImage$upper_right_x <- corners$ur[1]
oneImage$upper_right_y <- corners$ur[2]

#FROM TOM
#Upper Left: 36.315309, -122.000823
#Lower Left: 36.217145, -122.002073
#Upper Right: 36.314121, -121.867502
#Lower Right: 36.215961, -121.868919

oneImage$lower_left_x <- -122.002073
oneImage$lower_left_y <- 36.217145
oneImage$upper_right_x <- -121.868919
oneImage$upper_right_y <- 36.314121


#turn into a polygon
polysData <- getSpatialPolysDataFrameForOneImage(oneImage, 
                                                 proj=proj)

#plot(polysData)

tileBrick <- rasterizeFFImage(oneImage[1,],
                              proj="+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


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

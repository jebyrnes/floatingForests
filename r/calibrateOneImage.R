library(R.matlab)
source("./FFmethods.R")

load("../data/oneImagesAKP00016e6.Rdata")
#EPSG:3857?
#CRS("+init=epsg:3857"))

#polysData <- spTransform(polysData, "+proj=merc +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
library(rgdal)
#writeOGR(polysData, "../data/", "AKP00016e6", driver="ESRI Shapefile")
#load the SBCC data and reprocess into a SpatialPoints object
caKelp <- readMat("../data/kelp_043035_7_24_1999.mat")
caKelp <- caKelp$kelp[which(caKelp$kelp[,3]>0),]
caKelp.spoints <- SpatialPoints(caKelp[,2:1], 
                                proj4string=CRS(proj4string(polysData)))
#caKelp.spoints <- spTransform(caKelp.spoints, CRS("+init=epsg:3857"))

#get the bounds for plotting from the polygons data frame
longBounds <- bbox(polysData)[1,]
latBounds <- bbox(polysData)[2,]

#plot them together
plot(caKelp.spoints, xlim=longBounds, ylim=latBounds, pch=20, cex=0.5)
plot(polysData, add=T)

####
#look at the real coastline
####
lims <- as.numeric(polysData@data[1,c(10,8,11,9)])
coasts <- readOGR("../data/gshhg-shp-2.3.4/GSHHS_shp/h", layer="GSHHS_h_L1",  useC=FALSE)


plot(coasts, xlim=lims[1:2], ylim=lims[3:4], lwd=4)
plot(polysData, add=T)

tileBrick <- rasterizeFFImage(polysData@data[1,]$subject_zooniverse_id)
plotRGB(tileBrick)
plot(coasts, xlim=lims[1:2], ylim=lims[3:4], lwd=4, add=T)
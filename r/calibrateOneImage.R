library(R.matlab)
source("./FFmethods.R")

load("../data/oneImagesAKP00016e6.Rdata")
oneImage <- read.csv("../data/AKP00016e6.csv")
oneImage[,15:18] <- matrix(rep(c( -122.0022,  36.2172, -121.8677 ,    36.3143 ), nrow(oneImage)), ncol=4,byrow=T)
polysData <- getSpatialPolysDataFrameForOneImage(oneImage)
#polysData@bbox <- matrix(c(  -122.0022,  36.2172, -121.8677 ,    36.3143 ), nrow=2)
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

caKelp.spoints <- crop(caKelp.spoints, extent(polysData))
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

tileBrick <- rasterizeFFImage(oneImage[1,])
plotRGB(tileBrick)
plot(coasts, xlim=lims[1:2], ylim=lims[3:4], lwd=4, add=T)
plot(caKelp.spoints, xlim=longBounds, ylim=latBounds, pch=20, cex=0.5, 
     add=T, col=rgb(1,0,0,alpha=0.1))

#########
# OK, let's get down to calibriation
# and thresholds
#########
#400 x 364 in reality
#532 x 484 on FF
r <- raster(crs=polysData@proj4string, ext=extent(polysData), ncols = 400, nrows=364)
rastLayer <- rasterize(SpatialPolygons(polysData@polygons), r, fun="count")


plotRGB(tileBrick)
plot(rastLayer,
     legend.args=list(text='# of Users\nSelecting', 
                      side=3, font=7.2, line=0.5, cex=0.8), add=T)


longBounds <- bbox(rastLayer)[1,]
latBounds <- bbox(rastLayer)[2,]

#for the tight shot!
zoomLongBounds <- bbox(SpatialPolygons(polysData@polygons))[1,]
zoomLatBounds <- bbox(SpatialPolygons(polysData@polygons))[2,]

hasData <- which(!is.na(as.matrix(rastLayer)), arr.ind=TRUE)
rastZoomXBounds <- c(min(hasData[,2]), max(hasData[,2]))
rastZoomYBounds <- c(min(hasData[,1]), max(hasData[,1]))
zoomExtent <- extent(c(zoomLongBounds, zoomLatBounds))

#plot them together

plot(crop(rastLayer, extent(c(zoomLongBounds, zoomLatBounds))),
     legend.args=list(text='# of Users\nSelecting', 
                      side=3, font=2.2, line=0.5, cex=0.8)
)

plot(caKelp.spoints, xlim=zoomLongBounds, ylim=zoomLatBounds, pch=20, cex=0.5, add=T)

########
#Rasterize the good data
##########
caKelp.raster <- rasterize(caKelp.spoints, r, fun="count")


#See what this looks like
combined.raster <- overlay(caKelp.raster, rastLayer, fun="sum")

plot(crop(combined.raster, zoomExtent))
hist(as.matrix(combined.raster), breaks=1:16)

############
# Functions to caculate areas
#############
getKelpPixelsFromRaster <- function(rast, threshold=1){
  length(which(as.matrix(rast)>=threshold))
}

getKelpPixelsFromRasterV <- Vectorize(getKelpPixelsFromRaster, "threshold")

getUsers <- function(a,b) {
  if(is.na(a)) return(NA)
  b
}


kelpPixels <- data.frame(users=1:15, 
                         kelpPixels = getKelpPixelsFromRasterV(rastLayer, 1:15),
                         goodKelpPixels = getKelpPixelsFromRasterV(combined.raster, 1:15))
kelpPixels$kelpArea <- kelpPixels$kelpPixels*0.03*0.03

plot(goodKelpPixels ~ kelpPixels, data=kelpPixels, type="n")
text(kelpPixels$kelpPixels, kelpPixels$goodKelpPixels, labels=1:15)
plot(goodKelpPixels ~ users, data=kelpPixels)


plot(kelpPixels ~ users, data=kelpPixels)
abline(h=getKelpPixelsFromRaster(caKelp.raster))

###S
library(ggplot2)
qplot(users, kelpPixels, data=kelpPixels, geom=c("point"), size=I(10)) +
  xlab("\nMinimum # of Users Selecting a Pixel") + ylab("Total Pixels of Kelp\n") +
  geom_abline(intercept=getKelpPixelsFromRaster(caKelp.raster), lwd=2, lty=2, col="red") +
  theme_bw(base_size=24) +
  ylim(c(0,18000))



qplot(users, kelpArea, data=kelpPixels, geom=c("point"), size=I(10)) +
  xlab("\nMinimum # of Users Selecting a 30 x 30m Pixel") + ylab("Total Square Km. of Kelp\n") +
  geom_abline(intercept=0.03*0.03*getKelpPixelsFromRaster(caKelp.raster), slope=0, lwd=2, lty=2, col="red") +
  theme_bw(base_size=24) +
  ylim(c(0,10))


#Maybe a binomial model
goodIDX <- which(!is.na(as.matrix(caKelp.raster)))


calDF <- data.frame(calibration = as.vector(as.matrix(caKelp.raster)), 
                    FF = as.vector(as.matrix(rastLayer)))

calDF[which(is.na(calDF[,1])),1] <- 0
calDF[,1] <- as.numeric(calDF[,1]>0)
calDF[which(is.na(calDF[,2])),2] <- 0
#Only things that were kelp in either the calibration of FF set
calDF <- calDF[-which(rowSums(calDF)==0),]

calGLM <- glm(calibration ~ FF, data=calDF, family=binomial)
summary(calGLM)

ggplot(data=calDF, mapping=aes(x=FF, y=calibration)) +
  geom_point(alpha=0) +
  stat_smooth(method=glm, method.args=list(family=binomial), color="red", lwd=2) +
  geom_jitter(position = position_jitter(width = .5, height=0.01)) +
  theme_bw(base_size=24) +
  xlab("\n# of People Selecting Pixel") + ylab("Pixel Included in Calibration Set\n(1=yes, 0=no)\n")
    
library(dplyr)
cdfReduced <- calDF %>%
  group_by(FF, calibration) %>%
  summarise(`Number of Pixels`=n())
    
qplot(FF, calibration, size=`Number of Pixels`, data=cdfReduced) +
  theme_bw(base_size=24) +
  xlab("\n# of People Selecting Pixel") + ylab("Pixel Included in Calibration Set\n(1=yes, 0=no)\n")


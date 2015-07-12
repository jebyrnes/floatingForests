library(R.matlab)
source("./FFmethods.R")

load("../data/oneImagesAKP00016e6.Rdata")

#load the SBCC data and reprocess into a SpatialPoints object
caKelp <- readMat("../data/kelp_043035_7_24_1999.mat")
caKelp <- caKelp$kelp[which(caKelp$kelp[,3]>0),]
caKelp.spoints <- SpatialPoints(caKelp[,2:1])

#get the bounds for plotting from the polygons data frame
longBounds <- bbox(polysData)[1,]
latBounds <- bbox(polysData)[2,]

#plot them together
plot(caKelp.spoints, xlim=longBounds, ylim=latBounds, pch=20, cex=0.5)
plot(polysData, add=T)

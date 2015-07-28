library(rgeos)
source("./FFmethods.R")

oneImage <- read.csv("../data/AKP00016e6.csv")

#get a polygons dataframe
polysData <- getSpatialPolysDataFrameForOneImage(oneImage)
#pOver <- gUnionCascaded(gBuffer(polysData, width=0)) #not needed - but thanks, Remy!

#make a grid we'll be useing to count overlap with
GT <- GridTopology(c(oneImage$lower_left_x[1], oneImage$lower_left_y[1]), 
                   c((oneImage$upper_right_x[1]-oneImage$lower_left_x[1])/532,
                     (oneImage$upper_right_y[1]-oneImage$lower_left_y[1])/484),
                     c(533, 485))

SPG <- SpatialGrid(GT, proj4string=projection(polysData))

#let's see the grid with the polygon
plot(SPG, col="lightgrey")
plot(polysData, add=T, col="red")

#or see the polygon with the grid!
plot(polysData)
plot(SPG, add=T, col="lightgrey")

#get overlap on the grid
o <- over(polysData, SPG, returnList=T, fn=sum)
z <- unlist(o) #I know, z is a terrible variable name. I had just put HD to bed and was tired while writing this - will fix when I make it a function

#count number of polygons overlapping each pixel in the grid
pixelCount <- sapply(unique(z), function(x) sum(z==x))

#make some counts about that overlap
uniqueOverlapCount <- sapply(1:14, function(x) sum(pixelCount==x))
sumOverlapCount <- rev(cumsum(rev(uniqueOverlapCount)))

plot(1:14, uniqueOverlapCount, type="b")
plot(1:14, sumOverlapCount, type="b")

#turn the counts into a heatmap!
#ct <- sapply(o, length) #doesn't quite get me where I want to go
#ct <- sapply(1:length(SPG), function(x) sum(z==x)) #tooooo sloooow
#first, make a vector of counts matching the grid
#by getting all counts, and then using their indices to make a count
#vector that matches the length of the grid.
ct <- sapply(unique(z), function(x) sum(z==x))
ct2 <- rep(0, length(SPG)) #blank vector of 0's as defauls
ct2[unique(z)] <- ct
SPG.df <- SpatialGridDataFrame(SPG,  data=data.frame(ct=ct))
SPG.df <- SpatialGridDataFrame(SPG,  data=data.frame(ct=ct3))

#let's plot it using rasters
library(raster)
b <- brick(SPG.df) 
b <- crop(b, extent(polysData))
spplot(b)

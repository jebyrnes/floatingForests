library(rgeos)
source("./FFmethods.R")

oneImage <- read.csv("../data/AKP00016e6.csv")


polysData <- getSpatialPolysDataFrameForOneImage(oneImage)
#pOver <- gUnionCascaded(gBuffer(polysData, width=0)) #not needed

#make a grid we'll be useing to count overlap with
GT <- GridTopology(bbox(polysData)[,1], rep(0.001,2), c(532, 484))
SPG <- SpatialGrid(GT, proj4string=projection(polysData))

plot(polysData)
plot(SPG, add=T, col="lightgrey")
#get overlap on the grid
o <- over(polysData, SPG, returnList=T, fn=sum)
z <- unlist(o)
#count # of polygons overlapping each pixel in the grid
pixelCount <- sapply(unique(z), function(x) sum(z==x))

#make some counts
uniqueOverlapCount <- sapply(1:14, function(x) sum(pixelCount==x))
sumOverlapCount <- rev(cumsum(rev(uniqueOverlapCount)))

plot(1:14, uniqueOverlapCount, type="b")
plot(1:14, sumOverlapCount, type="b")

#ct <- sapply(o, length)
ct <- sapply(1:length(SPG), function(x) sum(z==x))
SPG.df <- SpatialGridDataFrame(SPG,  data=data.frame(ct=ct))
b <- brick(SPG.df) 
b <- crop(b, extent(polysData))
spplot(b)

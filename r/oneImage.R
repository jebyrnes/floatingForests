###################################
# File to parse a floating forests data file
# for easy analysis
#
# Jarrett Byrnes jarrett.byrnes@umb.edu
#
# Changelog
#
###################################

source("./FFmethods.R")
source("./parseZooData.R")

hasPaths <- kelpzoo %>% filter(nchar(relPath)>0)

sampledSubjects <- hasPaths %>% 
  group_by(subject_zooniverse_id) %>%
  summarise(nPolys=n(), nUsers=length(unique(user_name)))

counts <- sampledSubjects%>%group_by(nUsers) %>% summarise(IDS=n())

#images are 532x484
#http://static.zooniverse.org/www.floatingforests.org/subjects/53e2f74e4954734d8bfe2600.jpg

poly <- getPoly(hasPaths[20000,])
poly <- SpatialPolygons(list(Polygons(list(poly), 1)), proj4string=CRS("+proj=longlat +datum=WGS84"))
img <- rasterizeFFImage(hasPaths[1,])

plot(poly, col="red")
plotRGB(img)
plot(poly, col="red", add=T)

### ONE IMAGE
oneImage <- hasPaths %>%
  filter(subject_zooniverse_id == hasPaths$subject_zooniverse_id[2000])

polys <- getSpatialPolysForOneImage(oneImage)

plot(polys, col="red")

rownames(oneImage) = paste0(oneImage$classification_id, 1:nrow(oneImage))
polysData <- SpatialPolygonsDataFrame(polys, data=as.data.frame(oneImage))

plot(polysData, col=rainbow(13)[as.numeric(as.factor(polysData@data$user_name))], dens=85)

r <- raster(crs=polys@proj4string, ext=extent(polys))
rastLayer <- rasterize(polys, r, fun="count")
plot(rastLayer)

tileBrick <- rasterizeFFImage(oneImage[1,])
plotRGB(tileBrick)
#plot(rastLayer, add=T, density=0.1)
plot(polysData,  add=T)


jointAreas <- 
  sapply(1:14, function(x) length(which(as.matrix(rastLayer)==x)))

qplot(1:14, jointAreas, geom=c("point", "line")) +
  theme_bw()+
  xlab("Number of Users Selecting Pixels") +
  ylab("Number of Pixels")

usersSelection <- 
  sapply(1:14, function(x) length(which(as.matrix(rastLayer)>=x)))

qplot(1:14, usersSelection, geom=c("point", "line")) +
  theme_bw()+
  xlab("Number of Users Selecting Pixels") +
  ylab("Number of Pixels")

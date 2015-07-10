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
library(ggplot2)

hasPaths <- kelpzoo %>% filter(nchar(relPath)>0)

sampledSubjects <- hasPaths %>% 
  group_by(subject_zooniverse_id) %>%
  summarise(nPolys=n(), nUsers=length(unique(user_name)))


sampledSubjectsAll <- kelpzoo %>% 
  group_by(subject_zooniverse_id) %>%
  summarise(nClassifications=length(unique(user_name)))


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
#current image 2000 = 
oneImage <- hasPaths %>%
  filter(subject_zooniverse_id == hasPaths$subject_zooniverse_id[2000])

#get the spatial polygons
polys <- getSpatialPolysForOneImage(oneImage)

#naieve plot
plot(polys, col="red")


#Work with a spatialPointsDataFrame
polysData <- getSpatialPolysDataFrameForOneImage(oneImage)

plot(polysData, col=rainbow(13)[as.numeric(as.factor(polysData@data$user_name))], dens=85)

#Show each user's classification
pdf(file="../output/oneImage_each_user.pdf",width=96,height=96,pointsize=10)
par(mfrow=c(4,4))
par(mar=c(1,1,1,1))
sapply(unique(polysData@data$user_name), function(x){
  plot(polysData[which(polysData$user_name==x),], main=x)
})
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

#######
#Turn the polys into a raster file to see overlap
#######
r <- raster(crs=polys@proj4string, ext=extent(polys))
rastLayer <- rasterize(polys, r, fun="count")
jpeg("../output/oneImage_coastline_raster.jpg", type="quartz",
     height=640, width=740)
plot(rastLayer,
     legend.args=list(text='# of Users\nSelecting', 
                      side=3, font=2.2, line=0.5, cex=0.8))
dev.off()

#######
# Combine with actual image from FF
#######

#Get base image
tileBrick <- rasterizeFFImage(oneImage[1,])
#plot(rastLayer, add=T, density=0.1)

#plot the base image & the classifications
jpeg("../output/oneImage_coastline_with_outlines.jpg", 
     type="quartz", width=600, height=600)
plotRGB(tileBrick)
plot(polys,  add=T)
dev.off()

#######
#What was the shared area selected with precisely?
#######
jointAreas <- 
  sapply(1:13, function(x) length(which(as.matrix(rastLayer)==x)))

qplot(1:13, jointAreas, geom=c("point", "line")) +
  theme_bw()+
  xlab("Number of Users Selecting Pixels") +
  ylab("Number of Pixels")

#######
#How many pixels were selected with >= x users?
#######

usersSelection <- 
  sapply(1:13, function(x) length(which(as.matrix(rastLayer)>=x)))

jpeg("../output/oneImage_pixels_selected_by_n_users.jpg", type="quartz",
     width=600, height=500)
qplot(1:13, usersSelection, geom=c("point", "line")) +
  theme_bw(base_size=17)+
  xlab("\nNumber of Users Selecting >= X # of Pixels") +
  ylab("Number of Pixels\n")
dev.off()

#######
#Randomization of users and raster area
#######

u <- length(unique(polysData$user_name))

###THIS WILL TAKE A LONG TIME - RUN IT ON A SERVER
pixelsFromNUsers <- lapply(1:u, function(nusers){
  pixels <- combn(u, nusers, getAreaFromUserCombn)
  
  data.frame(nUsers=nusers, pixels=pixels)
})

pixelsFromNUsers <- plyr::ldply(pixelsFromNUsers)

write.csv(pixelsFromNUsers, 
          "../output/oneImage_pixels_from_n_users.csv", 
          row.names=F)

### OR IF YOU HAVE RUN IT ELSEWHERE...
pixelsFromNUsers <- read.csv("../output/oneImage_pixels_from_n_users.csv")

jpeg("../output/oneImage_pixels_from_n_users_shared_allusers.jpg", 
     type="quartz", width=600, height=600)
qplot(nUsers, pixels, data=pixelsFromNUsers, geom=c("point")) +
  theme_bw(base_size=17)+
  xlab("Number of Users") +
  ylab("Total Number of Shared Pixels Selected\n")
dev.off()


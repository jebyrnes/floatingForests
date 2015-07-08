library(RJSONIO)
library(RCurl)
library(dplyr)
library(readr)
library(maptools)
library(raster)
library(jpeg)

getImageInfo <- function(imageID, subjURL = "https://api.zooniverse.org/projects/kelp/subjects/"){
  fromJSON(getURL(paste0(subjURL, imageID)))
}

rasterizeFFImage <- function(arow){
  imageInfo <- getImageInfo(arow$subject_zooniverse_id)
  loc <- gsub("http://", "https://static.zooniverse.org/", imageInfo$location)
  
  img <-brick(loc,              
               crs="+proj=longlat +datum=WGS84")
  
  img@extent = extent(c(arow$lower_left_x, arow$upper_right_x, 
                        arow$lower_left_y, arow$upper_right_y))

  return(img)
  
}

#images are 532x484
#takes a single relative path and a row of data and returns a polygon
getPoly <- function(arow, polyPath, xpixels = 532, ypixels=484){
  
  #transform the row into a 
  polyPath <- polyPath[-length(polyPath)]
  polyPath <- t(sapply(polyPath, function(x){
    x <- gsub("^(.*)\\[", "", x)
    x <- gsub("\\'", "", x)
    x <- strsplit(x, ",")[[1]]
    as.numeric(x)}))
  
  rownames(polyPath) <- 0:(nrow(polyPath)-1)
  
  #make it absolute points  
  polyPath[,2] <- -1*(polyPath[,2]) #invert b/c y is opposite in the FF DB
  
  polyPath[,1] <- cumsum(polyPath[,1])
  polyPath[,2] <- cumsum(polyPath[,2])
  
  #close the polygon
  polyPath <- rbind(polyPath, polyPath[1,])
  
  #give it an absolute position
  polyPath[,1] <- polyPath[,1]+arow$startingPoint_x
  polyPath[,2] <- polyPath[,2]+ypixels-arow$startingPoint_y #invert b/c y is opposite in the FF DB
  
  #give it a position on the map
  xlen <- arow$upper_right_x - arow$lower_left_x
  ylen <- arow$upper_right_y - arow$lower_left_y
  polyPath[,1] <- polyPath[,1]*xlen/xpixels + arow$lower_left_x
  polyPath[,2] <- polyPath[,2]*ylen/ypixels + arow$lower_left_y
  #plot(polyPath[,1], polyPath[,2], type="l")
  
  #turn it into a sp::Polygon
  ret <- Polygon(polyPath, hole=F)
  ret
}

#takes a dataframe and returns a Polygons object
#with user_name as default for ID
getPolys <- function(aframe, idForPolys=aframe$user_name[1]){
  polyPaths <- strsplit(as.character(aframe$relPath), "\\]")
  polys <- vector("list", length(polyPaths))
  
  for(i in 1:length(polyPaths)){
    polys[[i]] <- getPoly(aframe[i,], polyPaths[[i]])
  }
  
  p <- Polygons(polys, ID=idForPolys)
  
}

#takes a dataframe and returns a SpatialPolygons object
getSpatialPolysForOneImage <- function(aframe, proj="+proj=longlat +datum=WGS84"){
  polys <- plyr::dlply(aframe, .(user_name), getPolys)
  SpatialPolygons(polys, proj4string=CRS(proj))
}

#takes a dataframe and returns a SpatialPolygons object
getSpatialPolysDataFrameForOneImage <- function(aframe, proj="+proj=longlat +datum=WGS84"){
  spatialPolys <- getSpatialPolysForOneImage(aframe, proj)

  newFrame <- aframe %>%
    group_by(classification_id, user_name,subject_zooniverse_id,created_at,
             rejection_water_percent,rejection_cloud_percent,clouds) %>%
    summarise(nBeds = length(user_name))
  
  row.names(newFrame) <- newFrame$user_name
  
  SpatialPolygonsDataFrame(spatialPolys, data=as.data.frame(newFrame))
}


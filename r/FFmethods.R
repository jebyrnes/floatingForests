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
getPoly <- function(arow, xpixels = 532, ypixels=484){
  
  #transform the row into a 
  polyPath <- strsplit(as.character(arow$relPath), "\\]")[[1]]
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
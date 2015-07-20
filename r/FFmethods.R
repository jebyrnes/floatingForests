library(RJSONIO)
library(RCurl)
library(dplyr)
library(readr)
library(maptools)
library(raster)
library(jpeg)
library(rgeos)

getImageInfo <- function(imageID, subjURL = "https://api.zooniverse.org/projects/kelp/subjects/"){
  fromJSON(getURL(paste0(subjURL, imageID)))
}

rasterizeFFImage <- function(arow, proj="+proj=utm +datum=WGS84"){
  imageInfo <- getImageInfo(arow$subject_zooniverse_id)
  loc <- gsub("http://", "https://static.zooniverse.org/", imageInfo$location)
  
  img <-brick(loc,              
               crs=proj)
  
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
getSpatialPolysForOneImage <- function(aframe, proj="+proj=merc +datum=WGS84"){
  polys <- plyr::dlply(aframe, "user_name", getPolys)
  SpatialPolygons(polys, proj4string=CRS(proj))
}

#takes a dataframe and returns a SpatialPolygons object
getSpatialPolysDataFrameForOneImage <- function(aframe, proj="+proj=merc +datum=WGS84"){
  spatialPolys <- getSpatialPolysForOneImage(aframe, proj)

  newFrame <- aframe %>%
    group_by(classification_id, user_name,subject_zooniverse_id,created_at,
             rejection_water_percent,rejection_cloud_percent,clouds,
             upper_right_x, upper_right_y, lower_left_x, lower_left_y) %>%
    summarise(nBeds = length(user_name)) %>%
  ungroup()
  
  row.names(newFrame) <- newFrame$user_name
  
  SpatialPolygonsDataFrame(spatialPolys, data=as.data.frame(newFrame))
}

#takes a list of indices of users and returns the 
#total or shared # of pixels in a raster
getAreaFromUserCombn <- function(idx, spdf=polysData, type="shared"){
  
  #aggregate all polygons to one raster
  
  combnR <- raster(crs=spdf@proj4string, ext=extent(spdf))
  rastLayerCombn <- rasterize(SpatialPolygons(spdf@polygons)[idx], combnR, 
                              fun="count")  
  if(type=="total") {
    return(length(which(as.matrix(rastLayerCombn)>=1)))
  }
  
  #defaults to shared
  length(which(as.matrix(rastLayerCombn)==length(idx)))
  
}

######Get correct corners
getCorrectCorners <- function(subject="AKP00016e6",
                              n_row=20, n_cols=20,
                              scene_y_pixels =7981, 
                              scene_x_pixels = 7271){

  imageInfo <- getImageInfo(subject)
  scene <-imageInfo$metadata$base_file_name
  lower_left <- imageInfo$metadata$lower_left #offset of row and col num
  upper_right <- imageInfo$metadata$upper_right

  #get row, col info
  row_no <- imageInfo$metadata$row_no
  col_no <- imageInfo$metadata$col_no


  #I know row and col #
  row_no <- imageInfo$metadata$row_no
  col_no <- imageInfo$metadata$col_no

  #I can calculate central lat/long
  cent <- c((lower_left[1] + upper_right[1])/2, 
          (lower_left[2] + upper_right[2])/2)


  #i can calculate central pixels
  cent_y_pixel <- (scene_y_pixels / n_row) * row_no 
  cent_x_pixel <- (scene_x_pixels / n_cols) *col_no


  #I can calculate image size?
  image_chunk_width = scene_x_pixels / n_row
  image_chunk_height = scene_y_pixels / n_cols

  ll_pixels <- c(cent_x_pixel-image_chunk_width/2, 
               cent_y_pixel-image_chunk_height/2)
  ur_pixels <- c(cent_x_pixel+image_chunk_width/2, 
               cent_y_pixel+image_chunk_height/2)

  #i can calculate rx = cdx/cpx
  conv_deg <- abs(c((upper_right[1] - lower_left[1])/image_chunk_width, 
                  (upper_right[2] - lower_left[2])/image_chunk_height))

  #I can calculate the scene xmin and ymin
  #cdx = cpx*d/p + xd0
  scene_ll <- c(cent[1]-cent_x_pixel*conv_deg[1],
              cent[2]-cent_y_pixel*conv_deg[2])

  scene_ur <- c(scene_ll[1]+scene_x_pixels*conv_deg[1], 
              scene_ll[2]+scene_y_pixels*conv_deg[2])

  #OK, now make a raster
  library(raster)
  ascene <- raster(nrow=scene_y_pixels, ncol=scene_x_pixels, 
                 xmn=scene_ll[1], xmx=scene_ur[1], 
                 ymn=scene_ll[2], ymx=scene_ur[2])
  #for CA
  projection(ascene) <- "+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  newx <- xFromCol(ascene, c(ll_pixels[1], ur_pixels[1]))
  newy <- yFromRow(ascene, c(ll_pixels[2], ur_pixels[2]))

  list(ll=c(newx[1], newy[1]), ur = c(newx[2], newy[2]))
}

################################
#
# Methods for reading and manipulating data
# from Panoptes export for Floating Forets
# project
#
################################

#### Load libraries
library(tidyverse)
library(jsonlite)
library(tidyjson)
library(sp)
library(raster)

#Note - projection of current sliced images is based on landsat
rasterizeFFImage <- function(arow, subj_data = test_subjects, 
                             zone=11,
                             proj=paste0("+proj=utm +zone=",zone," ellps=WGS84, +datum=WGS84 +units=m +no_defs")){
  
  imageInfo <- test_subjects %>% filter(subject_id == arow$subject_ids)
  
  loc <- fromJSON(imageInfo$locations)[[1]]
  
  img <-brick(loc,              
              crs=proj)
  
 # img@extent = extent(c(arow$lower_left_x, arow$upper_right_x, 
 #                        arow$lower_left_y, arow$upper_right_y))
  
  return(img)
  
}


#takes a dataframe and returns a SpatialPolygons object
getSpatialPolysDataFrameForOneImage <- function(aframe, 
                                                zone = 10,
                                                proj = paste0("+proj=utm +zone=",zone," ellps=WGS84, +datum=WGS84 +units=m +no_defs")){
  aaframe -> aframe
  aframe <- mutate(aframe, hasPolys = ifelse(grepl('\\\"value\\\":\\[\\]', annotations), FALSE, TRUE))
  
  spatialPolys <- getSpatialPolysForOneImage(aframe %>% filter(hasPolys), 
                                             zone, proj)
  
  newFrame <- aframe %>%
    group_by(classification_id, user_name, subject_ids, hasPolys) %>%
    summarise(nrows = n()) %>%
    ungroup() %>%
    dplyr::select(-nrows) %>%
    mutate(nUsers=n_distinct(user_name)) %>%
    filter(hasPolys) %>%
    as.data.frame()
  
  row.names(newFrame) <- newFrame$user_name
  
  ret <- SpatialPolygonsDataFrame(spatialPolys, data=as.data.frame(newFrame))
 # ret@bbox <- matrix(c(aframe$lower_left_x[1], aframe$lower_left_y[1],
 #                     aframe$upper_right_x[1], aframe$upper_right_y[1]), nrow=2)
  
  ret
}

#takes a dataframe and returns a SpatialPolygons object
getSpatialPolysForOneImage <- function(aframe, zone = 10,
                                       proj = paste0("+proj=utm +zone=",zone," ellps=WGS84, +datum=WGS84 +units=m +no_defs")){
  
  #polys <- plyr::dlply(aframe, "user_name", getPolys)
  #polys <- purrr::map(aframe, "classification_id", getPolys)
  #this will functionally do it by row
  ret <- aframe %>% group_by(user_name) %>% do(polys = getPolys(.))
  
  SpatialPolygons(ret$polys, proj4string=CRS(proj))
}


#takes a dataframe and returns a Polygons object
#with user_name as default for ID
getPolys <- function(arow, idForPolys=arow$user_name){
  #for each row, expand out to all of the polygon text strings

  annotations <- fromJSON(arow$annotations)
  kelpIDX <- grep("Mark", annotations$task_label)
  poly_text <- annotations$value[[kelpIDX]][[1]]
  
  polys <- map(poly_text, ~getOnePoly(.))
  
  
  p <- Polygons(polys, ID=idForPolys)
  
}

getOnePoly <- function(poly_string){
  poly_string <- poly_string
  poly_string <- gsub("[A-Z]", "", poly_string)
  poly_string <- gsub("  ", " ", poly_string)
  poly_string <- gsub("^ ", "", poly_string)
  
  #split it into an x,y data frame
  poly_df <- data.frame(str = strsplit(poly_string, " ")[[1]]) %>%
    tidyr::separate(str, c("x", "y"), sep=",") #%>%
    #dplyr::select(x, y)
  
  #ok, no longer a string!
  poly_df <- rbind(poly_df, poly_df[1,]) %>%
    map_df(as.numeric)
  
  #n.b. the original images are 532 x 484
  #flip the y, as it's currently incorrectly given
  poly_df$y <- poly_df$y * -1 + 484
    
  ret <- Polygon(as.matrix(poly_df), hole=F)
}
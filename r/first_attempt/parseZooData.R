###################################
# File to parse a json zooniverse data file
# into a data frame for easy analysis
#
# Jarrett Byrnes jarrett.byrnes@umb.edu
#
# Changelog
#
# 20140828 - First completed version
###################################

library(RJSONIO)
library(R.utils)
library(RCurl)
library(plyr)
library(sp)

#####
# Helper Functions
#####

#a function to take a parsed piece of Zooniverse data and return a data frame suitable for use in analysis
parseZooIn <- function(zooList, subjURL = "https://api.zooniverse.org/projects/kelp/subjects/"){
  #q is a qounter so I can tell here in j I should qatch an error
 # q<<- q+1
  #print(q)
  
	oid <- zooList[["_id"]][[1]]
	clouds <- zooList[["annotations"]][[1]]
	start <- zooList[["annotations"]][[3]][1]
	finish <- zooList[["annotations"]][[3]][2]
	user <- zooList[["user_id"]][[1]]
	imageID <- zooList$subjects[[1]]$zooniverse_id
	imageInfo <- fromJSON(getURL(paste0(subjURL, imageID)))
	
  #odd - sometimes, no user ID
  if(is.null(user)) user <- NA
  
	#Write a function to get area of kelp from the annotations
	#and we're good!
	
	data.frame(oid = oid,
		clouds=clouds,
		start=start,
		finish=finish,
		user=user,
		imageID=imageID,
		lat= imageInfo$coords[2],
		long=imageInfo$coords[1],
		region = imageInfo$group[3],
		date = imageInfo$metadata$timestamp,
			latUpperRight= imageInfo$metadata$upper_right[2],
		longUpperRight=imageInfo$metadata$upper_right[1],
			latLowerLeft= imageInfo$metadata$lower_left[2],
		longLowerLeft=imageInfo$metadata$lower_left[1],
    kelpArea = getArea(zooList)
	)
}
	


###### Area of one selected area of kelp in an image
getOneArea <- function(p){
  
  #create a path around the polygon using absolute xy points
  #zooniverse gives it to us in relative points, but includes starting values
  s <- as.numeric(p$startingPoint)
  s <- sapply(s, function(x) if(is.na(x)){return(0)}else{return(x)})
  x <- s[1]
  y <- s[2]
  for(i in 2:length(p$relPath)){
    p$relPath[i][[1]] <- as.numeric(p$relPath[i][[1]])
    x[i] <- x[i-1] + p$relPath[i][[1]][1]
    y[i] <- y[i-1] + p$relPath[i][[1]][2]
  }
  x[length(p$relPath)] <- x[1]
  y[length(p$relPath)] <- y[1]
  
  #turn it into a sp::Polygon
  w <- Polygon(cbind(x,y))
  
  #area from the area slot
  w@area
}

#Get the area of all polygons with kelp in them using the sp library
getArea <- function(imageWithPaths){

  #if there is no kelp selected, return 0
  if(class(imageWithPaths$annotations[[2]][2]) != "list") return(0)
  
  #if there is kelp selected, 
  polys <- names(imageWithPaths$annotations[[2]]$value)
  
  areas <- sapply(polys, function(i) getOneArea(imageWithPaths$annotations[[2]]$value[[i]]))
  
  sum(areas)
 
}


#What is the data file we're using?
jsonFile <- "./kelp-classifications.json"
#jsonFile <- "./test.json"

#how many lines of data do we have?
#n <- 10
n <- countLines(jsonFile)

#open a connection to the file
jsonData <- file(jsonFile, "r")

#create a blank list that the data will go into
firstLine <- parseZooIn(fromJSON(readLines(jsonData, 1)))

kelpdata <- as.matrix(firstLine)
kelpdata <- rbind(kelpdata, matrix(NA, nrow=n-1, ncol=ncol(firstLine)))
#kelpdata <- data.frame(kelpdata)
#names(kelpdata) <- names(firstLine)
#kelpdata[1,] <- firstLine

#q is a qounter so I can tell here in j I should qatch an error
q <- 1
#read in the file
for (i in 2:n){
  q <- q+1
  newline <- (fromJSON(readLines(jsonData, 1)))
  kelpdata[i,] <- as.matrix(parseZooIn(newline))
}

#close the file connection
close(jsonData)

kelpdata <- data.frame(kelpdata)

#fix the dates up for ease of later use
library(lubridate)

d <- ymd_hms(as.character(kelpdata$date))
kelpdata$Year <- year(d)
kelpdata$month <- month(d)

#write the resulting file
write.csv(kelpdata, "zookelpdata20140918.csv", row.names=F)

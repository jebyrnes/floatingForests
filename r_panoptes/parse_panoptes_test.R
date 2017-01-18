source("./FF_panoptes_methods.R")

test_data <- read_csv("../panoptes_data/floating-forest-relaunch-classifications.csv") %>%
  group_by(user_name, subject_ids) %>%
  slice(1L) %>%
  ungroup()

test_subjects <- read_csv("../panoptes_data/floating-forest-relaunch-subjects.csv")


head(test_data)
names(test_data)

#let's look at one image  
aframe <- subset(test_data, test_data$subject_ids==test_data$subject_ids[1])

#Do the polygons we create line up with an image?
test_img <- rasterizeFFImage(aframe[1,])
plotRGB(test_img)

test_polys <- getSpatialPolysDataFrameForOneImage(aframe)
plot(test_polys, add=T)

#can we create a list of spatialPolygonsData Frames
test_all <- map(unique(test_data$subject_ids), 
                  ~getSpatialPolysDataFrameForOneImage(filter(test_data, subject_ids==.)))

names(test_all) <- unique(test_data$subject_ids)

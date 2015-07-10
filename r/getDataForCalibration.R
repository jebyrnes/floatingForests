###################################
# File to create a dataset that can be used
# for calibration of the FF Dataset
#
# Jarrett Byrnes jarrett.byrnes@umb.edu
#
# Changelog
#
###################################
source("./FFmethods.R")
source("./parseZooData.R")


hasPaths <- kelpzoo %>% filter(nchar(relPath)>0)

#get the number of classifications for each image from CA
#Get the IDs of the good images from CA we want
sampledSubjectsFullCA <- kelpzoo %>% 
  filter(lower_left_y>0) %>%
  group_by(subject_zooniverse_id) %>%
  summarise(nClassifications=length(unique(user_name)))%>% 
  filter(nClassifications>=15)



#extract the lines of data we'll be working with
fullyClassifiedCA <- kelpzoo %>%
  filter(subject_zooniverse_id %in% sampledSubjectsFullCA$subject_zooniverse_id)

fullyClassifiedCA <- join(fullyClassifiedCA, sampledSubjectsFullCA)


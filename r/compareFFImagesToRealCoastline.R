####
#load at the FF data and get 5 images from CA and 5 from tazzie
####
source("./FFmethods.R")
source("./parseZooData.R")
hasPathsCA <- kelpzoo %>% filter(nchar(relPath)>0) %>% 
  filter(upper_right_y>0)

numPathsCA <- hasPathsCA %>%
  group_by(subject_zooniverse_id) %>%
  summarise(n=length(unique(user_name))) %>%
  ungroup() %>%
  filter(n>13)


hasPathsTaz <- kelpzoo %>% filter(nchar(relPath)>0) %>% 
  filter(upper_right_y<0)

numPathsTaz <- hasPathsTaz %>%
  group_by(subject_zooniverse_id) %>%
  summarise(n=length(unique(user_name))) %>%
  ungroup() %>%
  filter(n>13)

caSubj <- unique(numPathsCA$subject_zooniverse_id)
tazSubj <- unique(numPathsTaz$subject_zooniverse_id)

caImg <- hasPathsCA %>% filter(subject_zooniverse_id %in% caSubj[1:5]) %>%
  group_by(subject_zooniverse_id) %>%
  summarise(lower_left_x=lower_left_x[1], lower_left_y=lower_left_y[1],
            upper_right_x=upper_right_x[1], upper_right_y=upper_right_y[1])

tazImg <- hasPathsTaz %>% filter(subject_zooniverse_id %in% tazSubj[1:5]) %>%
  group_by(subject_zooniverse_id) %>%
  summarise(lower_left_x=lower_left_x[1], lower_left_y=lower_left_y[1],
            upper_right_x=upper_right_x[1], upper_right_y=upper_right_y[1])


Imgs <- rbind(caImg, tazImg)

rm(kelpzoo)

####
#load at the real coastline
####
library(rgdal)
coasts <- readOGR("../data/gshhg-shp-2.3.4/GSHHS_shp/h", layer="GSHHS_h_L1",  useC=FALSE)


####
#Plot coasts & images
####

for(i in 4:nrow(Imgs)){
  #tell us things are working
  print(i)
  
  #subset
  arow <- Imgs[i,]

  #get the bounding box
  lims <- as.numeric(arow[c(2,4,3,5)])
  limsPaste <- paste(lims, collapse="_")
  
  #get the image
  tileBrick <- rasterizeFFImage(arow)
  
  #start a jpg
  jpeg(paste0("../output/coastline_matchup/", arow$subject_zooniverse_id, 
              "_", limsPaste, ".jpg"),
  width=800, height=800)
  
  #plot the image
  plotRGB(tileBrick)
  
  #plot the cropped coastline on top
  coastCrop <- crop(coasts, extent(tileBrick))
  plot(coastCrop, add=T, lwd=4)
  #plot(coasts, xlim=lims[1:2], ylim=lims[3:4], lwd=4, add=T)
  
  #close the jpg
  dev.off()
}

source("./FFmethods.R")
source("./parseZooData.R")
#oneImage <- read.csv("../data/AKP00016e6.csv")

subjects <- unique(kelpzoo$subject_zooniverse_id)

getScene <- function(subj){
  getImageInfo(subj)$metadata$file
}


scenes <- sapply(subjects,function(x){
  print(x)
 try( getScene(x) )
} )

scenes <- unique(scenes)
scenes <- gsub('.tar.gz', '', scenes)

#rm(kelpzoo)

lsMeta <- read_csv("../../landsat_metadata/landsat_metadata_selected.csv")

lsMeta2 <- lsMeta %>% filter( sceneID %in% scenes)


write.csv(lsMeta2, "../data/landsat_scenes_filtered_metadata.csv", row.names=F)

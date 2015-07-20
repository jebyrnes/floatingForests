#Load functions of use
source("./FFmethods.R")

#Load polysData to get corners, etc
load("../data/oneImagesAKP00016e6.Rdata")

#extract metadata
imageInfo <- getImageInfo("AKP00016e6")
scene <-imageInfo$metadata$base_file_name
lower_left <- imageInfo$metadata$lower_left #offset of row and col num
upper_right <- imageInfo$metadata$upper_right

#get row, col info from meta-data
row_no <- imageInfo$metadata$row_no
col_no <- imageInfo$metadata$col_no

#I know from the image processing how a full scene is handled
n_row = 20
n_cols = 20

#I know row and col # of this image
row_no <- imageInfo$metadata$row_no
col_no <- imageInfo$metadata$col_no

#I can calculate central lat/long of the image
cent <- c((lower_left[1] + upper_right[1])/2, 
          (lower_left[2] + upper_right[2])/2)

#I know image size in pixels from the imageProcessing script
scene_y_pixels <- 7981 
scene_x_pixels <- 7271

#i can calculate central pixels in the geotiff
cent_y_pixel <- (scene_y_pixels / 20 * row_no) 
cent_x_pixel <- (scene_x_pixels / 20 *col_no)


#I can calculate image size of any image from the imageprocessing
#script - oddly, this is different than the jpgs on FF
image_chunk_width = scene_x_pixels / n_row
image_chunk_height = scene_y_pixels / n_cols

#with this info, I can calculate the pixel coordinates
#of the corners within the scene
ll_pixels <- c(cent_x_pixel-image_chunk_width/2, 
               cent_y_pixel-image_chunk_height/2)
ur_pixels <- c(cent_x_pixel+image_chunk_width/2, 
               cent_y_pixel+image_chunk_height/2)

#i can calculate rx = cdx/cpx
#which is a conversion factor of degrees per pixel
conv_deg <- abs(c((upper_right[1] - lower_left[1])/image_chunk_width, 
                  (upper_right[2] - lower_left[2])/image_chunk_height))

#I can calculate the scene xmin and ymin in degrees
#cdx = cpx*d/p + xd0
scene_ll <- c(cent[1]-cent_x_pixel*conv_deg[1],
              cent[2]-cent_y_pixel*conv_deg[2])

scene_ur <- c(scene_ll[1]+scene_x_pixels*conv_deg[1], 
              scene_ll[2]+scene_y_pixels*conv_deg[2])

#OK, now make a raster that is the same size that the
#geotiff was
library(raster)
ascene <- raster(nrow=scene_y_pixels, ncol=scene_x_pixels, 
                 xmn=scene_ll[1], xmx=scene_ur[1], 
                 ymn=scene_ll[2], ymx=scene_ur[2])

#give it a projection - in this case, for CA
#using details from http://landsat.usgs.gov/Landsat_Processing_Details.php
#GeoTIFF output format
#Cubic Convolution (CC) resampling method
#30-meter (TM, ETM+) and 60-meter (MSS) pixel size (reflective bands)
#Universal Transverse Mercator (UTM) map projection (Polar Stereographic projection for scenes with a center latitude greater than or equal to -63.0 degrees)
#World Geodetic System (WGS) 84 datum
#MAP (North-up) image orientation
#projection(ascene) <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
projection(ascene) <-  "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

newx <- xFromCol(ascene, c(ll_pixels[1], ur_pixels[1]))
newy <- yFromRow(ascene, c(ll_pixels[2], ur_pixels[2]))

ptsReproj <- spTransform(SpatialPoints(cbind(newx, newy), 
                          proj4string=CRS(projection(ascene))),
            CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
newx <- attr(ptsReproj, "coords")$newx
newy <- attr(ptsReproj, "coords")$newy

list(ll=c(newx[1], newy[1]), ur = c(newx[2], newy[2]))
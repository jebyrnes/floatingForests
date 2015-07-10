###################################
# File to parse a floating forests data file
# for easy analysis
#
# Jarrett Byrnes jarrett.byrnes@umb.edu
#
# Changelog
#
###################################

library(dplyr)
library(readr)
library(lubridate)

kelpzoo <- read_csv("../data/2015-06-30_kelp_classifications 2.csv",
                    col_types="cccccddddcccdddddddccddd")


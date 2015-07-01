###################################
# File to Analyze distribution of contributions
# Jarrett Byrnes jarrett.byrnes@umb.edu
#
# Changelog
#
###################################

source("./parseZooData.R")

#What are the contributions of individual users?
ffUsers <- kelpzoo %>% group_by(user_name) %>%
  summarise(n=n()) %>% ungroup()

#plot a histogram
library(ggplot2)
ggplot(data=ffUsers) + geom_bar(mapping=aes(x=n)) +
  scale_x_log10()

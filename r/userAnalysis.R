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
<<<<<<< HEAD
  summarise(n=length(unique(subject_zooniverse_id))) %>% ungroup()
=======
  summarise(n=n()) %>% ungroup()
>>>>>>> a9908e5ed42054bd08a0eb729a8d6536192e6254

#plot a histogram
library(ggplot2)
ggplot(data=ffUsers) + geom_bar(mapping=aes(x=n)) +
  scale_x_log10()
<<<<<<< HEAD

#treemap! from https://zoobackchannel.wordpress.com/2013/05/01/treemaps-of-volunteer-contributions/
library(treemap)
pdf(file="classifications_per_user.pdf",width=1024,height=768,pointsize=10)
treemap(as.data.frame(ffUsers), index='user_name', vSize='n', vColor='user_name',
       type='index',fontsize.labels=0,lowerbound.cex.labels=1,title='')
dev.off
=======
>>>>>>> a9908e5ed42054bd08a0eb729a8d6536192e6254

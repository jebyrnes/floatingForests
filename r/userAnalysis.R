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
  summarise(n=length(unique(subject_zooniverse_id))) %>% ungroup()

#plot a histogram
library(ggplot2)
library(scales)

options(scipen=999)#supress scientific notation
pdf(file="../output/classifications_per_user_hist.pdf")
ggplot(data=ffUsers) + geom_bar(mapping=aes(x=n), binwidth=.01) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
  theme_minimal(base_size=17) +
  xlab("Number of Classifications") +
  ylab("Number of Users")
dev.off()

#treemap! from https://zoobackchannel.wordpress.com/2013/05/01/treemaps-of-volunteer-contributions/
library(treemap)
pdf(file="../output/classifications_per_user.pdf",width=128,height=96,pointsize=10)
treemap(as.data.frame(ffUsers), index='user_name', vSize='n', vColor='user_name',
       type='index',fontsize.labels=0,lowerbound.cex.labels=1,title='')
dev.off()


#who are the top few?
ffUsers %>% arrange(desc(n))
ffUsers %>% filter(user_name=="jebyrnes")

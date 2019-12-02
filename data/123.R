
# Author: Pingxin Gao
# Created: 25 Nov, 2019
# data source: http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt

# import packages
library(dplyr)
library(stringr)
library(GGally)
library(corrplot)
library(ggplot2)
setwd('/Users/edward/IODS-project/data')


library(FactoMineR)
library(tidyr)
# load the data



data("tea")

glimpse(tea)

cha <- dplyr::select(tea, one_of(c('Tea','How','sugar','where','healthy','sex')))
glimpse(cha)


ggplot(gather(cha), aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
cha_mca <- MCA(cha, graph = FALSE)
summary(cha_mca)


par(mfrow = c(1,3)) # Set some graphical params.
plot(cha_mca, choix = "var", title = "MCA variables") # The variable biplot.
plot(cha_mca, choix = "ind", invisible = "var") # The individuals plot.
plot(cha_mca, choix = "ind", invisible = "ind") # The categories plot.



# Author: Pingxin Gao
# Created: 25 Nov, 2019
# data source: http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt

# import packages
library(dplyr)
library(stringr)

## Data wranging (exercise 4) ##

# Set working directory.
setwd('/Users/edward/IODS-project/data')

# Read in the data.
hd <- as.data.frame(read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F))
gii <- as.data.frame(read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = ".."))

# Explore structure and dimensions of the data frames with glimpse().
glimpse(hd)
glimpse(gii)

# Summarise the variables.
summary(hd)
summary(gii)

# Shorten column names. There are several methods for this. For details, see:
# http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/
names(hd)[1] <- 'hdi_r'
names(hd)[2] <- 'country'
names(hd)[3] <- 'hdi'
names(hd)[4] <- 'life_exp'
names(hd)[5] <- 'edu_exp'
names(hd)[6] <- 'edu_mean'
names(hd)[7] <- 'gni_cap'
names(hd)[8] <- 'gni_r_sub_hdi_r'
names(gii)[1] <- 'gii_r'
names(gii)[2] <- 'country'
names(gii)[3] <- 'gii'
names(gii)[4] <- 'mmr'
names(gii)[5] <- 'abr'
names(gii)[6] <- 'mp_share'
names(gii)[7] <- 'se_f'
names(gii)[8] <- 'se_m'
names(gii)[9] <- 'lfp_f'
names(gii)[10] <- 'lfp_m'

# Extend the GII DF with some new variables.
gii <- mutate(gii, se_f_of_m = se_f/se_m)
gii <- mutate(gii, lfp_f_of_m = lfp_f/lfp_m)

# Join the datasets.
human <- inner_join(hd, gii, by = 'country')

# Write the joined DF to a file.
write.table(human, file = "human.csv", sep = "\t", col.names = TRUE)


# Author: Pingxin Gao
# Created: 2 Dec, 2019

## Data wranging (exercise 5) ##


# Mutate gni_cap
human <- mutate(human, gni_cap = as.numeric(str_replace(human$gni_cap, pattern=",", replace ="")))

# Exclude variables
human <- select(human, one_of('country','se_f_of_m','lfp_f_of_m','edu_exp','life_exp','gni_cap','mmr','abr','mp_share'))

# Remove all rows with missing values
human <- na.omit(human)

# Remove the observations which relate to regions instead of countries.
human <- head(human, -7)

# Define the row names of the data by the country names and remove the country name column from the data. 
rownames(human) <- human$country
human <- human[,-1]


# Overwrite the data that was written in the end of Exercise 4.
write.table(human, file = "human.csv", sep = "\t", col.names = TRUE)





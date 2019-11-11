# R Script for web course Introduction to Open Data Science
# RStudio Exercise 2.
# 
# Author: Pingxin Gao
# Created: Mon 11 Nov, 2019

########################
## Data wranging part ##
########################

library(dplyr)

lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

dim(lrn14)
# [1] 183  60

str(lrn14)
# 'data.frame':	183 obs. of  60 variables:
# $ Aa      : int  3 2 4 4 3 4 4 3 2 3 ...
# $ Ab      : int  1 2 1 2 2 2 1 1 1 2 ...
# $ Ac      : int  2 2 1 3 2 1 2 2 2 1 ...
# $ Ad      : int  1 2 1 2 1 1 2 1 1 1 ...
# $ Ae      : int  1 1 1 1 2 1 1 1 1 1 ...
# $ Af      : int  1 1 1 1 1 1 1 1 1 2 ...
# $ ST01    : int  4 4 3 3 4 4 5 4 4 4 ...
# $ SU02    : int  2 2 1 3 2 3 2 2 1 2 ...
# $ D03     : int  4 4 4 4 5 5 4 4 5 4 ...
# ...

# questions related to deep, surface and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# Add averaged variables to the data frame.
new_lrn14$deep <- rowMeans(select(lrn14, one_of(deep_questions)))
new_lrn14$stra <- rowMeans(select(lrn14, one_of(surface_questions)))
new_lrn14$surf <- rowMeans(select(lrn14, one_of(strategic_questions)))

new_lrn14 <- select(lrn14, one_of(c('Age', 'Attitude', 'Points', 'gender')))

# Convert all analysis DF variable names to lowercase:
names(new_lrn14) <- tolower(names(new_lrn14))

new_lrn14 <- filter(new_lrn14, points > 0)
dim(new_lrn14)
# 166   7

write.csv(new_lrn14, "learning2014.csv", row.names = FALSE) # generate csv file

a <- read.csv("learning2014.csv") # read csv file
dim(a)
# 166   7

head(a, n = 3) # Show the first ten rows of the newly created DF.
# gender age attitude     deep  stra     surf points
# 1       F  53       37 3.583333 3.375 2.583333     25
# 2       M  55       31 2.916667 2.750 3.166667     12
# 3       F  49       25 3.500000 3.625 2.250000     24

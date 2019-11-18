# RStudio Exercise 3.
# Author: Pingxin Gao
# Created: Nox 18 2019


########################
## Data wranging part ##
########################

library(dplyr)

# Set working directory.
setwd('/Users/edward/IODS-project/data/')

# Read data
math <- as.data.frame(read.table('./student-mat.csv', sep=';', header = TRUE))
por <- as.data.frame(read.table('./student-por.csv', sep=';', header = TRUE))

# Explore structure and dimensions
glimpse(mat)
glimpse(por)

# Create an inner join of the math and portugese tables on fields
# defined by the exercise instructions.
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by, suffix = c("_m", "_p"))

# Explore structure and dimensions of the DF of the joined data.
glimpse(alc)

# print out the column names of 'math_por'
colnames(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# columns that were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns


# (This is method 'a' of the instructions!)
# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# Calculate the average (of weekends and weekdays) alcohol consumption.
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# Define a logical column of high use.
alc <- mutate(alc, high_use = alc_use > 2)

# Explore structure and dimensions of the DF with glimpse().
glimpse(alc)

# Calculate the average (of weekends and weekdays) alcohol consumption.
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# Define a logical column of high use.
alc <- mutate(alc, high_use = alc_use > 2)

# Explore structure and dimensions of the DF with glimpse().
glimpse(alc)

# Write the DF to a file.
write.table(alc, file = "alc.csv", sep = "\t", col.names = TRUE)






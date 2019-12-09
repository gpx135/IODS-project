
# Access the packages dplyr and tidyr
library(dplyr)
library(tidyr)


BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep  ="\t", header = T)


# Look at the structure of BPRS & RATS
str(BPRS)
str(RATS)

# Print out summaries of the variables
summary(BPRS)
summary(RATS)

# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# Convert the data sets to long form.
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
RATSL <-  RATS %>% gather(key = times, value = rats, -ID, -Group)

# Add a week variable to BPRS and a Time variable to RATS.
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))
RATSL <-  RATSL %>% mutate(time = as.integer(substr(times,3, 4)))

glimpse(BPRSL)
glimpse(RATSL)


summary(BPRSL)
summary(RATSL)




################################
## Essential Packages
################################

library(tibble)
library(tidyverse)
library(dplyr)

################################
## Data Pre-processing and Cleaning
################################

# Convert to tibble datatype
data <- as_tibble(read.csv("data.csv"))
label <- as_tibble(read.csv("labels.csv"))

# Quick Snapshot of the first few entries of our dataframe
str(data)
str(label)
head(data)
head(label,5)

# Finds the Unqiue categories in the "Class" column of the label dataframe
unique(label[ , 2])

# Convert the categories from character to factor type
label$Class <- as.factor(label$Class)
label$X <- as.factor(label$X)
data$X <- as.factor(data$X)

# Finds the Dimensions of the label and data datatypes
dim(label) # 801  X  2
dim(data) # 801 X 20532

# Finds the Column names
data.frame(colnames(data))

# Calculates the Summary Statistics
(describe <- summary (data[  , 2:dim(data)[2]] ))

# Merge both the dataframes together
df <- as_tibble(cbind(label,data[-1]))
dim(df) # 801 × 20533

# Checks for any null entries
sum(is.null(df)) # sum is 0 implies no  NULL entries

# Find the total NA values in data frame
sum(is.na(df)) # sum is 0 implies no NA's

# Find the total NA values by column
sapply(df, function(x) sum(is.na(x)))

# Checks to see any genes has duplicated values or error values
duplicates <- df[duplicated(df), ]
print(duplicates) # 0 × 20,533

# Save the merged df to ".Rdata" format
save(df,file="ICMR.Rdata")



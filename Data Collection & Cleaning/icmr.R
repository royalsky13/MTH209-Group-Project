# Essential libaries:
library(tibble)
library(tidyverse)
library(dplyr)

# Importing Data and Data Preprocessing:

# Convert to tibble datatype
data <- as_tibble(read.csv("data.csv"))
label <- as_tibble(read.csv("labels.csv"))
# Quick Snapshoot of the first few entries of our dataframe
str(data)
str(label)
head(data)
head(label,5)
# Unqiue categories in the Class column of the label df
unique(label[ , 2])
# Converting the categories from character to factor type
label$Class <- as.factor(label$Class)
label$X <- as.factor(label$X)
data$X <- as.factor(data$X)
# Finding the Dimensions of the label and data datatypes
dim(label) # 801  X  2
dim(data) # 801 X 20532
# Column names of data and label
data.frame(colnames(data))
# Summary Statistics
(describe <- summary (data[  ,2:dim(data)[2]]))
# Merge both the dataframes
df <- as_tibble(merge(label,data,by="X"))
dim(df) # 801 × 20533
# Checks for any null entries
sum(is.null(df)) # sum is 0 implies no  NULL entries
# Find total NA values in data frame
sum(is.na(df)) # sum is 0 implies no NA's
# Find total NA values by column
sapply(df, function(x) sum(is.na(x)))
# Check to see any genes has duplicated values or error values
duplicates <- df[duplicated(df), ]
print(duplicates) # 0 × 20,533
# Save merged df to .Rdata format
save(df,file="ICMR.Rdata")



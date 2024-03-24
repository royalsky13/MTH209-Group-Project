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

#########################################################################################
#### Gender-wise different sites of cancer dataset:
url <- read_html("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10231735/table/ijmr_1821_22-t001/?report=objectonly")
df <- url %>% html_table(); df <- df[[1]]
df <- df[-c(1), ]
# Dataframe 1 (male):
dat1 <- df[,c(1,2,3,4,5)]; dat1 <- dat1[3:dim(dat1)[1] ,]
colnames(dat1) <- c("Site","Cases", "CR", "AAR", "Cum-risk")

dat1$Site <- as.factor(dat1$Site)

dat1$Cases <- gsub(",","",dat1$Cases)
dat1$Cases <- gsub("-",NA,dat1$Cases)
dat1$Cases <- as.numeric(dat1$Cases)

dat1$CR <- gsub(",","",dat1$CR)
dat1$CR <- gsub("-",NA,dat1$CR)
dat1$CR <- as.numeric(dat1$CR)

dat1$AAR <- gsub(",","",dat1$AAR)
dat1$AAR <- gsub("-",NA,dat1$AAR)
dat1$AAR <- as.numeric(dat1$AAR)
dat1$Gender <- 0 # 0 for male

# Dataframe 2 (female):
dat2 <- df[,c(1,6,7,8,9)]; dat2 <- dat2[3:dim(dat2)[1] ,]
colnames(dat2) <- c("Site","Cases", "CR", "AAR", "Cum-risk")

dat2$Site <- as.factor(dat2$Site)

dat2$Cases <- gsub(",","",dat2$Cases)
dat2$Cases <- gsub("-",NA,dat2$Cases)
dat2$Cases <- as.numeric(dat2$Cases)

dat2$CR <- gsub(",","",dat2$CR)
dat2$CR <- gsub("-",NA,dat2$CR)
dat2$CR <- as.numeric(dat2$CR)

dat2$AAR <- gsub(",","",dat2$AAR)
dat2$AAR <- gsub("-",NA,dat2$AAR)
dat2$AAR <- as.numeric(dat2$AAR)

dat2$Gender <- 1 # 1 for female

# Dataframe 3 (male & female):
dat3 <- df[,c(1,10,11,12,13)]; dat3 <- dat3[3:dim(dat3)[1] ,]
colnames(dat3) <- c("Site","Cases", "CR", "AAR", "Cum-risk")

dat3$Site <- as.factor(dat3$Site)

dat3$Cases <- gsub(",","",dat3$Cases)
dat3$Cases <- gsub("-",NA,dat3$Cases)
dat3$Cases <- as.numeric(dat3$Cases)

dat3$CR <- gsub(",","",dat3$CR)
dat3$CR <- gsub("-",NA,dat3$CR)
dat3$CR <- as.numeric(dat3$CR)

dat3$AAR <- gsub(",","",dat3$AAR)
dat3$AAR <- gsub("-",NA,dat3$AAR)
dat3$AAR <- as.numeric(dat3$AAR)

dat3$Gender <- 2 # 2 for both male and female

# Merge
dat <- rbind(dat1,dat2,dat3)
dat$`Cum-risk` <- gsub (",","",dat$`Cum-risk`)
dat$`Cum-risk` <- gsub("1 in ", "", dat$`Cum-risk`)
dat$`Cum-risk` <- gsub("-", NA, dat$`Cum-risk`)
dat$`Cum-risk` <- as.numeric(dat$`Cum-risk`)
dat$`Cum-risk` <- 1/dat$`Cum-risk`

# Save the merged df to ".Rdata" format
save(dat,file="Cancer_Sites.Rdata")






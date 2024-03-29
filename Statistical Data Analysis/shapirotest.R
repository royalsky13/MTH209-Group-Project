################################
## Essential Packages
################################
library(tibble)
library(rvest)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stats)
library(gplots)

################################
## Load the data
################################
rm(list = ls())
ls()
load("ICMR.Rdata")
ls()
View(df)


################################
## Perform the one way ANOVA test
################################

# Features
X <- as.matrix(df[ , -c(1,2)]) 

# Target variable
y <- df$Class  

# Scale the features
X_scaled <- scale(X)

# Create an empty data frame to store the results


# Perform hypothesis testing for each gene
shapiro_test_results <- list()

# Perform ANOVA for each gene
for (i in 1:ncol(X_scaled)) 
{
  gene_expression <- X_scaled[, i]
  # Skip if there are missing values
  if (any(is.na(gene_expression))) 
  {
    next
  }
  # Skip the constant genes
  if (sd(gene_expression) == 0) 
  {
    next
  }
  # Create group expressions
  #group_expressions <- split(gene_expression, y)
  # anova_result variable is a list object, so we need to access the outer list!
  # Store the results only if p-value < 0.05 i.e.
  # if the null hypothesis, H0 gets rejected)
  test_result <- shapiro.test(gene_expression)
  shapiro_test_results <- rbind(shapiro_test_results, 
                                data.frame(Group = paste("gene", i, sep = "_"), 
                                           W = test_result$statistic, 
                                           p_value = test_result$p.value))
  
}
print(shapiro_test_results, row.names = FALSE)

# Extract the p-values from the result_df data frame
p_values <- shapiro_test_results$p_value

# Apply multiple testing correction
corrected_pvals <- p.adjust(p_values, method = "fdr")

# Update result_df$p_value with corrected p-values
shapiro_test_results$p_value <- corrected_pvals

# Determine rejected hypotheses based on corrected p-values
rejected <- corrected_pvals < 0.05

# Add 'rejected' column to result_df
shapiro_test_results$rejected <- rejected

# Select genes based on variance and save to .csv file
p_values_sorted <- shapiro_test_results %>% arrange(desc(p_values))
write.csv(p_values_sorted,file="shapiro.csv",row.names = FALSE)

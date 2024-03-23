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
result_df <- data.frame(
  Gene = character(),
  F_statistic = numeric(),
  p_value = numeric(),
  variance = numeric(),
  stringsAsFactors = FALSE
)

# Perform hypothesis testing for each gene
anova_results <- list()

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
  group_expressions <- split(gene_expression, y)
  # anova_result variable is a list object, so we need to access the outer list!
  # Store the results only if p-value < 0.05 i.e.
  # if the null hypothesis, H0 gets rejected)
  anova_result <- summary(aov(formula = gene_expression ~ y))
  if (anova_result[[1]]$`Pr(>F)`[1] < 0.05) 
    { 
    # Extract F-statistic and p-value
    f_statistic <- anova_result[[1]]$`F value`[1]
    p_value <- anova_result[[1]]$`Pr(>F)`[1]
    variance <- var(X[,i-1])

    # Extract gene name
    gene_name <- paste("gene", i-1, sep = "_")
    
    # Add results to data frame
    result_df <- rbind(result_df, data.frame(Gene = gene_name, F_statistic = f_statistic, p_value = p_value,variance = variance))
  }
}
print(result_df, row.names = FALSE)

# Extract the p-values from the result_df data frame
p_values <- result_df$p_value

# Apply multiple testing correction
corrected_pvals <- p.adjust(p_values, method = "fdr")

# Update result_df$p_value with corrected p-values
result_df$p_value <- corrected_pvals

# Determine rejected hypotheses based on corrected p-values
rejected <- corrected_pvals < 0.05

# Add 'rejected' column to result_df
result_df$rejected <- rejected

# Select genes based on variance and save to .csv file
variances_sorted <- result_df %>% arrange(desc(variance))
write.csv(variances_sorted,file="One way ANOVA.csv",row.names = FALSE)

# Extract the top 3 and the bottom 3 genes from the high_variance_genes list
selected_genes <- c(head(variances_sorted, 3)$Gene, tail(variances_sorted, 3)$Gene)

# Print selected genes
cat("Selected genes for analysis:", selected_genes, "\n")

# Define function for "one vs. all" t-tests
unique_cancer_types <- unique(df$Class)
p_values <- c()

for (i in 3:ncol(df)) {
  gene <- colnames(df)[i]
  
  for (cancer_type in unique_cancer_types) {
    # Subset data for the specific cancer type
    group1 <- df[df$Class == cancer_type, i]
    # Subset data for all other cancer types combined
    group2 <- df[df$Class != cancer_type, i]
    
    # Check if both groups have enough observations
    if (length(group1) > 1 && length(group2) > 1) {
      # Perform the t-test
      t_test_result <- t.test(group1, group2, var.equal = FALSE)
      # Store the p-value
      p_values <- c(p_values, t_test_result$p.value)
      cat(paste(cancer_type, "vs All for", gene, ": p-value =", t_test_result$p.value, "\n"))
    } else {
      cat("Not enough observations for", cancer_type, "vs All for", gene, "\n")
    }
  }
}

# Apply FDR correction to the p-values
corrected_p_values <- p.adjust(p_values, method = 'fdr')

# Output corrected p-values
cat("Corrected p-values:", corrected_p_values, "\n")
  























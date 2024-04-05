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

################################
## Histogram of Variance Range
################################
vars <- apply (df[-c(1,2)],2,var)
# Print min, max, and mean variances
cat("min value of Variance:", min(vars), "\n")
cat("max value of Variance:", max(vars), "\n")
cat("mean value of Variance:", mean(vars), "\n")
# Calculate the range of variances
variance_range <- max(vars) - min(vars)
# Histogram
ggplot(data.frame(Variance = vars), aes(x = Variance)) +
  geom_histogram(bins = 20, fill = 'pink', color = 'black') +
  labs(x = 'Variance Range', y = 'Frequency', title = 'Histogram of Variance Range') +
  theme_minimal()
# Print the number of columns with variance comapred to the threshold
cat("no. of columns with variance greater than or equal to certain threshold:", length(vars[vars>=20]), "\n")
cat("no. of columns with variance less than or equal to certain threshold:", length(vars[vars<=0.2]), "\n")

################################
## Calculate the average expression for each gene across all samples
################################

gene_names_df <- as.tibble(colnames(df)[-c(1,2)])
colnames(gene_names_df) <- "Gene"
gene_names_df$Gene <- as.factor(gene_names_df$Gene)
average_sorted_gene_expression <- gene_names_df %>% mutate(Expression=apply(df[-c(1,2)], 2, mean)) %>% arrange(desc(Expression))


################################
## Perform the one way ANOVA test
################################

# Features
X <- as.matrix(df[ , -c(1,2)]) 

# Groups
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
  # if the null hypothesis, H0 gets rejected
  anova_result <- summary(aov(formula = gene_expression ~ y))
  if (anova_result[[1]]$`Pr(>F)`[1] < 0.05) 
    { 
    # Extract F-statistic and p-value
    f_statistic <- anova_result[[1]]$`F value`[1]
    p_value <- anova_result[[1]]$`Pr(>F)`[1]
    variance <- var(X[,i])

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

# Update result_df with corrected p-values
result_df$p_value <- corrected_pvals
result_df <- result_df[result_df$p_value<0.05, ]

# Select genes based on variance
variances_sorted <- result_df %>% arrange(desc(variance))

# Extract the top 3 and the bottom 3 genes from the high_variance_genes list
selected_genes <- c(head(variances_sorted, 3)$Gene, tail(variances_sorted, 3)$Gene)

# Print selected genes
cat("Selected genes for analysis:", selected_genes, "\n")

################################
## Perform the One vs. All t-test
################################
perform_one_vs_all_ttests <- function (df, gene)
{
  unique_cancer_types <- unique(df$Class)
  for (cancer_type in unique_cancer_types)
  {
    group1 <- df[df$Class==cancer_type,gene]
    group2 <- df[df$Class!=cancer_type,gene]
    t_test_result <- t.test(group1, group2, var.equal = FALSE)
    p_val <- p.adjust(t_test_result$p.value,method="fdr")
    cat(paste(cancer_type, "vs. All for", gene, ": p-value =", format(p_val, digits = 8)), "\n")
  }
  corrected_p_values <- p.adjust(p_values, method = "fdr")
  return(corrected_p_values)
  }
for (gene in selected_genes) {
  cat(paste("Performing 'one vs. all' t-tests for", gene, ":\n"))
  corrected_p_values <- perform_one_vs_all_ttests(df, gene)
}
  
##############################################################################

################################
## Load the data
################################
rm(list = ls())
ls()
load("Cancer_Sites.Rdata")
ls()

# Summary Statistics
summary_stats <- dat %>%
  group_by(Gender) %>%
  summarise(
    mean_cases = mean(Cases, na.rm = TRUE),
    mean_CR = mean(CR, na.rm = TRUE),
    mean_AAR = mean(AAR, na.rm = TRUE),
    mean_cumrisk = mean(`Cum-risk`, na.rm = TRUE))e


gender_labels <- c("0" = "Male", "1" = "Female", "2" = "Both")

# Define custom colors for genders
gender_colors <- c("0" = "blue", "1" = "red", "2" = "green")

# Boxplot of Cases by Gender
ggplot(dat, aes(x = factor(Gender), y = Cases, fill = factor(Gender))) +
  geom_boxplot() + scale_x_discrete(labels = gender_labels) +
  scale_fill_manual(values = gender_colors, labels = gender_labels) +  # Apply custom colors and labels
  labs(title = "Boxplot of Cases by Gender", x = "Gender", y = "Cases") +
  theme_minimal() +  # Minimalistic theme
  theme(plot.title = element_text(hjust = 0.5))  # Centered title



# Mean CR by Gender
mean_CR_comparison <- dat %>% group_by(Gender) %>% summarise(mean_CR = mean(CR, na.rm = TRUE))

# Identifying High-Risk Sites
threshold <- 0.01  # We set this!
# Filter organs with Cum-risk greater than the threshold
high_risk_organs_female <- dat %>% filter(Gender == 1 & `Cum-risk` > threshold)
high_risk_organs_male <- dat %>% filter(Gender == 0 & `Cum-risk` > threshold)
high_risk_organs_both <- dat %>% filter(Gender == 2 & `Cum-risk` > threshold)





















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
## Visualise the Distribution of cancer types
################################

my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
ggplot(data = df, aes(x = Class, fill = Class)) +
  geom_bar(color = "black", fill = my_colors) +  # Add black border around bars
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 3, color = "black") +  # Adjust text size, color, and position
  labs(title = "Distribution of Cancer Types", x = "Class", y = "Count") +  # Add meaningful titles to axes
  theme_minimal()  # Apply a minimal theme 

################################
## Calculate the average expression for each gene across all samples
################################

gene_names_df <- as.tibble(colnames(df)[-c(1,2)])
colnames(gene_names_df) <- "Gene"
gene_names_df$Gene <- as.factor(gene_names_df$Gene)
average_gene_expression <- gene_names_df %>% mutate(Expression=apply(df[-c(1,2)], 2, mean))

################################
## Plot the distribution of average expression levels
################################

ggplot(average_gene_expression, aes(x = Expression)) +
  geom_histogram(binwidth = 0.3, fill = "#4B0082", color = "black", aes(y = ..density..), alpha = 0.7) +
  geom_density(fill = "#FF5733", alpha = 0.4) +
  labs(x = "Average Gene Expression", y = "Density", title = "Distribution of Average Gene Expression Levels") +
  theme_minimal()

################################
## Extract gene expression data and class labels
################################

gene_expression_data <- data.matrix(df[, -c(1,2)])
class_labels <- df$Class

################################
## Perform hierarchical clustering
################################

hc <- hclust(dist(gene_expression_data), method = "ward.D2")

# Define color mapping for class labels
color_mapping <- c("class1" = "red", 
                   "class2" = "blue", 
                   "class3" = "green",
                   "class4" = "orange",
                   "class5" = "black")

# Map the cancer types to the colors
row_colors <- color_mapping[as.character(class_labels)]

# Create the clustered heatmap
heatmap.2(gene_expression_data, Rowv = as.dendrogram(hc), Colv = as.dendrogram(hc),
          col = colorRampPalette(c("black", "white", "red"))(100),  # Adjust the color scale as needed
          scale = "none", key = TRUE, keysize = 1, key.title = NA, symkey = FALSE,
          density.info = "none", trace = "none", cexRow = 0.5, cexCol = 0.8,
          margins = c(8, 8), colRow = row_colors,
          main = "Clustered Heatmap of Gene Expression Data")

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

    # Extract gene name
    gene_name <- paste("gene", i-1, sep = "_")
    
    # Add results to data frame
    result_df <- rbind(result_df, data.frame(Gene = gene_name, F_statistic = f_statistic, p_value = p_value))
  }
}
print(result_df, row.names = FALSE)
write.csv(result_df,file="One way ANOVA.csv",row.names = FALSE)

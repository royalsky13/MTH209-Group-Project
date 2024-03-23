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

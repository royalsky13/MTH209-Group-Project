library(tidyverse)
library(ggplot2)
library(stats)
library(gplots)
library(tibble)

################################
## Load the data
################################
rm(list = ls())
ls()
load("ICMR.Rdata")
ls()


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
average_sorted_gene_expression <- gene_names_df %>% mutate(Expression=apply(df[-c(1,2)], 2, mean)) %>% arrange(desc(Expression))

################################
## Plot the distribution of average expression levels
################################

ggplot(average_sorted_gene_expression <- gene_names_df %>% mutate(Expression=apply(df[-c(1,2)], 2, mean)) %>% 
         arrange(desc(Expression)), aes(x = Expression)) +
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













#==============================================================================#
########### Age Specific Cancer Incidence Visualizations With  Gender ##########
#==============================================================================#

# Class Limits are : 0-14, 15-39, 40-64, 65+

FRE_M <- c(21308, 74872, 341230, 274766)
dma <- rep(c(15, 40, 65, 80), FRE_M)
plot(density(dma, bw = 8), xlim = c(3.8, 100), ylim = c(0.00105, 0.035),
     main = "Age-wise Cancer Incidence Density",
     xlab = "Age", ylab = "Density", col = "blue")

FRE_F <- c(13709, 94166, 425918, 215458)
dfe <- rep(c(15, 40, 65, 80), FRE_F)
lines(density(dfe, bw = 8), col = "red",lty=2)
legend("topleft", legend = c("Male", "Female"), col = c("blue", "red"), lty = c(1,2))

###############################################################################
#==============================================================================#
################ Gender wise top ten cancer sites Visualizations ###############
#==============================================================================#
Site_1 <- read.csv("Site_M_F_Cancer.csv")
Site <- Site_1[-54,]
M_Site <- Site[-3]
F_Site <- Site[-2]
M_Site_ord <- M_Site[order(Site$Male, decreasing = FALSE),]
F_Site_ord <- F_Site[order(Site$Female, decreasing = FALSE),]
m_t <- tail(M_Site_ord, 10)
f_t <- tail(F_Site_ord, 10)
ggplot(m_t, aes(x = Male, y = reorder(Site, Male))) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Top Ten Cancer Sites of Male",
       y = "", x = "Frequency")
ggplot(f_t, aes(x = Female, y = reorder(Site, Female))) +
  geom_bar(stat = "identity", fill = "deepskyblue3") +
  labs(title = "Top Ten Cancer Sites of Female",
       y = "", x = "Frequency")
#==============================================================================#
################ Different type of Cancer Sites Visualization ##################
#==============================================================================#

Can_lab <- df[c(1,2)]
Can_lab_tab <- data.frame(table(Can_lab$Class))
Can_lab_tab$Prop <- round(Can_lab_tab$Freq/sum(Can_lab_tab$Freq) * 100, 2)
Can_lab_tab$Prop <- paste(as.character(Can_lab_tab$Prop), "%", sep = "")
colnames(Can_lab_tab) <- c("Types", "Freq", "Prop")
ggplot(Can_lab_tab, aes(x = "", y = Freq, fill = Types)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Different Types of Cancer Sites") +
  theme_void() +
  geom_text(aes(label = Prop), color = "white",
            position = position_stack(vjust = c(0.5))) +
  scale_fill_brewer(palette="Set1")


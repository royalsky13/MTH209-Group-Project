#==============================================================================#
####################### Multinomial Logistic Regression ########################
#==============================================================================#

library(dplyr)

data <- as_tibble(read.csv("C:/Users/Swapnonil/Downloads/data.csv/data.csv"))
label <- as_tibble(read.csv("C:/Users/Swapnonil/Downloads/labels.csv"))

# head(data)
# head(label)

data_label <- merge(data, label)

#head(data_label)
#data_label$Class

library(VGAM) 

data_label$Class <- as.factor(data_label$Class)

fit <- vglm(Class ~ gene_1 + gene_2, 
            data = data_label, 
            family = multinomial)
summary(fit)

new_data <- data.frame(gene_1 = c(3.4678533, 2.9411814, 5.3177020),
                       gene_2 = c(3.5819176, 2.6632763, 1.6426785))

predict(fit, new_data, type = "response")

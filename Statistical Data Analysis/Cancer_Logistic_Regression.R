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

fit <- vglm(Class ~ gene_1, 
            data = data_label, 
            family = multinomial)
summary(fit)

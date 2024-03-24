#==============================================================================#
########### Age Specific Cancer Incidence Visualizations With  Gender ##########
#==============================================================================#

# Class Limits are : 0-14, 15-39, 40-64, 65+

FRE_M <- c(21308, 74872, 341230, 274766)
dma <- rep(c(15,40,65,80), FRE_M)
#table(cut(dma, breaks = c(0,15,40,65,90), right = TRUE))
#hist(dma, breaks = c(0,15,40,65,80))
plot(density(dma, bw = 8), xlim = c(3.8,100), ylim = c(0.00105, 0.035),
     main = "Age-wise Male Cancer Incidence Density",
     xlab = "Age")

FRE_F <- c(13709, 94166, 425918, 215458)
dfe <- rep(c(15,40,65,80), FRE_F)
plot(density(dfe, bw = 8), xlim = c(3.8,100), ylim = c(0.00105, 0.035),
     main = "Age-wise Female Cancer Incidence Density",
     xlab = "Age")

#==============================================================================#
################ Gender wise top ten cancer sites Visualizations ###############
#==============================================================================#

Site_1 <- read.csv("C:/Users/Swapnonil/Documents/Site_M_F_Cancer.csv")
Site <- Site_1[-54,]

M_Site_ord <- M_Site[order(Site$Male, decreasing = FALSE),]
F_Site_ord <- M_Site[order(Site$Female, decreasing = FALSE),]
m_t <- tail(M_Site_ord, 10)
f_t <- tail(F_Site_ord, 10)
#par(mar=c(4.5,11,6,4))
#barplot(head(M_Site_ord$Male, 10), names.arg = head(M_Site_ord$Site, 10),
#        col = seq(2, 12, 1))
#par(mar=c(4.5,11,6,4))
#barplot(tail(F_Site_ord$Site, 10) ~ tail(F_Site_ord$Female, 10),
#        col = "cornflowerblue", border = "cornflowerblue",
#        las = 2, horiz = TRUE, ylim = c(0, 7.5), xlim = c(0, 2.5e5))

library(ggplot2)

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

Can_lab <- read.csv("C:/Users/Swapnonil/Downloads/labels.csv")
# table(Can_lab$Class)
Can_lab_tab <- data.frame(table(Can_lab$Class))

library(ggplot2)

ggplot(Can_lab_tab, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Different Types of Cancer Sites",
       y = "", x = "Frequency")

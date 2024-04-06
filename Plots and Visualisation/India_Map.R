################################################################################

# To run this code all the India_files must be downloaded

library(sf)
library(ggplot2)
# library(dplyr)

mydata <- st_read("C://Users//Swapnonil//Downloads//IND_Map//India_State_Boundary.shp", stringsAsFactors = FALSE)
#str(mydata)
#View(mydata1)
#str(shp3)

#mydata1 <- shp2[order(shp2$State_Name),]

dat <- read.csv("C://Users//Swapnonil//Downloads//Est_Cancer_Data.csv")
dat1 <- dat[-38,]
dat.a <- dat[order(dat1$State),]
dat[dat$State == "Pondicherry",]$State = "Puducherry"
#dat.a[39,] = dat[dat$State == "Pondicherry",]
mydata[mydata$State_Name == "Puducherry",]
mydata_m = mydata[-13,]
#View(mydata_m)
mydata_m[37,] = mydata_m[3,]
mydata_m[37,]$State_Name = "Daman"
mydata_m[3,]$State_Name = "Dadra & Nagar Haveli"
mydata_map = mydata_m[order(mydata$State_Name),]


#View(mydata_map)
dat.a$State = mydata_map$State_Name

map_and_data <- merge(mydata_map, dat.a, by.x = "State_Name", by.y = "State",
                      all = TRUE)

map_and_data$X2020 <- as.numeric(map_and_data$X2020)
map_and_data$X2021 <- as.numeric(map_and_data$X2021)
map_and_data$X2022 <- as.numeric(map_and_data$X2022)
colnames(map_and_data) <- c("State_Name", "Incidence_of_2020", "Incidence_of_2021", "Incidence_of_2022", "geometry")
# names(map_and_data$X2020) = "Incidence_of_2020"
# names(map_and_data$X2021) = "Incidence_of_2021"
# names(map_and_data$X2022) = "Incidence_of_2022"

#View(map_and_data)

# Choropleth Map for the year 2020
ggplot(map_and_data) +
  geom_sf(aes(geometry = geometry, fill = Incidence_of_2020),
          linewidth = 0,
          color = "gray50") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Cancer Incidence of India in 2020") +
  scale_fill_gradient(low="cornflowerblue", high="navyblue")

# Choropleth Map for the year 2021
ggplot(map_and_data) +
  geom_sf(aes(geometry = geometry, fill = Incidence_of_2021),
          linewidth = 0,
          color = "gray50") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Cancer Incidence of India in 2021") +
  scale_fill_gradient(low="cornflowerblue", high="navyblue")

# Choropleth Map for the year 2022
ggplot(map_and_data) +
  geom_sf(aes(geometry = geometry, fill = Incidence_of_2022),
          linewidth = 0,
          color = "gray50") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Cancer Incidence of India in 2022") +
  scale_fill_gradient(low="cornflowerblue", high="navyblue")

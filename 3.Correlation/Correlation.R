
library(tidyverse)
library(dplyr)
library(sf)
library(spdep)
library(reshape2)

setwd("E:\\Articulo9_SeqAnalyis\\After review\\Replicability\\Correlation")


## Join data

data <- read.csv("Imput_correlation.csv",sep = ';')

shp <- st_read("MUN_points.shp")

shp <- shp[, c('PROVMUN', 'geometry')]

colnames(shp) <- c("CODE_MUN","geometry")

shp_data <- left_join(shp, data, by ="CODE_MUN", "")

shp_data <- drop_na(shp_data)

shp_data$Cluster <- recode(shp_data$Cluster, "0"="Growth",
                            "1"="Consistent High decline",
                            "2"="Consistent Decline",
                            "3"="Increasing High Decline",
                            "4"="Increasing Decline",
                            "5"="Increasing Moderate Decline",
                            "6"="Growth to Decline")

shp_data <- subset(shp_data, Cluster != "Growth")


## Create factor

factor <-  as.factor(shp_data$Cluster)

names(factor) <- rownames(shp_data)
#names(factor) <- shp_data$CODE_MUN


## Create .nb list

shp_data <- st_transform(shp_data, "+proj=longlat +ellps=WGS84 +datum=WGS84")
list <- st_coordinates(shp_data)

list.k2.nb <- knn2nb(knearneigh(list, 2))
print(is.symmetric.nb(list.k2.nb))


## Correlations

#joincount.multi(factor, nb2listw(list.k2.nb))
model <-data.frame(joincount.multi(factor, listw2U(nb2listw(list.k2.nb))))
model



## Data to table

model_output <- cbind(rownames(model), model)

rownames(model_output) <- NULL

model_output$Index <- model_output$Joincount / model_output$Expected

colnames(model_output) <- c("Type", "Joincount", "Expected", "Variance", "z.value", "Index")

model_output[c("Type1", "Type2")] <- str_split_fixed(model_output$Type, ':', 2)

model_output <- model_output[, c("Type1", "Type2", "Joincount", "Expected", "Variance", "z.value", "Index")]

model_output$Joincount <- round(model_output$Joincount, 0)
model_output$Expected <- round(model_output$Expected, 0)
model_output$Variance <- round(model_output$Variance, 2)
model_output$z.value <- round(model_output$z.value, 2)
model_output$Index <- round(model_output$Index, 2)

model_output <- model_output[, c(1,2,7,3,4,5,6)]

write.csv(model_output, "model_output.csv")


## Data to plot

model_matrix <- model_output[, c('Type1', "Type2", 'Index')]

model_matrix <- subset(model_matrix, Type1 != "Jtot")

model_matrix <- spread(model_matrix, Type1, Index)

colnames(model_matrix)[1] = "Type"

write.csv(model_matrix, "Results_heatmap.csv")

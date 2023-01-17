
#Library
library(ggplot2)
library(hrbrthemes)
library(reshape)
library(tidyverse)

setwd("E:\\Articulo9_SeqAnalyis\\After review\\Replicability\\Correlation")


#Data


data <- read.csv(file = 'Results_heatmap.csv', sep = ',')

data <- data[,-1]

colnames(data) <- c("Type", "Consistent Decline", "Consistent High Decline", "Growth to Decline",          
                    "Increasing Decline", "Increasing High Decline", "Increasing Moderate Decline")


data <- melt(data, id=c("Type"))

data$variable <- recode(data$variable,
                        "Consistent Decline"="Consistent\nDecline",
                        "Consistent High Decline"="Consistent\nHigh Decline",
                        "Growth to Decline"="Growth to\nDecline",          
                        "Increasing Decline"="Increasing\nDecline",
                        "Increasing High Decline"="Increasing\nHigh Decline",
                        "Increasing Moderate Decline"="Increasing\nModerate Decline")



#Visualisation

bmp(file="Heatmap.bmp", width=7, height=2, units="in", res=300)

ggplot(data, aes(variable, Type, fill= value)) + 
  geom_tile() +
  scale_fill_distiller(direction = 1, na.value="white")+
  scale_x_discrete(position = "top")+
  geom_text(aes(label = value), colour ="black", size=2.4, fontface = "bold")+
  labs(x=NULL, y = NULL) +
  
  annotate("text", x = 1.14, y = 1, size = 3.2, label = "*")+
  annotate("text", x = 2.14, y = 1, size = 3.2, label = "*")+
  annotate("text", x = 6.14, y = 1, size = 3.2, label = "*")+
  
  annotate("text", x = 2.14, y = 2, size = 3.2, label = "*")+
  annotate("text", x = 4.14, y = 2, size = 3.2, label = "*")+
  annotate("text", x = 5.14, y = 2, size = 3.2, label = "*")+
  annotate("text", x = 6.14, y = 2, size = 3.2, label = "*")+
  
  annotate("text", x = 3.14, y = 3, size = 3.2, label = "*")+
  annotate("text", x = 6.14, y = 3, size = 3.2, label = "*")+
  
  annotate("text", x = 4.14, y = 4, size = 3.2, label = "*")+
  annotate("text", x = 5.14, y = 4, size = 3.2, label = "*")+
  annotate("text", x = 6.14, y = 4, size = 3.2, label = "*")+
  
  annotate("text", x = 5.14, y = 5, size = 3.2, label = "*")+
  annotate("text", x = 6.14, y = 5, size = 3.2, label = "*")+
  
  annotate("text", x = 6.14, y = 6, size = 3.2, label = "*")+
  
  theme_minimal()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        
        axis.text.x = element_text(colour = "grey10", size = 8.3, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 8.3, face="plain"),
        legend.position = "none")

dev.off()

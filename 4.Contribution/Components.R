
library(dplyr)
library(ggplot2)
library(viridis)


#Directory
setwd("E:\\Articulo9_SeqAnalyis\\After review\\Replicability\\Contribution")


#Data
data <- read.csv(file = 'Components.csv', sep = ';')

#Selecting just decline
data <-data[ which(data$Cluster != 'Growth'),]
data <-data[ which(data$Year != '2020'),]



#To factors
data$Cluster <- as.factor(data$Cluster)
data$Type <- as.factor(data$Type)
levels(data$Cluster)
levels(data$Type)

#To numeric
data$Year <- as.numeric(data$Year)
data$Value <- as.numeric(data$Value)


#Ordering type
data$Type <- factor(data$Type, levels = c("International net-migration", "Internal net-migration",
                                            "Natural change"))
levels(data$Type)


#Recoding clusters

data$Cluster <- recode(data$Cluster, "Consistent high decline"="Consistent High Decline (n=1499)",
                       "Consistent decline"="Consistent Decline (n=969)",
                       "Increasing high decline"="Increasing High Decline (n=566)",
                       "Increasing decline"="Increasing Decline (n=968)",
                       "Increasing moderate decline"="Increasing Moderate Decline (n=641)",
                       "Growth to decline"="Growth to Decline (n=463)")

#Ordering clusters
data$Cluster <- factor(data$Cluster, levels = c("Consistent High Decline (n=1499)", "Increasing High Decline (n=566)",
                                                "Consistent Decline (n=969)", "Increasing Decline (n=968)",
                                                "Increasing Moderate Decline (n=641)", "Growth to Decline (n=463)"))
levels(data$Cluster)


#Plot

bmp(file="Components.bmp", width = 9, height = 6.5, units = 'in', res = 300)

ggplot(data = data,aes(x = Year, y = Value, group=Type)) +
  
  facet_wrap(.~Cluster, ncol=3)+
  
  geom_area(aes(fill=Type),size=0, alpha=0.6)+
            #filter(data, Type != "Growth rate"))+
  
  #geom_line(aes(Year, Value), color="gray10", alpha = 0.8,size = 0.8, 
            #filter(data, Type == "Growth rate")) +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                     limits=c(-3.65,3.4), breaks=seq(-3,3,1))+
  
  scale_x_continuous(limits=c(2001.5,2020.2), breaks=seq(2002,2020,2),expand=c(0,0))+
  

  scale_fill_viridis(discrete = T, direction = -1)+
  guides(fill = guide_legend(reverse=TRUE))+
  
  geom_hline(yintercept = 0, size=0.6, colour ="grey5")+
  
  labs(x=NULL,y = "Rates (%)") +
  
  theme_bw()+
  theme(text = element_text(size = 14, face="plain"),
        
        panel.spacing = unit(0.8, "lines"),
        
        axis.title.y = element_text(vjust=3,size=11, face="plain"),
        strip.background = element_rect(color="grey15",size = 0.35),
        
        panel.border = element_rect(color="grey15",size = 0.35),
        panel.grid.major = element_line(colour = "grey70", size=0.1),
        panel.grid.minor=element_blank(),
        
        axis.text.x = element_text(colour = "grey10", size = 11, angle = 90, hjust =0.6, vjust = 0.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 11, hjust = 0.6, vjust = 0.5, face="plain"),
        axis.ticks = element_line(color="grey5", size = 0.6),
        
        legend.title = element_blank (),
        legend.text = element_text(colour="black", size =12, face="plain"),
        legend.position= "bottom",
        #legend.position=c(0.84,0.24),
        legend.direction = "horizontal",
        legend.justification=c(0.5,0.5),
        legend.key.width=unit(.6,"cm"),
        legend.key.height=unit(.6,"cm"))

dev.off()

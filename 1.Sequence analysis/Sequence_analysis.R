
#this is the main package we will use to perform the sequence analysis
library(TraMineR)
#used for evaluating cluster solutions
library(TraMineRextras)
#used for clustering
library(cluster)
library(WeightedCluster)
#additional packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(viridis)

#-------------------------------------------------------------------------------

#read in the data - 3-year smoothed rates of decline
data <- read_excel("E:\\Articulo9_SeqAnalyis\\After review\\Replicability\\Sequence analysis\\Data.xlsx",
                  col_types = c("text", "text", "text",
                                "text", "text", "text", "text", "text",
                                "text", "text", "text", "text", "text",
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text","text","text"))

#remove areas of population growth, leaving behind n = 5106 areas characterised by their declining populations
decline <- data %>% 
  filter(Decline == 1) %>% 
  mutate(id = row_number()) %>% 
  select(-Decline, -RU)

#recode state boundary codes to state labels
decline[decline=="-H"]<-"High Decline"
decline[decline=="-M"]<-"Decline"
decline[decline=="-L"]<-"Moderate Decline"
decline[decline=="S"]<-"Stable"
decline[decline=="L"]<-"Moderate Growth"
decline[decline=="M"]<-"Growth"
decline[decline=="H"]<-"High Growth"

#set the sequence alphabet
labels <- c("High Decline", "Decline", "Moderate Decline", "Stable","Moderate Growth", "Growth", "High Growth")
scode <- c("HDE", "MDE","LDE", "ST", "LGR", "MGR", "HGR")

#create sequence object
seq_all <- seqdef(decline, 2:21, alphabet = labels, states = scode, labels = labels, xtstep = 6)

#preliminary plots - state distribution and state index plots
#first change the colour scheme
cpal(seq_all) <- c("#045a8d", "#2b8cbe", "#bdc9e1","#FFFFFF", "#fcae91","#fb6a4a", "#de2d26")
windows()
seqdplot(seq_all, with.legend = T, border = NA, tick.last = TRUE)
seqIplot(seq_all, with.legend = T, border = NA, tick.last = TRUE)

## Distance Matrix- Dynamic Hamming Distances ##
#calculate substitution costs based on transition rates observed in the data and a 1 indel cost
submat <- seqsubm(seq_all, method = "TRATE")
dist.dhd <- seqdist(seq_all, method = "DHD")

## Clustering ##
#use wards hierarchical clustering procedure to group areas
clusterward <- agnes(dist.dhd, diss = TRUE, method = "ward")

#Generate an object with 1-10 cluster solutions
wardrange.OM <-as.clustrange(clusterward, diss=dist.dhd, ncluster=10)

#test a range of cluster solutions. show cluster cut-off measure values - indicate four optimal cluster solutions
summary(wardrange.OM, max.rank=4)

#graph to show empirical fit of cluster solutions
windows()
plot(wardrange.OM, stat=c("ASW", "HGSD", "PBC"), norm="zscore")

#Set number of clusters to 6
clusters.6 <- cutree(clusterward, k = 6)
cluster <- factor(clusters.6, labels = paste("Type", 1:6))

#### retrieve cluster id labels from row index ####

#returns a dataframe for each cluster containing the row id of original decline sequence dataset and corresponding cluster label
CD <- seq_all[cluster=="Consistent Decline",]
CD <- as.data.frame(CD) %>% 
  mutate(id = rownames(CD),
         Cluster = "CD") %>% 
  select(id, Cluster)

ID <- seq_all[cluster=="Increasing Decline",]
ID <- as.data.frame(ID) %>% 
  mutate(id = rownames(ID),
         Cluster = "ID") %>% 
  select(id, Cluster)

ImD <- seq_all[cluster=="Increasing Moderate Decline",]
ImD <- as.data.frame(ImD) %>% 
  mutate(id = rownames(ImD),
         Cluster = "ImD") %>% 
  select(id, Cluster)

IhD <- seq_all[cluster=="Increasing High Decline",]
IhD <- as.data.frame(IhD) %>% 
  mutate(id = rownames(IhD),
         Cluster = "IhD") %>% 
  select(id, Cluster)

GtD <- seq_all[cluster=="Growth to Decline",]
GtD <- as.data.frame(GtD) %>% 
  mutate(id = rownames(GtD),
         Cluster = "GtD") %>% 
  select(id, Cluster)

ChD <- seq_all[cluster=="Consistent High Decline",]
ChD <- as.data.frame(ChD) %>% 
  mutate(id = rownames(ChD),
         Cluster = "ChD") %>% 
  select(id, Cluster)

#combine id labels into one data frame
cluster.id <- rbind(CD,ID, ImD, IhD, GtD, ChD)
#set id column as numeric
cluster.id$id <- as.numeric(cluster.id$id)


#merge with original sequence file, returns a dateframe with area codes and cluster label
decline.cluster <-  left_join(decline, cluster.id, by.x = "id", by.y = "id") %>%
  select(CODE, Cluster)


#-------------------------------------------------------------------------------

# Plots 

#label clusters
labels <- c("Consistent Decline","Increasing Decline", "Increasing Moderate Decline",
            "Increasing High Decline","Growth to Decline","Consistent High Decline")
cluster <- factor(clusters.6, labels = labels)


#state index and state distribution plots by cluster
windows()
seqIplot(seq_all, group = cluster, border = NA, sortv= dist.dhd)
seqdplot(seq_all, group = cluster, border = NA)

#Individual Index and Distribution plots
#Repeat for each cluster
seqIplot(seq_all[cluster=="Consistent High Decline",],
         ylab = NA,
         space = 0, 
         border = NA,
         xaxis = TRUE,
         sortv= dist.dhd,
         yaxis = FALSE,
         xtstep = 10,
         tick.last = TRUE,
         with.legend = FALSE,
         cex.axis = 1.5)

#Repeat for each cluster
seqdplot(seq_all[cluster=="Consistent High Decline",],
         ylab = NA,
         space = 0, 
         border = NA,
         xaxis = TRUE,
         sortv= dist.dhd,
         yaxis = FALSE,
         xtstep = 10,
         tick.last = TRUE,
         with.legend = FALSE,
         cex.axis = 1.5)

#Mean Time Plots
CD <- data.frame(seqmeant(seq_all[cluster=="Consistent Decline",]))
CD <- CD %>% 
  mutate(state = scode,
         id = "CD") %>% 
  relocate(state)

ID <- data.frame(seqmeant(seq_all[cluster=="Increasing Decline",]))
ID <- ID %>% 
  mutate(state = scode,
         id = "ID") %>% 
  relocate(state)

ImD <- data.frame(seqmeant(seq_all[cluster=="Increasing Moderate Decline",]))
ImD <- ImD %>% 
  mutate(state = scode,
         id = "ImD") %>% 
  relocate(state)

IhD <- data.frame(seqmeant(seq_all[cluster=="Increasing High Decline",]))
IhD <- IhD %>% 
  mutate(state = scode,
         id = "IhD") %>% 
  relocate(state)

GtD <- data.frame(seqmeant(seq_all[cluster=="Growth to Decline",]))
GtD <- GtD %>% 
  mutate(state = scode,
         id = "GtD") %>% 
  relocate(state)

ChD <- data.frame(seqmeant(seq_all[cluster=="Consistent High Decline",]))
ChD <- ChD %>% 
  mutate(state = scode,
         id = "ChD") %>% 
  relocate(state)

#combine to a single dataframe
meant <- rbind(CD, ID, ImD, IhD, GtD, ChD)

#set id and state variables to factors
meant$id <- as.factor(meant$id)
meant$state <- as.factor(meant$state)

#set the colours to the same as previous plots
cols <- c("#045a8d", "#2b8cbe", "#bdc9e1","#FFFFFF", "#fcae91","#fb6a4a", "#de2d26")

#reorder states
meant$state <- factor(meant$state, levels = c("HDE", "MDE", "LDE", 
                                              "ST", "LGR", "MGR", "HGR"))
#plot
t <-   theme(panel.grid = element_blank(),
             axis.title = element_blank())
windows()
ggplot(meant, aes(fill=state, y=Mean, x=id)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() +
  scale_fill_manual(values = cols) + t + theme_classic() + theme(text = element_text(size=20), axis.text=element_text(size=20))

#legend
windows()
seqlegend(seq_all, cex = 1, ncol = 4)


library(tidyr)
library(nnet)
library(stargazer)
library(questionr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(reshape)
library(stringr)
library(dplyr)
library(ggplot2)
library(viridis)


#Read data

setwd("E:\\Articulo9_SeqAnalyis\\After review\\Replicability\\Multinomial_model")

#Data are not open access, but it can be access under request. See sources in the paper
data <- read.csv(file = 'VARIABLES_2.csv', sep = ';')


#Remove NA

data <- data %>% drop_na()


#Categorical variables

data$Cluster <- as.factor(data$Cluster)
data$Distance <- as.factor(data$Distance)


#Standardization

data$Pop_E <- (data$Pop - mean(data$Pop)) / sd(data$Pop)
data$Age_20_39_E <- (data$Age_20_39 - mean(data$Age_20_39)) / sd(data$Age_20_39)
data$Older84_E <- (data$Older84 - mean(data$Older84)) / sd(data$Older84)
data$Foreing_E <- (data$Foreing - mean(data$Foreing)) / sd(data$Foreing)
data$Pop_capital_E <- (data$Pop_capital - mean(data$Pop_capital)) / sd(data$Pop_capital)
data$Altitud_E <- (data$Altitud - mean(data$Altitud)) / sd(data$Altitud)
data$H_rent_E <- (data$H_rent - mean(data$H_rent)) / sd(data$H_rent)
data$No_farmers_E <- (data$No_farmers - mean(data$No_farmers)) / sd(data$No_farmers)



#Model

M <- multinom(Cluster ~ Pop_E + Age_20_39_E + Older84_E + Foreing_E+
                Pop_capital_E + Distance + Altitud_E +
                H_rent_E + No_farmers_E,
              data=data)


#Coeficientes

a <- data.frame(summary(M)$coefficients)


#Confidence intervals

b <- data.frame(confint(M))


#R2

nnet.mod.loglik <- nnet:::logLik.multinom(M)
nnet.mod0 <- multinom(Cluster ~ 1, data=data)
nnet.mod0.loglik <- nnet:::logLik.multinom(nnet.mod0)
(nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik/nnet.mod0.loglik))


#Table

tab_model(M, transform=NULL,collapse.ci = TRUE, p.style = "stars")



###### VISUALIZATION ######


### Data.frame coeficients


#Remove intercept

a <- a[, c("Pop_E", "Age_20_39_E", "Older84_E", "Foreing_E", "Pop_capital_E",
           "Distance2", "Altitud_E", "H_rent_E", "No_farmers_E")]


#Name first column as clusters

a <- cbind(rownames(a), a)
rownames(a) <- NULL
colnames(a) <- c("Cluster","Pop_E", "Age_20_39_E", "Older84_E", "Foreing_E", "Pop_capital_E",
                 "Distance2", "Altitud_E", "H_rent_E", "No_farmers_E")


#Melt data

a <- melt(a, id="Cluster")



### Data.frame CI


#Remove intercept

b<-b[2:10, 1:12]


#Name first column of clusters

b <- cbind(rownames(b), b)
rownames(b) <- NULL
colnames(b) <- c("variable", "X2.5...1", "X97.5...1", "X2.5...2", "X97.5...2",
                 "X2.5...3" , "X97.5...3", "X2.5...4" , "X97.5...4", "X2.5...5",
                 "X97.5...5", "X2.5...6", "X97.5...6")


#Melt data

b <- melt(b, id="variable")


#Col manes

colnames(b) <- c("variable", "variable2","value")


#Substract clusters and CI

b$Cluster <- str_sub(b$variable2,-1,-1)

b$CI <- substr(b$variable2,1,2)


#Recode CI

b$CI <- recode(b$CI,"X2" = "Min",
               "X9" = "Max")


#Name variables

b <- b[, c("variable", "Cluster", "value", "CI")]


#Expread data

b <- spread(b, CI, value)



### Join fields
M.rr <- left_join(a, b, by =c("Cluster", "variable"))


#Recode name of variables and clusters

M.rr$variable <- as.factor(M.rr$variable)
M.rr$variable <- recode(M.rr$variable,
                         "Altitud_E"="Altitude",
                         "Distance2"="> 40 Km from core cities",
                         "H_rent_E"="% of housing rentals",
                         "Age_20_39_E"="% young adults",
                         "Older84_E"="Over-ageing",
                         "Foreing_E"="% foreign-born",
                         "No_farmers_E"="% non-farming jobs",
                         "Pop_capital_E"="Population capital city",
                         "Pop_E"="Population")


M.rr$Cluster2 <- recode(M.rr$Cluster, "1"="Consistent High Decline",
                        "2"="Consistent Decline",
                        "3"="Increasing High Decline",
                        "4"="Increasing Decline",
                        "5"="Increasing Moderate Decline",
                        "6"="Growth to Decline")


#Order

M.rr$Cluster2<- factor(M.rr$Cluster2, levels = c("Consistent High Decline",
                                                 "Increasing High Decline",
                                                 "Consistent Decline",
                                                 "Increasing Decline",
                                                 "Increasing Moderate Decline",
                                                 "Growth to Decline"))

levels(M.rr$Cluster2)
M.rr$variable<- factor(M.rr$variable, levels = c("Population",
                                                 "% young adults",
                                                 "% foreign-born",
                                                 "Population capital city",
                                                 "% non-farming jobs",
                                                 "% of housing rentals",
                                                  "Altitude",
                                                  "> 40 Km from core cities",
                                                  "Over-ageing"))




##############################
########   Plot   ########
##############################



bmp(file='Model.bmp', width = 6.5, height = 4.5, units = 'in', res = 300)

ggplot(data = M.rr, aes(x = variable, y = value, ymin = Min, ymax = Max, colour = Cluster2)) +
  
  geom_point(position = position_dodge(width = 0.75), size=1.4) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0, size=0.35) +
  
  #coord_flip() +
  
  scale_colour_manual(values=c("#000004", "#420a68", "#932667",
                               "#dd513a", "#fca50a", "#f2e661"))+

  #scale_colour_viridis(option="inferno", discrete = TRUE, direction = -1)+
  #guides(colour = guide_legend(reverse=T))+
  
  scale_y_continuous(limits=c(-2.3,2.3), breaks=seq(-2,2,0.5))+
  
  labs(x=NULL,y = "Coefficients") +
  
  geom_hline(yintercept = 0, size=0.5, colour ="grey15", linetype="dashed")+
  
  geom_segment(
    x = 0.6, y = -1.45,
    xend = 0.6, yend = -2.35,
    lineend = "round",
    linejoin = "round",
    size = 0.35, 
    arrow = arrow(length = unit(0.05, "inches")),
    colour = "#000004") +
  
  geom_segment(
    x = 0.75, y = -1.45,
    xend = 0.75, yend = -2.35,
    lineend = "round",
    linejoin = "round",
    size = 0.35, 
    arrow = arrow(length = unit(0.05, "inches")),
    colour = "#420a68") +
  
  geom_segment(
    x = 0.9, y = -1.45,
    xend = 0.9, yend = -2.35,
    lineend = "round",
    linejoin = "round",
    size = 0.35, 
    arrow = arrow(length = unit(0.05, "inches")),
    colour = "#932667") +
  
  geom_segment(
    x = 1.05, y = -1.45,
    xend = 1.05, yend = -2.35,
    lineend = "round",
    linejoin = "round",
    size = 0.35, 
    arrow = arrow(length = unit(0.05, "inches")),
    colour = "#dd513a") +
  
  geom_segment(
    x = 1.35, y = -1.45,
    xend = 1.35, yend = -2.35,
    lineend = "round",
    linejoin = "round",
    size = 0.35, 
    arrow = arrow(length = unit(0.05, "inches")),
    colour = "#f2e661") +
  
  annotate("text", x = 0.6, y = -1.2, size = 2.2, label = "-94.3",
           angle=90, col="#000004", fontface =2)+
  annotate("text", x = 0.75, y = -1.2, size = 2.2, label = "-74.3",
           angle=90, col="#420a68", fontface =2)+
  annotate("text", x = 0.9, y = -1.25, size = 2.2, label = "-7.1",
           angle=90, col="#932667", fontface =2)+
  annotate("text", x = 1.05, y = -1.25, size = 2.2, label = "-2.9",
           angle=90, col="#dd513a", fontface =2)+
  annotate("text", x = 1.35, y = -1.2, size = 2.2, label = "-32.7",
           angle=90, col="#f2e661", fontface =2)+
  
  annotate("rect", xmin =1.5 , xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.1)+
  annotate("rect", xmin =3.5 , xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.1)+
  annotate("rect", xmin =5.5 , xmax = 6.5, ymin = -Inf, ymax = Inf, alpha = 0.1)+
  annotate("rect", xmin =7.5 , xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = 0.1)+
  
  theme_bw()+
  
  theme(text = element_text(size = 10.5),
        strip.background = element_rect(size =0.6),
        
        panel.border = element_rect(size=0.6),
        
        axis.title.y = element_text(size=8.5, vjust =1 ),
        
        axis.text.x = element_text(size = 8.5, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8.5),
        
        #axis.line = element_line(size=0.35),
        
        axis.ticks.x = element_line(size = 0.6),
        axis.ticks.y = element_line(size = 0.6),
        
        panel.grid.major.y=element_line(colour = "grey65",size=0.3,linetype="dotted"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.x=element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size =8.5),
        legend.position="bottom",
        legend.direction = "horizontal")

dev.off()


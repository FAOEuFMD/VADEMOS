
install.packages("mc2d")
install.packages("RColorBrewer")
install.packages("ggplot2")

library(ggplot2)
library(mc2d)
library(tidyverse)
library(dplyr)

rm(test,level_order,stack_order,labels)

test<-read.csv('~/Documents/Sensitivity_Test.csv')


Value <- as.integer(test$Value)


level_order<-c("Density","LR Outbreak coverage","Outbreaks","SR_Prop","LR_Prop","Population SR","Population LR")
stack_order<-c("75%","50%","25%","-25%","-50%","-75%")

labels<-c("LR Outbreak coverage"= expression("Large Ruminant Outbreak Coverage, OC"[s]),"P Outbreak coverage" = expression("Pig Outbreak Coverage, OC"[s]), 
          "Population <12 mo proportion P"= expression("Pig Youngstock Proportion, a"[s]),"Population P"= expression("Pig Population,P"[s]),"SR Outbreak coverage"= expression("Small Ruminant Outbreak Coverage, OC"[s]),
          "P_Prop"= expression("Pig Prophylactic Vaccine Coverage, VC"[s]),"Outbreaks"= "Number of Outbreaks, ON","Population <12 mo proportion LR" = expression("Large Ruminant Youngstock Proportion, a"[s]),
          "Population <12 mo proportion SR" = expression("Small Ruminant Youngstock Proportion,a"[s]),"LR_Prop"=expression("Large Ruminant Prophylactic Vaccine Coverage, VC"[s]),
          "SR_Prop"= expression("Small Ruminant Prophylactic Vaccine Coverage, VC"[s]),"Population SR"= expression("Small Ruminant Population, P"[s]),
          "Population LR" = expression("Large Ruminant Population, P"[s]),"Density" = expression("Livestock Density, LD"))

test %>%
  filter(!is.na(test$Proportion))%>% # filter on non-missing values

ggplot(aes(fill=factor(Proportion,level=stack_order),y=Value,x=factor(Parameter, level= level_order)))+
  geom_bar(position = "stack", stat = "identity")+
  ggtitle("Sensitivity Analysis")+
  coord_flip()+
  scale_x_discrete(labels=labels)+
  scale_y_continuous(breaks = seq(-1,1, 0.1))+
  labs(y= "Log Ratio", x= "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.6),
        axis.title.x = element_text(size = 11, hjust = 0.6),
        axis.title.y = element_text(size = 11, hjust = 0.5))
  
  
  
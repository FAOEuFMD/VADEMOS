

library(ggplot2)
rm(population)
population<-read.csv('~/Documents/Population forecasting VADEMOS.csv')

livestock_order<-c("Large Ruminants", "Small Ruminants","Pigs")

ggplot(population, aes(x=Year,y=Population, color=Country, group=Country))+
  geom_line()+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2020, 2030, by = 2))+
  labs(y="Livestock Population per 1,000 head")+
facet_wrap(~factor(Livestock,level=livestock_order), scales = "free")

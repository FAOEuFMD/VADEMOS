
library(ggplot2)
rm(vaccines)
rm(country_order)
vaccines<-read.csv('~/Documents/Emergency & prophylactic & population forecasting VADEMOS.csv')

country_order<-c("Country A", "Country B","Country C","Country D","Country E","Country F")

ggplot(vaccines, aes(x=Year,y=Vaccine.Dose, color=Type.of.Vaccine, group=Type.of.Vaccine))+
  geom_line()+
  labs(y="Vaccine Doses and Population / 1,000,000")+
  ggtitle("Prophylactic and Emergency Dose Number compared to Population")+
  scale_x_continuous(breaks = seq(2020, 2030, by = 2))+
  theme(
    legend.title = element_blank()
  )+ 
  facet_wrap(~factor(Country,level=country_order),scales = "free")

  
  
  
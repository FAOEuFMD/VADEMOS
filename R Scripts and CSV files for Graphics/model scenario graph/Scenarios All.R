

library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)
library(streamgraph)
library(viridis)
library(DT)
library(plotly)
rm(scenarios)
rm(country_order)
scenarios<-read.csv('~/Documents/Case Scenarios All.csv')

scenario_order<-c("Scenario 1 - Baseline","Scenario 2 - 100% Vaccine Coverage in All Species","Scenario 3 - Increased PCP Progression by 1 Stage ")
country_order<-c("Country D","Country E","Country B","Country F","Country A","Country C")

  ggplot(scenarios,aes(x=Year, y= TotalVaccines, color=factor(Scenario,level=scenario_order), group=factor(Scenario,level=scenario_order)))+
  geom_line()+
    scale_x_continuous(breaks = seq(2020, 2030, by = 2))+
    ggtitle("Case Scenarios") +
    labs(y= "Total Vaccine No. /1,000,000 Doses", x= "Year")+
    theme(
      legend.title = element_blank()
    )+ 
    facet_wrap(~factor(Country,level=country_order), scales = "free")
    




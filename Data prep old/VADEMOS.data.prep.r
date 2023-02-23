
#########################################################
##### Creation of countries database for VADEMOS ########
#########################################################

# # Loading libraries e working directory ####

library(tidyverse)
library(data.table)
library(reshape)
library(dplyr)
library(readxl)
library(plyr)


# Loading data,  selecting columns and recoding country names

FAO.DB <- fread("FAOSTATgeneral.csv", header = TRUE, stringsAsFactors = FALSE) 
# From 2022 FAOSTAT: https://www.fao.org/faostat/en/#data/QCL
# World_data <- fread("F:/PROGETTI/APHA/Module 2/Risk Ranking/Risk of Release of Exotic Disase in Italy Tool/Support Data/Worlddata.csv", header = TRUE, stringsAsFactors = FALSE)
FAO.DB$Area <- recode(FAO.DB$Area, "Cabo Verde" = "Cape Verde", "UAE" ='United Arab Emirates', 
                      "Syria" ='Syrian Arab Republic', "DRC"= 'Democratic Republic of the Congo',
                      "Namibia (zone)" = 'Namibia', 
                      "Sao Tome et Principe" = 'Sao Tome and Principe', 
                      "Guinea Bissau" = 'Guinea-Bissau', 
                      "Iran" = 'Iran (Islamic Republic of)',
                      "Tanzania" = 'United Republic of Tanzania',
                      "CÃ´te d'Ivoire"= "Ivory Coast",
                      "Côte d'Ivoire" = "Ivory Coast",
                      "Cote d'Ivoire" = "Ivory Coast",
                      "C?te d'Ivoire" = "Ivory Coast",
                      "C?te d'Ivoire" = "Ivory Coast" ) 

colnames(FAO.DB)[4] <- "Country_name"
FAO.DB <-FAO.DB[, c(4,8,10,12 )]
colnames(FAO.DB)[4] <- "Pop"
#setting 97% of the value for Anatolia and 3% for Thrace
FAO_turkey<-FAO.DB[which(FAO.DB$Country_name %in% 'Turkey'),] 
FAO_turkey$Pop<- FAO_turkey$Pop* 0.97
FAO_turkey$Country_name<- 'Turkey (Anatolia)'
FAO_turkey_thrace<-FAO.DB[which(FAO.DB$Country_name %in% 'Turkey'),] 
FAO_turkey_thrace$Pop<- FAO_turkey_thrace$Pop* 0.03
FAO_turkey_thrace$Country_name<- 'Turkey (Thrace)'
FAO.DB<- rbind(FAO.DB, FAO_turkey) 
FAO.DB<- rbind(FAO.DB, FAO_turkey_thrace) 

World_data <- fread("Worlddata.csv", header = TRUE, stringsAsFactors = FALSE)
World_data$NAME <- recode(World_data$NAME, "Cabo Verde" = "Cape Verde", "UAE" ='United Arab Emirates', 
                          "Syria" ='Syrian Arab Republic', "DRC"= 'Democratic Republic of the Congo',
                          "Namibia (zone)" = 'Namibia', 
                          "Sao Tome et Principe" = 'Sao Tome and Principe', 
                          "Guinea Bissau" = 'Guinea-Bissau', 
                          "Turkey" ='Turkey (Anatolia)', 
                          "Iran" = 'Iran (Islamic Republic of)',
                          "Tanzania" = 'United Republic of Tanzania',
                          "Côte d'Ivoire" = "Ivory Coast",
                          "Cote d'Ivoire" = "Ivory Coast",
                          "C?te d'Ivoire" = "Ivory Coast",
                          "C?te d'Ivoire" = "Ivory Coast" ) 

colnames(World_data)[1] <- "Country_code"
colnames(World_data)[6] <- "Country_name"
World_data <-World_data[, c(1,6,11,12 )]
#add row for South Sudan as it is missing
ss_row<- data.frame(Country_code= "SSD", Country_name= "South Sudan",LON= 31.30, LAT= 6.877)
thrace_row<-data.frame(Country_code= 'TUR2', Country_name= "Turkey (Thrace)", LON=24.88,   LAT=41.12)
World_data<- rbind(World_data, ss_row)
World_data<- rbind(World_data, thrace_row)




Agricultural.land <- fread("FAOSTATland.csv", header = TRUE, stringsAsFactors = FALSE)
Agricultural.land$Area <- recode(Agricultural.land$Area , "Cabo Verde" = "Cape Verde", 
                                 "UAE" ='United Arab Emirates', 
                                "Syria" ='Syrian Arab Republic', "DRC"= 'Democratic Republic of the Congo',
                                "Namibia (zone)" = 'Namibia', 
                                "Sao Tome et Principe" = 'Sao Tome and Principe', 
                                "Guinea Bissau" = 'Guinea-Bissau', 
                                "Iran" = 'Iran (Islamic Republic of)',
                                "Tanzania" = 'United Republic of Tanzania',
                                "Côte d'Ivoire" = "Ivory Coast",
                                "Cote d'Ivoire" = "Ivory Coast",
                                "C?te d'Ivoire" = "Ivory Coast",
                                "C?te d'Ivoire" = "Ivory Coast" ) 
colnames(Agricultural.land)[4] <- "Country_name"
Agricultural.land <-Agricultural.land[, c(4,10,12 )]
colnames(Agricultural.land)[3] <- "Agricultural_land"
#setting 97% of the value for Anatolia and 3% for Thrace
Agricultural_land_turkey<-Agricultural.land[which(Agricultural.land$Country_name %in% 'Turkey'),] 
Agricultural_land_turkey$Agricultural_land<- Agricultural_land_turkey$Agricultural_land* 0.97
Agricultural_land_turkey$Country_name<- 'Turkey (Anatolia)'
Agricultural_land_turkey_thrace<-Agricultural.land[which(Agricultural.land$Country_name %in% 'Turkey'),] 
Agricultural_land_turkey_thrace$Agricultural_land<- Agricultural_land_turkey_thrace$Agricultural_land* 0.03
Agricultural_land_turkey_thrace$Country_name<- 'Turkey (Thrace)'
Agricultural.land<- rbind(Agricultural.land, Agricultural_land_turkey) 
Agricultural.land<- rbind(Agricultural.land, Agricultural_land_turkey_thrace) 

beef.dairy.cattle <- fread("FAOSTATanimal.csv", header = TRUE, stringsAsFactors = FALSE)
beef.dairy.cattle$Area <- recode(beef.dairy.cattle$Area,
                                 "Cabo Verde" = "Cape Verde", "UAE" ='United Arab Emirates', 
                                 "Syria" ='Syrian Arab Republic', "DRC"= 'Democratic Republic of the Congo',
                                 "Namibia (zone)" = 'Namibia', 
                                 "Sao Tome et Principe" = 'Sao Tome and Principe', 
                                 "Guinea Bissau" = 'Guinea-Bissau',
                                 "Iran" = 'Iran (Islamic Republic of)',
                                 "Tanzania" = 'United Republic of Tanzania',
                                 "Côte d'Ivoire" = "Ivory Coast",
                                 "Cote d'Ivoire" = "Ivory Coast",
                                 "C?te d'Ivoire" = "Ivory Coast",
                                 "C?te d'Ivoire" = "Ivory Coast" ) 

colnames(beef.dairy.cattle)[4] <- "Country_name"
beef.dairy.cattle <-beef.dairy.cattle[, c(4,8,10,12)]
#setting 97% of the value for Anatolia and 3% for Thrace
b_turkey<-beef.dairy.cattle[which(beef.dairy.cattle$Country_name %in% 'Turkey'),] 
b_turkey$Value<- b_turkey$Value* 0.97
b_turkey$Country_name<- 'Turkey (Anatolia)'
b_turkey_thrace<-beef.dairy.cattle[which(beef.dairy.cattle$Country_name %in% 'Turkey'),] 
b_turkey_thrace$Value<- b_turkey_thrace$Value* 0.03
b_turkey_thrace$Country_name<- 'Turkey (Thrace)'
beef.dairy.cattle<- rbind(beef.dairy.cattle, b_turkey) 
beef.dairy.cattle<- rbind(beef.dairy.cattle, b_turkey_thrace) 

VADEMOS.countries <- fread("VADEMOS.country_list.csv", header = TRUE, stringsAsFactors = FALSE)
VADEMOS.countries$Country <- recode(VADEMOS.countries$Country, 
                                    "Cabo Verde" = "Cape Verde", "UAE" ='United Arab Emirates', 
                                    "Syria" ='Syrian Arab Republic', "DRC"= 'Democratic Republic of the Congo',
                                    "Namibia (zone)" = 'Namibia', 
                                    "Sao Tome et Principe" = 'Sao Tome and Principe', 
                                    "Guinea Bissau" = 'Guinea-Bissau', 
                                    "Turkey" ='Turkey (Anatolia)', 
                                    "Iran" = 'Iran (Islamic Republic of)',
                                    "Tanzania" = 'United Republic of Tanzania',
                                    "Côte d'Ivoire" = "Ivory Coast",
                                    "Cote d'Ivoire" = "Ivory Coast",
                                    "C?te d'Ivoire" = "Ivory Coast",
                                    "C?te d'Ivoire" = "Ivory Coast" ,
                                    "Côte d’Ivoire"= "Ivory Coast") 
                                 
VADEMOS.countries <-VADEMOS.countries[, c(1:4 )]
colnames(VADEMOS.countries)[2] <- "Subregion"
colnames(VADEMOS.countries)[4] <- "Country_name"
thrace_row2<-data.frame(Region= 'Asia', Subregion= "West Eurasia", Roadmap= "WE", Country_name= "Turkey (Thrace)")
VADEMOS.countries<- rbind(VADEMOS.countries, thrace_row2)

PCP <- as.data.frame(read_excel("pcp.xlsx"))
PCP$Country_name <- recode(PCP$Country_name, 
                                    "Cabo Verde" = "Cape Verde", "UAE" ='United Arab Emirates', 
                                    "Syria" ='Syrian Arab Republic', "DRC"= 'Democratic Republic of the Congo',
                                    "Namibia (zone)" = 'Namibia', 
                                    "Sao Tome et Principe" = 'Sao Tome and Principe', 
                                    "Guinea Bissau" = 'Guinea-Bissau', 
                                   "Turkey" ='Turkey (Anatolia)', 
                                    "Iran" = 'Iran (Islamic Republic of)',
                                    "Tanzania" = 'United Republic of Tanzania',
                      "West Bank" = "Palestine",
                      "Côte d'Ivoire" = "Ivory Coast",
                      "Cote d'Ivoire" = "Ivory Coast",
                      "C?te d'Ivoire" = "Ivory Coast",
                      "C?te d'Ivoire" = "Ivory Coast") 

PCP <-PCP[, c(1:5)]
colnames(PCP)[5] <- "PCP"



Outbreaks <-as.data.frame(read_excel("Outbreaks.xlsx"))
Outbreaks$Country_name <- recode(Outbreaks$Country_name, 
                      "Cabo Verde" = "Cape Verde", "UAE" ='United Arab Emirates', 
                      "Syria" ='Syrian Arab Republic', "DRC"= 'Democratic Republic of the Congo',
                      "Namibia (zone)" = 'Namibia', 
                      "Sao Tome et Principe" = 'Sao Tome and Principe', 
                      "Guinea Bissau" = 'Guinea-Bissau', 
                      "Turkey" ='Turkey', 
                      "Türkiye"= 'Turkey',
                      "Iran" = 'Iran (Islamic Republic of)',
                      "Tanzania" = 'United Republic of Tanzania',
                      "Tanzania" = 'United Republic of Tanzania',
                      "Congo (Dem. Rep. of the)" = 'Democratic Republic of the Congo',
                      "South Sudan (Rep. of)" = "South Sudan",
                      "Côte d'Ivoire" = "Ivory Coast",
                      "Cote d'Ivoire" = "Ivory Coast",
                      "C?te d'Ivoire" = "Ivory Coast",
                      "C?te d'Ivoire" = "Ivory Coast")

Outbreaks <- aggregate(Outbreaks ~ Country_name+Year, Outbreaks, sum, na.rm = TRUE)
colnames(Outbreaks)[3] <- "TotalOutbreaks"
#setting 97% of the value for Anatolia and 3% for Thrace
o_turkey<-Outbreaks[which(Outbreaks$Country_name %in% 'Turkey'),] 
o_turkey$TotalOutbreaks<- o_turkey$TotalOutbreaks* 0.97
o_turkey$Country_name<- 'Turkey (Anatolia)'
o_turkey_thrace<-Outbreaks[which(Outbreaks$Country_name %in% 'Turkey'),] 
o_turkey_thrace$TotalOutbreaks<- o_turkey_thrace$TotalOutbreaks* 0.03
o_turkey_thrace$Country_name<- 'Turkey (Thrace)'
Outbreaks<- rbind(Outbreaks, o_turkey) 
Outbreaks<- rbind(Outbreaks, o_turkey_thrace) 

#############################################
#Merging world data Data
#############################################

FAO.new.cn <- merge(VADEMOS.countries, FAO.DB)

FAO.new.cn <- merge(FAO.new.cn, World_data) # Adding ISO3 codes, LON and LAT from the world data file




############################################
# Create animal categories
############################################

unique(FAO.new.cn$Item)

Large.ruminants <- c("Cattle", "Buffaloes") # Category for Large Animals
Small.ruminants <- c("Goats", "Sheep") # Category for Small Animals


FAO.new.cn <- subset(FAO.new.cn, Item %in% c(Large.ruminants, Small.ruminants, "Pigs")) # Subset only the species of interest

FAO.new.cn$Species <- FAO.new.cn$Item # Create a new column for the species category 
FAO.new.cn$Species[which(FAO.new.cn$Species %in% Large.ruminants)] <- "LR" 
FAO.new.cn$Species[which(FAO.new.cn$Species %in% Small.ruminants)] <- "SR"
FAO.new.cn$Species[which(FAO.new.cn$Species == "Pigs")] <- "P" # Remind that some countries do not have a record for pigs (i.e. Muslim countries)


str(FAO.new.cn)

######################################
# Creation of the data DB for VADEMOS
######################################

VADEMOS.db<-FAO.new.cn

VADEMOS.db <- aggregate(Pop ~ Country_name+Region+Subregion+Year+Roadmap+Species+Country_code+LAT+LON, VADEMOS.db, sum, na.rm = TRUE) # Calculate the population by species category (i.e. LR = cattle + buffalo)





########################################
#Adding regional and subregionals aggregates
##########################################

#AFRICA
all_africa <-VADEMOS.db[which(VADEMOS.db$Region %in% "Africa"),] 
all_africa$Country_name <- 'All Africa'
#get aggregates pop and agricultural land

all_africa_df<- aggregate(Pop ~ Country_name+Year+Species,data=all_africa,FUN= sum, na.rm = TRUE)


#populate other variables
all_africa_df$Region<- 'Africa'
all_africa_df$Subregion<- 'All Africa Subregions'
all_africa_df$Roadmap<-'All Roadmaps'
all_africa_df$Country_code<-'AllAF'
all_africa_df$LAT<-8.78
all_africa_df$LON<-34.50

#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_africa_df)

#ASIA

all_asia <-VADEMOS.db[which(VADEMOS.db$Region %in% "Asia"),] 
all_asia$Country_name <- 'All Asia'
#get aggregates pop and agricultural land

all_asia_df<- aggregate(Pop ~ Country_name+Year+Species,data=all_asia,FUN= sum, na.rm = TRUE)


#populate other variables
all_asia_df$Region<- 'Asia'
all_asia_df$Subregion<- 'All Subregions'
all_asia_df$Roadmap<-'All Roadmaps'
all_asia_df$Country_code<-'AllAS'
all_asia_df$LAT<-45.45
all_asia_df$LON<-68.83


#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_asia_df)


# 1.West Eurasia       

all_westeurasia <-VADEMOS.db[which(VADEMOS.db$Subregion %in% "West Eurasia"),] 
all_westeurasia$Country_name <- 'All West Eurasia'
#get aggregates pop and agricultural land

all_westeurasia_df<- aggregate(Pop ~ Country_name+Year+Species,data=all_westeurasia,FUN= sum, na.rm = TRUE)


#populate other variables
all_westeurasia_df$Region<- 'Asia'
all_westeurasia_df$Subregion<- 'West Eurasia'
all_westeurasia_df$Roadmap<-'WE'
all_westeurasia_df$Country_code<-'AllWE'
all_westeurasia_df$LAT<-28.99
all_westeurasia_df$LON<-49.25


#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_westeurasia_df)


# 2. Southern Africa      

all_southernafrica <-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Southern Africa"),] 
all_southernafrica$Country_name <- 'All Southern Africa'
#get aggregates pop and agricultural land

all_southernafrica_df<- aggregate(Pop ~ Country_name+Year+Species,data=all_southernafrica,FUN= sum, na.rm = TRUE)


#populate other variables
all_southernafrica_df$Region<- 'Africa'
all_southernafrica_df$Subregion<- 'Southern Africa'
all_southernafrica_df$Roadmap<-'SADC'
all_southernafrica_df$Country_code<-'AllSADC'
all_southernafrica_df$LAT<-19.56
all_southernafrica_df$LON<-24.35


#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_southernafrica_df)



# 3. Middle East      

all_middleeast <-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Middle East"),] 
all_middleeast$Country_name <- 'All Middle East'
#get aggregates pop and agricultural land

all_middleeast_df<- aggregate(Pop ~ Country_name+Year+Species,data=all_middleeast,FUN= sum, na.rm = TRUE)


#populate other variables
all_middleeast_df$Region<- 'Asia'
all_middleeast_df$Subregion<- 'Middle East'
all_middleeast_df$Roadmap<-'ME'
all_middleeast_df$Country_code<-'AllME'
all_middleeast_df$LAT<-29.29
all_middleeast_df$LON<-42.55



#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_middleeast_df)


# 4. South Asia      

all_southasia <-VADEMOS.db[which(VADEMOS.db$Subregion %in% "South Asia"),] 
all_southasia$Country_name <- 'All South Asia '
#get aggregates pop and agricultural land

all_southasia_df<- aggregate(Pop ~ Country_name+Year+Species,data=all_southasia,FUN= sum, na.rm = TRUE)


#populate other variables
all_southasia_df$Region<- 'Asia'
all_southasia_df$Subregion<- 'South Asia'
all_southasia_df$Roadmap<-'SAARC'
all_southasia_df$Country_code<-'AllSAARC'
all_southasia_df$LAT<-25.03
all_southasia_df$LON<-76.45

#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_southasia_df)


# 5. Western Africa     

all_westernafrica <-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Western Africa"),] 
all_westernafrica$Country_name <- 'All Western Africa'
#get aggregates pop and agricultural land

all_westernafrica_df<- aggregate(Pop ~ Country_name+Year+Species,data=all_westernafrica,FUN= sum, na.rm = TRUE)


#populate other variables
all_westernafrica_df$Region<- 'Africa'
all_westernafrica_df$Subregion<- 'Western Africa'
all_westernafrica_df$Roadmap<-'WA'
all_westernafrica_df$Country_code<-'AllWA'
all_westernafrica_df$LAT<-13.53
all_westernafrica_df$LON<-2.46

#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_westernafrica_df)




# 6. East Africa    

all_eastafrica <-VADEMOS.db[which(VADEMOS.db$Subregion %in% "East Africa"),] 
all_eastafrica$Country_name <- 'All East Africa'
#get aggregates pop and agricultural land

all_eastafrica_df<- aggregate(Pop ~ Country_name+Year+Species,data=all_eastafrica,FUN= sum, na.rm = TRUE)


#populate other variables
all_eastafrica_df$Region<- 'Africa'
all_eastafrica_df$Subregion<- 'East Africa'
all_eastafrica_df$Roadmap<-'EA'
all_eastafrica_df$Country_code<-'AllEA'
all_eastafrica_df$LAT<-1.95
all_eastafrica_df$LON<-37.29


#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_eastafrica_df)

# 7. Central Africa     

all_centralafrica <-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Central Africa"),] 
all_centralafrica$Country_name <- 'All Central Africa'
#get aggregates pop and agricultural land

all_centralafrica_df<- aggregate(Pop~ Country_name+Year+Species,data=all_centralafrica,FUN= sum, na.rm = TRUE)


#populate other variables
all_centralafrica_df$Region<- 'Africa'
all_centralafrica_df$Subregion<- 'Central Africa'
all_centralafrica_df$Roadmap<-'CA'
all_centralafrica_df$Country_code<-'AllCA'
all_centralafrica_df$LAT<-6.61
all_centralafrica_df$LON<-20.93


#Add to VADEMOS
VADEMOS.db <- bind_rows(VADEMOS.db,all_centralafrica_df)



#########################################################
# Creation of the prevpop DB for population growth rates 
########################################################

prevpop <- aggregate(Pop ~ Country_code+Year+Species, VADEMOS.db, sum, na.rm = TRUE) # Sum the population value by country, year and species category.

prevpop <- prevpop[prevpop$Year>1992, ]


Date <- Sys.time()
Date <- format(Date, format="%y %m %d")
write.csv(prevpop,paste("prevpop", Date, ".csv"), row.names = FALSE)


######################################
#PopGrowth difference with previous year
#####################################

#create a temp df to calculate population growth difference

tmp <- VADEMOS.db[, c("Country_name", "Year", "Species","Pop")]

tmp <-tmp %>% 
  distinct() %>% 
  spread(Species, Pop) 


tmp[["P"]][is.na(tmp[["P"]])] <- 1


tmp$LR.diff <- c(0 ,diff(tmp$LR))
tmp$LR.diff <- c(0,tmp$LR.diff[2:length(tmp$LR.diff)]/tmp$LR[1:length(tmp$LR)-1])

tmp$SR.diff <- c(0 ,diff(tmp$SR))
tmp$SR.diff <- c(0,tmp$SR.diff[2:length(tmp$SR.diff)]/tmp$SR[1:length(tmp$SR)-1])

tmp$P.diff <- c(0 ,diff(tmp$P))
tmp$P.diff <- c(0,tmp$P.diff[2:length(tmp$P.diff)]/tmp$P[1:length(tmp$P)-1])


#merge tmp 
tmp<-tmp [, c(1,2, 6:8)]
names(tmp) [3]<-"LR"
names(tmp) [4]<-"SR"
names(tmp) [5]<-"P"
tmp<-as.data.frame(tmp)
tmp<-melt(tmp, id=c("Country_name", "Year"))
colnames(tmp)[4] <- "PopGrowth"
colnames(tmp)[3] <- "Species" 
#tmp<-subset(tmp, Year >2009 )  #remove 2009 as when not running the loop this year has wrong pop growth dif


VADEMOS.db <- merge(x = VADEMOS.db, y = tmp, by.x = c("Country_name", "Year", "Species"), by.y = c("Country_name", "Year", "Species") , all.x = TRUE) 

unique(VADEMOS.db$Country_name) 


#####subset per year

VADEMOS.db <- subset(VADEMOS.db, Year > 2009) # Subset the database of the new countries by first data year used in VADEMOS (2011)

VADEMOS.db <- subset(VADEMOS.db, Year < 2020) # Subset the database of the new countries by last data year used in VADEMOS (2019)



#################################
#Adding agricultural land
################################

Agricultural.land$Agricultural_land<- Agricultural.land$Agricultural_land*10 # Transform the unit of Agricultural land from 1000h to 100h 


VADEMOS.db <- merge(x = VADEMOS.db, y = Agricultural.land, by.x = c("Country_name", "Year"), by.y = c("Country_name", "Year") , all.x = TRUE) # Add the Agricultural land value

unique(VADEMOS.db$Country_name)

#adding the agregated agricultural land for the aggreagted areas
a<-VADEMOS.db[which(VADEMOS.db$Region %in% "Africa"),]
a$Country_name <- "All Africa"
a<- aggregate(Agricultural_land ~ Country_name + Species +Year, a, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All Africa'] <- a$Agricultural_land

as<-VADEMOS.db[which(VADEMOS.db$Region %in% "Asia"),]
as$Country_name <- "All Asia"
as<- aggregate(Agricultural_land ~ Country_name + Species +Year, as, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All Asia'] <- as$Agricultural_land

wa<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "West Eurasia"),]
wa$Country_name <- "All West Eurasia"
wa<- aggregate(Agricultural_land ~ Country_name + Species +Year, wa, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All West Eurasia'] <- wa$Agricultural_land

sa<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "South Asia"),]
sa$Country_name <- "All South Asia"
sas<- aggregate(Agricultural_land ~ Country_name + Species +Year, sa, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All South Asia '] <- sas$Agricultural_land

me<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Middle East"),]
me$Country_name <- "All Middle East"
me<- aggregate(Agricultural_land ~ Country_name + Species +Year, me, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All Middle East'] <- me$Agricultural_land

soa<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Southern Africa"),]
soa$Country_name <- "All Southern Africa"
soa<- aggregate(Agricultural_land ~ Country_name + Species +Year, soa, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All Southern Africa'] <- soa$Agricultural_land

wea<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Western Africa"),]
wea$Country_name <- "All Western Africa"
wea<- aggregate(Agricultural_land ~ Country_name + Species +Year, wea, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All Western Africa'] <- wea$Agricultural_land

ea<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "East Africa"),]
ea$Country_name <- "All East Africa"
ea<- aggregate(Agricultural_land ~ Country_name + Species +Year, ea, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All East Africa'] <- ea$Agricultural_land

ca<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Central Africa"),]
ca$Country_name <- "All Central Africa"
ca<- aggregate(Agricultural_land ~ Country_name + Species +Year, ca, sum, na.rm = TRUE)
VADEMOS.db$Agricultural_land[VADEMOS.db$Country_name =='All Central Africa'] <- ca$Agricultural_land






VADEMOS.db$LSD_agriculturalarea <- VADEMOS.db$Pop/VADEMOS.db$Agricultural_land# Calculate the Livestock Stock Density 
options(scipen=999) #scientific notation off


##################################################
# Adding the proportion of beef and dairy cattle
#################################################

prop.beef.dairy <- subset(beef.dairy.cattle, Item %in% c("Meat, cattle", "Milk, whole fresh cow"))

prop.beef.dairy <- prop.beef.dairy %>% spread(Item, Value) # using "spread" function is calculated the number of heads for beef and dairy category

names(prop.beef.dairy)[names(prop.beef.dairy) == "Meat, cattle"] <- "PopBeef"
names(prop.beef.dairy)[names(prop.beef.dairy) == "Milk, whole fresh cow"] <- "PopDairy"

VADEMOS.db <- merge(x = VADEMOS.db, y = prop.beef.dairy, by.x = c("Country_name", "Year") , by.y = c("Country_name", "Year"), all.x = TRUE) 


VADEMOS.db$PopBeef[which(VADEMOS.db$Species != "LR")] <- NA
VADEMOS.db$PopDairy[which(VADEMOS.db$Species != "LR")] <- NA


VADEMOS.db$PropDairy <- VADEMOS.db$PopDairy/(VADEMOS.db$PopBeef + VADEMOS.db$PopDairy)
VADEMOS.db$PropBeef <- VADEMOS.db$PopBeef/(VADEMOS.db$PopBeef + VADEMOS.db$PopDairy)

VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name == "All Africa")] <- 0.50
VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name == "All Asia")] <- 0.50
VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name == "All West Eurasia")] <- 0.50
VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name == "All Southern Africa")] <- 0.50
VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name == "All Middle East")] <- 0.50
VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name == "All South Asia ")] <- 0.50
VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name == "All Central Africa")] <- 0.50
VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name == "All Western Africa")] <- 0.50
VADEMOS.db$PropDairy[which(VADEMOS.db$Country_name ==  "All East Africa")] <- 0.50

VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name == "All Africa")] <- 0.35
VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name == "All Asia")] <- 0.35
VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name == "All West Eurasia")] <- 0.35
VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name == "All Southern Africa")] <- 0.35
VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name == "All Middle East")] <- 0.35
VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name == "All South Asia ")] <- 0.35
VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name == "All Central Africa")] <- 0.35
VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name == "All Western Africa")] <- 0.35
VADEMOS.db$PropBeef[which(VADEMOS.db$Country_name ==  "All East Africa")] <- 0.35




#############################
#Add PCP 
#############################

VADEMOS.db <- merge(x = VADEMOS.db, y = PCP, by.x = c("Country_name", "Year"), by.y = c("Country_name", "Year") , all.x = TRUE)

#PCP for aggregated areas using median and thrace
VADEMOS.db$PCP[which(VADEMOS.db$Country_name ==  "Turkey (Thrace)")] <- 4
VADEMOS.db$PCP[which(VADEMOS.db$Country_name == "All Africa")] <- 2
VADEMOS.db$PCP[which(VADEMOS.db$Country_name == "All Asia")] <- 2
VADEMOS.db$PCP[which(VADEMOS.db$Country_name == "All West Eurasia")] <- 2
VADEMOS.db$PCP[which(VADEMOS.db$Country_name == "All Southern Africa")] <- 2
VADEMOS.db$PCP[which(VADEMOS.db$Country_name == "All Middle East")] <- 2
VADEMOS.db$PCP[which(VADEMOS.db$Country_name == "All South Asia ")] <- 2
VADEMOS.db$PCP[which(VADEMOS.db$Country_name == "All Central Africa")] <- 1
VADEMOS.db$PCP[which(VADEMOS.db$Country_name == "All Western Africa")] <- 1
VADEMOS.db$PCP[which(VADEMOS.db$Country_name ==  "All East Africa")] <- 1

#pool for aggregated areas combined
VADEMOS.db$Pool[which(VADEMOS.db$Country_name ==  "Turkey (Thrace)")] <- 'Pool 3'
VADEMOS.db$Pool[which(VADEMOS.db$Country_name == "All Africa")] <- 'Combination of 6, 5 and 4'
VADEMOS.db$Pool[which(VADEMOS.db$Country_name == "All Asia")] <- 'Combination of 2, 3 and 4'
VADEMOS.db$Pool[which(VADEMOS.db$Country_name == "All West Eurasia")] <- 'Pool 3'
VADEMOS.db$Pool[which(VADEMOS.db$Country_name == "All Southern Africa")] <- 'Pool 4, 5 and 6'
VADEMOS.db$Pool[which(VADEMOS.db$Country_name == "All Middle East")] <- 'Pool 4, 5 and 6'
VADEMOS.db$Pool[which(VADEMOS.db$Country_name == "All South Asia ")] <- 'Pool 2'
VADEMOS.db$Pool[which(VADEMOS.db$Country_name == "All Central Africa")] <-'Pool 5 '
VADEMOS.db$Pool[which(VADEMOS.db$Country_name == "All Western Africa")] <- 'Pool 5'
VADEMOS.db$Pool[which(VADEMOS.db$Country_name ==  "All East Africa")] <- 'Pool 4'

#serotype for aggregated areas combined
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name ==  "Turkey (Thrace)")] <- 'O,A,ASIA1'
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == "All Africa")] <- 'SAT1, SAT2,SAT3, o, A'
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == "All Asia")] <- 'Asia1, SAT2, o, A'
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == "All West Eurasia")] <- 'Asia1, o, A'
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == "All Southern Africa")] <- 'SAT1, SAT2, SAT3, o, A'
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == "All Middle East")] <- 'SAT1, SAT2, SAT3, o, A'
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == "All South Asia ")] <- 'Asia1, o, A'
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == "All Central Africa")] <-'SAT2, o, A '
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == "All Western Africa")] <- 'SAT1, SAT2, SAT3, o, A'
VADEMOS.db$Serotype[which(VADEMOS.db$Country_name ==  "All East Africa")] <- 'SAT1, SAT2, SAT3, o, A'


##################################
# Add parameters with fixed values
#################################
VADEMOS.db$VaccinationArea <- 314
VADEMOS.db$DiffByRegion <- ""  
VADEMOS.db$SubnationalRegion <- ""
VADEMOS.db$SubnationalRegion [which(VADEMOS.db$Country_name ==  "Turkey (Thrace)")] <- 'Thrace'
VADEMOS.db$SubnationalRegion [which(VADEMOS.db$Country_name ==  "Turkey (Anatolia)")] <- 'Anatolia'
VADEMOS.db$PopPropByRegion <- ""
VADEMOS.db$PopPropByRegion[which(VADEMOS.db$Country_name ==  "Turkey (Thrace)")] <- 0.06
 

VADEMOS.db$VaccineSchedule_AS <- 2 #was already commented but using it as the below dont work
VADEMOS.db$VaccineSchedule_YS <- 2

#VADEMOS.db$VaccineSchedule_AS[which(VADEMOS.db$Species == "LR")] <- 2
VADEMOS.db$VaccineSchedule_AS[which(VADEMOS.db$Species == "SR")] <- 1

#VADEMOS.db$VaccineSchedule_YS[which(VADEMOS.db$Species == "LR")] <- 2
VADEMOS.db$VaccineSchedule_YS[which(VADEMOS.db$Species == "SR")] <- 1


VADEMOS.db$PropYS <- 0.3
VADEMOS.db$PropSmall <- NA

#################################
#adding Outbreaks
#######################
VADEMOS.db <- merge(x = VADEMOS.db, y = Outbreaks, by.x = c("Country_name", "Year"), by.y = c("Country_name", "Year") , all.x = TRUE)


#adding the agregated agricultural land for the aggreagted areas
a1<-VADEMOS.db[which(VADEMOS.db$Region %in% "Africa"),]
a1$Country_name <- "All Africa"
a1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, a1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All Africa'] <- a1$TotalOutbreaks

as1<-VADEMOS.db[which(VADEMOS.db$Region %in% "Asia"),]
as1$Country_name <- "All Asia"
as1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, as1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All Asia'] <- as1$TotalOutbreaks

wa1<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "West Eurasia"),]
wa1$Country_name <- "All West Eurasia"
wa1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, wa1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All West Eurasia'] <- wa1$TotalOutbreaks

sa1<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "South Asia"),]
sa1$Country_name <- "All South Asia"
sa1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, sa1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All South Asia '] <- sa1$TotalOutbreaks

me1<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Middle East"),]
me1$Country_name <- "All Middle East"
me1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, me1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All Middle East'] <- me1$TotalOutbreaks

soa1<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Southern Africa"),]
soa1$Country_name <- "All Southern Africa"
soa1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, soa1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All Southern Africa'] <- soa1$TotalOutbreaks

wea1<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Western Africa"),]
wea1$Country_name <- "All Western Africa"
wea1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, wea1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All Western Africa'] <- wea1$TotalOutbreaks

ea1<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "East Africa"),]
ea1$Country_name <- "All East Africa"
ea1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, ea1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All East Africa'] <- ea1$TotalOutbreaks

ca1<-VADEMOS.db[which(VADEMOS.db$Subregion %in% "Central Africa"),]
ca1$Country_name <- "All Central Africa"
ca1<- aggregate(TotalOutbreaks ~ Country_name + Species +Year, ca1, sum, na.rm = TRUE)
VADEMOS.db$TotalOutbreaks[VADEMOS.db$Country_name =='All Central Africa'] <- ca1$TotalOutbreaks








##################################
#no pig countries
################################

country_names <-VADEMOS.countries$Country_name
No.pig.countries <- function(x) {
  Vademos <- unique(x[,c(1,3)])
  Vademos <- unique(Vademos[which(Vademos$Species == "P"),])
  country.no.pigs <- country_names[-which(country_names %in% Vademos$Country_name)]
    return(country.no.pigs) # Function to find the countries that do not have pig records (pig records have to be present in the file)
}



country.no.pigs <- No.pig.countries(VADEMOS.db) # Vector with the countries with no pigs records

country.no.pigs

# New.pig.rows <- VADEMOS.db   copy DF with types and all
# New.pig.rows[] <- NA

New.pig.rows <-data.frame(matrix(ncol = length(colnames(VADEMOS.db)), nrow = 0)) # Create a new Data frame similar to Vademos DB but empty
colnames(New.pig.rows) <- colnames(VADEMOS.db)

New.pig.rows

for (i in 1:length(country.no.pigs))  { # Function to create pig records for the countries do not have them
  country <- country.no.pigs[i]
  tmp <- subset(VADEMOS.db, Country_name == country & Species == "SR")
  tmp$Pop <- 1 # Pig population is assumed to be "1"
  tmp$Species <- "P"
  tmp$LSD_agriculturalarea <- tmp$Pop/tmp$Agricultural_land
  tmp$VaccineSchedule_AS <- NA
  tmp$VaccineSchedule_YS <- NA
  
  New.pig.rows <- rbind(New.pig.rows,tmp)
}

New.pig.rows$Country_name <- as.character(New.pig.rows$Country_name)
New.pig.rows$YEAR <-as.integer(New.pig.rows$Year)
colnames(VADEMOS.db)
VADEMOS.db <- bind_rows(VADEMOS.db,New.pig.rows)




unique(VADEMOS.db$Country_name) #73 countries





#########Rearange columns 



VADEMOS.db <- VADEMOS.db[, c(1,2,10, 14,15,3,11, 16, 17,12,13,7,4,5,20:29,6,8,9,18,19)]





#change null to 0
VADEMOS.db$PCP[is.na(VADEMOS.db$PCP)] <- 0


####################Create New records for 2030

New.records <- data.frame(matrix(ncol = length(colnames(VADEMOS.db)), nrow = 0))
colnames(New.records) <- colnames(VADEMOS.db)

j <- 0


for (i in 1:length(unique(VADEMOS.db$Country_name))) { # Function to create blank records for predicted years
  
  tmp <- data.frame(matrix(ncol = length(colnames(VADEMOS.db)), nrow = 11*3))
  colnames(tmp) <- colnames(VADEMOS.db)
  country <- unique(VADEMOS.db$Country_name)[i]
  tmp$Country_name <- country
  
  tmp$Year <- seq(from = 2020, to = 2030, by = 1)
  
  tmp$Species <- rep(c("LR", "SR", "P"))     #,11
  
  
  tmp$PCP <- VADEMOS.db$PCP[which(VADEMOS.db$Country_name == country & VADEMOS.db$Year== 2019)] 
  
  
  #static values
  country_code <- unique(VADEMOS.db$Country_code[which(VADEMOS.db$Country_name == country )])
  tmp$Country_code <- country_code
  
  region <- unique(VADEMOS.db$Region[which(VADEMOS.db$Country_name == country )])
  tmp$Region <- region
  
  subregion <- unique(VADEMOS.db$Subregion[which(VADEMOS.db$Country_name == country )])
  tmp$Subregion <- subregion
  
  subnationalregion <- unique(VADEMOS.db$SubnationalRegion[which(VADEMOS.db$Country_name == country )])
  tmp$SubnationalRegion <- subnationalregion
  
  tmp$Pool <- VADEMOS.db$Pool[which(VADEMOS.db$Country_name == country & VADEMOS.db$Year == 2019)]
  tmp$Serotype<-unique(VADEMOS.db$Serotype[which(VADEMOS.db$Country_name == country & VADEMOS.db$Year == 2019)])
  
  
  LON<-unique(VADEMOS.db$LON[which(VADEMOS.db$Country_name == country)])
  tmp$LON <- LON
  
  LAT<-unique(VADEMOS.db$LAT[which(VADEMOS.db$Country_name == country)])
  tmp$LAT <-LAT
  
  Roadmap<-unique(VADEMOS.db$Roadmap[which(VADEMOS.db$Country_name == country)])
  tmp$Roadmap <- Roadmap
  
  tmp$VaccinationArea <- 314
  tmp$VaccineSchedule_AS[which(tmp$Species == "LR")] <- 2
  tmp$VaccineSchedule_AS[which(tmp$Species == "SR")] <- 1
  tmp$VaccineSchedule_YS[which(tmp$Species == "LR")] <- 2
  tmp$VaccineSchedule_YS[which(tmp$Species == "SR")] <- 1
  tmp$PropYS <- 0.3
  
  
  New.records <- rbind(New.records,tmp)
  
  j<-j+1
  print(j)
}



unique(New.records$Country_code)


#############################

VADEMOS.db.DEF <- rbind(VADEMOS.db, New.records)


VADEMOS.db.DEF<-as.matrix(VADEMOS.db.DEF)




write.csv(VADEMOS.db.DEF,paste("VADEMOSDB", ".csv"), row.names = FALSE, na = "")






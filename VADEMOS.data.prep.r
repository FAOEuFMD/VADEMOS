
#########################################################
##### Creation of database for VADEMOS ########
#########################################################

# # Loading libraries e working directory ####

library(tidyverse)
library(data.table)
library(reshape)
library(dplyr)
library(readxl)
library(countrycode)


# Loading data,  selecting columns from the new density and existing database
density_df <-fread('all_species_head.csv')
density_df <- subset(density_df, CONTINENT %in% c("Asia", "Africa"))
colnames(density_df)
density_df <- as.data.frame(density_df)
density_df <- density_df%>% 
  dplyr::select("fid","GID_0","NAME_0","VARNAME_0","GID_1","NAME_1","VARNAME_1","TYPE_1","ENGTYPE_1", "GID_2","NAME_2","VARNAME_2","TYPE_2","ENGTYPE_2","GID_3","NAME_3","VARNAME_3","TYPE_3","ENGTYPE_3","COUNTRY" ,"CONTINENT","Specie","%"  )

#checking columns that are empty or that will not be used to remove them from the list above
unique(density_df$VARREGION)

# Add Subregion and Country_name columns to density_df from the countrycode package
#https://github.com/datasets/country-codes/blob/master/data/country-codes.csv
#countrycode::codelist colnames() to see possible destinations
density_df$Subregion <- countrycode(density_df$GID_0, origin = 'iso3c', destination = 'region23')
density_df$Country_name <- countrycode(density_df$GID_0, origin = 'iso3c', destination = 'country.name.en')

# Save density_df to a CSV file
write.csv(density_df, file = "density_df.csv", row.names = FALSE)


vademos_df <-fread('vademos_db.csv')

# Extract unique country names from both data frames
countries_density <- unique(density_df$GID_0)
countries_vademos <- unique(vademos_df$Country_code)

# Find missing countries in each data frame
missing_in_vademos <- setdiff(countries_density, countries_vademos)
missing_in_density <- setdiff(countries_vademos, countries_density)

# Display the results
cat("Countries in density_df but not in vademos_df:\n")
print(missing_in_vademos)

cat("\nCountries in vademos_df but not in density_df:\n")
print(missing_in_density)


# Filter density_df to get rows where country_name is in missing_in_vademos
missing_density_df <- density_df[density_df$GID_0 %in% missing_in_vademos, ]
missing_density_df





#if wanting to redo de outbreaks average
Outbreaks <-as.data.frame(read_excel("Outbreaks.xlsx"))
Outbreaks$Country <- recode(Outbreaks$Country, 
                      "Cabo Verde" = "Cape Verde", "UAE" ='United Arab Emirates', 
                      "Syria" ='Syrian Arab Republic', "DRC"= 'Democratic Republic of the Congo',
                      "Namibia (zone)" = 'Namibia', 
                      "Sao Tome et Principe" = 'Sao Tome and Principe', 
                      "Guinea Bissau" = 'Guinea-Bissau', 
                      "Turkey (Anatolia)" ='Turkey', 
                      "Iran" = 'Iran (Islamic Republic of)',
                      "Tanzania" = 'United Republic of Tanzania',
                      "CÃ´te d'Ivoire" = "Côte d’Ivoire",
                      "C?te d'Ivoire" = "Côte d’Ivoire",
                      "Cote d’Ivoire" = "Côte d’Ivoire") 

Outbreaks <-Outbreaks[, c(1,4,5,10)]
colnames(Outbreaks)[4] <- "TotalOutbreaks"

Outbreaks <- aggregate(TotalOutbreaks ~ Country+Year, Outbreaks, sum, na.rm = TRUE)
Outbreaks$TotalOutbreaks[is.na(Outbreaks$TotalOutbreaks)] <- 0


#############################################
#Merging Data
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


#########################################################
# Creation of the prevpop DB for population growth rates 
########################################################

prevpop <- aggregate(POP ~ Country_code+Year+Species, FAO.new.cn, sum, na.rm = TRUE) # Sum the population value by country, year and species category.
prevpop



prevpop <- prevpop[prevpop$Year>1992, ]


Date <- Sys.time()
Date <- format(Date, format="%y %m %d")
write.csv(prevpop,paste("prevpop", Date, ".csv"), row.names = FALSE)

unique(prevpop$Country_code)

#72 Countries added


######################################
# Creation of the data DB for VADEMOS
######################################

VADEMOS.db<-FAO.new.cn

VADEMOS.db <- subset(VADEMOS.db, Year > 2008) # Subset the database of the new countries by first data year used in VADEMOS (2011)
#adding 2008 to be able to delete 2009 when passing pop diff
VADEMOS.db <- subset(VADEMOS.db, Year < 2020) # Subset the database of the new countries by last data year used in VADEMOS (2019)

unique(VADEMOS.db$Country)

VADEMOS.db <- aggregate(POP ~ Country_name+Region+Subregion+Year+Roadmap+Species+Country_code+LAT+LON, VADEMOS.db, sum, na.rm = TRUE) # Calculate the population by species category (i.e. LR = cattle + buffalo)


colnames(VADEMOS.db)[1] <- "Country_name" # Change the name of the column regarding the country name
colnames(VADEMOS.db)[10] <- "POP"  #is Pop?



#################################
#Adding agricultural land
################################


Agricultural.land


Agricultural.land$Agricultural_land<- Agricultural.land$Agricultural_land*10 # Transform the unit of Agricultural land from 1000h to 100h 


VADEMOS.db <- merge(x = VADEMOS.db, y = Agricultural.land, by.x = c("Country_name", "Year"), by.y = c("Country", "Year") , all.x = TRUE) # Add the Agricultural land value

unique(VADEMOS.db$Country_name)

VADEMOS.db$LSD_agriculturalarea <- VADEMOS.db$POP/VADEMOS.db$Agricultural_land# Calculate the Livestock Stock Density 
options(scipen=999) #scientific notation off


##################################################
# Adding the proportion of beef and dairy cattle
#################################################

prop.beef.dairy <- subset(beef.dairy.cattle, Item %in% c("Meat, cattle", "Milk, whole fresh cow"))

prop.beef.dairy <- prop.beef.dairy %>% spread(Item, Value) # using "spread" function is calculated the number of heads for beef and dairy category

names(prop.beef.dairy)[names(prop.beef.dairy) == "Meat, cattle"] <- "PopBeef"
names(prop.beef.dairy)[names(prop.beef.dairy) == "Milk, whole fresh cow"] <- "PopDairy"

VADEMOS.db <- merge(x = VADEMOS.db, y = prop.beef.dairy, by.x = c("Country_name", "Year") , by.y = c("Country", "Year"), all.x = TRUE) # Add the ISO3 code from the world data file

unique(VADEMOS.db$Country_name) #72 countries

VADEMOS.db$PopBeef[which(VADEMOS.db$Species != "LR")] <- NA
VADEMOS.db$PopDairy[which(VADEMOS.db$Species != "LR")] <- NA


# VADEMOS.db$PopBeef[which(VADEMOS.db$Species == "LR")] <- round(VADEMOS.db$Pop[which(VADEMOS.db$Species == "LR")] * 0.5,0)
# VADEMOS.db$PopDairy[which(VADEMOS.db$Species == "LR")] <- round(VADEMOS.db$Pop[which(VADEMOS.db$Species == "LR")] * 0.5,0)

VADEMOS.db$PropDairy <- VADEMOS.db$PopDairy/(VADEMOS.db$PopBeef + VADEMOS.db$PopDairy)
VADEMOS.db$PropBeef <- VADEMOS.db$PopBeef/(VADEMOS.db$PopBeef + VADEMOS.db$PopDairy)

# VADEMOS.db$PropDairy <- 0.5
# VADEMOS.db$PropBeef <- 0.5




######################################
#PopGrowth parameters
#####################################

population.growth.list <- list() # create a list for the PopGrowth parameters

#create a temp df to calculate population growth difference

tmp <- VADEMOS.db[, c("Country_name", "Year", "Species","POP")]


tmp <-tmp %>% 
  distinct() %>% 
  spread(Species, POP) 
  

tmp[["P"]][is.na(tmp[["P"]])] <- 1

country_names<-unique(VADEMOS.db$Country_name)
#did not run loop as it gives error diffs done without loop making the first year of each cournty to be erronous to fix

for (i in 1:length(country_names))  { # This loop produces the population growth for each species
  
   #tmp<-tmp[tmp$Country_name==Vademos.db.countries[i],]
  #temp <- subset(tmp, Country_name == i) # not sure why subsetting while tmp already has all countries

  tmp$LR.diff <- c(0 ,diff(tmp$LR))
  tmp$LR.diff <- c(0,tmp$LR.diff[2:length(tmp$LR.diff)]/tmp$LR[1:length(tmp$LR)-1])
  
  tmp$SR.diff <- c(0 ,diff(tmp$SR))
  tmp$SR.diff <- c(0,tmp$SR.diff[2:length(tmp$SR.diff)]/tmp$SR[1:length(tmp$SR)-1])
  
  tmp$P.diff <- c(0 ,diff(tmp$P))
  tmp$P.diff <- c(0,tmp$P.diff[2:length(tmp$P.diff)]/tmp$P[1:length(tmp$P)-1])
  
  
  population.growth.list[[i]] <- tmp
  
  # paste0("population.growth", "1") <- tmp
  
}

population.growth.list


all.population.growth = do.call(rbind, population.growth.list)

all.population.growth$P.diff <- replace(all.population.growth$P.diff, all.population.growth$P.diff == "NaN", 0)

all.population.growth <- all.population.growth[ ,c(1,2,6:8) ]

all.population.growth <- melt(all.population.growth, id=c("Country_name","Year"))

all.population.growth$variable <-as.character(all.population.growth$variable)

all.population.growth$variable[all.population.growth$variable == "LR.diff"] <- "LR"
all.population.growth$variable[all.population.growth$variable == "SR.diff"] <- "SR"
all.population.growth$variable[all.population.growth$variable == "P.diff"] <- "P"

all.population.growth
colnames(all.population.growth)[4] <- "PopGrowth"
colnames(all.population.growth)[3] <- "Species"

#merge tmp for now until for loop gets fixed
tmp<-tmp [, c(1,2, 6:8)]
names(tmp) [3]<-"LR"
names(tmp) [4]<-"SR"
names(tmp) [5]<-"P"
tmp<-as.data.frame(tmp)
tmp<-melt(tmp, id=c("Country_name", "Year"))
colnames(tmp)[4] <- "PopGrowth"
colnames(tmp)[3] <- "Species" 
tmp<-subset(tmp, Year >2009 )  #remove 2009 as when not running the loop this year has gron pop growth dif


VADEMOS.db <- merge(x = VADEMOS.db, y = tmp, by.x = c("Country_name", "Year", "Species"), by.y = c("Country_name", "Year", "Species") , all.x = TRUE) 

unique(VADEMOS.db$Country_code)

###end of tmp to swap with looped allpopgrowth once fixed
VADEMOS.db <- merge(x = VADEMOS.db, y = all.population.growth, by.x = c("Country_name", "Year", "Species"), by.y = c("Country_name", "Year", "Species") , all.x = TRUE) 



VADEMOS.db$PopGrowth

unique(VADEMOS.db$Country_name) 



###########################################
#Atributing Region and Subregion
######################################
#comented out since we kept the list provided by Etiene 


# Attribute.region <- function(x) {
#   vector <- 1:length(x)
#   # vector <- NA
#   for (i in 1:length(x))  {
#     if (x[i] %in% VADEMOS.countries$Country)  {
#         vector[i] <- VADEMOS.countries$Region[which(x[i] == VADEMOS.countries$Country )]
#     }
#   }
#   return(vector)
# } # Function to attribute the region
# 
# VADEMOS.db$Region <-  Attribute.region(VADEMOS.db$Country_name) # Region attribution
# 
# 
# Attribute.subregion <- function(x) {
#   vector <- 1:length(x)
#   # vector <- NA
#   for (i in 1:length(x))  {
#     if (x[i] %in% VADEMOS.countries$Country)  {
#       vector[i] <- VADEMOS.countries$Area[which(x[i] == VADEMOS.countries$Country )]
#     }
#   }
#   return(vector) # Function to attribute the subregion
# }
# 
# VADEMOS.db$Subregion <-  Attribute.subregion(VADEMOS.db$Country_name) #Subregion attribution


#############################
#Add PCP 
#############################

VADEMOS.db <- merge(x = VADEMOS.db, y = PCP, by.x = c("Country_name", "Year"), by.y = c("Country", "Year") , all.x = TRUE)


##################################
# Add parameters with fixed values
#################################

#VADEMOS.db$PCP <- 2 # PCP here to add PCP database and expectation for future years or general rule  
VADEMOS.db$VaccinationArea <- 314
VADEMOS.db$DiffByRegion <- ""
VADEMOS.db$SubnationalRegion <- ""
VADEMOS.db$PopPropByRegion <- ""

# VADEMOS.db$VaccineSchedule_AS <- 2
# VADEMOS.db$VaccineSchedule_YS <- 1

VADEMOS.db$VaccineSchedule_AS[which(VADEMOS.db$Species == "LR")] <- 2
VADEMOS.db$VaccineSchedule_AS[which(VADEMOS.db$Species == "SR")] <- 1

VADEMOS.db$VaccineSchedule_YS[which(VADEMOS.db$Species == "LR")] <- 2
VADEMOS.db$VaccineSchedule_YS[which(VADEMOS.db$Species == "SR")] <- 1


VADEMOS.db$PropYS <- 0.3
VADEMOS.db$PropSmall <- NA

#################################
#adding Outbreaks
#######################
VADEMOS.db <- merge(x = VADEMOS.db, y = Outbreaks, by.x = c("Country_name", "Year"), by.y = c("Country", "Year") , all.x = TRUE)


##################################
#no pig countries
################################

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



VADEMOS.db
unique(VADEMOS.db$Country_name) #72 countries

New.records <- data.frame(matrix(ncol = length(colnames(VADEMOS.db)), nrow = 0))
colnames(New.records) <- colnames(VADEMOS.db)

New.records

unique(VADEMOS.db$Country_name)

 

j <- 0


for (i in 1:length(unique(VADEMOS.db$Country_name))) { # Function to create blank records for predicted years
  
  tmp <- data.frame(matrix(ncol = length(colnames(VADEMOS.db)), nrow = 11*3))
  colnames(tmp) <- colnames(VADEMOS.db)
  country <- unique(VADEMOS.db$Country_name)[i]
  tmp$Country_name <- country
  
  tmp$Year <- seq(from = 2020, to = 2030, by = 1)
  
  tmp$Species <- rep(c("LR", "SR", "P"))     #,11
  

  
  country_code <- unique(VADEMOS.db$Country_code[which(VADEMOS.db$Country_name == country )])
  tmp$Country_code <- country_code
  tmp$Country_code
  region <- unique(VADEMOS.db$Region[which(VADEMOS.db$Country_name == country )])
  tmp$Region <- region
  
  subregion <- unique(VADEMOS.db$Subregion[which(VADEMOS.db$Country_name == country )])
  tmp$Subregion <- subregion
  
  #adding LON and LAT 
  LON<-unique(VADEMOS.db$LON[which(VADEMOS.db$Country_name == country)])
  tmp$LON <- LON
  LAT<-unique(VADEMOS.db$LAT[which(VADEMOS.db$Country_name == country)])
  tmp$LAT <-LAT
  tmp$VaccinationArea <- 314
  
  tmp$VaccineSchedule_AS[which(tmp$Species == "LR")] <- 2
  tmp$VaccineSchedule_AS[which(tmp$Species == "SR")] <- 1
  
  tmp$VaccineSchedule_YS[which(tmp$Species == "LR")] <- 2
  tmp$VaccineSchedule_YS[which(tmp$Species == "SR")] <- 1
  
  tmp$PCP <- 2
  tmp$PropYS <- 0.3
  
  
  New.records <- rbind(New.records,tmp)
  
  j<-j+1
    print(j)
}



unique(New.records$Country_code)

##################################
#Adding subnationalregion for Turkey 
#####################################

VADEMOS.db$SubnationalRegion[which(VADEMOS.db$Country_name == "Turkey")] <- "Anatolia"


turkey<-VADEMOS.db[which(VADEMOS.db$Country_name %in% "Turkey"),] 

turkey$PCP <-4
turkey$SubnationalRegion <- "Thrace"
turkey$PopPropByRegion <- 0.06
VADEMOS.db <- rbind(VADEMOS.db,turkey)

#############################

VADEMOS.db.DEF <- rbind(VADEMOS.db, New.records)



VADEMOS.db.DEF <- VADEMOS.db.DEF[, c(1,2,10, 13, 14, 3, 17, 15, 16, 11,12, 7, 4, 5, 20:29, 6, 8,9, 18, 19)]


VADEMOS.db.DEF<-as.matrix(VADEMOS.db.DEF)




write.csv(VADEMOS.db.DEF,paste("data2", Date, ".csv"), row.names = FALSE, na = "")


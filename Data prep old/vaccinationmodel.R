#####################################################################################################
## Generate variables for monte-carlo simulation
## hyojung.lee@fao.org - Mar.2021 ~

# List of countries
# ~Nov.2021: GEO, TUR, IRN, ETH, ZMB, UGA
# Dec.2021 ~: WA countries + Pakistan
#####################################################################################################

###################################################
# 0 - Load libraries
###################################################
library(mc2d) # mc2d: to construct & analyse 1D and 2D Monte-Carlo Simulation
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(modules)
library(DT)

###################################################
# 1 - Data & Source files
###################################################
# data <- read.csv('data_Dec2021.csv')
# data <- read.csv('data_Feb2022.csv', sep = ";")
# data <- read.csv('data_Mar2022.csv', sep = ",")
# data <- read.csv('data_Mar2022 18 03 22.csv', sep = ",")  #funciona

data <- read.csv('newdata.csv', sep = ",") #nao funciona
data <- data[,1:24]
colnames(data)[10] <- 'Agricultural.land'
colnames(data)[3] <- 'Pop'
data$TotalOutbreaks[is.na(data$TotalOutbreaks)] <- 0
data$PCP[is.na(data$PCP)] <- 0
# data %>% 
#   mutate(TotalOutbreaks = if_else(is.na(TotalOutbreaks), true = 0, false = TotalOutbreaks)) %>%
#   mutate(PCP = if_else(is.na(PCP), true = 0, false = PCP))

# library(data.table)
# setDT(data)
# data[is.na(TotalOutbreaks), TotalOutbreaks := 0]
# data[is.na(PCP), PCP := 0]

# # Replace missing values for landarea in future years using the previous values
# df.c <- df.c %>% tidyr::fill(., Agricultural.land)


delphi <- read.csv('delphi-round1.csv')

source("predictPops.R") # lognormal regression -> to be substituted with better prediction model in future iterations

###################################################
# 2 - Functions to prepare input parameters
###################################################
# countrycode <- "IRN"
# year_to_predict <- 2023

###################################################
# a. Helper functions
###################################################
year_to_split <- 2016 # year to split train-test


get.growthrate <- function(df){
  # pct_changes <- df %>%
  #   split(df$Species) %>%
  #   sapply(function(x) {-diff(x$Pop)/x$Pop[-1]})
  # 
  # pct_changes <- data.frame(pct_changes)
  # years <- unique(df[df$Year > min(df$Year), "Year"])
  # # rownames(pct_changes) <- years # TODO: Remove later this commented part
  # 
  # df2 <- data.frame()
  # for (s in unique(df$Species)){
  #   filtered_by_s <- filter(df, Species == s & Year %in% years)
  #   filtered_by_s$PopGrowth <- pct_changes[, s]
  #   df2 <- rbind(df2, filtered_by_s)
  # } # for loop end
  # return(df2)
  df2 <- df %>%
    group_by(Species) %>%
    mutate(Pop_diff = Pop - dplyr::lag(Pop)) %>% 
    mutate(PopGrowth = Pop_diff / dplyr::lag(Pop)) %>%
    select(-Pop_diff)
  return(df2)
} # function end


get.growthrate.predicted <- function(df){
  df2 <- df %>%
    group_by(Species) %>%
    mutate(Pop_predicted_diff = Pop_predicted - dplyr::lag(Pop_predicted)) %>% 
    mutate(PopGrowth = Pop_predicted_diff / dplyr::lag(Pop_predicted)) %>%
    select(-Pop_predicted_diff)
  return(df2)
} # function end

get.outbreaks <- function(df_train, df_test, df_pred){
  poisson.model <- glm(TotalOutbreaks ~ Year+PCP+TotalPop,
                       df_train,
                       na.action=na.exclude,
                       family=poisson(link="log"))
  # print(summary(poisson.model))
  
  # Test
  pred <- as.integer(predict(poisson.model,
                             df_test,
                             na.action=na.exclude,
                             type="response"))
  #print(summary(pred))
  
  # Future year"s oubreaks: predicted based on the predefined poisson regression
  ON_predicted <- as.integer(predict(poisson.model,
                                     df_pred,
                                     na.action=na.exclude,
                                     type="response"))
  
  # Save the result as a dataframedf, year_to_split, year_to_predict
  ON_predicted <- data.frame(ON_predicted)
  return(ON_predicted)
}


# Delphi method was used to get PCP-dependent parameters
# Get df delphi for pcp of the selected year
# Countries can have either national or subnational level PCPs
get.df.delphi <- function(delphi, pcp){
  pcp <- as.integer(pcp)
  # If next year's PCP is not NA, and a number between 1 and 4
  if (pcp %in% c(1,2,3,4)){
    pcpstr <- sprintf('_%s_m', pcp)
    pcp_dependent_qs <-delphi[grepl(pcpstr, names(delphi))]
    
    # Prophylactic vaccine coverage %: Q2 of Delphi
    q2_ <- pcp_dependent_qs[grepl("Q2_", names(pcp_dependent_qs))]
    
    # FMD outbreak # coverage %: Q3 of Delphi
    q3 <- pcp_dependent_qs[grepl("Q3_", names(pcp_dependent_qs))]
    
    # Outbreak reduction %: Q4 of Delphi
    q4 <- pcp_dependent_qs[grepl("Q4_", names(pcp_dependent_qs))]
    
    df_delphi_output <- data.frame(list(q2_, q3, q4))
    # } else if (pcp==5) { # If next year's PCP is not NA and 5
  } else { # If next year's PCP is not NA and 5
    pcpstr <- "_5_m"
    pcp_dependent_qs <- delphi[grepl(pcpstr, names(delphi))]
    
    # Prophylactic vaccine coverage %: Q2 of Delphi
    q2 <- pcp_dependent_qs[grepl("Q2_", names(pcp_dependent_qs))]
    
    # FMD outbreak # coverage %: Q3 of Delphi
    q3 <- pcp_dependent_qs[grepl("Q3_", names(pcp_dependent_qs))]
    
    # Outbreak reduction %: Q4 of Delphi
    q4 <- pcp_dependent_qs[grepl("Q4_", names(pcp_dependent_qs))]
    
    df_delphi_output <- data.frame(list(q2, q3, q4))
  }
  # } else{
  #   print('Error in loading Delphi data')
  # } # if-else
  df_delphi_output$PCP <- pcp
  return(df_delphi_output)
}


###################################################
# b. Functions that return dfs
###################################################
# For inputtable
get_inputtable <- function(df, countrycode, year_to_predict, subnational=NA){
  
  # Select data of the selected country code
  df.c <- df[df$Country_code == countrycode, ]
  
  # Select data by subnational if exist
  if(!is.na(subnational)){
    df.c <- df.c[df.c$SubnationalRegion == subnational, ]
  }
  
  # Replace missing values for landarea in future years using the previous values
  df.c <- df.c %>% tidyr::fill(., Agricultural.land)
  
  # Get dataframe with predicted pop this could just take the data from VADEMOS.DB 
  df_pred <- get.df.with.pop.predicted(df.c, countrycode, year_to_split, year_to_predict)
  
  ###################################################
  # Livestock density
  # Calculate the value with the new pop_predicted
  ###################################################
  
  df_pred$LSD_agriculturalarea <- df_pred[,"Pop_predicted"] / df_pred[,"Agricultural.land"]
  
  ###################################################
  # Sector pop ?
  ###################################################
  # Add a total population column by adding predicted pops for each animal types
  df_prev <- df.c[df.c$Year < min(df_pred$Year), ]
  
  TotalPop <- aggregate(df_prev$Pop, by=list(Category=df_prev$Country_code, df_prev$Year), FUN=sum, na.rm=TRUE) # use real pop values
  TotalPop_pred <- aggregate(df_pred$Pop_predicted, by=list(Category=df_pred$Country_code, df_pred$Year), FUN=sum, na.rm=TRUE) # use predicted pop values
  TotalPops <- rbind(na.omit(TotalPop), TotalPop_pred) # concat the above two
  names(TotalPops) <- c("Country_code", "Year", "TotalPop")
  
  df_prev <- merge(df_prev, TotalPops, by=c("Country_code", "Year"), all.x=TRUE)
  df_pred <- merge(df_pred, TotalPops, by=c("Country_code", "Year"), all.x=TRUE)
  
  ###################################################
  # Outbreaks
  ###################################################
  # Train and test using the previous, years records
  df_train <- df_prev[df_prev$Year < year_to_split, ]
  df_test <- df_prev[(df_prev$Year >= year_to_split) & (df_prev$Year <= year_to_split+2), ]
  
  ON_predicted <- get.outbreaks(df_train, df_test, df_pred)
  ON_predicted$Year <- df_pred$Year
  ON_predicted$Species <- df_pred$Species
  
  # Append Year and species data
  if (is.na(subnational)){
    names(ON_predicted) <- c("ON_predicted", "Year", "Species")
    # Add predicted outbreaks column
    df_pred <- merge(df_pred, ON_predicted, by = c("Year", "Species"), all.x = TRUE)
  } else {
    ON_predicted$SubnationalRegion <- unique(df_pred$SubnationalRegion)
    names(ON_predicted) <- c("ON_predicted", "Year", "Species", "SubnationalRegion")
    # Add predicted outbreaks column
    df_pred <- merge(df_pred, ON_predicted, by = c("Year", "Species", "SubnationalRegion"), all.x = TRUE)
  }
  
  ###################################################
  # new dataframe - futuredata
  ###################################################
  futuredata <- df_pred[, c("Year", "Species", "Country_name", "Country_code", "Pop_predicted", "TotalPop", "LSD_agriculturalarea",
                            "ON_predicted", "PCP", "SubnationalRegion", "VaccineSchedule_AS","VaccineSchedule_YS",
                            "VaccinationArea", "PropDairy", "PropBeef", "PropYS")]
  
  pastdata <- df_prev[, c("Year", "Species", "Country_name", "Country_code", "Pop", "PopGrowth", "TotalOutbreaks","LSD_agriculturalarea",
                          "PCP", "SubnationalRegion", "VaccineSchedule_AS","VaccineSchedule_YS",
                          "VaccinationArea", "PropDairy", "PropBeef", "PropYS")]
  
  ###################################################
  # Add Growth rates
  ###################################################
  # 1. calculated differences between predicted values -> uniform, as used regression
  
  pastdata2 <- get.growthrate(pastdata)
  futuredata2 <- get.growthrate.predicted(futuredata)
  
  df_merged <-bind_rows(pastdata2, futuredata2) 
  df_merged <- df_merged[order(df_merged$Year),]
  
  # Add a total outbreaks column
  # Use na.rm=TRUE statement to ignore NAS when getting the totals
  TotalON <- aggregate(df_merged$ON_predicted,
                       by=list(Category=df_merged$Country_code, df_merged$SubnationalRegion, df_merged$Year), FUN=sum)
  names(TotalON) <- c("Country_code", "SubnationalRegion", "Year", "TotalON_predicted")
  df2 <- merge(df_merged, TotalON, by=c("Country_code", "SubnationalRegion", "Year"), all.x=TRUE)
  
  df2$Pop_predicted <- round(df2$Pop_predicted, 0)
  
  return(df2)
}

# df_inputtable <- get_inputtable(data, "IRN", 2021)
# df_inputtable <- get_inputtable(data, "TUR", 2023, "Anatolia")
# df_inputtable <- get_inputtable (data, countrycode, year_to_predict)
# df_inputtable

get_reducedtable <- function(df_inputtable){
  
  df_species <- data.frame()
  
  df_lr <- df_inputtable %>%
    filter(Year > 2020, Species == "LR") %>%
    select(c("Year", "PCP", "Pop_predicted"))
  
  df_sr <- df_inputtable %>%
    filter(Year > 2020, Species == "SR") %>%
    select(c("Year", "Pop_predicted"))
  
  df_p <- df_inputtable %>%
    filter(Year > 2020, Species == "P") %>%
    select(c("Year", "Pop_predicted", "TotalON_predicted", "VaccinationArea"))
  
  df_species <- merge(df_lr, df_sr, by='Year')
  df_species <- merge(df_species, df_p, by='Year')
  df_species$VaccinationArea <- sqrt(unique(df_inputtable[, "VaccinationArea"])/3.14)
  colnames(df_species) <- c("Year", "PCP", "Pop_LR", "Pop_SR", "Pop_P", "TotalOutbreaks", "VaccinationArea(km)")
  return(df_species)
}

# get_reducedtable(df_inputtable)
#get_reducedtable(get_inputtable(data, "GEO", 2021))

get_mutated_inputtable <- function(df_edited){
  # Read inputtable
  inputtable <- df_edited
  
  # Convert the radius input to area (km^2)
  inputtable$VaccinationArea.km. <- 3.14*inputtable$VaccinationArea.km.**2
  
  # Mutate the input values similar to the original dataframe
  lr <- inputtable[, c("Year", "PCP", "TotalOutbreaks", "VaccinationArea.km.", "Pop_LR")]
  colnames(lr) <- c("Year", "PCP", "TotalON_predicted", "VaccinationArea", "Pop_predicted")
  lr$Species <- "LR"
  
  sr <- inputtable[, c("Year", "PCP", "TotalOutbreaks", "VaccinationArea.km.", "Pop_SR")]
  colnames(sr) <- c("Year", "PCP", "TotalON_predicted", "VaccinationArea", "Pop_predicted")
  sr$Species <- "SR"
  
  p <- inputtable[, c("Year", "PCP", "TotalOutbreaks", "VaccinationArea.km.", "Pop_P")]
  colnames(p) <- c("Year", "PCP", "TotalON_predicted", "VaccinationArea", "Pop_predicted")
  p$Species <- "P"
  
  # Vertically bind each species' data
  mutated_inputtable <- rbind(lr, p, sr) # order should be the same as how the species are ordered in the inputtable
  
  # Get total population
  TotalPop <- aggregate(mutated_inputtable$Pop, by=list(mutated_inputtable$Year), FUN=sum, na.rm=TRUE) # use real pop values
  names(TotalPop) <- c("Year", "TotalPop")
  
  mutated_inputtable <- merge(mutated_inputtable, TotalPop, by="Year", all.x=TRUE)
  
  mutated_inputtable <- mutated_inputtable[order(mutated_inputtable$Year, mutated_inputtable$Species), ]
  return(mutated_inputtable)
}

# mutated_inputtable <- get_mutated_inputtable()
# mutated_inputtable


# Function to get parameters
run_mc <- function(df_inputtable, year_to_predict,
                   input_vs_lr_as=NA, input_vs_sr_as=NA, input_vs_p_as=NA,
                   input_vs_lr_ys=NA, input_vs_sr_ys=NA, input_vs_p_ys=NA,
                   input_prop_lr_ys=NA, input_prop_sr_ys=NA, input_prop_p_ys=NA,
                   input_propdairy=NA, input_propbeef=NA, input_propsmall=NA,
                   #input_pcp=NA,
                   input_vc_lr=NA, input_vc_sr=NA, input_vc_p=NA,
                   input_oc_lr=NA, input_oc_sr=NA, input_oc_p=NA,
                   edited=FALSE, df_edited=NA){
  
  # if input value is 0, probaility simulation won't be able to yeild an output
  # therefore substitute 0 with 0.001
  for (input in c(input_vs_lr_as, input_vs_sr_as, input_vs_p_as,
                  input_vs_lr_ys, input_vs_sr_ys, input_vs_p_ys,
                  input_prop_lr_ys, input_prop_sr_ys, input_prop_p_ys,
                  input_propdairy, input_propbeef, input_propsmall,
                  input_vc_lr, input_vc_sr, input_vc_p,
                  input_oc_lr, input_oc_sr, input_oc_p)){
    
    if (is.na(input)){
      invisible()
    } else if (as.numeric(input)==0){
      input <- 0.001
    } else {
      input <- as.numeric(input)
    } # if-else 
  } # for
  
  ###################################################
  # Delphi
  ###################################################
  # We will refer the year for which we are predicting the vaccine demand as 'next year'
  # and the year right before 'next year' as 'prev year' (= year_to_predict - 1)
  # 'year_to_predict' will be chosen by users via app interface
  
  # Get PCP status of 'prev year' and 'next year'
  # Countries can have either national or subnational level PCPs
  #add_delphivars <- function(df2, year_to_predict){
  # merge pcp national and subnational column into one uniuqe col
  df3 <- data.frame()
  
  for (pcp in df_inputtable$PCP){
    df_delphi <- get.df.delphi(delphi, pcp)
    df2 <- merge(df_inputtable, df_delphi, by="PCP")
    df3 <- bind_rows(df3, df2)
  }
  
  df3 <- df3[order(df3$Year),]
  
  df3 <- df3[!duplicated(df3), ]
  # print(tail(df3, 10))
  
  # Params for the year of prediction
  df_nextyear <- df3[df3$Year == year_to_predict, ]
  # Params from one previous year
  df_prevyear <- df3[df3$Year == year_to_predict-1, ]
  # Params from the past years
  df_past <- df3[df3$Year <= year_to_predict-1, ]
  
  # Population of one previous year (year of prediction - 1)
  # 2011-2019 pop_lr, sr, p -> real data
  # from 2020 pop_lr, sr, p -> estimated data
  if (all(is.na(df_prevyear$Pop))){
    pop_LR <- df_prevyear[df_prevyear$Species=="LR", "Pop_predicted"]
    pop_SR <- df_prevyear[df_prevyear$Species=="SR", "Pop_predicted"]
    pop_P <- df_prevyear[df_prevyear$Species=="P", "Pop_predicted"]
  } else {
    pop_LR <- df_prevyear[df_prevyear$Species =="LR", "Pop"]
    pop_SR <- df_prevyear[df_prevyear$Species == "SR", "Pop"]
    pop_P <- df_prevyear[df_prevyear$Species == "P", "Pop"]
  }
  
  # Take growth rate from the previous years
  GR_LR <- df_past[df_past$Species=="LR", "PopGrowth"]
  GR_SR <- df_past[df_past$Species=="SR", "PopGrowth"]
  GR_P <- df_past[df_past$Species=="P", "PopGrowth"]
  
  GR.LR <<-mcstoc(rnorm, type="U", mean=mean(GR_LR, na.rm=TRUE), sd=sd(GR_LR, na.rm=TRUE))
  GR.SR <<-mcstoc(rnorm, type="U", mean=mean(GR_SR, na.rm=TRUE), sd=sd(GR_SR, na.rm=TRUE))
  GR.P <<-mcstoc(rnorm, type="U", mean=mean(GR_P, na.rm=TRUE), sd=sd(GR_P, na.rm=TRUE))
  
  # in case of no pig census available:
  if (is.na(mean(GR.P))){
    GR.P <<- mcstoc(rnorm, type="U", mean=0, sd=0.00001)
  }
  print("Growth rate OK")
  
  # Future year's population: this year's pop + growth rate
  futurepop_LR <<- pop_LR+pop_LR*GR.LR
  futurepop_SR <<- pop_SR+pop_SR*GR.SR
  futurepop_P <<- pop_P+pop_P*GR.P
  
  # in case of no pig census available:
  if (is.na(mean(futurepop_P))){
    futurepop_P <- 0
  }
  
  futurepop <<- futurepop_LR + futurepop_SR + futurepop_P
  
  
  if (edited == TRUE){
    mutated <- get_mutated_inputtable(df_edited)[ , c("Year", "Species", "Pop_predicted")]
    lrpop_from_user <- mutated[(mutated$Year==year_to_predict) & (mutated$Species=="LR"), "Pop_predicted"]
    srpop_from_user <- mutated[(mutated$Year==year_to_predict) & (mutated$Species=="SR"), "Pop_predicted"]
    ppop_from_user <- mutated[(mutated$Year==year_to_predict) & (mutated$Species=="P"), "Pop_predicted"]
    
    futurepop_LR_ <<-mcstoc(rnorm, type="U", mean=as.numeric(lrpop_from_user), sd=sd(futurepop_LR, na.rm=TRUE))
    futurepop_SR_ <<-mcstoc(rnorm, type="U", mean=as.numeric(srpop_from_user), sd=sd(futurepop_SR, na.rm=TRUE))
    futurepop_P_ <<-mcstoc(rnorm, type="U", mean=as.numeric(ppop_from_user), sd=sd(futurepop_P, na.rm=TRUE))
    
    futurepop <<- futurepop_LR_ + futurepop_SR_ + futurepop_P_
    futurepop_LR <<- futurepop_LR_
    futurepop_SR <<- futurepop_SR_
    futurepop_P <<- futurepop_P_
    print("Future popluation edited")
    
  } else if (edited == FALSE){
    print("Future popluation calculated")
    
  } else {
    print("Population error!")
  }
  
  # Delphi responses
  q2_LR <<- unique(df_nextyear[grepl('Q2_LR', names(df_nextyear))])
  q2_SR <<- unique(df_nextyear[grepl('Q2_SR', names(df_nextyear))])
  q2_P <<- unique(df_nextyear[grepl('Q2_P', names(df_nextyear))])
  
  if (is.na(input_vc_lr)){
    q2_LR_mode <- as.numeric(q2_LR[grepl('most', names(q2_LR))])
  } else {
    q2_LR_mode <- as.numeric(input_vc_lr)
  }
  
  if (is.na(input_vc_sr)){
    q2_SR_mode <- as.numeric(q2_SR[grepl('most', names(q2_SR))])
  } else {
    q2_SR_mode <- as.numeric(input_vc_sr)
  }
  
  if (is.na(input_vc_p)){
    q2_P_mode <- as.numeric(q2_P[grepl('most', names(q2_P))])
  } else {
    q2_P_mode <- as.numeric(input_vc_p)
  }
  
  q3_LR <<- unique(df_nextyear[grepl('Q3_LR', names(df_nextyear))])
  q3_SR <<- unique(df_nextyear[grepl('Q3_SR', names(df_nextyear))])
  q3_P <<- unique(df_nextyear[grepl('Q3_P', names(df_nextyear))])
  
  if (is.na(input_oc_lr)){
    q3_LR_mode <- as.numeric(q3_LR[grepl('most', names(q3_LR))])
  } else {
    q3_LR_mode <- as.numeric(input_oc_lr)
  }
  
  if (is.na(input_oc_sr)){
    q3_SR_mode <- as.numeric(q3_SR[grepl('most', names(q3_SR))])
  } else {
    q3_SR_mode <- as.numeric(input_oc_sr)
  }
  
  if (is.na(input_oc_p)){
    q3_P_mode <- as.numeric(q3_P[grepl('most', names(q3_P))])
  } else {
    q3_P_mode <- as.numeric(input_oc_p)
  }
  
  print("Delphi data loaded")
  
  ###################################################################
  # Sector proportions
  ###################################################################
  # Take sector proportions from the previous years
  dairy_data <- df_past[df_past$Species=="LR", "PropDairy"]
  dairy_prop_mean <- mean(dairy_data, na.rm=TRUE)
  dairy_prop_sd <- sd(dairy_data, na.rm=TRUE)
  beef_data <- df_past[df_past$Species=="LR", "PropBeef"]
  beef_prop_mean <- mean(beef_data, na.rm=TRUE)
  beef_prop_sd <- sd(beef_data, na.rm=TRUE)
  
  # No small holder data -> use arbitrary values
  smallholder_prop_mean <- 0.001
  smallholder_prop_sd <- 0.001
  
  if (is.na(input_propdairy)){
    Dairy_Proportion <<- mcstoc(rnorm, type="V", mean=dairy_prop_mean, sd=dairy_prop_sd)
  } else {
    Dairy_Proportion <<- mcstoc(rnorm, type="V", mean=input_propdairy, sd=dairy_prop_sd)
  }
  
  if (is.na(input_propbeef)){
    Beef_Proportion <<- mcstoc(rnorm, type="V", mean=beef_prop_mean, sd=beef_prop_sd)
  } else {
    Beef_Proportion <<- mcstoc(rnorm, type="V", mean=input_propbeef, sd=beef_prop_sd)
  }
  
  if (is.na(input_propsmall)){
    SmallHolder_Proportion <<- mcstoc(rnorm, type="V", mean=smallholder_prop_mean, sd=smallholder_prop_sd)
  } else {
    SmallHolder_Proportion <<- mcstoc(rnorm, type="V", mean=input_propsmall, sd=smallholder_prop_sd)
  }
  
  df_nextyear[df_nextyear$Species=="LR", "PropDairy"] <- dairy_prop_mean
  df_nextyear[df_nextyear$Species=="LR", "PropBeef"] <- beef_prop_mean
  df_nextyear[df_nextyear$Species=="LR", "PropSmall"] <- smallholder_prop_mean
  
  Dairy_Pop <<- Dairy_Proportion*futurepop_LR
  Beef_Pop <<- Beef_Proportion*futurepop_LR
  Smallholder_Pop <<- SmallHolder_Proportion*futurepop_LR
  
  ###################################################################
  # Adult vs Young (~1 yr) stock proportions
  ###################################################################
  # Split populations into young- and adult-stocks
  ys_LR <- df_past[df_past$Species=="LR", "PropYS"]
  ys_LR_sd <- sd(ys_LR, na.rm=TRUE)
  ys_SR <- df_past[df_past$Species=="SR", "PropYS"]
  ys_SR_sd <- sd(ys_SR, na.rm=TRUE)
  ys_P <- df_past[df_past$Species=="P", "PropYS"]
  ys_P_sd <- sd(ys_P, na.rm=TRUE)
  
  # if not existing sr or p props, use lr as default to avoid NA errors
  if (all(is.na(ys_SR))){
    ys_SR <- ys_LR
    ys_SR_sd <- ys_LR_sd
  }
  if (all(is.na(ys_P))){
    ys_P <- ys_LR
    ys_P_sd <- ys_LR_sd
  }
  
  if (is.na(input_prop_lr_ys)){
    ys_prop_LR_mean <- mean(ys_LR, na.rm=TRUE)
  } else {
    ys_prop_LR_mean <- input_prop_lr_ys
  }
  
  if (is.na(input_prop_sr_ys)){
    ys_prop_SR_mean <- mean(ys_SR, na.rm=TRUE)
  } else {
    ys_prop_SR_mean <- input_prop_sr_ys
  }
  
  if (is.na(input_prop_p_ys)){
    ys_prop_P_mean <- mean(ys_P, na.rm=TRUE) # pigs young prop not available -> using sr instead... 
  } else {
    ys_prop_P_mean <- input_prop_p_ys
  }
  
  df_nextyear[df_nextyear$Species=="LR", "PropYS"] <- ys_prop_LR_mean
  df_nextyear[df_nextyear$Species=="SR", "PropYS"] <- ys_prop_SR_mean
  df_nextyear[df_nextyear$Species=="P", "PropYS"] <- ys_prop_P_mean
  
  # Currently, no SR and P YS available in the data -> use arbitrary data
  YS_Proportion_LR <<- mcstoc(rnorm, type="V", mean=ys_prop_LR_mean, sd=ys_LR_sd)
  YS_Proportion_SR <<- mcstoc(rnorm, type="V", mean=ys_prop_SR_mean, sd=ys_SR_sd)
  YS_Proportion_P <<- mcstoc(rnorm, type="V", mean=ys_prop_P_mean, sd=ys_P_sd)
  AS_Proportion_LR <<- (1-YS_Proportion_LR)
  AS_Proportion_SR <<- (1-YS_Proportion_SR)
  AS_Proportion_P <<- (1-YS_Proportion_P)
  
  ###################################################################
  # Sector vaccine number for youngstock
  ###################################################################
  if (is.na(input_vs_lr_as)){
    vs_LR_as <<- df_nextyear[df_nextyear$Species=="LR", "VaccineSchedule_AS"]
  } else {
    vs_LR_as <<- input_vs_lr_as
  }
  
  if (is.na(input_vs_sr_as)){
    vs_SR_as <<- df_nextyear[df_nextyear$Species=="SR", "VaccineSchedule_AS"]
  } else {
    vs_SR_as <<- input_vs_sr_as
  }
  
  if (is.na(input_vs_p_as)){
    vs_P_as <<- 0.01  #### If no vaccine shedule data available
  } else {
    vs_P_as <<- input_vs_p_as
  }
  
  if (is.na(input_vs_lr_ys)){
    vs_LR_ys <<- df_nextyear[df_nextyear$Species=="LR", "VaccineSchedule_YS"]
  } else {
    vs_LR_ys <<- input_vs_lr_ys
  }
  
  if (is.na(input_vs_sr_ys)){
    vs_SR_ys <<- df_nextyear[df_nextyear$Species=="SR", "VaccineSchedule_YS"]
  } else {
    vs_SR_ys <<- input_vs_sr_ys
  }
  
  if (is.na(input_vs_p_ys)){
    vs_P_ys <<- 0.01  #### If no vaccine shedule data available
  } else {
    vs_P_ys <<- input_vs_p_ys
  }
  
  #Sector vaccine number for young stock
  Dairy_YS_VN <<- YS_Proportion_LR * Dairy_Pop * unique(vs_LR_ys)
  Beef_YS_VN <<- YS_Proportion_LR * Beef_Pop * unique(vs_LR_ys)
  Smallholder_YS_VN <<- YS_Proportion_LR * Smallholder_Pop * unique(vs_LR_ys)
  SR_YS_VN <<- YS_Proportion_SR * futurepop_SR * unique(vs_SR_ys)
  P_YS_VN <<- YS_Proportion_P * futurepop_P * unique(vs_LR_ys)
  
  #Sector vaccine number for adult stock
  Dairy_AS_VN <<- AS_Proportion_LR * Dairy_Pop * unique(vs_LR_as)
  Beef_AS_VN <<- AS_Proportion_LR * Beef_Pop * unique(vs_LR_as)
  Smallholder_AS_VN <<- AS_Proportion_LR * Smallholder_Pop * unique(vs_LR_as)
  SR_AS_VN <<- AS_Proportion_SR * futurepop_SR * unique(vs_SR_as)
  P_AS_VN <<- AS_Proportion_P * futurepop_P * unique(vs_P_as)
  
  # Prophylactic vaccine coverage %: Q2 of Delphi
  Prophylactic_VC_LR <<- mcstoc(rpert, type="U", min = min(q2_LR, na.rm=TRUE), mode = mean(q2_LR_mode, na.rm=TRUE), max = max(q2_LR, na.rm=TRUE))
  Prophylactic_VC_SR <<- mcstoc(rpert, type="U", min = min(q2_SR, na.rm=TRUE), mode = mean(q2_SR_mode, na.rm=TRUE), max = max(q2_SR, na.rm=TRUE))
  Prophylactic_VC_P <<- mcstoc(rpert, type="U", min = min(q2_P, na.rm=TRUE), mode = mean(q2_P_mode, na.rm=TRUE),  max = max(q2_P, na.rm=TRUE))
  
  # sum of sector vaccine numbers multiplied by vaccine coverage % for PCP stage
  RVN_LR <<- (Dairy_YS_VN+Beef_YS_VN+Smallholder_YS_VN+Dairy_AS_VN+Beef_AS_VN+Smallholder_AS_VN)*Prophylactic_VC_LR
  RVN_SR <<- (SR_YS_VN+SR_AS_VN)*Prophylactic_VC_SR
  RVN_P <<- (P_YS_VN+P_AS_VN)*Prophylactic_VC_P
  
  # livestock density -> normal distribution ?
  lsd_lr <- df_past[df_past$Species=="LR", "LSD_agriculturalarea"]
  lsd_lr_mean <- mean(lsd_lr, na.rm=TRUE)
  lsd_lr_sd <- sd(lsd_lr, na.rm=TRUE)
  
  lsd_sr <- df_past[df_past$Species=="SR", "LSD_agriculturalarea"]
  lsd_sr_mean <- mean(lsd_lr, na.rm=TRUE)
  lsd_sr_sd <- sd(lsd_lr, na.rm=TRUE)
  
  lsd_p <- df_past[df_past$Species=="P", "LSD_agriculturalarea"]
  lsd_p_mean <- mean(lsd_lr, na.rm=TRUE)
  lsd_p_sd <- sd(lsd_lr, na.rm=TRUE)
  
  LSD_LR <<- mcstoc(rnorm, type="V", lsd_lr_mean, lsd_lr_sd)
  LSD_SR <<- mcstoc(rnorm, type="V", lsd_sr_mean, lsd_sr_sd)
  LSD_P <<- mcstoc(rnorm, type="V", lsd_p_mean, lsd_p_sd)
  
  # no. of livestock in the vaccination area(ring): LSD * vaccination area (ring with 10km radius - 314km2)
  VA <- unique(df_prevyear$VaccinationArea)
  Pop_VA_LR <<- VA * LSD_LR
  Pop_VA_SR <<- VA * LSD_SR
  Pop_VA_P <<- VA * LSD_P
  
  # Outbreak number *poisson dist* *uncertainty*
  # Selected year's TOTAL outbreak number
  if (all(!is.na(df_nextyear$TotalOutbreak))){
    ON_predicted <- unique(df_nextyear$TotalOutbreaks)
  } else {ON_predicted <- unique(df_nextyear$TotalON_predicted)}
  ON_predicted <- unique(df_nextyear$TotalON_predicted)
  
  if (ON_predicted != 0) {
    ON_predicted <<- mcstoc(rpois, type="U", lambda=ON_predicted)
  } else {ON_predicted <<- mcstoc(rpois, type="U", lambda=0.001)} # if-else
  print(paste("Predicted ourbreaks for year", year_to_predict, "is", unique(ON_predicted)))
  
  # Outbreak reduction %: q3 of Delphi
  OC_LR <<- mcstoc(rpert, type="U", min = min(q3_LR, na.rm=TRUE),
                   mode = mean(q3_LR_mode, na.rm=TRUE),
                   max = max(q3_LR, na.rm=TRUE))
  OC_SR <<- mcstoc(rpert, type="U", min = min(q3_SR, na.rm=TRUE),
                   mode = mean(q3_SR_mode, na.rm=TRUE),
                   max = max(q3_SR, na.rm=TRUE))
  OC_P <<- mcstoc(rpert, type="U", min = min(q3_P, na.rm=TRUE),
                  mode = mean(q3_P_mode, na.rm=TRUE),
                  max = max(q3_P, na.rm=TRUE))
  
  # (outbreak#) * (livestock density per sector * vaccination area) * outbreaks covered by emergency vaccines
  O_VC_LR <<- ON_predicted * OC_LR * Pop_VA_LR
  O_VC_SR <<- ON_predicted * OC_SR * Pop_VA_SR
  O_VC_P <<- ON_predicted * OC_P * Pop_VA_P
  
  # Routine vaccine total
  Total_Routine_Vaccines <<- RVN_LR + RVN_SR + RVN_P
  
  # Emergency vaccine total
  Total_Emergency_Vaccines <<- O_VC_LR + O_VC_SR + O_VC_P
  
  # Total doses for the country per year
  Total_vaccines <<- Total_Routine_Vaccines+Total_Emergency_Vaccines
  print('Total vaccines defined')
  
  df4 <- bind_rows(df_past, df_nextyear)
  df4 <- df4[order(df4$Year),]
  # print(df_past$Year)
  # print(df_prevyear$Year)
  # print(df_nextyear$Year)
  # print(df4$Year)
  ###################################################
  # 4 Monte-Carlo Simulation
  ###################################################
  #run_mc_simulation <- function(n1=1001, n2=101,){
  itervar <<- 2001
  iterunc <<- 101
  
  # model
  model_vaccine <- mcmodel({
    
    ndvar(itervar) # default: 1,001 iterations in the variability dimension
    ndunc(iterunc) # default: 101 iterations in the uncertainty dimension
    
    mcobject <- mc(
      # Take growth rate from the previous years
      GR.LR, GR.SR, GR.P,
      
      # Future year's population: this year's pop + growth rate
      futurepop,
      
      # Sector proportions for large ruminants
      Dairy_Proportion, Beef_Proportion, SmallHolder_Proportion,
      Dairy_Pop, Beef_Pop, Smallholder_Pop,
      
      # Age proportions
      YS_Proportion_LR, YS_Proportion_SR, YS_Proportion_P,
      AS_Proportion_LR, AS_Proportion_SR, AS_Proportion_P,
      
      # Prophylactic vaccine coverage %: Q2 of Delphi
      Prophylactic_VC_LR, Prophylactic_VC_SR, Prophylactic_VC_P,
      
      # sum of sector vaccine numbers multiplied by vaccine coverage % for PCP stage
      RVN_LR,  RVN_SR,  RVN_P,
      
      Total_Routine_Vaccines,
      
      # livestock density -> normal distribution ?
      LSD_LR, LSD_SR, LSD_P,
      
      # Predicted # of outbreaks
      ON_predicted,
      
      # Oubreak coverage %: Q3 of Delphi
      OC_LR, OC_SR, OC_P,
      
      # Outbreak reduction %: Q4 of Delphi
      # OR_LR, OR_SR, OR_P,
      
      # (outbreak# * outbreak coverage%) * (livestock density per sector * vaccination area)
      O_VC_LR, O_VC_SR, O_VC_P,
      
      # Emergency vaccine total
      Total_Emergency_Vaccines,
      
      # Total doses for the country per year
      Total_vaccines
    ) # mc object
  }) # mc model
  
  evalmcmod(model_vaccine, seed=666)
  mcresult <- evalmcmod(model_vaccine, seed=666)
  
  
  # Initiate an empty dataframe where to save the results
  vaccine_estimates <- data.frame()
  
  # Total vaccine numbers
  # estimated_ci <- data.frame(quantile(mcresult$Total_vaccines, probs=c(0.025,0.975)))
  # low <- estimated_ci[["mcresult.Total_vaccines.2.5."]][1] # median #Rob:... 2.5th percentile with median uncertainty  variability.
  # high <- estimated_ci[["mcresult.Total_vaccines.97.5."]][1] # median #Rob:... 95th percentile with median uncertainty.
  low <- quantile(apply(unmc(mcresult$Total_vaccines),1, median), probs = 0.025, na.rm= TRUE)
  high <- quantile(apply(unmc(mcresult$Total_vaccines),1, median), probs = 0.975, na.rm= TRUE)
  
  estimated_n <- round(median(mcresult$Total_vaccines, na.rm=TRUE), 0)
  
  print(paste('Predicted vaccines for year', year_to_predict, 'is', estimated_n))
  
  df5 <- df4[df4$Year==year_to_predict, ] # Additional df for the results by species - by Rob (16/02/22)
  df4 <- df4[df4$Year==year_to_predict, ]
  df4$RoutineVaccine <- round(median(mcresult$Total_Routine_Vaccines), 0)
  df4$EmergencyVaccine <- round(median(mcresult$Total_Emergency_Vaccines), 0)
  df4$TotalVaccine <- estimated_n
  df4$TotalVaccine_low <- low
  df4$TotalVaccine_high <- high
  
  vaccine_estimates <- bind_rows(vaccine_estimates, df4)
  
  ###### Additional results by Rob ###
  vaccine_estimatesS <- data.frame()
  # df5$Dairy <- round(median(mcresult$Dairy_Pop), 0)
  # df5$Beef <- round(median(mcresult$Beef_Pop), 0)
  # df5$Smallholder <- round(median(mcresult$Smallholder_Pop), 0)
  df5$LargeRuminants <- round(median(mcresult$RVN_LR), 0)
  df5$SmallRuminants <- round(median(mcresult$RVN_SR), 0)
  df5$Pigs <- round(median(mcresult$RVN_P), 0)
  
  vaccine_estimatesS <- bind_rows(vaccine_estimatesS, df5)
  
  # Select columns to show in the result panel only
  vaccine_estimates <- unique(vaccine_estimates[, c('Year', 'Country_name', 'SubnationalRegion', 'PCP',
                                                    'RoutineVaccine', 'EmergencyVaccine', 'TotalVaccine',
                                                    'TotalVaccine_low', 'TotalVaccine_high')])
  names(vaccine_estimates) <- c('Year', 'Country', 'Region', 'PCP',
                                'ProphylacticVaccines', 'EmergencyVaccines', 'TotalVaccines',
                                'CI_low', 'CI_high')
  # Select columns to show in the result panel for the species results (vaccine_estimatesS - by Rob (16/02/22)
  # vaccine_estimatesS <- unique(vaccine_estimatesS[, c('Year', 'Country_name',"LargeRuminants", "SmallRuminants" )])
  vaccine_estimatesS <- unique(vaccine_estimatesS[, c('Year', 'Country_name', "LargeRuminants","SmallRuminants", "Pigs")])
  names(vaccine_estimatesS) <- c('Year', 'Country', "Large Ruminants", "Small Ruminants", "Pigs")
  
  
  # Round the values
  vaccine_estimates <- vaccine_estimates %>% 
    mutate_if(is.numeric, round) %>%
    mutate_if(is.numeric, as.integer)
  
  print("vaccine numbers estimated")
  return(list(df_inputtable, df4, vaccine_estimates, mcresult, vaccine_estimatesS)) # Modified by Rob (16/02/22)
}

# df_inputtable[which((df_inputtable$Year > 2020) & (df_inputtable$Year <= 2021)),
#               c("Year", "PCP", "VaccinationArea", "Species", "Pop_predicted", "TotalPop", "TotalON_predicted")] <- get_mutated_inputtable()[ ,c("Year", "PCP", "VaccinationArea", "Species", "Pop_predicted", "TotalPop", "TotalON_predicted")]
# df_inputtable

# Concatenate the functions and iter by years
get_results <- function(df, countrycode, year_to_predict,
                        input_vs_lr_as=NA, input_vs_sr_as=NA, input_vs_p_as=NA,
                        input_vs_lr_ys=NA, input_vs_sr_ys=NA, input_vs_p_ys=NA,
                        input_prop_lr_ys=NA, input_prop_sr_ys=NA, input_prop_p_ys=NA,
                        input_propdairy=NA, input_propbeef=NA, input_propsmall=NA,
                        #input_pcp=NA,
                        subnational=NA,
                        input_vc_lr=NA, input_vc_sr=NA, input_vc_p=NA,
                        input_oc_lr=NA, input_oc_sr=NA, input_oc_p=NA,
                        edited=FALSE, df_edited=NA){
  # Initiate an empty dataframe where to sotre results
  df_result <- data.frame()
  df_resultS <- data.frame() # by Rob (16/02/22)
  
  # Select country- or subcountry level data
  if (countrycode != "TUR"){
    df_ <- df[df$Country_code==countrycode, ]
  } else if (countrycode == "TUR"){
    df_ <- df[df$SubnationalRegion == subnational, ]
  }
  
  # Populate input parameters
  df_inputtable <- get_inputtable(df_, countrycode, year_to_predict, subnational)
  
  # If any user inputs from the datatable, substitute the values
  if (edited == TRUE){
    print(df_edited)
    df_inputtable[which((df_inputtable$Year > 2020) & (df_inputtable$Year <= year_to_predict)),
                  c("Year", "PCP", "VaccinationArea", "Species", "Pop_predicted", "TotalPop", "TotalON_predicted")] <- get_mutated_inputtable(df_edited)[ ,c("Year", "PCP", "VaccinationArea", "Species", "Pop_predicted", "TotalPop", "TotalON_predicted")]
  } else{
    invisible()
  }
  
  
  for (year in c(2020:year_to_predict-1)){
    # TODO: Remove this commented code
    # if (df_inputtable[df_inputtable$Year==year, "PCP"] != df_inputtable[df_inputtable$Year==year_to_predict, "PCP"]) {
    results <- run_mc(df_inputtable, year,
                      input_vs_lr_as, input_vs_sr_as, input_vs_p_as,
                      input_vs_lr_ys, input_vs_sr_ys, input_vs_p_ys,
                      input_prop_lr_ys, input_prop_sr_ys, input_prop_p_ys,
                      input_propdairy, input_propbeef, input_propsmall,
                      input_vc_lr = NA, input_vc_sr = NA, input_vc_p = NA,
                      input_oc_lr = NA, input_oc_sr = NA, input_oc_p = NA,
                      edited, df_edited)
    #   
    # 
    # } else {
    # results <- run_mc(df_inputtable, year,
    #                   input_vs_lr_as, input_vs_sr_as, input_vs_p_as,
    #                   input_vs_lr_ys, input_vs_sr_ys, input_vs_p_ys,
    #                   input_prop_lr_ys, input_prop_sr_ys, input_prop_p_ys,
    #                   input_propdairy, input_propbeef, input_propsmall,
    #                   input_vc_lr, input_vc_sr, input_vc_p,
    #                   input_oc_lr, input_oc_sr, input_oc_p,
    #                   edited, df_edited)
    # }
    df_paramtable_ <- results[[1]]
    df_year_predicted_ <- results[[2]]
    vaccine_estimates_ <- results[[3]]
    mcresult_ <- results[[4]]
    vaccine_estimatesS_ <- results[[5]] # Rob (16/02/22)
    df_result <- bind_rows(df_result, vaccine_estimates_)
    df_resultS <- bind_rows(df_resultS, vaccine_estimatesS_) #add by Rob (16/02/22)
  }
  
  results2 <- run_mc(df_inputtable, year_to_predict,
                     input_vs_lr_as, input_vs_sr_as, input_vs_p_as,
                     input_vs_lr_ys, input_vs_sr_ys, input_vs_p_ys,
                     input_prop_lr_ys, input_prop_sr_ys, input_prop_p_ys,
                     input_propdairy, input_propbeef, input_propsmall,
                     input_vc_lr, input_vc_sr, input_vc_p,
                     input_oc_lr, input_oc_sr, input_oc_p,
                     edited, df_edited)
  
  df_paramtable <- results2[[1]]
  df_year_predicted <- results2[[2]]
  vaccine_estimates <- results2[[3]]
  mcresult <- results2[[4]]
  vaccine_estimatesS <- results2[[5]] # by Rob
  df_result <- bind_rows(df_result, vaccine_estimates)
  df_resultS <- bind_rows(df_resultS, vaccine_estimatesS) # by Rob (16/02/22)
  
  return(list(df_paramtable, df_year_predicted, df_result, mcresult, df_resultS)) # by modified by Rob (16/02/22)
} # function

# df_paramtable <- get_results(data, countrycode, year_to_predict)[[1]]
# df_year_predicted <- get_results(data, countrycode, year_to_predict)[[2]]
# vaccine_estimates <- get_results(data, countrycode, year_to_predict)[[3]]
# mcresult <- get_results(data, countrycode, year_to_predict)[[4]]


# Transform the resulttable output
reformat_datatable <- function(vaccine_estimates){
  
  vaccine_estimates$ProphylacticVaccines<- format(vaccine_estimates$ProphylacticVaccines, big.mark=',', scientific=FALSE) 
  vaccine_estimates$EmergencyVaccines <- format(vaccine_estimates$EmergencyVaccines, big.mark=',', scientific=FALSE) 
  vaccine_estimates$TotalVaccines <- format(vaccine_estimates$TotalVaccines, big.mark=',', scientific=FALSE)
  vaccine_estimates$CI_low <- format(vaccine_estimates$CI_low, big.mark=',', scientific=FALSE)
  vaccine_estimates$CI_high <- format(vaccine_estimates$CI_high, big.mark=',', scientific=FALSE)
  return(vaccine_estimates)
}

### Added by Rob (16/02/22)
reformat_datatable2 <- function(vaccine_estimatesS){
  # if (sum(vaccine_estimates2$`Smallholder population`)<0) {
  #   vaccine_estimates2$`Smallholder population` <- 0
  # }
  vaccine_estimatesS$'Large Ruminants' <- format(vaccine_estimatesS$'Large Ruminants', big.mark=',', scientific=FALSE)
  vaccine_estimatesS$'Small Ruminants' <- format(vaccine_estimatesS$'Small Ruminants', big.mark=',', scientific=FALSE)
  vaccine_estimatesS$'Pigs' <- format(vaccine_estimatesS$'Pigs', big.mark=',', scientific=FALSE)
  # vaccine_estimates$EmergencyVaccines <- format(vaccine_estimates$EmergencyVaccines, big.mark=',', scientific=FALSE) 
  # vaccine_estimates$TotalVaccines <- format(vaccine_estimates$TotalVaccines, big.mark=',', scientific=FALSE)
  # vaccine_estimates$CI_low <- format(vaccine_estimates$CI_low, big.mark=',', scientific=FALSE)
  # vaccine_estimates$CI_high <- format(vaccine_estimates$CI_high, big.mark=',', scientific=FALSE)
  return(vaccine_estimatesS)
}



# Sanity check
# vaccine_estimates <- get_results(data, "GEO", 2025)[[3]]
# vaccine_estimates

#reformat_datatable(vaccine_estimates)
# res <- get_results(data, "TUR", 2022, subnational="Thrace")[[4]]
for (year in c(2021, 2025)){
  res <- get_results(data, "TUR", year, subnational="Anatolia")[[4]]
  tormodel <- tornado(res)
  plot(tormodel, main=year, cex.lab=0.8)
}

# get_results(data, "BHR", 2021)[[3]]
# hist(mcresult$Total_Routine_Vaccines)
# hist(mcresult$Total_Emergency_Vaccines)
# hist(mcresult$Total_vaccines)
#as.integer(tail(vaccine_estimates$PCP, 1))
# reformat_datatable(get_results(data, "ETH", 2022, edited=FALSE)[[3]])


# country_code <- "TUR"
# subnat <- "Anatolia"
# year_to_predict <- 2024
# df1 <- data[data$Country_code == country_code & data$SubnationalRegion == subnat, ]
# df2 <- get_results(data, country_code, year_to_predict, subnational=subnat)[[1]]

## Plots
# g1 <- ggplot(data=df1, aes(x=Year, y=Pop, color=Species, shape=Species), show.legend=T) +
#   geom_point(size=4) +
#   ylab(NULL) +
#   geom_point(data=df2[df2$Year > 2020, ], aes(x=Year, y=Pop_predicted, color=Species, shape=Species),
#              alpha=.4, size=4, show.legend=T) +
#   ylim(0, 50000000) +
#   xlim(2011, 2025) +
#   #theme_pubr() +
#   theme_classic(base_size = 15) +
#   ggtitle("Livestock (Head)") +
#   theme(title = element_text(size=15),
#         axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = .5, face = "plain"),
#         axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),
#         axis.title.x = element_text(size = 18, angle = 0, vjust = .5, face = "plain"),
#         axis.title.y = element_text(size = 18, angle = 90, vjust = .5, face = "plain"),
#         legend.text=element_text(size=10),
#         legend.position="bottom") +
#   scale_fill_brewer(palette = "Pastel2")
#   scale_x_continuous(breaks = seq(from = 2010, to = 2025, by = 5))
# g1
# g2 <- ggplot(data=df1, aes(x=Year, y=TotalOutbreaks, color=TotalOutbreaks), show.legend=T) +
#   geom_point(size=4) +
#   ylab(NULL) +
#   geom_point(data=df2[df2$Year > 2020, ], aes(x=Year, y=TotalON_predicted, color=TotalON_predicted),
#              alpha=.4, size=4, show.legend=T) +
#   ylim(0, 2000) +
#   xlim(2011, 2025) +
#   theme_pubr() +
# #  theme_classic(base_size = 15) +
#   #ggtitle("FMD outbreaks (N)") +
#   theme(title = element_text(size=15),
#         axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = .5, face = "plain"),
#         axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
#         axis.title.x = element_text(size = 18, angle = 0, vjust = .5, face = "plain"),
#         axis.title.y = element_text(size = 18, angle = 90, vjust = .5, face = "plain"),
#         legend.text=element_text(size=10),
#         legend.position="bottom") + 
#   scale_colour_gradient2(
#     low = "black", mid = "grey", high = "red",
#     space = "Lab", na.value = "grey50",
#     guide = "colourbar", aesthetics = "colour"
#   )
#   #scale_x_continuous(breaks = seq(from = 2010, to = 2025, by = 5))
# g2
# 
# library(ggpubr)
# figure <- ggarrange(g1, g2, 
#           labels = c("Livestock (Head)", "FMD outbreaks (N)"),
#           ncol = 2, nrow = 1, align="v")
# figure
# 
# 
# 
# mcresult <- get_results(data, country_code, year_to_predict, subnational=subnat)[[4]]
# hist(mcresult$Total_vaccines, xlab="Total Vaccines (N)")

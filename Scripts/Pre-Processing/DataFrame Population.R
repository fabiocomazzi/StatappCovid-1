setwd("~/GitHub/StatappCovid")
rm(list = ls())
source("Scripts/Utility Functions.R")
library(tidyverse)
library(dplyr)
library(tibble)

###### Read the data (population of comuni from 2011 to 2020) 
file_2012 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2012.csv', header = TRUE)
file_2013 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2013.csv', header = TRUE)
file_2014 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2014.csv', header = TRUE)
file_2015 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2015.csv', header = TRUE)
file_2016 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2016.csv', header = TRUE)
file_2017 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2017.csv', header = TRUE)
file_2018 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2018.csv', header = TRUE)
file_2019 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2019.csv', header = TRUE)
file_2020 = read.csv(file = 'RawData/Population Comuni (2012-2020)/2020.csv', header = TRUE)
#### Remove useless columns 
file_2012 = file_2012[,-c(2,4,5,6,7,9,10,11,12)]
file_2013 = file_2013[,-c(2,4,5,6,7,9,10,11,12)]
file_2014 = file_2014[,-c(2,4,5,6,7,9,10,11,12)]
file_2015 = file_2015[,-c(2,4,5,6,7,9,10,11,12)]
file_2016 = file_2016[,-c(2,4,5,6,7,9,10,11,12)]
file_2017 = file_2017[,-c(2,4,5,6,7,9,10,11,12)]
file_2018 = file_2018[,-c(2,4,5,6,7,8,9,10,12,13,14,15,16,17,18)]
file_2019 = file_2019[,-c(2,4,5,6,7,8,9,10,12,13,14,15,16,17,18)]
file_2020 = file_2020[,-c(2,4,5,6,7,8,9,10,12,13,14,15,16,17,18)]
#### Rename columns
file_2012 = file_2012 %>% rename(Eta = 2,Totale.Maschi12 = Totale.Maschi, Totale.Femmine12 = Totale.Femmine)
file_2013 = file_2013 %>% rename(Eta = 2,Totale.Maschi13 = Totale.Maschi, Totale.Femmine13 = Totale.Femmine)
file_2014 = file_2014 %>% rename(Eta = 2,Totale.Maschi14 = Totale.Maschi, Totale.Femmine14 = Totale.Femmine)
file_2015 = file_2015 %>% rename(Eta = 2,Totale.Maschi15 = Totale.Maschi, Totale.Femmine15 = Totale.Femmine)
file_2016 = file_2016 %>% rename(Eta = 2,Totale.Maschi16 = Totale.Maschi, Totale.Femmine16 = Totale.Femmine)
file_2017 = file_2017 %>% rename(Eta = 2,Totale.Maschi17 = Totale.Maschi, Totale.Femmine17 = Totale.Femmine)
file_2018 = file_2018 %>% rename(Codice.Comune = Codice.comune,Eta = 2,Totale.Maschi18 = Totale.Maschi, Totale.Femmine18 = Totale.Femmine)
file_2019 = file_2019 %>% rename(Codice.Comune = Codice.comune,Eta = 2,Totale.Maschi19 = Totale.Maschi, Totale.Femmine19 = Totale.Femmine)
file_2020 = file_2020 %>% rename(Codice.Comune = Codice.comune,Eta = 2,Totale.Maschi20 = Totale.Maschi, Totale.Femmine20 = Totale.Femmine)
#### Add column of total population 
file_2012 = mutate(file_2012, Totale12 = Totale.Maschi12 + Totale.Femmine12)
file_2013 = mutate(file_2013, Totale13 = Totale.Maschi13 + Totale.Femmine13)
file_2014 = mutate(file_2014, Totale14 = Totale.Maschi14 + Totale.Femmine14)
file_2015 = mutate(file_2015, Totale15 = Totale.Maschi15 + Totale.Femmine15)
file_2016 = mutate(file_2016, Totale16 = Totale.Maschi16 + Totale.Femmine16)
file_2017 = mutate(file_2017, Totale17 = Totale.Maschi17 + Totale.Femmine17)
file_2018 = mutate(file_2018, Totale18 = Totale.Maschi18 + Totale.Femmine18)
file_2019 = mutate(file_2019, Totale19 = Totale.Maschi19 + Totale.Femmine19)
file_2020 = mutate(file_2020, Totale20 = Totale.Maschi20 + Totale.Femmine20)
#### Merge datasets
total_population = merge(file_2012,file_2013)
total_population = merge(total_population,file_2014)
total_population = merge(total_population,file_2015)
total_population = merge(total_population,file_2016)
total_population = merge(total_population,file_2017)
total_population = merge(total_population,file_2018)
total_population = merge(total_population,file_2019)
total_population = merge(total_population,file_2020)
#### Remove row of total population aggregated by age (Eta = 999)
total_population = total_population %>% filter(Eta < 500)
#### Now we group the dataset by the Codice.Comune and age class
total_population = total_population %>% mutate(CL_ETA = age_to_fact(Eta)) %>% dplyr::select(-Eta)
total_population = total_population %>% group_by(Codice.Comune, CL_ETA) %>%
  summarise_at(c("Totale.Femmine12", "Totale.Maschi12", "Totale12","Totale.Femmine13", "Totale.Maschi13",
                 "Totale13","Totale.Femmine14", "Totale.Maschi14", "Totale14","Totale.Femmine15",
                 "Totale.Maschi15", "Totale15","Totale.Femmine16", "Totale.Maschi16", "Totale16",
                 "Totale.Femmine17", "Totale.Maschi17", "Totale17","Totale.Femmine18", "Totale.Maschi18",
                 "Totale18","Totale.Femmine19", "Totale.Maschi19", "Totale19","Totale.Femmine20", "Totale.Maschi20",
                 "Totale20"),sum)
total_population$Codice.Comune<- as.character(total_population$Codice.Comune)
#### Add the population of 2011 (assumed equal to the population of 2012)
dataframe_temp = total_population %>% dplyr::select(Totale.Femmine12:Totale12)
dataframe_temp = dataframe_temp[,2:4]
dataframe_temp = dataframe_temp %>% rename(Totale.Femmine11 = Totale.Femmine12, Totale.Maschi11 = Totale.Maschi12, Totale11 = Totale12)
total_population = add_column(total_population,dataframe_temp,.before = "Totale.Femmine12")
save(total_population, file = Rdata_path("Dataset_Population_Comuni"))

###### Create the dataset for the population of regions and provinces
setwd("~/GitHub/StatappCovid")
rm(list = ls())
source("Scripts/Utility Functions.R")
load("Output/Data/Dataset_Population_Comuni.Rdata")
comune_provincia_regione = read.csv("RawData/Comune - Provincia - Regione.csv")
#### Add the province and the region corresponding to the comune
comune_provincia_regione$COD_COM = as.character(comune_provincia_regione$COD_COM)
total_population = inner_join(total_population, comune_provincia_regione, by = c("Codice.Comune" = "COD_COM"))
total_population = total_population[c(1,33,34,2:32)]

#### Create and save the dataset (provinces)
total_population_province = total_population %>% group_by(PROV,CL_ETA) %>%
  summarise_at(c(paste0("Totale.Maschi",11:20),paste0("Totale.Femmine",11:20),paste0("Totale",11:20)),sum)
save(total_population_province, file = Rdata_path("Dataset_Population_Province"))

#### Create and save the dataset (regions)
total_population_regioni = total_population %>% group_by(REG,CL_ETA) %>%
  summarise_at(c(paste0("Totale.Maschi",11:20),paste0("Totale.Femmine",11:20),paste0("Totale",11:20)),sum)
save(total_population_regioni, file = Rdata_path("Dataset_Population_Regioni"))

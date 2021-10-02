### This script aggregates the orignal dataset at comunal, provincial and regional level, with the prescribed 
### aggregation at age level, and saves the resulting list of dataframes. It also saves the comuni, provinces,
### and region names

setwd("~/GitHub/StatappCovid")
rm(list = ls())
source("Scripts/Utility Functions.R")

######## AVOID RUNNING THIS CODE (Creation of Databases to work on, takes time)

###### Adjust the date format (GE)
modified_istat_dataset = read.csv("RawData/ISTAT - Death Data.csv", header = T)
modified_istat_dataset = subset(modified_istat_dataset, select = -c(M_21,F_21,T_21))
modified_istat_dataset$GE = as.character(modified_istat_dataset$GE)
months = str_sub(modified_istat_dataset$GE,1,-3)
days = str_sub(modified_istat_dataset$GE,-2,-1)
modified_istat_dataset$partial_date_death = as.Date(paste0("2020-",months,"-",days)) #### Adjusted the date format. They are all at 2020 for simplicity, but the actual year is specified in the column names.
modified_istat_dataset$CL_ETA = as.character(modified_istat_dataset$CL_ETA)
modified_istat_dataset$COD_PROVCOM = as.character(modified_istat_dataset$COD_PROVCOM)
modified_istat_dataset$T_20 = as.numeric(modified_istat_dataset$T_20)
modified_istat_dataset$F_20 = as.numeric(modified_istat_dataset$F_20)
modified_istat_dataset$M_20 = as.numeric(modified_istat_dataset$M_20)
modified_istat_dataset = modified_istat_dataset %>% replace(is.na(.),0)
modified_istat_dataset = modified_istat_dataset %>% dplyr::select(-GE)
save(modified_istat_dataset, file = Rdata_path("Modified_Istat_Dataset"))


###### Aggregation by age class at comuni, province and region levels
firstvec = c(0,11,15) # THESE VECTORS CONTROL THE AGGREGATION BY AGE (See notation in the file "ISTAT - Death Data Description.pdf") 
lastvec = c(10,14,21) # (We aggregates class from 0 to 10, from 11 to 14 and from 15 to 21)


#### Dataset aggregated by comuni - age class
load("Output/Data/Modified_Istat_Dataset.Rdata")
keys_comuni = modified_istat_dataset %>% group_by(COD_PROVCOM,CL_ETA) %>% group_keys()
data_comuni = modified_istat_dataset %>% group_by(COD_PROVCOM,CL_ETA)
rm(modified_istat_dataset)
gc(verbose = TRUE)
## Due to RAM limit we process data roughly one milion at the time being carefull not to split comunes
aggregated_comuni = aggregate_classes(data_comuni[1:1057311,],keyname = "COD_PROVCOM",firstvec,lastvec)
gc(verbose = TRUE)
aggregated_comuni_temp = aggregate_classes(data_comuni[1057312:2111718,],keyname = "COD_PROVCOM",firstvec,lastvec)
aggregated_comuni = rbind(aggregated_comuni, aggregated_comuni_temp)
rm(aggregated_comuni_temp)
aggregated_comuni_temp = aggregate_classes(data_comuni[2111719:3058315,],keyname = "COD_PROVCOM",firstvec,lastvec)
aggregated_comuni = rbind(aggregated_comuni, aggregated_comuni_temp)
rm(aggregated_comuni_temp)
aggregated_comuni_temp = aggregate_classes(data_comuni[3058316:3863107,],keyname = "COD_PROVCOM",firstvec,lastvec)
aggregated_comuni = rbind(aggregated_comuni, aggregated_comuni_temp)
rm(aggregated_comuni_temp)
gc()
codes_comuni = unique(as.character(keys_comuni$COD_PROVCOM))
save(list = c("aggregated_comuni","codes_comuni"), file = Rdata_path("Dataset_Aggregated_Comuni"))
rm(aggregated_comuni)
rm(data_comuni)
rm(keys_comuni)


#### Dataset aggregated by region - age class
load("Output/Data/Modified_Istat_Dataset.Rdata")
keys_regioni = modified_istat_dataset %>% group_by(NOME_REGIONE,CL_ETA) %>% group_keys()
data_regioni = modified_istat_dataset %>% group_by(NOME_REGIONE,CL_ETA,partial_date_death) %>%
  summarise_at(c(paste0("M_",c(11:20)),paste0("F_",c(11:20)),paste0("T_",c(11:20))), sum)
aggregated_regioni = aggregate_classes(data_regioni,keyname = "NOME_REGIONE",firstvec,lastvec)
names_regioni = unique(as.character(keys_regioni$NOME_REGIONE))
save(list = c("aggregated_regioni", "names_regioni"), file = Rdata_path("Dataset_Aggregated_Regioni"))
rm(aggregated_regioni)
rm(data_regioni)
rm(keys_regioni)
rm(modified_istat_dataset)


#### Dataset aggregated by province - age class
load("Output/Data/Modified_Istat_Dataset.Rdata")
keys_province = modified_istat_dataset %>% group_by(NOME_PROVINCIA,CL_ETA) %>% group_keys()
data_province = modified_istat_dataset %>% group_by(NOME_PROVINCIA,CL_ETA,partial_date_death) %>%
  summarise_at(c(paste0("M_",c(11:20)),paste0("F_",c(11:20)),paste0("T_",c(11:20))), sum) 
aggregated_province = aggregate_classes(data_province,keyname = "NOME_PROVINCIA",firstvec,lastvec)
names_province = unique(as.character(keys_province$NOME_PROVINCIA))
## Change names to avoid error for accents
aggregated_province$NOME_PROVINCIA[36708:37792] = "Forli-Cesena"
aggregated_province$NOME_PROVINCIA[105445:106433] = "Valle d'Aosta"
names_province[35] = "Forli-Cesena"
names_province[99] = "Valle d'Aosta"
save(list = c("aggregated_province","names_province"), file = Rdata_path("Dataset_Aggregated_Province"))
rm(aggregated_province)
rm(data_province)
rm(keys_province)
rm(modified_istat_dataset)

######## RUNNARE SOLO DA QUA IN POI
###### Here we enrich our data with additional information on the population. We have population information
###### by age, for each comune and provincia, in 2020.
#### Let us start with comuni:
rm(list = ls())
load("Output/Data/Dataset_Aggregated_Comuni.Rdata")
source("Scripts/Utility Functions.R")
data_comuni = read.csv("RawData/Population Comuni (2020).csv")
data_comuni = data_comuni %>% dplyr::select(Codice.comune, Eta, Totale.Maschi, Totale.Femmine) %>% 
  filter(Eta != 999) %>% mutate(istat = Codice.comune, num_residenti = Totale.Maschi + Totale.Femmine) %>% 
  dplyr::select(istat,num_residenti, Eta,Totale.Maschi, Totale.Femmine)
data_comuni = data_comuni %>% mutate(CL_ETA = age_to_fact(Eta)) %>% dplyr::select(-Eta) #### Now we perform aggregation (see factorization function in "Utility Functions.R")
data_comuni = data_comuni %>% group_by(istat, CL_ETA) %>% summarise_at(c("Totale.Femmine", "Totale.Maschi", "num_residenti"),sum)
#### Align codes (some comuni are missing)
data_comuni$istat = as.character(data_comuni$istat)
diff1 = setdiff(data_comuni$istat,codes_comuni)
diff2 = setdiff(codes_comuni,data_comuni$istat)
data_comuni = data_comuni %>% filter(!(istat %in% diff1))
aggregated_comuni = aggregated_comuni %>% filter(!(COD_PROVCOM %in% diff2))
codes_comuni = unique(aggregated_comuni$COD_PROVCOM)
save(list = c("aggregated_comuni","codes_comuni", "data_comuni"), file = Rdata_path("Dataset_Aggregated_Comuni"))


#### Now we repeat with province:
rm(list = ls())
load("Output/Data/Dataset_Aggregated_Province.Rdata")
source("Scripts/Utility Functions.R")
data_province = read.csv("RawData/Population Province (2020).csv")
data_province = data_province %>% dplyr::select(Provincia, Eta, Totale.Maschi, Totale.Femmine) %>% 
  filter(Eta != "Totale") %>% mutate(provincia = Provincia, num_residenti = Totale.Maschi + Totale.Femmine) %>% 
  dplyr::select(provincia,num_residenti, Eta,Totale.Maschi, Totale.Femmine)
data_province$Eta = as.numeric(data_province$Eta)
data_province = data_province %>% mutate(CL_ETA = age_to_fact(Eta)) %>% dplyr::select(-Eta)
data_province = data_province %>% group_by(provincia, CL_ETA) %>% summarise_at(c("Totale.Femmine", "Totale.Maschi", "num_residenti"),sum)
#### Align names (some names are different due to accent)
diff1 = setdiff(data_province$provincia,aggregated_province$NOME_PROVINCIA)
diff2 = setdiff(aggregated_province$NOME_PROVINCIA,data_province$provincia)
data_province$provincia[295:297] = "Valle d'Aosta"
data_province$provincia[229:231] = "Reggio Calabria"
data_province$provincia[103:105] = "Forli-Cesena"
save(list = c("aggregated_province","names_province", "data_province"), file = Rdata_path("Dataset_Aggregated_Province"))


###### We now focus on the shapefiles for comuni, regioni and province.
rm (list = ls())
source("Scripts/Utility Functions.R")

#### Regioni
reg_poly = readOGR("RawData/Shapefile Regioni/Reg01012021_g_WGS84.shp")
reg_poly$DEN_REG[reg_poly$DEN_REG == "Friuli Venezia Giulia"] = "Friuli-Venezia-Giulia"
save(reg_poly, file = Rdata_path("Regioni_Geometry_Polygons"))

#### Province
prov_poly = readOGR("RawData/Shapefile Province/ProvCM01012018_g_WGS84.shp")
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Aosta"] = "Valle d'Aosta"
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Bolzano"] = "Bolzano/Bozen"
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Massa Carrara"] = "Massa-Carrara"
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Forli'-Cesena"] = "Forli-Cesena" 
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Reggio di Calabria"] = "Reggio Calabria"
save(prov_poly, file = Rdata_path("Province_Geometry_Polygons"))

#### Comuni
com_poly = readOGR("RawData/Shapefile Comuni/Com01012020_WGS84.shp")
load("Output/Data/Dataset_Aggregated_Comuni.Rdata")
ind_to_drop = which(com_poly$PRO_COM %in% setdiff(com_poly$PRO_COM,unique(aggregated_comuni$COD_PROVCOM)))
com_poly = com_poly[-ind_to_drop,]
save(com_poly, file = Rdata_path("Comuni_Geometry_Polygons"))

###### Lastly, we create dataset aggregated by year, to perform the scalar analysis
rm(list = ls())
source("Scripts/Utility Functions.R")
load("Output/Data/Dataset_Aggregated_Comuni.Rdata")
load("Output/Data/Dataset_Aggregated_Province.Rdata")
aggregated_comuni_lisa = aggregated_comuni %>% group_by(COD_PROVCOM,CL_ETA) %>% summarise_at(c(paste0("M_",11:20),paste0("F_",11:20),paste0("T_",11:20)),sum)
aggregated_comuni_lisa = aggregated_comuni_lisa %>% left_join(data_comuni, by = c("COD_PROVCOM" = "istat","CL_ETA" = "CL_ETA"))
aggregated_province_lisa = aggregated_province %>% group_by(NOME_PROVINCIA,CL_ETA) %>% summarise_at(c(paste0("M_",11:20),paste0("F_",11:20),paste0("T_",11:20)),sum)
aggregated_province_lisa = aggregated_province_lisa %>% left_join(data_province, by = c("NOME_PROVINCIA" = "provincia","CL_ETA" = "CL_ETA"))
#### We want to divide deaths by residents
aggregated_province_lisa[3:32] = aggregated_province_lisa[3:32] / aggregated_province_lisa$num_residenti
aggregated_comuni_lisa[,3:32] = aggregated_comuni_lisa[,3:32] / aggregated_comuni_lisa$num_residenti
save(aggregated_comuni_lisa, file = Rdata_path("Dataset_Aggregated_Comuni_LISA"))
save(aggregated_province_lisa, file = Rdata_path("Dataset_Aggregated_Province_LISA"))

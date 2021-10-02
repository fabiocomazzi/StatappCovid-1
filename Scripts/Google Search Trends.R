setwd("~/GitHub/StatappCovid/Scripts")
rm(list = ls())
source("~/GitHub/StatappCovid/Scripts/Utility Functions.R")
library(fda)
library(fields)
library(fdakma)
library(fdapace)
library(dynCorr)
library(data.table)
library(gtrendsR)
library(lubridate)
library(rgeos)
library(ggplot2)
library(tidyverse)
load("~/GitHub/StatappCovid/Output/Data/Dataset_Aggregated_Regioni.Rdata")
load("~/GitHub/StatappCovid/Output/Data/Dataset_Population_Regioni.Rdata")
load("~/GitHub/StatappCovid/Output/Data/Regioni_Geometry_Polygons.Rdata")

# Helper function to retrieve trend search data for a given keyword
retrieveTrendSearch = function(key){
  regions = data.frame(Code = c("IT-65","IT-77","IT-78","IT-72","IT-45",
                                "IT-36","IT-62","IT-42","IT-25","IT-57",
                                "IT-67","IT-21","IT-75","IT-88","IT-82",
                                "IT-52","IT-32","IT-55","IT-23","IT-34"),
                       Region = c("Abruzzo","Basilicata","Calabria",
                                  "Campania","Emilia-Romagna",
                                  "Friuli-Venezia-Giulia","Lazio",
                                  "Liguria","Lombardia","Marche",
                                  "Molise","Piemonte","Puglia","Sardegna",
                                  "Sicilia","Toscana","Trentino-Alto Adige",
                                  "Umbria","Valle d'Aosta","Veneto"))
  trends =  data.frame(matrix(ncol = 3, nrow = 0))
  columns = c("date", "interest","geo")
  colnames(trends) = columns
  for(region in regions$Code){
    search = gtrends(
      keyword = key,
      geo = region,
      time = "all",
      category = 0,
      hl = "en-US",
      low_search_volume = FALSE,
      onlyInterest = TRUE
    )
    if(is.null(search$interest_over_time)){
      next
    }
    monthly_trend = search$interest_over_time %>%
      dplyr::select(date,hits,geo) %>%
      group_by(date) %>%
      dplyr::filter(str_split(date,"-")[[1]][1] == "2020") %>%
      mutate(Month = str_split(date,"-")[[1]][2]) %>%
      mutate(hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
      ungroup() %>%
      dplyr::select(-date)
    
    t = c()
    for(m in 1:12){
      if(m %in% c(4,6,9,11)){
        toAdd1 = paste("2020",as.character(m),"01",sep="-")
        toAdd2 = paste("2020",as.character(m),"30",sep="-")
        toAdd = paste(toAdd1,toAdd2,sep=" ")
        t = c(t,toAdd)
      } else if (m == 2){
        toAdd1 = paste("2020",as.character(m),"01",sep="-")
        toAdd2 = paste("2020",as.character(m),"29",sep="-")
        toAdd = paste(toAdd1,toAdd2,sep=" ")
        t = c(t,toAdd)
      } else{
        toAdd1 = paste("2020",as.character(m),"01",sep="-")
        toAdd2 = paste("2020",as.character(m),"31",sep="-")
        toAdd = paste(toAdd1,toAdd2,sep=" ")
        t = c(t,toAdd)
      }
    }
    
    daily_trend =  data.frame(matrix(ncol = 3, nrow = 0))
    columns = c("date", "hits","geo")
    colnames(daily_trend) = columns
    for(time in t){
      search = gtrends(
        keyword = key,
        geo = region,
        time = time,
        category = 0,
        hl = "en-US",
        low_search_volume = FALSE,
        onlyInterest = TRUE
      )
      if(!is.null(search$interest_over_time)){
        daily_trend = rbind(daily_trend,search$interest_over_time %>% dplyr::select(date,hits,geo))
      }
    }
    if(dim(daily_trend)[1] == 0){
      next
    }
    daily_trend = daily_trend %>% group_by(date) %>%
      mutate(Month = str_split(date,"-")[[1]][2]) %>%
      mutate(hits = as.integer(ifelse(hits == '<1', '0', hits)))
    region_trend = merge(monthly_trend,daily_trend,by = c("Month","geo")) %>%
      ungroup() %>% group_by(date) %>%
      mutate(interest = hits.x * hits.y / 100) %>%
      dplyr::select(date,interest,geo)
    trends = rbind(trends,region_trend)
    Sys.sleep(5)
  }
  trends = trends %>% pivot_wider(names_from = "geo", values_from = "interest")
  trends[is.na(trends)] = 0
  return(trends)
}

# Compute the mortality rates by region in 2020
deaths = aggregated_regioni %>%
  dplyr::select(NOME_REGIONE,partial_date_death,T_20) %>%
  group_by(NOME_REGIONE,partial_date_death) %>%
  summarize(deaths = sum(T_20)) %>%
  ungroup()
deaths[which(deaths$NOME_REGIONE == "Trentino-Alto Adige/Südtirol"),]$NOME_REGIONE = "Trentino-Alto Adige"
deaths[which(deaths$NOME_REGIONE == "Valle d'Aosta/Vallée d'Aoste"),]$NOME_REGIONE = "Valle d'Aosta"
deaths[which(deaths$NOME_REGIONE == "Friuli-Venezia Giulia"),]$NOME_REGIONE = "Friuli-Venezia-Giulia"
population = total_population_regioni %>%
  dplyr::select(REG,Totale20) %>%
  group_by(REG) %>%
  summarize(population = sum(Totale20)) %>%
  ungroup()
population[which(population$REG == "Emilia Romagna"),]$REG = "Emilia-Romagna"
population[which(population$REG == "Valle D'Aosta"),]$REG = "Valle d'Aosta"
population[which(population$REG == "Friuli-Venezia Giulia"),]$REG = "Friuli-Venezia-Giulia"
mortality_rates = merge(deaths,population,by.x="NOME_REGIONE",by.y="REG") %>%
  ungroup %>% group_by(NOME_REGIONE,partial_date_death) %>%
  mutate(mortality = deaths / population) %>%
  dplyr::select(NOME_REGIONE,partial_date_death,mortality) %>%
  ungroup() %>% group_by(NOME_REGIONE) %>%
  arrange(partial_date_death,.by_group = TRUE)
colnames(mortality_rates) = c("name","date","mortality")
mortality_rates = mortality_rates %>%
  pivot_wider(names_from = name,values_from = mortality)
rm(list = c("deaths","total_population_regioni","population","aggregated_regioni","names_regioni"))

# Smooth the data
rangeval = c(1,366)
norder = 4
nbasis = 4:50
argvals = 1:366
gcv = numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis = create.bspline.basis(rangeval, nbasis[i], norder)
  for (j in 1:20){
    y = as.matrix(mortality_rates[,j + 1])
    gcv[i] = gcv[i] + smooth.basis(argvals, y, fdParobj=basis)$gcv
  }
}
x11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
nbasis = 27 # Optimal basis cardinality
basis = create.bspline.basis(rangeval, nbasis, norder)
mortality_rates_functional = Data2fd(argvals,as.matrix(mortality_rates[,-1]),basisobj = basis)
mortality_rates_matrix = eval.fd(argvals, mortality_rates_functional)
mortality_rates[,2:21] = mortality_rates_matrix
mortality_rates = mortality_rates %>% gather(key = "variable", value = "value", -date)

pdf("~/GitHub/StatappCovid/Output/Plot/Regional Rates - Smoothed.pdf",width = 20,height = 14)
g = ggplot(mortality_rates,aes(x=date,y=value)) + geom_line(aes(colour = variable, linetype = "solid")) +
  scale_color_manual(values = rep("#000000",20)) + 
  labs(title = "Daily Mortality Rates (Regional)", x = "Date", y = "Mortality", size = 1) +
  theme(axis.title.x=element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size=20), title = element_text(size=20))
plot(g)
dev.off()

#### Polmonite
## Correlation per region
load("~/GitHub/StatappCovid/Output/Data/Polmonite.Rdata")
date = trends$date
trends = trends %>% ungroup() %>% dplyr::select(-date)
trends = as.data.frame(t(t(trends) / sapply(trends,max)))
rangeval = c(1,366)
norder = 4
nbasis = 4:50
argvals = 1:366
gcv = numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis = create.bspline.basis(rangeval, nbasis[i], norder)
  for (j in 1:20){
    y = as.matrix(trends[,j])
    gcv[i] = gcv[i] + smooth.basis(argvals, y, fdParobj=basis)$gcv
  }
}
x11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
nbasis = 24 # Optimal basis cardinality
basis = create.bspline.basis(rangeval, nbasis, norder)
trends_functional = Data2fd(argvals,as.matrix(trends),basisobj = basis)
trends_matrix = eval.fd(argvals, trends_functional)
trends = as.data.frame(trends_matrix)
trends$date = date
trends = trends %>% gather(key = "variable", value = "value", -date)

pdf("~/GitHub/StatappCovid/Output/Plot/Polmonite - Smoothed.pdf",width = 20,height = 14)
g = ggplot(trends,aes(x=date,y=value)) + geom_line(aes(colour = variable, linetype = "solid")) +
  scale_color_manual(values = rep("#000000",20)) + 
  labs(title = "Google Search Interest (Polmonite)", x = "Date", y = "Interest") +
  theme(axis.title.x=element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "none",axis.title = element_text(size=20), title = element_text(size=20))
plot(g)
dev.off()

corr = DynCorr(t(mortality_rates_matrix),t(trends_matrix),1:366) # Correlation per region
mean(corr) # Correlation overall

## Plot
correlation_by_region = data.frame(Region = c("Abruzzo","Basilicata","Calabria",
                                              "Campania","Emilia-Romagna",
                                              "Friuli-Venezia-Giulia","Lazio",
                                              "Liguria","Lombardia","Marche",
                                              "Molise","Piemonte","Puglia","Sardegna",
                                              "Sicilia","Toscana","Trentino-Alto-Adige",
                                              "Umbria","Valle-Aosta","Veneto"),
                                   Correlation = abs(corr))
reg_poly$DEN_REG[which(reg_poly$DEN_REG == "Trentino-Alto Adige")] = "Trentino-Alto-Adige"
reg_poly$DEN_REG[which(reg_poly$DEN_REG == "Valle d'Aosta")] = "Valle-Aosta"
colorlim = c(0,1)
reg_poly_t = reg_poly
reg_poly_t@data = reg_poly@data %>% left_join(correlation_by_region, by = c("DEN_REG" = "Region")) 
reg_poly_t = st_as_sf(reg_poly_t)

pdf("~/GitHub/StatappCovid/Output/Plot/Correlation - Polmonite.pdf",width = 20,height = 14)
g = ggplot(reg_poly_t) + geom_sf(aes_string(fill = "Correlation")) + 
  ggtitle("Correlation - Polmonite") +
  scale_fill_viridis(option = "inferno",limits = colorlim) + labs(fill = "Dynamic Correlation")
plot(g)
dev.off()

#### Febbre
## Correlation per region
load("~/GitHub/StatappCovid/Output/Data/Febbre.Rdata")
date = trends$date
trends = trends %>% ungroup() %>% dplyr::select(-date)
trends = as.data.frame(t(t(trends) / sapply(trends,max)))
rangeval = c(1,366)
norder = 4
nbasis = 4:50
argvals = 1:366
gcv = numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis = create.bspline.basis(rangeval, nbasis[i], norder)
  for (j in 1:20){
    y = as.matrix(trends[,j])
    gcv[i] = gcv[i] + smooth.basis(argvals, y, fdParobj=basis)$gcv
  }
}
x11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
nbasis = 18 # Optimal basis cardinality
basis = create.bspline.basis(rangeval, nbasis, norder)
trends_functional = Data2fd(argvals,as.matrix(trends),basisobj = basis)
trends_matrix = eval.fd(argvals, trends_functional)
trends = as.data.frame(trends_matrix)
trends$date = date
trends = trends %>% gather(key = "variable", value = "value", -date)

pdf("~/GitHub/StatappCovid/Output/Plot/Febbre - Smoothed.pdf",width = 20,height = 14)
g = ggplot(trends,aes(x=date,y=value)) + geom_line(aes(colour = variable, linetype = "solid")) +
  scale_color_manual(values = rep("#000000",20)) + 
  labs(title = "Google Search Interest (Polmonite)", x = "Date", y = "Interest") +
  theme(axis.title.x=element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "none")
plot(g)
dev.off()

corr = DynCorr(t(mortality_rates_matrix),t(trends_matrix),1:366)
mean(corr)

## Plot
correlation_by_region = data.frame(Region = c("Abruzzo","Basilicata","Calabria",
                                              "Campania","Emilia-Romagna",
                                              "Friuli-Venezia-Giulia","Lazio",
                                              "Liguria","Lombardia","Marche",
                                              "Molise","Piemonte","Puglia","Sardegna",
                                              "Sicilia","Toscana","Trentino-Alto-Adige",
                                              "Umbria","Valle-Aosta","Veneto"),
                                   Correlation = abs(corr))
reg_poly$DEN_REG[which(reg_poly$DEN_REG == "Trentino-Alto Adige")] = "Trentino-Alto-Adige"
reg_poly$DEN_REG[which(reg_poly$DEN_REG == "Valle d'Aosta")] = "Valle-Aosta"
colorlim = c(0,1)
reg_poly_t = reg_poly
reg_poly_t@data = reg_poly@data %>% left_join(correlation_by_region, by = c("DEN_REG" = "Region")) 
reg_poly_t = st_as_sf(reg_poly_t)
pdf("~/GitHub/StatappCovid/Output/Plot/Correlation - Febbre.pdf",width = 20,height = 14)
g = ggplot(reg_poly_t) + geom_sf(aes_string(fill = "Correlation")) + 
  ggtitle("Correlation - Febbre") +
  scale_fill_viridis(option = "inferno",limits = colorlim) + labs(fill = "Dynamic Correlation")
plot(g)
dev.off()

###### Google Trend Searchs: create the dataframes
key = "polmonite"
trends = retrieveTrendSearch(key)
save(trends, file = Rdata_path("Polmonite"))

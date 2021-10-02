library(tidyverse)
library(stringr)
library(scales)
library(ggalt)
library(ggthemes)
library(tibble)
library(viridis)
library(readxl)
library(geoR)
library(sf)
library(rgdal)
library(ggpubr)
library(gridExtra)
library(lattice)
library(ggrepel)
library(MASS)
library(car)
library(GGally)


Rdata_path= function(name_str) 
{
  return(paste0("Output/Data/",name_str,".Rdata"))
}

Plot_path= function(name_str) 
{
  return(paste0("Output/Plot/",name_str,".Rdata"))
}



Make_covid_plot = function (data,firstkeys,firstkeyvarname,years,classes, gender = "T", plotfunction = geom_smooth, args = NULL, point = T)
{
  temp_data = data %>% filter(get(firstkeyvarname) %in% firstkeys, CL_ETA %in% classes) %>% dplyr::select(any_of(c("partial_date_death","CL_ETA",firstkeyvarname,paste0(gender,"_",years))))%>%
    pivot_longer(.,cols = paste0(gender,"_",years), names_to = "Year", values_to = "Deaths")
  if (is.null(args))
    plot = ggplot(temp_data,mapping = aes(x = partial_date_death, y = Deaths, color = Year)) + plotfunction() +facet_grid(CL_ETA~get(firstkeyvarname), scale = "free_y") 
  else
    plot = ggplot(temp_data,mapping = aes(x = partial_date_death, y = Deaths, color = Year)) + plotfunction(span = args) +facet_grid(CL_ETA~get(firstkeyvarname), scale = "free_y") 
  if (point)
    plot = plot + geom_point()
  plot = plot + scale_x_date(date_labels = "%b")
  return(plot)
}


aggregate_classes = function(data,keyname,firstvec,lastvec) #### BE VERY CAREFUL THAT THE VECTORS ARE COHERENT!
{
  n = length(firstvec)
  newdata = data
  for (i in 1:n){
    first = firstvec[i]
    last = lastvec[i]
    classes = as.character(first:last)
    newdata$CL_ETA[newdata$CL_ETA %in% classes] = paste0(first,"_",last)
    newdata = newdata %>% group_by(get(keyname), partial_date_death, CL_ETA)%>%
      summarise_at(c(paste0("M_",c(11:20)),paste0("F_",c(11:20)),paste0("T_",c(11:20))), sum) %>% ungroup()
    names(newdata)[1] = keyname
  }
  return(newdata)
}

# factorization = function (age)
# {
#   if(age < 10)
#     return("0_2")
#   if(age < 30)
#     return("3_6")
#   if(age < 50)
#     return("7_10")
#   if(age < 70)
#     return("11_14")
#   return("15_21")
# }

factorization = function (age)
{
  if(age < 50)
    return("0_10")
  if(age < 70)
    return("11_14")
  return("15_21")
}

age_to_fact = Vectorize(factorization)

istat_age_mapping = function(age)
{
  if(age == 0)
    return(0)
  else
    return(age%/%5 + 1)
}
age_to_istat_cl = Vectorize(istat_age_mapping)

f_to_age = function(class){
  if(class == "0_2")
    return ("0_9")
  if(class == "3_6")
    return ("10_29")
  if(class=="7_10")
    return ("30_49")
  if(class=="11_14")
    return ("50_69")
  if(class == "15_21")
    return ("70+")
}
fact_to_age = Vectorize(f_to_age)
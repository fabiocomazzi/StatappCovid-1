setwd("~/GitHub/StatappCovid")
rm(list = ls())
library(tseries)
library(ggfortify)
library(forecast)
source("Scripts/Utility Functions.R")
load("Output/Data/Dataset_Aggregated_Comuni.Rdata")

###### Analysis for the city of Milan
dataframe_milano = aggregated_comuni %>% filter(COD_PROVCOM == 15146)
#### Convert data in a time series table
time_series = dataframe_milano %>% filter(partial_date_death != "2020-02-29") %>%
  dplyr::select(c(partial_date_death, CL_ETA, T_11))
colnames(time_series)[3] = "Morti"
time_series$partial_date_death = gsub("2020","2011",time_series$partial_date_death)
for(year in 12:20){
  if((year %% 4)==0){
    X = dataframe_milano %>% dplyr::select(c(partial_date_death, CL_ETA, paste0("T_",year)))
    colnames(X)[3] = "Morti"
    }
  else{
    X = dataframe_milano%>%filter(partial_date_death != "2020-02-29" )%>%
      dplyr::select(c(partial_date_death, CL_ETA, paste0("T_",year)))
    colnames(X)[3] = "Morti"
  }
  X$partial_date_death = gsub("2020",paste0("20",year),X$partial_date_death)
  time_series = rbind(time_series, X)
}
rm(X)
## Look at the death trend for the elders
inds = seq(as.Date("2011-01-01"), as.Date("2020-12-31"), by = "day")
time_series_elders = time_series %>% filter(CL_ETA == '15_21')
deaths = ts(time_series_elders %>% dplyr::select(Morti), start = c(2011, 1), frequency = 365.25)
deaths_11_19 = ts(time_series_elders$Morti[1:3288], start = c(2011,1), frequency = 365)                                                               
x11()
par(mar=c(1,1,1,1))
ggplot(time_series_elders[1:(365+366),], aes(x=as.Date(partial_date_death), y=Morti, group = 1)) +
  geom_line() + labs(x = "Day", y = "Deaths") +
  scale_x_date(date_labels = "%Y %b %d")
## STL decompositon
decomposed_deaths = decompose(deaths_11_19, type = "additive")
x11()
autoplot(decomposed_deaths)
adf.test(decomposed_deaths$random[-c(which(is.na(decomposed_deaths$random)))])
x11()
ggtsdisplay(decomposed_deaths$random[-c(which(is.na(decomposed_deaths$random)))])

#### Better make the data weekly
p = 0.8
weekly_deaths_11_19 = ts(deaths_11_19, frequency=7)  # make data weekly
weekly_deaths_11_19 = ts(aggregate(weekly_deaths_11_19, FUN=sum), start = c(2011,1), frequency = 52)
x11()
autoplot(weekly_deaths_11_19)
n = length(weekly_deaths_11_19)
n_train = round(p * length(weekly_deaths_11_19))
data_train = ts(weekly_deaths_11_19[1:n_train], start = c(2011,1), frequency = 52)
data_test = ts(weekly_deaths_11_19[-c(1:n_train)], start = end(data_train), frequency = 52)
x11()
ggtsdisplay(data_train, main = "Weekly Deaths Series")
## Fit ARIMA model
fit = Arima(data_train, order = c(1,0,0), seasonal = list(order = c(0,1,1), period = 52),  method="CSS")
x11()
checkresiduals(fit , main = "ARIMA Diagnostics")
adf.test(fit$residuals) ## Residuals are stationary!
x11()
plot(as.numeric(fit$fitted), as.numeric(fit$residuals), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals of the Model (Training Set)")
abline(h=0, col ="red")
x11()
layout(rbind(1,2))
qqnorm(fit$residuals)
qqline(fit$residuals)
plot(fit$fitted, fit$residuals)
x11()
autoplot(data_train) + autolayer(fit$fitted, series = "Arima(1,0,5)(1,0,1)52") ## Plot fitted values
## Prediction on test set
y_test = forecast(fit, h = 94, level = 95)
x11()
plot(y_test, include = 200, ylab = "Weekly Deaths")
lines(data_test, col  ="red")
legend("topright", legend = c("Forecast", "Actual", "95% Forecast CI"), fill = c("blue", "red", "gray80"))
error = as.numeric(y_test$mean) - as.numeric(data_test)
MSE_test = mean((error)^2)
sqrt(MSE_test)
x11()
plot(as.numeric(y_test$mean), error, type = "p", pch = 16, main = "Residuals vs Fitted Values on Test Set", xlab = "Fitted Values")
abline(h=0, col = "red")
shapiro.test(error)

#### We now fit the model on the data from 2011 to 2019 and use it to predict 2020
X_covid = ts(deaths, frequency=7)
X_covid = ts(aggregate(X_covid, FUN=sum), start = c(2011,1), frequency = 52)
X_covid = X_covid[(length(X_covid)-52+1) : length(X_covid)]
X_covid = ts(X_covid, start = c(2020,1), frequency = 52)
fit = Arima(weekly_deaths_11_19, order = c(1,0,0), seasonal = list(order = c(0,1,1), period = 52),  method="CSS")
y_covid = forecast(fit, h = 52, level = 95)
x11()
plot(y_covid, include = 100, main = "Forecasts vs Actual", xlab = "Date", ylab = "Weekly Deaths", ylim = c(min(as.numeric(y_covid$lower)), max(X_covid)), xaxt='n')
lines(X_covid, col = "red")
legend("topleft", legend = c("Forecast", "Actual", "95% Forecast CI"), fill = c("blue", "red", "gray80"))
axis(side = 1, at=c(2018,2019,2020,2021),labels=c("2018","2019","2020","2021"))
i = seq(as.Date("2020-01-01"), by = 7, length = 52)
x11()
plot(i, as.numeric(y_covid$mean), type = "l", col = "blue", ylim = c(min(as.numeric(y_covid$lower)), max(X_covid)), main = "Actual Mortality vs 95% Forecast CI (2020)", xlab = "Date", ylab = "Weekly Deaths")
polygon(c(rev(i), i),c(rev(as.numeric(y_covid$lower)),as.numeric(y_covid$upper)), col = "gray80" )
lines(i, X_covid, col = "red")
legend("topright", legend = c("Actual", "95% Forecast CI"), fill = c("red", "gray80"))
diff = X_covid - as.numeric(y_covid$upper[,1])
diff = ifelse(diff>0, diff, 0)
sum(diff)
sum(diff[1:26])
sum(diff[26:52])

###### Repeat the process with every province
load("Output/Data/Dataset_Aggregated_Province.Rdata")
i = seq(as.Date("2020-01-01"), by = 7, length = 52)
data = data.frame(prov = names_province, ondata1 = rep(0,107), ondata2 = rep(0,107), tot = rep(0,107), RMSE = rep(0,107), MAPE = rep(0,107), mean = rep(0,107))
for(provincia in names_province){
  dataframe_provincia = aggregated_province %>% filter(NOME_PROVINCIA == provincia)
  time_series = dataframe_provincia %>% filter(partial_date_death != "2020-02-29") %>% dplyr::select(c(partial_date_death, CL_ETA, T_11))
  colnames(time_series)[3] = "Morti"
  time_series$partial_date_death = gsub("2020","2011",time_series$partial_date_death)
  for(year in 12:20){
    if((year %% 4) == 0){
      dataframe_temp = dataframe_provincia %>% dplyr::select(c(partial_date_death, CL_ETA, paste0("T_",year)))
      colnames(dataframe_temp)[3] = "Morti"
    }
    else{
      dataframe_temp = dataframe_provincia %>% filter(partial_date_death != "2020-02-29")%>%dplyr::select(c(partial_date_death, CL_ETA, paste0("T_",year)))
      colnames(dataframe_temp)[3] = "Morti"
    }
    dataframe_temp$partial_date_death = gsub("2020",paste0("20",year),dataframe_temp$partial_date_death)
    time_series = rbind(time_series, dataframe_temp)
  }
  inds = seq(as.Date("2011-01-01"), as.Date("2020-12-31"), by = "day")
  time_series_elders = time_series %>% filter(CL_ETA == '15_21')
  deaths = ts(time_series_elders%>%dplyr::select(Morti), start = c(2011, 1), frequency = 365.25)
  deaths_11_19 =ts(time_series_elders$Morti[1:3287], start = c(2011,1), frequency = 365.25)
  weekly_deaths_11_19 = ts(deaths_11_19, frequency=7)  ## Make data weekly
  weekly_deaths_11_19 = ts(aggregate(weekly_deaths_11_19, FUN=sum), start = c(2011,1), frequency = 52)
  X_covid = ts(deaths, frequency=7)  ## Make data weekly
  X_covid = ts(aggregate(X_covid, FUN=sum), start = c(2011,1), frequency = 52)
  X_covid = X_covid[(length(X_covid)-52+1) : length(X_covid)]
  X_covid = ts(X_covid, start = c(2020,1), frequency = 52)
  fit = Arima(weekly_deaths_11_19, order = c(1,0,0), seasonal = list(order = c(0,1,1), period = 52),  method="CSS")
  y_covid = forecast(fit, h = 52)
  diff = X_covid - as.numeric(y_covid$upper[,2])
  diff = ifelse(diff>0, diff, 0)
  data$tot[data$prov == provincia] = sum(diff)
  data$ondata1[data$prov == provincia] = sum(diff[1:26])
  data$ondata2[data$prov == provincia] = sum(diff[26:52])
  data$MAPE[data$prov == provincia] = accuracy(fit)[5]
  data$RMSE[data$prov == provincia] = accuracy(fit)[2]
  data$mean[data$prov == provincia] = mean(weekly_deaths_11_19)
  #pdf(paste0("~/GitHub/StatappCovid/Output/Plot/Province/", provincia ,".pdf"),width = 20,height = 14)
  #par(mar = c(5,5,5,5))
  #plot(i, as.numeric(y_covid$mean), type = "l", col = "blue", ylim = c(min(as.numeric(y_covid$lower[,2])), max(max(X_covid), max(y_covid$upper[,2]))), main = provincia, xlab = "Date", ylab = "Weekly Deaths", cex.main=1.75,cex.lab=1.75)
  #polygon(c(rev(i), i),c(rev(as.numeric(y_covid$lower[,2])),as.numeric(y_covid$upper[,2])), col = "gray80" )
  #lines(i, X_covid, col = "red")
  #legend("topright",legend = c("Actual", "95% Forecast CI"), fill = c("red","gray80"))
  #dev.off()
}

#### Create dataset for further analysis
load("Output/Data/Dataset_Population_Province.Rdata")
population = total_population_province %>% filter(CL_ETA == "15_21")%>%dplyr::select(Totale20)
population$PROV[population$PROV == "Aosta"] = "Valle d'Aosta"
population$PROV[population$PROV == "Bolzano"] = "Bolzano/Bozen"
population$PROV[population$PROV == "Reggio Emilia"] = "Reggio nell'Emilia"
population = population %>% ungroup()
population = population %>% add_row(PROV = "Sud Sardegna", Totale20 = 63687)
mortality_covid = merge(data, population, by.x = "prov", by.y = "PROV")
mortality_covid$ondata1 = mortality_covid$ondata1/mortality_covid$Totale20
mortality_covid$ondata2 = mortality_covid$ondata2/mortality_covid$Totale20
mortality_covid$tot = mortality_covid$tot/mortality_covid$Totale20
save(mortality_covid, file = Rdata_path("Mortality_Covid"))
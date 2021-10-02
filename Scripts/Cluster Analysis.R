setwd("~/GitHub/StatappCovid")
rm(list = ls())
source("Scripts/Utility Functions.R")
library(rgeos)
library(dplyr)
load("Output/Data/Mortality_Covid.Rdata")
load("Output/Data/Dataset_Aggregated_Comuni_LISA.Rdata")
load("Output/Data/Dataset_Aggregated_Province_LISA.Rdata")
load("Output/Data/Comuni_Geometry_Polygons.Rdata")
load("Output/Data/Province_Geometry_Polygons.Rdata")

###### Clustering Italian provinces based on deaths during first and second semester of 2020
#### Data exploration: plot the estimated number of deaths in the two semesters of 2020 in each province
x11()
plot(mortality_covid$ondata1, mortality_covid$ondata2, cex=0.01, xlab = "January - June 2020", ylab = "July - December 2020", main = "Mortality During Semesters of 2020")
text(mortality_covid$ondata2 ~ mortality_covid$ondata1, labels=mortality_covid$prov, data=mortality_covid, cex=0.5, font=2)
abline(0, 1)
mortality_covid = mortality_covid[, 1:3]
colnames(mortality_covid)[2:3] = c("First_Semester","Second_Semester")
semester = c("First_Semester","Second_Semester")
plist = list()
colorlim = list(c(0.000,0.03))
names(colorlim) = "Plots"
plist[["Plots"]]=list()
for(sem in semester){
  data = mortality_covid %>% dplyr::select(prov,one_of(sem))
  prov_poly_t = prov_poly
  prov_poly_t@data = prov_poly@data %>% left_join(data, by = c("DEN_PCM" = "prov")) 
  prov_poly_t = st_as_sf(prov_poly_t)
  g = ggplot(prov_poly_t) + geom_sf(aes_string(fill = sem)) + 
    ggtitle(paste0(sem)) +
    scale_fill_viridis(option = "inferno",limits = colorlim[["Plots"]]) + labs(fill = "deaths/residents")
  plist[["Plots"]][[sem]] = g
}
pdf(paste0("Output/Plot/Mortality - Semesters (2020).pdf"),width = 20,height = 14)
plot(ggarrange(plotlist = plist[["Plots"]],common.legend = F))
dev.off()

#### Cluster analysis
total_dataframe = mortality_covid
mortality_covid = mortality_covid[,2:3]
mortality_covid.euclidean = dist(mortality_covid, method = 'euclidean')
mortality_covid.clusters = hclust(mortality_covid.euclidean, method = "ward.D2") ## Euclidean Distance and Ward Linkage
x11()
plot(mortality_covid.clusters, main='Euclidean Distance and Ward Linkage', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
mean = sapply(mortality_covid[, 1:2], mean)
SS1 = sum((mortality_covid[,1]-mean[1])^2 + (mortality_covid[,2]-mean[2])^2)
## 2 Clusters
clusters = cutree(mortality_covid.clusters, k=2) 
dataframe_two_clusters = cbind(mortality_covid, clusters)
m1 = tapply(dataframe_two_clusters$First_Semester, dataframe_two_clusters$clusters, mean)
m2 = tapply(dataframe_two_clusters$Second_Semester, dataframe_two_clusters$clusters, mean)
x1 = c(m1[1], m2[1])
x2 = c(m1[2], m2[2])
first_cluster = dataframe_two_clusters%>%filter(clusters == "1")
second_cluster = dataframe_two_clusters%>%filter(clusters == "2")
SS2 = sum((first_cluster[,1]-x1[1])^2 + (first_cluster[,2]-x1[2])^2) +
  sum((second_cluster[,1]-x2[1])^2 + (second_cluster[,2]-x2[2])^2)
x11()
plot(mortality_covid, col=ifelse(clusters==1,'red', 'blue'), pch=19)
points(x1[1], x1[2], pch = 4, col = "red")
points(x2[1], x2[2], pch = 4, col = "blue")
## Three Clusters
clusters = cutree(mortality_covid.clusters, k=3)
dataframe_three_clusters = cbind(mortality_covid, clusters)
m1 = tapply(dataframe_three_clusters$First_Semester, dataframe_three_clusters$clusters, mean)
m2 = tapply(dataframe_three_clusters$Second_Semester, dataframe_three_clusters$clusters, mean)
x1 = c(m1[1], m2[1])
x2 = c(m1[2], m2[2])
x3 = c(m1[3], m2[3])
first_cluster = dataframe_three_clusters%>%filter(clusters == "1")
second_cluster = dataframe_three_clusters%>%filter(clusters == "2")
third_cluster = dataframe_three_clusters%>%filter(clusters == "3")
SS3 = sum((first_cluster[,1]-x1[1])^2 + (first_cluster[,2]-x1[2])^2) +
  sum((second_cluster[,1]-x2[1])^2 + (second_cluster[,2]-x2[2])^2) +
  sum((third_cluster[,1]-x3[1])^2 + (third_cluster[,2]-x3[2])^2)
x11()
plot(mortality_covid, col=ifelse(clusters==1,'red', ifelse(clusters==2, 'green', 'blue')), pch=19)
points(x1[1], x1[2], pch = 4, col = "red")
points(x2[1], x2[2], pch = 4, col = "green")
points(x3[1], x3[2], pch = 4, col = "blue")

## 4 Clusters
clusters <- cutree(mortality_covid.clusters, k=4)
dataframe_four_clusters = cbind(mortality_covid, clusters)
m1 = tapply(dataframe_four_clusters$First_Semester, dataframe_four_clusters$clusters, mean)
m2 = tapply(dataframe_four_clusters$Second_Semester, dataframe_four_clusters$clusters, mean)
x1 = c(m1[1], m2[1])
x2 = c(m1[2], m2[2])
x3 = c(m1[3], m2[3])
x4 = c(m1[4], m2[4])
first_cluster = dataframe_four_clusters%>%filter(clusters == "1")
second_cluster = dataframe_four_clusters%>%filter(clusters == "2")
third_cluster = dataframe_four_clusters%>%filter(clusters == "3")
fourth_cluster = dataframe_four_clusters%>%filter(clusters == "4")
SS4 = sum((first_cluster[,1]-x1[1])^2 + (first_cluster[,2]-x1[2])^2) +
  sum((second_cluster[,1]-x2[1])^2 + (second_cluster[,2]-x2[2])^2) +
  sum((third_cluster[,1]-x3[1])^2 + (third_cluster[,2]-x3[2])^2) +
  sum((fourth_cluster[,1]-x4[1])^2 + (fourth_cluster[,2]-x4[2])^2)
x11()
plot(mortality_covid, col=ifelse(clusters==1,'red', ifelse(clusters==2, 'green', ifelse(clusters==3, 'blue', 'pink'))), pch=19)
points(x1[1], x1[2], pch = 4, col = "red")
points(x2[1], x2[2], pch = 4, col = "green")
points(x3[1], x3[2], pch = 4, col = "blue")
points(x4[1], x4[2], pch = 4, col = "pink")
## Cophenetic
cp = cophenetic(mortality_covid.clusters)
cor(cp, mortality_covid.euclidean)
x = c("1","2","3","4")
SS = c(SS1, SS2, SS3, SS4)
x11()
plot(x, SS)

#### Map
dataframe_clusters = cbind(total_dataframe, clusters)
colnames(dataframe_clusters) = c("Provincia", "First_Semester", "Second_Semester", "Cluster")
dataframe_clusters$Cluster[dataframe_clusters$Cluster == "1"] = "44"
dataframe_clusters$Cluster[dataframe_clusters$Cluster == "2"] = "33"
dataframe_clusters$Cluster[dataframe_clusters$Cluster == "3"] = "11"
dataframe_clusters$Cluster[dataframe_clusters$Cluster == "4"] = "22"
dataframe_clusters$Cluster[dataframe_clusters$Cluster == "44"] = "4"
dataframe_clusters$Cluster[dataframe_clusters$Cluster == "22"] = "2"
dataframe_clusters$Cluster[dataframe_clusters$Cluster == "33"] = "3"
dataframe_clusters$Cluster[dataframe_clusters$Cluster == "11"] = "1"
save(dataframe_clusters, file = Rdata_path("Dataframe_Clusters"))
data = dataframe_clusters %>% dplyr::select(Provincia,Cluster)
prov_poly_t = prov_poly
prov_poly_t@data = prov_poly_t@data %>% left_join(data, by = c("DEN_PCM" = "Provincia"))
prov_poly_t = st_as_sf(prov_poly_t)
pdf("~/GitHub/StatappCovid/Output/Plot/Semester - Clusters.pdf",width = 20,height = 14)
ggplot(prov_poly_t) + geom_sf(aes_string(fill = "Cluster")) + 
  ggtitle("Cluster Analysis (4 Clusters)") +
  scale_fill_viridis_d(option = "inferno") + labs(fill = "Cluster")
dev.off()

#### Barplots
### Divide the four datasets
first_cluster = dataframe_clusters%>%filter(Cluster == "1")%>%dplyr::select(-c("Cluster", "Provincia"))
n1 = dim(first_cluster)[1]
second_cluster = dataframe_clusters%>%filter(Cluster == "2")%>%dplyr::select(-c("Cluster", "Provincia"))
n2 = dim(second_cluster)[1]
third_cluster = dataframe_clusters%>%filter(Cluster == "3")%>%dplyr::select(-c("Cluster", "Provincia"))
n3 = dim(third_cluster)[1]
fourth_cluster = dataframe_clusters%>%filter(Cluster == "4")%>%dplyr::select(-c("Cluster", "Provincia"))
n4 = dim(fourth_cluster)[1]
m1 = sapply(first_cluster, mean)*1000
m2 = sapply(second_cluster, mean)*1000
m3 = sapply(third_cluster, mean)*1000
m4 = sapply(fourth_cluster, mean)*1000
m = rbind(m1, m2, m3, m4)
lev1 = rep(c("First Semester", "Second Semester"),4)
lev2 = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")
x11()
layout(matrix(c(1, 1, 2), 1, 3))
barplot(c(m1[1], m1[2], m2[1], m2[2], m3[1], m3[2], m4[1], m4[2]), 
        names.arg = lev1, 
        col = c("black", "black", rainbow(8)[7], rainbow(8)[7], rainbow(8)[2], rainbow(8)[2], "yellow", "yellow"), 
        border = "black", 
        main = "Surplus of Mortality in the Semesters of 2020 (for 1000 Inhabitants)", 
        legend.text = c("Cluster 1", "", "Cluster 2", "","Cluster 3", "","Cluster 4", ""),
        args.legend = list(x = "topright", cex = 1.5))
barplot(c(m1[1] + m1[2], m2[1]+ m2[2], m3[1] + m3[2], m4[1] + m4[2]), 
        names.arg = lev2, 
        col = c("black", rainbow(8)[7], rainbow(8)[2], "yellow"), 
        border = "black", 
        main = "Overall Surplus of Mortality in 2020 (for 1000 Inhabitants)", 
        legend.text = c("Cluster 1", "Cluster 2","Cluster 3", "Cluster 4"),
        args.legend = list(x = "topright", cex = 1.5))



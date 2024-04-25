source('lab6/desc_analysis.R')

rm(list=ls())

data_scaled = scale(data,center=mins,scale=maxs-mins)
data_scaled
data

library(BBmisc)
data_sc_norm = normalize(data_scaled,method="range",range=c(0,1))
data_sc_norm


#Построения локтя
library (factoextra)
library (cluster)
fviz_nbclust(data_sc_norm[,-(length(colnames(data_sc_norm)))], kmeans, method = "wss")

#Построили метод силуэта
fviz_nbclust(data_sc_norm[,-(length(colnames(data_sc_norm)))], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#
fviz_nbclust( data, kmeans, method = "wss")
gap_stat <- clusGap(data_sc_norm[,-(length(colnames(data_sc_norm)))], FUN = kmeans, nstart = 5,K.max =13, B = 5)
#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)


#Алгоритм на основе консенсуса
install.packages('parameters')
library(parameters)
n_clust <- n_clusters(data.frame(data_sc_norm[,-(length(colnames(data_sc_norm)))]),
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust
plot(n_clust)


dist.datas=dist(data_sc_norm[,-(length(colnames(data_sc_norm)))])
labels_datas=data_sc_norm[,(length(colnames(data_sc_norm)))]
clust.datas=hclust(dist.datas,'ward.D')
plot(clust.datas,labels_datas,cex=0.5)
rect.hclust(clust.datas,k=4,border="red")

groups <- cutree(clust.datas, k=4)
g1=data_sc_norm[groups==1,]
g1
g2=data_sc_norm[groups==2,]
g2

source('lab6/desc_analysis.R')
data_scaled = scale(data,center=mins,scale=maxs-mins)
data_scaled
data

library(BBmisc)
data_sc_norm = normalize(data_scaled,method="range",range=c(0,1))
data_sc_norm

data_k=data_sc_norm[,-(length(colnames(data_sc_norm)))]
data_k
#Построения локтя
library (factoextra)
library (cluster)
fviz_nbclust(data_k, kmeans, method = "wss")

#Построили метод силуэта
set.seed(123)
fviz_nbclust(data_k, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#
set.seed(123)
fviz_nbclust(data, kmeans, method = "wss")
gap_stat <- clusGap(data_k, FUN = kmeans, K.max =12)
#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)


#Алгоритм на основе консенсуса
library(parameters)
n_clust <- n_clusters(data.frame(data_k),
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust
plot(n_clust)


#Кластеризация для 5
dist.datas=dist(data_k)
labels_datas=data_sc_norm[,(length(colnames(data_sc_norm)))]
clust.datas=hclust(dist.datas,'ward.D')
plot(clust.datas,labels_datas,cex=0.5)
rect.hclust(clust.datas,k=5,border="red")

groups <- cutree(clust.datas, k=5)
data[groups==1,]
data[groups==2,]
data[groups==3,]
data[groups==4,]
data[groups==5,]
options(max.print=999999)
g1=cbind(data_k[groups==1,],c(1))
colnames(g1)[14]=c('class')
g1
g2=cbind(data_k[groups==2,],c(2))
colnames(g2)[14]=c('class')
g2
g3=cbind(data_k[groups==3,],c(3))
colnames(g3)[14]=c('class')
g3
g4=cbind(data_k[groups==4,],c(4))
colnames(g4)[14]=c('class')
g4
g5=cbind(data_k[groups==5,],c(5))
colnames(g5)[14]=c('class')
g5

layout(matrix(1:2,nrow=1,ncol=2))
df = t(t(data.frame(colMeans(g1),colMeans(g2),colMeans(g3),colMeans(g4),colMeans(g5))))
barplot(df[-14,],main='heart illnesses',col=rainbow(13),bty='n',beside=TRUE)
plot(1:2,1:2,xaxt="n",yaxt="n",main="Легенда",xlab="",ylab="")
legend("center", legend = rownames(df),cex=0.6, col=rainbow(13), lwd=10, bty = "n")

data_classes = rbind(g1,g2,g3,g4,g5)
data_classes
data_for_boxplot=cbind(data[,-14],data_classes[,14])
data_for_boxplot
layout(matrix(1:16,nrow=4,ncol=4))
for(i in 1:(length(colnames(data))-1)){
  boxplot(data_for_boxplot[,i]~data_for_boxplot[,14],data=data_for_boxplot,
          xlab="class",ylab=colnames(data)[i])
}

#Кластеризируем kmeans
km5=km = kmeans(data_sc_norm,5)
fviz_cluster(km, data_k,palette="Set2", ggtheme = theme_minimal())
print(km)
data[km$cluster == 1,]
data[km$cluster == 2,]
data[km$cluster == 3,]
data[km$cluster == 4,]
data[km$cluster == 5,]


#Строим скаттерплот
pairs(data[,-14])
pairs(data[,-14],main= "Зависимости данных", col = c("red","green","blue","yellow","purple"))
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07","green","blue")
pairs(data[,-14],main= "Люди по кластерам",pch = 19, cex = 0.8,col = my_cols[km$cluster],
      lower.panel=NULL)

#Строим 3d кластеризацию
library("scatterplot3d")
colors <- c("#999999", "#E69F00","#56B4E9","red","green")
colors <- colors[as.numeric(km$cluster)]
colors
s3d <- scatterplot3d(data[,c(1:3)], main= "Люди по кластерам", pch = 16, color=colors)
legend(s3d$xyz.convert(100, 0, 1.5), legend = c("Класс 1","Класс 2","Класс 3","Класс 4","Класс 5"),
       col =c("#999999", "#E69F00", "#56B4E9","red","green"), pch = 16)


#Кластеризация для 2
dist.datas=dist(data_k)
labels_datas=data_sc_norm[,(length(colnames(data_sc_norm)))]
clust.datas=hclust(dist.datas,'ward.D')
plot(clust.datas,labels_datas,cex=0.5)
rect.hclust(clust.datas,k=2,border="red")

groups <- cutree(clust.datas, k=2)
data[groups==1,]
data[groups==2,]

options(max.print=999999)
g1=cbind(data_k[groups==1,],c(1))
colnames(g1)[14]=c('class')
g1
g2=cbind(data_k[groups==2,],c(2))

colnames(g2)[14]=c('class')
g2

layout(matrix(1:2,nrow=1,ncol=2))
df = t(t(data.frame(colMeans(g1),colMeans(g2))))
barplot(df[-14,],main='heart illnesses',col=rainbow(13),bty='n',beside=TRUE)
plot(1:2,1:2,xaxt="n",yaxt="n",main="Легенда",xlab="",ylab="")
legend("center", legend = rownames(df),cex=0.6, col=rainbow(13), lwd=10, bty = "n")

data_classes = rbind(g1,g2)
data_classes
data_for_boxplot=cbind(data[,-14],data_classes[,14])
data_for_boxplot
layout(matrix(1:16,nrow=4,ncol=4))
for(i in 1:(length(colnames(data))-1)){
  boxplot(data_for_boxplot[,i]~data_for_boxplot[,14],data=data_for_boxplot,
          xlab="class",ylab=colnames(data)[i])
}

#Кластеризируем kmeans
km2=km = kmeans(data_sc_norm,2)
fviz_cluster(km, data_k,palette="Set2", ggtheme = theme_minimal())
print(km)
data[km$cluster == 1,]
data[km$cluster == 2,]


#Строим скаттерплот
pairs(data[,-14])
pairs(data[,-14],main= "Зависимости данных", col = c("red","green"))
my_cols <- c("#00AFBB", "#E7B800")
pairs(data[,-14],main= "Люди по кластерам",pch = 19, cex = 0.8,col = my_cols[km$cluster],
      lower.panel=NULL)

#Строим 3d кластеризацию
library("scatterplot3d")
colors <- c("#999999", "#E69F00")
colors <- colors[as.numeric(km$cluster)]
colors
s3d <- scatterplot3d(data[,c(1:3)], main= "Люди по кластерам", pch = 16, color=colors)
legend(s3d$xyz.convert(100, 0, 1.5), legend = c("Класс 1","Класс 2"),
       col =c("#999999", "#E69F00" ), pch = 16)
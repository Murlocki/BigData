library(rvest)
url_data  = read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021")

#Извлекли шапку таблицы
header_text = html_text(html_nodes(url_data,"thead tr th"))
header_text

#Выдернули строки
tag_text = html_text(html_nodes(url_data,"tbody tr td"))
tag_text

#Создаем датафрейм
data = data.frame(t(matrix(tag_text,nrow=11)))
data
#Составил вектор рангов стран
country_ranks = c(1:(length(tag_text)))
country_ranks
data[,1] = country_ranks

#Переименовали столбцы
colnames(data)=header_text

data


rm(list=ls())
#Доп задание
library(readr)
assess <- read_table("lab5/assess.dat")
View(assess)

needed_col = assess[,3:12]

mins = apply(needed_col,2,min)
maxs = apply(needed_col,2,max)
assess[,3:12] = scale(needed_col,center=mins,scale=maxs-mins)
assess

#Сделали матрицу расстояний
dist.assess = dist(needed_col)
#Сделали кластеры
clust.assess = hclust(dist.assess,"ward.D")
clust.assess

#Рисуем дендрограммы
layout(matrix(c(1,2),nrow=1,ncol=2))

plot(clust.assess,labels = as.vector(t(assess[,2])))
rect.hclust(clust.assess,k=4,border="red")
abline(h=4,col="blue",lwd=3)

plot(clust.assess,labels = as.vector(t(assess[,2])))
rect.hclust(clust.assess,k=3,border="red")
abline(h=8,col="blue",lwd=3)


groups1 = cutree(clust.assess,4)
groups2 = cutree(clust.assess,3)

assess[groups1==1,]
assess[groups1==2,]
assess[groups1==3,]
assess[groups1==4,]

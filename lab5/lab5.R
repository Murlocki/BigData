library(rvest)
url_data  = read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021")

#Выдернули года
years = html_nodes(url_data,"select[name='title'] option") %>% html_attr("value")
years = years[years>="2014" & years<="2021"]
years

#Функция получения таблицы по ссылке
getTable = function(year){
  
  country_names = c("Finland", "Denmark", "France", "Germany","Romania")
  
  link = paste("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=",year,sep="")
  url_data = read_html(link)
  table = html_table(html_nodes(url_data,"div table"))[[2]] %>% as.data.frame()
  table[,1]=c(1:length(table[,1]))
  table=table[table$Country %in% country_names,] 
  
  table = cbind(data.frame(Year=year,table))
  return(table)
  
}

#Создаем 1 общую таблицу
data = do.call(rbind,lapply(years,getTable))
rownames(data)=c(1:dim(data)[1])
data[data=="-"]=0
data


createPlot=function(number,title){
  plot.new()
  grid(nx = NULL, ny = NULL,
       lty = 1,      # Grid line type
       col = "gray", # Grid line color
       lwd = 1)      # Grid line width
  par(new=TRUE)
  print(c(min(data[,number]),max(data[,number])))
  plot(c(1:length(years)),rev(data[,number][data$Country=="Finland"]),ylab=title,xlab="",type='b', lwd=2,col="green",xaxt="n",ylim=c(as.numeric(min(data[,number])),as.numeric(max(data[,number]))),
       pch=20,main="Значение выбранного параметра для стран")
  lines(c(1:length(years)),rev(data[,number][data$Country=="Denmark"]),col='blue',type='b',pch=20,lwd=2)
  lines(c(1:length(years)),rev(data[,number][data$Country=="France"]),col='red',type='b',pch=20,lwd=2)
  lines(c(1:length(years)),rev(data[,number][data$Country=="Germany"]),col='purple',type='b',pch=20,lwd=2)
  lines(c(1:length(years)),rev(data[,number][data$Country=="Romania"]),col='orange',type='b',pch=20,lwd=2)
  
  #Настроили заголовки
  axis.labels = rev(years)
  axis(1, at = c(1:length(years)), labels = axis.labels,las=2,cex.axis=0.8)
  title(xlab="Year", line=4, cex.lab=1.2)
  return("")  
}

warnings()

createPlot(2,"Rank")
legend('top',cex=1,title='Страны',c("Finland", "Denmark", "France", "Germany","Romania"),lty=c(1,1,1,1,1),pch=c(20,20,20,20,20),col=c('green','blue','red','purple','orange'))


#Рисуем график 
layout(matrix(1:6,nrow=2,ncol=3))
createPlot(4,"Country Quality.of.Life.Index")
createPlot(5,"Purchasing.Power.Index")
createPlot(6,"Safety.Index")
createPlot(7,"Health.Care.Index")
#Легенда
plot(1:2,1:2,xaxt="n",yaxt="n",main="Легенда",xlab="",ylab="")
legend('topright',cex=2.7,title='Страны',c("Finland", "Denmark", "France", "Germany","Romania"),lty=c(1,1,1,1,1),pch=c(20,20,20,20,20),col=c('green','blue','red','purple','orange'))




layout(matrix(1:6,nrow=2,ncol=3))
createPlot(8,"Cost.of.Living.Index")
createPlot(9,"Property.Price.to.Income.Ratio")
createPlot(10,"Traffic.Commute.Time.Index")
createPlot(11,"Pollution.Index")
createPlot(12,"Climate.Index")

plot(1:2,1:2,xaxt="n",yaxt="n",main="Легенда",xlab="",ylab="")
legend('topright',cex=2,title='Страны',c("Finland", "Denmark", "France", "Germany","Romania"),lty=c(1,1,1,1,1),pch=c(20,20,20,20,20),col=c('green','blue','red','purple','orange'))


rm(list=ls())


#Задание 2
url = "https://ru.m.wikipedia.org/wiki/%D0%A1%D0%BF%D0%B8%D1%81%D0%BE%D0%BA_%D0%BC%D1%83%D0%B7%D0%B5%D0%B5%D0%B2_%D0%A0%D0%BE%D1%81%D1%82%D0%BE%D0%B2%D1%81%D0%BA%D0%BE%D0%B9_%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D0%B8"
data = read_html(url) %>% html_nodes("div section table") %>% html_table() %>% as.data.frame()
data = data[c(-1,-5,-4)]
dim(data)
data

#Ссылки на музеи
museum = read_html(url) %>% html_nodes("div>section>table>tbody>tr>td")
museum=museum[seq(2,length(museum),by=5)]
length(museum)
ex=html_nodes(museum,"sup>a")
allNameLinks = html_nodes(museum,"a")
Links=allNameLinks[!(allNameLinks %in% ex)] %>% html_attr("href")
Links= Links[!duplicated(Links)]
Links = data.frame(Links=paste(c("https://ru.m.wikipedia.org/"),Links,sep=""))
print(Links)

#Ссылки на картинки
getPictLink = function(x){
  if(length(html_nodes(x,"span>a"))>0){
    link = (html_nodes(x,"span>a") %>% html_attr("href"))
    return(paste(c("https://ru.m.wikipedia.org/"),link,sep=""))
  }
  else{
    return(NA)
  }
}

pictures = read_html(url) %>% html_nodes("div>section>table>tbody>tr>td")
pictures= pictures[seq(4,length(pictures),by=5)]
pictures= sapply(pictures,getPictLink)
pictures = data.frame(PictureLink=pictures)
print(pictures)

result = cbind(data,pictures) %>% cbind(Links)
result

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

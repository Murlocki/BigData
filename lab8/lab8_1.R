rm(list=ls())
require(stats); require(graphics)

#Задание 8.2.1
data = longley
data
#1 Определяем количество переменных
variable_number = ncol(data)
variable_number
#2 Определяем количество строк
row_number = nrow(data)
row_number
#3 Проводим дискриптивный анализ

#Находим квартили
apply(data,2,summary)

#Находим моду
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modas = apply(data,2,getmode)
modas

#Находим размахы
apply(data,2,max) - apply(data,2,min)
#Находим межквартильный размах
quantile_25 = apply(data,2,function(x){quantile(x,probs=0.25)})
quantile_75 = apply(data,2,function(x){quantile(x,probs=0.75)})
quantile_75 - quantile_25

#Находим квадратичное отклонение и дисперсию
apply(data,2,var)
apply(data, 2, sd)

#Строим боксплоты
par(mfrow=c(3,3))
for(i in 1:variable_number){
  boxplot(data[,i],main=colnames(data)[i],ylab="value")
}
#Строим распределения
#Смотрим распределение
par(mfrow=c(3,3))
for(i in 1:variable_number){
  hist(data[,i],xlab='values',main=colnames(data)[i],col=rainbow(length(unique(data[,i]))))
}

#4 Получаем корреляционную матрицу
cor(data)

#install.packages(pkgs=c("ellipse"))
library(ellipse)
plotcorr(cor(data))

#5 Графически фиксируем прибыль
#Строим графики зависимостей
library(car)
scatterplotMatrix(data, spread=FALSE, lty.smooth=2,
                  main="Матрица диаграмм рассеяния")

par(mfrow=c(4,7))
for(i in 1:variable_number){
  for(j in 1:4)
    plot(data[,i],data[,j],main="Dependency",xlab=colnames(data)[i],ylab=colnames(data)[j])
}

par(mfrow=c(3,7))
for(i in 1:variable_number){
  for(j in 5:variable_number)
    plot(data[,i],data[,j],main="Dependency",xlab=colnames(data)[i],ylab=colnames(data)[j])
}

#6 Проверяем на нормальность распределения
apply(data,2,shapiro.test)

#Показываем графически, что величины не норм
par(mfrow=c(3,3))
for(i in 1:variable_number){
  hist(data[,i],xlab='values',main=colnames(data)[i],col=rainbow(length(unique(data[,i]))))
}

#Проверяем наличие нелинейной корреляции
cor(data,method="spearman")
plotcorr(cor(data,method="spearman"))

cor(data,method="kendall")
plotcorr(cor(data,method="kendall"))

symnum(cor(data))
symnum(cor(data,method="spearman"))
symnum(cor(data,method="kendall"))
getwd()

x=c(174,162,188,192,165,168,172)
str(x)#выводим данные об объекте
pol=c("male","female","male","male","female","male","male")
is.character(pol)
is.factor(vector)
is.vector(pol)
str(pol)
table(pol)
pol.f=factor(pol)
is.factor(pol.f)
pol.f
str(pol.f)
as.numeric(pol.f)
plot(pol.f)

#source("C:/Users/kiril/Desktop/Rdata/main1.R",echo=TRUE)
# pch - символы для вывода
w=c(69,68,93,87,59,82,72)
plot(x,w,pch=as.numeric(pol.f),col=as.numeric(pol.f))
legend("topleft",pch=1:2,col=1:2,legend=levels(pol.f))

plot(x,w,pch=(7:8), col=c("magenta","green"))
legend("topleft",pch=7:8, col=c("magenta","green"),legend=levels(pol.f))

m=c("L","S","XL","XXL","S","M","L")
m.f = factor(m)
m.f
m.o = ordered(m.f,levels=c("S","M","L","XL","XXL"))
m.o
m


h=c(8,10,NA,NA,8,NA,8)
mean(h,na.rm=TRUE)
h[is.na(h)]=mean(h,na.rm=TRUE)
h

names(w)=c("Коля","Женя","Петя","Саша","Катя","Вася","Жора")
d=data.frame(weight=w,height=x,size=m.o,pol=pol.f)
d$weight
d[[1]]
d[,"weight"]

d[d$pol=="female",]

d[order(d$pol,d$height),]

d[,order(colnames(d))]

library(openxlsx)
library(magrittr)
ex_data <- read.xlsx(".\\lab2-3\\lab2.xlsx", 1)
colnames(ex_data)
colnames(ex_data) = c("ФИО","Bugs","Hitboxes","AI","Optimization","Interface","Allies","Donate","Cutscenes","Random","Levelling","DLC","Openworld")
ex_data

apply(ex_data[,c(2:length(colnames(ex_data)))],2,min)
apply(ex_data[,c(2:length(colnames(ex_data)))],2,max)
apply(ex_data[,c(2:length(colnames(ex_data)))],2,mean)

as.vector(apply(apply(ex_data[,c(2:length(colnames(ex_data)))],2,function(x) x<3),2,sum))
as.vector(apply(apply(ex_data[,c(2:length(colnames(ex_data)))],2,function(x) x>7),2,sum))


apply(ex_data[,c(2:length(colnames(ex_data)))],2,mean) %>% sort(decreasing = TRUE)

barplot(apply(ex_data[,c(2:length(colnames(ex_data)))],2,mean),xlab="Объекты оценки",cex.names=0.6,ylab="Средняя оценка")
barplot(apply(ex_data[,c(2:length(colnames(ex_data)))],2,sum),xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")

ex_data$Bugs
hist(ex_data$Bugs,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Hitboxes,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$AI,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Optimization,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Interface,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Allies,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Donate,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Cutscenes,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Random,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Levelling,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$DLC,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(ex_data$Openworld,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")

boxplot(ex_data[,c(2:length(colnames(ex_data)))],xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
#lab 3
apply(ex_data,2,median)


summary(ex_data)
attach(ex_data)
summary(Bugs)
detach(ex_data)

library(dplyr)
ex_data %>%filter(Bugs>2)
apply(ex_data,2,sd)
apply(ex_data,2,var)
apply(ex_data,2,IQR,na.rm=TRUE)
boxplot(ex_data$Bugs~ex_data$AI)
summary(ex_data)

res = boxplot(ex_data$Bugs~ex_data$AI)
res
bxp(re)

#2
summary(ex_data)
apply(ex_data,2,sd,na.rm=TRUE)
apply(ex_data,2,var,na.rm=TRUE)
apply(ex_data,2,IQR,na.rm=TRUE)
boxplot(ex_data[,c(2:length(colnames(ex_data)))])
#3Выполняем сортировку наборов данных по выбранному признаку
ex_data[order(ex_data$Bugs)]
ex_data[order(ex_data$AI)]
ex_data[order(ex_data$Allies)]
#4
new_data2 = subset(ex_data,AI>=5 & Bugs<10 | Hitboxes < 9, select = ФИО:Cutscenes)
print(new_data2)

print(dim(new_data2))

summary(new_data2)


apply(new_data2,2,sd)
apply(new_data2,2,var)
apply(new_data2,2,IQR,na.rm=TRUE)

barplot(apply(new_data2[,c(2:length(colnames(new_data2)))],2,mean),xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")

boxplot(new_data2[,c(2:length(colnames(new_data2)))])

hist(new_data2$Bugs,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(new_data2$Hitboxes,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(new_data2$AI,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(new_data2$Optimization,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(new_data2$Interface,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(new_data2$Allies,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(new_data2$Donate,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")
hist(new_data2$Cutscenes,xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")

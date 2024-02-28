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
ex_data <- read.xlsx(".\\lab2\\lab2.xlsx", 1)
colnames(ex_data)
colnames(ex_data) = c("ФИО","Bugs","Hitboxes","AI","Optimization","Interface","Allies","Donate","Cutscenes","Random","Levelling","DLC","Openworld")
ex_data

apply(ex_data[,c(2:length(colnames(ex_data)))],2,min)
apply(ex_data[,c(2:length(colnames(ex_data)))],2,max)
apply(ex_data[,c(2:length(colnames(ex_data)))],2,mean)

as.vector(apply(apply(ex_data[,c(2:length(colnames(ex_data)))],2,function(x) x<3),2,sum))
as.vector(apply(apply(ex_data[,c(2:length(colnames(ex_data)))],2,function(x) x>7),2,sum))


apply(ex_data[,c(2:length(colnames(ex_data)))],2,mean) %>% sort(decreasing = TRUE)

barplot(apply(ex_data[,c(2:length(colnames(ex_data)))],2,sum),xlab="Объекты оценки",cex.names=0.6,ylab="Суммарная оценка")

v = c(1:10)
dim(v)=c(2,5)
v
v = matrix(c(1:10),nrow=2,ncol=5,byrow=TRUE)
v
apply(v,2,function(x) x>3)

    
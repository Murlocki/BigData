data = data.frame(read.csv('lab7/athlete_events.csv'))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
data$Height[is.na(data$Height)]=mean(data$Height[!is.na(data$Height)])
data$Weight[is.na(data$Weight)]=mean(data$Weight[!is.na(data$Weight)])
data$Age[is.na(data$Age)]=mean(data$Age[!is.na(data$Age)])
data$Medal[is.na(data$Medal)]="None"


summary(data)

modas = apply(data,2,getmode)
modas

variances = apply(data,2,var)
variances

sds = apply(data,2,sd)
sds

#Делаем графики боксплотов
par(mfrow=c(2,2))
boxplot(data$Weight,main="weight")
boxplot(data$Height,main="Height")
boxplot(data$Age,main="age")

boxplot(data$Weight[!is.na(data$Weight)],main="weight")
boxplot(data$Height[!is.na(data$Height)],main="Height")
boxplot(data$Age[!is.na(data$Age)],main="age")

#Смотрим распределение
par(mfrow=c(4,4))
for(i in colnames(data)){
  if(is.numeric(data[,i])){
    hist(data[,i],main=i)
  }
  else{
    f=factor(data[,i])
    sort(levels(f))
    levels(f) = sort(levels(f))
    as.numeric(f)
    hist(as.numeric(f),main=i,xaxt="n")
    axis(1,at=1:length(levels(f)),labels=unique(levels(f))) 
  }
}
#Проверяем на нормальное распределение тестом шапиро
shapiro.test(data[sample(1:nrow(data),5000,replace=FALSE),"Height"])
shapiro.test(data[sample(1:nrow(data),5000,replace=FALSE),"Age"])
shapiro.test(data[sample(1:nrow(data),5000,replace=FALSE),"Weight"])


f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"Medal"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f)) 

f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"Name"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))

f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"Sex"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))

f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"Team"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))
f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"NOC"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))
f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"Games"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))
f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"Season"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))
f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"City"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))
f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"Sport"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))
f=factor(data[sample(1:nrow(data),5000,replace=FALSE),"Event"])
levels(f) = sort(levels(f))
shapiro.test(as.numeric(f))

#Проверим графически нормальность
library(car)
par(mfrow=c(4,4))
colnames(data)
for(i in colnames(data)){
  if(is.numeric(data[,i])){
    qqPlot(data[,i],main=i)
  }
  else{
    f=factor(data[,i])
    sort(levels(f))
    levels(f) = sort(levels(f))
    as.numeric(f)
    qqPlot(as.numeric(f),main=i)
  }
}

#Проверяем средний вес
data_gym=data[data$Sport=="Gymnastics","Weight"]
shapiro.test(data_gym[sample(1:nrow(data),5000,replace=FALSE)])
t.test(data_gym,mu=61.1)
#Проведем тест без учета,что величина нормальная
wilcox.test(data_gym,mu=61.999,conf.int=TRUE)


#Сделаем выборки 2-ух видов спорта
data_box_f=data[data$Sport=="Boxing"&data$Sex=="F","Weight"]
data_box_m=data[data$Sport=="Boxing"&data$Sex=="M","Weight"]

data_gym_f=data[data$Sport=="Gymnastics"&data$Sex=="F","Weight"]
data_gym_m=data[data$Sport=="Gymnastics"&data$Sex=="M","Weight"]

var(data_gym_f)
var(data_box_f)

var(data_gym_m)
var(data_box_m)

shapiro.test(data_gym_f[sample(1:nrow(data),5000,replace=FALSE)])
shapiro.test(data_box_f)
shapiro.test(data_gym_m[sample(1:nrow(data),5000,replace=FALSE)])
shapiro.test(data_box_m[sample(1:nrow(data),5000,replace=FALSE)])


t.test(data_gym_f,data_box_f)
t.test(data_gym_m,data_box_m)

wilcox.test(data_gym_f,data_box_f)
wilcox.test(data_gym_m,data_box_m)

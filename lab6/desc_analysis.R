data = data.frame(read.csv('lab6/heart.csv'))
data
#Посчитаем центральные тенденции для каждого из параметров
means = apply(data,2,mean)
means
medians = apply(data,2,median)
medians

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modas = apply(data,2,getmode)
modas

#Считаем дисперсия и стандартное отклонения
variances = apply(data,2,var)
variances

sds = apply(data,2,sd)
sds

# Считаем квартили,мин и макс
mins = apply(data,2,min)
mins

maxs = apply(data,2,max)
maxs

quartilies = apply(data,2,quantile)
quartilies
#Делаем графики боксплотов
par(mfrow=c(4,4))
for(i in 1:(length(colnames(data)))){
  boxplot(data[,i],main=colnames(data)[i])
}
colnames(data)
length(colnames(data))

#Смотрим распределение
par(mfrow=c(4,4))
for(i in 1:(length(colnames(data)))){
  hist(data[,i],xlab='values',main=colnames(data)[i],col=rainbow(length(unique(data[,i]))))
}



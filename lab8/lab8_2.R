rm(list=ls())
data = read.csv("C:\\Users\\kiril\\Desktop\\Rdata\\BigData\\lab8\\data.csv")
#Выкинули строки
data = data[c(-23,-24,-25,-26,-27),]
#Выкинули столбцы
data = data[,c(-1,-2,-4)]
col_names = data[,1]
col_names
#Сделали датасет
data_t = t(data[,c(-1)])
colnames(data_t) = col_names
rownames(data_t) = 1989:2018
data_t[data_t == '..'] <- NA


#Сделали график ураааа
data = data_t
par(mfrow=c(2,1))
plot(rownames(data),data[,2],type="o",xlab="year",ylab="GDP growth",main="France GDP growth per year")
plot(rownames(data),data[,1],type="o",xlab="year",ylab="Current GDP",main="France GDP per year")


data = apply(data,2,as.numeric)
rownames(data) = 1989:2018

#Матрицы корреляций с учетом пропущенных и без учета пропущенных

data_t = data[, colSums(is.na(data)) < nrow(data)]
data_t_col = colnames(data_t)
colnames(data_t)=1:length(data_t_col)
data_t_col
plotcorr(cor(data_t,method="spearman",use="pairwise.complete.obs"))
plotcorr(cor(na.omit(data_t),method="spearman"))

plotcorr(cor(data_t,use="pairwise.complete.obs"))
plotcorr(cor(na.omit(data_t)))

plotcorr(cor(data_t,method="kendall",use="pairwise.complete.obs"))
plotcorr(cor(na.omit(data_t),method="kendall"))

# 2
# 1 Корреляция Роста ввп и прироста населения
cor(data[,2],data[,14])
cor(data[,2],data[,14],method="spearman")
cor(data[,2],data[,14],method="kendall")

cor.test(data[,2],data[,14])
cor.test(data[,2],data[,14],method="spearman")
cor.test(data[,2],data[,14],method="kendall")

data_t = data[!is.na(data[,14]) & !is.na(data[,2]),]
data_t = data_t[order(data_t[,2]),]
par(mfrow=c(2,1))
plot(sort(data_t[,2]),data_t[,14],type="o",xlab="GDP growth",ylab="Population growth")
data_t = data_t[order(data_t[,14]),]
plot(sort(data_t[,14]),data_t[,2],type="o",ylab="GDP growth",xlab="Population growth")

# 2 Прирост населения на динамику безработицы
cor(data[,14],data[,17])
cor(data[,14],data[,17],method="spearman")
cor(data[,14],data[,17],method="kendall")

cor.test(data[,14],data[,17])
cor.test(data[,14],data[,17],method="spearman")
cor.test(data[,14],data[,17],method="kendall")

data_t = data[!is.na(data[,14]) & !is.na(data[,17]),]
data_t = data_t[order(data_t[,14]),]
par(mfrow=c(2,1))
plot(sort(data[,14]),data[,17],type="o",ylab="Unemployment",xlab="Population growth")
data_t = data_t[order(data_t[,17]),]
plot(sort(data[,17]),data[,14],type="o",xlab="Unemployment",ylab="Population growth")

# 3 Расходы на медицину на среднюю продолжительность жизни
cor(data[,12],data[,13],use="pairwise.complete.obs")
cor(data[,12],data[,13],method="spearman",use="pairwise.complete.obs")
cor(data[,12],data[,13],method="kendall",use="pairwise.complete.obs")

cor.test(data[,12],data[,13],use="pairwise.complete.obs")
cor.test(data[,12],data[,13],method="spearman",use="pairwise.complete.obs")
cor.test(data[,12],data[,13],method="kendall",use="pairwise.complete.obs")

data_t = data[!is.na(data[,12]) & !is.na(data[,13]),]

par(mfrow=c(2,1))
data_t = data_t[order(data_t[,12]),]
plot(sort(data_t[,12]),data_t[,13],type="o",ylab="Life expectancy",xlab="Health expenditure")
data_t = data_t[order(data_t[,13]),]
plot(sort(data_t[,13]),data_t[,12],type="o",xlab="Life expectancy",ylab="Health expenditure")

plotcorr(cor(data[,c(12,13)],use="pairwise.complete.obs"))

# 3 Расходы на медицину на смертность
cor(data[,12],data[,18],use="pairwise.complete.obs")
cor(data[,12],data[,18],method="spearman",use="pairwise.complete.obs")
cor(data[,12],data[,18],method="kendall",use="pairwise.complete.obs")

cor.test(data[,12],data[,18],use="pairwise.complete.obs")
cor.test(data[,12],data[,18],method="spearman",use="pairwise.complete.obs")
cor.test(data[,12],data[,18],method="kendall",use="pairwise.complete.obs")

data_t = data[!is.na(data[,12]) & !is.na(data[,18]),]

par(mfrow=c(2,1))
data_t = data_t[order(data_t[,12]),]
plot(sort(data_t[,12]),data_t[,18],type="o",ylab="Death rate",xlab="Health expenditure")
data_t = data_t[order(data_t[,18]),]
plot(sort(data_t[,18]),data_t[,12],type="o",xlab="Death rate",ylab="Health expenditure")

plotcorr(cor(data[,c(12,18)],use="pairwise.complete.obs"))

# 4 Прирост населения с высшим образованием на экспорт товаров
cor(data[,19],data[,17],use="pairwise.complete.obs")
cor(data[,19],data[,17],method="spearman",use="pairwise.complete.obs")
cor(data[,19],data[,17],method="kendall",use="pairwise.complete.obs")

cor.test(data[,19],data[,17],use="pairwise.complete.obs")
cor.test(data[,19],data[,17],method="spearman",use="pairwise.complete.obs")
cor.test(data[,19],data[,17],method="kendall",use="pairwise.complete.obs")

data_t = data[!is.na(data[,19]) & !is.na(data[,17]),]
data_t = data_t[order(data_t[,19]),]
par(mfrow=c(2,1))
plot(sort(data_t[,19]),data_t[,17],type="o",ylab="Export",xlab="Бакалавры")
data_t = data_t[order(data_t[,17]),]
plot(sort(data_t[,17]),data_t[,19],type="o",xlab="Export",ylab="Бакалавры")

plotcorr(cor(data[,c(19,17)],use="pairwise.complete.obs"))

# 4 Прирост населения с высшим образованием на прирост высокотехнологического производства
cor(data[,19],data[,22],use="pairwise.complete.obs")
cor(data[,19],data[,22],method="spearman",use="pairwise.complete.obs")
cor(data[,19],data[,22],method="kendall",use="pairwise.complete.obs")

cor.test(data[,19],data[,22],use="pairwise.complete.obs")
cor.test(data[,19],data[,22],method="spearman",use="pairwise.complete.obs")
cor.test(data[,19],data[,22],method="kendall",use="pairwise.complete.obs")

data_t = data[!is.na(data[,19]) & !is.na(data[,22]),]

data_t = data_t[order(data_t[,19]),]
par(mfrow=c(2,1))
plot(data_t[,19],data_t[,22],type="o",ylab="Hightech production",xlab="Бакалавры")
data_t = data_t[order(data_t[,22]),]
plot(data_t[,22],data_t[,19],type="o",xlab="Hightech production",ylab="Бакалавры")

plotcorr(cor(data[,c(19,22)],use="pairwise.complete.obs"))

# 5 Расходы на образование на кумулятивный прирост балаквров среди женщин
cor(data[,15],data[,20],use="pairwise.complete.obs")
cor(data[,15],data[,20],method="spearman",use="pairwise.complete.obs")
cor(data[,15],data[,20],method="kendall",use="pairwise.complete.obs")

cor.test(data[,15],data[,20],use="pairwise.complete.obs")
cor.test(data[,15],data[,20],method="spearman",use="pairwise.complete.obs")
cor.test(data[,15],data[,20],method="kendall",use="pairwise.complete.obs")

data_t = data[!is.na(data[,15]) & !is.na(data[,20]),]
data_t = data_t[order(data_t[,15]),]
par(mfrow=c(2,1))
plot(data_t[,15],data_t[,20],type="o",xlab="Education expenditure",ylab="Бакалавры среди женщин")
data_t = data_t[order(data_t[,20]),]
plot(data_t[,20],data_t[,15],type="o",ylab="Education expenditure",xlab="Бакалавры среди женщин")

plotcorr(cor(data[,c(15,20)],use="pairwise.complete.obs"))

# 6 Прирост бакалвров на количество научных статей
cor(data[,19],data[,22],use="pairwise.complete.obs")
cor(data[,19],data[,22],method="spearman",use="pairwise.complete.obs")
cor(data[,19],data[,22],method="kendall",use="pairwise.complete.obs")

cor.test(data[,19],data[,22],use="pairwise.complete.obs")
cor.test(data[,19],data[,22],method="spearman",use="pairwise.complete.obs")
cor.test(data[,19],data[,22],method="kendall",use="pairwise.complete.obs")

data_t = data[!is.na(data[,19]) & !is.na(data[,22]),]
data_t = data_t[order(data_t[,19]),]
par(mfrow=c(2,1))
plot(data_t[,19],data_t[,22],type="o",xlab="Прирост бакалавров",ylab="Количество научных статей")
data_t = data_t[order(data_t[,22]),]
plot(data_t[,22],data_t[,19],type="o",ylab="Прирост бакалавров",xlab="Количество научных статей")

plotcorr(cor(data[,c(19,22)],use="pairwise.complete.obs"))


# 7
library(car)
library(ellipse)

data_t = data[, colSums(is.na(data)) < nrow(data)]
data_t
colnames(data_t)
colnames(data_t)=1:length(data_t_col)
scatterplotMatrix(data_t,main="Матрица диаграмм рассеяния")
plotcorr(cor(data_t,use="pairwise.complete.obs"))

# 8 
data_t = data[, colSums(is.na(data)) < nrow(data)]

data_fit = as.data.frame(data_t[,c(1,7,10,13)])
colnames(data_fit)
colnames(data_fit)[1] = "gdp"
colnames(data_fit)[2] = "iofas"
colnames(data_fit)[3] = "lexp"
colnames(data_fit)[4] = "gim"

data_fit = na.omit(data_fit)


fit <- lm(formula=gdp~iofas+lexp+gim,data=as.data.frame(data_fit[1:28,]))
fit
summary(fit)
residuals(fit)
new_results = predict(fit,newdata=data_fit[29:30,])
new_results

par(mfrow=c(2,1))
style <- c(rep(1,28), rep(2,4))
plot(1989:2018,c(data_fit[1:28,1], new_results),
       ylab="GDP",
       xlab="Year", pch=style, col=style)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width
plot(1989:2018,data_fit$gdp,
     ylab="GDP",
     xlab="Year", pch=style, col=style)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width
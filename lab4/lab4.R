num<-c(1:12)
square<-num*num
plot(num,square,type="b")
help(plot)
#Выводим параметры графиков
opar<-par(no.readonly = TRUE)
#Задем новые параметры
par(pch=17,lty=2)
# pch - тип точек
# lty - тип линии
# lwd - ширина линии
#col - изменить цвет
# col.axis,col.lab - оси,col.main - заголовки,col.sub - подзаголовки
# fg - график, bg - фон
# rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n) и cm.colors(n).
n <- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels=mycolors, col=mycolors) # Круговая диаграмма
mygrays <- gray(0:n/n)
pie(rep(1, n), labels=mygrays, col=mygrays)

# cex - размер символов
# axis,lab,main,sub
windowsFonts(
  A=windowsFont('Arial Black'),
  B=windowsFont('Bookman Old Style'),
  C=windowsFont('Comic Sans MS')
)

#pin - размер диаграммы в дюймах
# mai - размеры полей в дюмах
# mar - размеры полей в строках

par(pin=c(2,3))
par(lwd=2,cex=1.5)
par(cex.axis=.75,font.axis=3)
plot(num,square,type='b',pch=19,lty=2,col='red')
plot(num, square, type="b", pch=23, lty=6, col="blue", bg="green")
par(opar)

month<-c("янв","февр","март" ,"апр" ,"май" ,"июнь" ,"июль","авг","сент","окт","ноя","дек")
month.f=month
month.o = ordered(month.f,levels=month)

plot(month.o, square, type="b", pch=23, lty=6, col="blue", bg="green",las=3)
plot(num, square, type="b", pch=23, lty=6, col="blue", bg="green", las=3)


plot(num, square, type="b",
     col="green", lty=2, pch=2,
     lwd=2,
     main="Квадратичная
зависимость",
     sub="Просто квадрат числа",
     xlab="Month", ylab="Квадрат
числа",
     xlim=c(0, 12), ylim=c(0, 300))


plot(num, square, type="b", ann=FALSE,
       col="green", lty=2, pch=2, lwd=2,
       xlim=c(0, 12), ylim=c(0, 300))
title(main="Квадратичная зависимость", col.main="red",
        sub="Просто квадрат числа", col.sub="blue",
        xlab="Month", ylab="Квадрат числа",
        col.lab="green", cex.lab=1)


#Можно создавать свои оси
#axis(side,at,labels,pos,lty,col,las,tck)
#side - с какой стороны диаграммы рисовать ось(1 низ, 2 лево, 3 верх, 4 право)
# at - положение делений на осях
# labels - подписи под делениями осей
# pos - точка пересечение осей
# tсk - длина деления оси


x = c(1:10)
y=x
z=10/x
opar=par(no.readonly = TRUE)
par(mar=c(5,4,4,8)+0.1)
plot(x,y,type='b',pch=21,col='red',yaxt='n',lty=3,ann=FALSE)
lines(x,z,type='b',pch=22,col='blue',lty=2)
axis(2,at=x,labels=x,col.axis='red',las=2)
axis(4,at=z,labels = round(z,digits=2),col.axis='blue',las=2,cex.axis=0.7,tck=0.01)
mtext('y=1/x',side=4,line=3,cex.lab=1,las=2,col='blue')
title('Приме креативных осей',xlab='Значение x',ylab='Y=X')
par(opar)

#
#text(location, "Текст аннотации", pos, ...) - внутри диаграммы
#mtext("Текст", side, line=n, ...) - снаружи диаграммы

#Добавление легенды
legend('topright',inset=.01,title='Два графика',c('Y=X','Y=1/X'),lty=c(1,2),pch=c(21,22),col=c('red','blue'))

#par(mfrow=c(x,y)) - создает рамку Графиков из x строк и y столлбцов

#layout(матрица шаблона со значениями,вектор значений ширины каждого столбца, вектор высоты каждой строки)
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE), widths=c(3, 1), heights=c(1, 2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
#Собрали датасет
years=c(1988,1996,2000,2004,2008,2012,2016,2020)
first_place_pers=c(1,0,1,1,1,1,1,0)
second_place_pers=c(0,1,0,1,0,1,1,1)
third_place_pers=c(1,0,1,0,0,0,0,0)
fourth_place_pers=c(0,1,0,0,1,0,0,1)
fifth_place_pers=c(0,0,0,0,0,0,0,0)
sixth_place_pers=c(0,0,0,0,0,0,0,0)
seventh_place_pers=c(0,0,0,0,0,0,0,0)
eight_place_pers=c(0,0,0,0,0,0,0,0)
data_pers = data.frame(years,first_place_pers,second_place_pers,third_place_pers,fourth_place_pers,fifth_place_pers,sixth_place_pers,seventh_place_pers,eight_place_pers)
colnames(data_pers)=c('years',c(1:8))
data_pers

places=c(0,3,1,1,1,1,1,2)
count_of_people=c(0,6,6,6,6,6,5,5)
data_group = data.frame(years,places,count_of_people)
data_group

# Динамика количества призовых мест
plot.new()
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width
par(new=TRUE)
plot(data_pers$years,apply(data_pers[c(2:4)],1,sum),pch=20,lty=1,lwd=3,type='l',col='blue',xlab='Года летних олимпиад',ylab='Количество призовых мест',main='Динамика количества призовых мест по годам',ylim=c(0,8))
lines(data_pers$years,data_group$count_of_people,lty=1,lwd=3,type='l',col='green')
legend('topright',title='Количество призовых мест',c('Многоборье','Групповые'),lty=c(1,1),col=c('blue','green'))


#Стобчатые диаграммы по местам
layout(matrix(c(1,2,3,4,5,6,7,8),nrow=2,ncol=4))
barplot(data_pers[,2],main='1-ое место многоборье',xlab='Год олимпиады',ylab='Количество мест',names=data_pers[,1])
barplot(data_pers[,3],main='2-ое место многоборье',xlab='Год олимпиады',ylab='Количество мест',names=data_pers[,1])
barplot(data_pers[,4],main='3-ое место многоборье',xlab='Год олимпиады',ylab='Количество мест',names=data_pers[,1])
barplot(data_pers[,5],main='4-ое место многоборье',xlab='Год олимпиады',ylab='Количество мест',names=data_pers[,1])
barplot(data_pers[,6],main='5-ое место многоборье',xlab='Год олимпиады',ylab='Количество мест',names=data_pers[,1])
barplot(data_pers[,7],main='6-ое место многоборье',xlab='Год олимпиады',ylab='Количество мест',names=data_pers[,1])
barplot(data_pers[,8],main='7-ое место многоборье',xlab='Год олимпиады',ylab='Количество мест',names=data_pers[,1])
barplot(data_pers[,9],main='8-ое место многоборье',xlab='Год олимпиады',ylab='Количество мест',names=data_pers[,1])

#Групповые
layout(matrix(c(1,2,3,4,5,6,7,8),nrow=2,ncol=4))
barplot(data_group[,3]*(data_group[,2]==1),main='1-ое место групповые',xlab='Год олимпиады',ylab='Количество мест',names=data_group[,1])
barplot(data_group[,3]*(data_group[,2]==2),main='2-ое место групповые',xlab='Год олимпиады',ylab='Количество мест',names=data_group[,1])
barplot(data_group[,3]*(data_group[,2]==3),main='3-ое место групповые',xlab='Год олимпиады',ylab='Количество мест',names=data_group[,1])
barplot(data_group[,3]*(data_group[,2]==4),main='4-ое место групповые',xlab='Год олимпиады',ylab='Количество мест',names=data_group[,1])
barplot(data_group[,3]*(data_group[,2]==5),main='5-ое место групповые',xlab='Год олимпиады',ylab='Количество мест',names=data_group[,1])
barplot(data_group[,3]*(data_group[,2]==6),main='6-ое место групповые',xlab='Год олимпиады',ylab='Количество мест',names=data_group[,1])
barplot(data_group[,3]*(data_group[,2]==7),main='7-ое место групповые',xlab='Год олимпиады',ylab='Количество мест',names=data_group[,1])
barplot(data_group[,3]*(data_group[,2]==8),main='8-ое место групповые',xlab='Год олимпиады',ylab='Количество мест',names=data_group[,1])

#Круговая
layout(matrix(c(1,2),nrow=2))
pie(data_pers[data_pers[,2]>0,2], labels=data_pers[data_pers[,2]>0,1], col=rainbow(8),main='Распределение 1-ых мест многоборье по годам')
pie(data_group[data_group[,2]==1,3], labels=data_group[data_group[,2]==1,1], col=rainbow(length(data_group[data_group[,2]==1,1])),main='Распределение 1-ых мест многоборье по годам')


#3
#Изменения по золотым медаляем по 7 странам призерам
countries = c('USA','China','Japan','England','Russia',
              'Australia','Netherlands')
count_gold_20=c(39,38,27,22,20,17,10)
count_gold_16=c(46,26,12,27,19,8,8)
count_gold_12=c(46,38,7,29,22,8,6)
count_gold_8=c(36,51,9,19,23,14,7)
count_gold_4=c(36,32,16,9,28,17,4)
count_gold_0=c(37,28,5,11,32,16,12)
gold_6_years = data.frame(countries,count_gold_20,count_gold_16,count_gold_12,count_gold_8,count_gold_4,count_gold_0)
colnames(gold_6_years)=c('country',c(2020,2016,2012,2008,2004,2000))
gold_6_years


plot.new()
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width
par(new=TRUE)
plot(colnames(gold_6_years),gold_6_years[1,],pch=20,lty=1,type='p',col='blue',ylim=c(0,60),xlab='Года олимпиад',ylab='Количество золотых медалей',main='Динамика количества золотых медалей 7 стран-призеров олимпиады 2022')
lines(colnames(gold_6_years),gold_6_years[1,],pch=20,lty=1,type='l',col='blue')
lines(colnames(gold_6_years),gold_6_years[2,],pch=20,lty=1,type='p',col='green')
lines(colnames(gold_6_years),gold_6_years[2,],pch=20,lty=1,type='l',col='green')
lines(colnames(gold_6_years),gold_6_years[3,],pch=20,lty=1,type='p',col='red')
lines(colnames(gold_6_years),gold_6_years[3,],pch=20,lty=1,type='l',col='red')
lines(colnames(gold_6_years),gold_6_years[4,],pch=20,lty=1,type='p',col='yellow')
lines(colnames(gold_6_years),gold_6_years[4,],pch=20,lty=1,type='l',col='yellow')
lines(colnames(gold_6_years),gold_6_years[5,],pch=20,lty=1,type='p',col='purple')
lines(colnames(gold_6_years),gold_6_years[5,],pch=20,lty=1,type='l',col='purple')
lines(colnames(gold_6_years),gold_6_years[6,],pch=20,lty=1,type='p',col='black')
lines(colnames(gold_6_years),gold_6_years[6,],pch=20,lty=1,type='l',col='black')
lines(colnames(gold_6_years),gold_6_years[7,],pch=20,lty=1,type='p',col='orange')
lines(colnames(gold_6_years),gold_6_years[7,],pch=20,lty=1,type='l',col='orange')
legend('topleft',inset=.001,cex=0.7,title='Количество золотых медалей',c('США','Китай','Япония','Англия','Россия','Австралия','Нидерланды'),lty=c(1,1,1,1,1,1,1),pch=c(20,20,20,20,20,20,20),col=c('blue','green','red','yellow','purple','black','orange'))

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3))
barplot(gold_6_years[,2],main='Распределение в 2020',sub='Страны',ylab='Количество золотых медалей',names=gold_6_years[,1],las=3)
barplot(gold_6_years[,3],main='Распределение в  2016',sub='Страны',ylab='Количество золотых медалей',names=gold_6_years[,1],las=3)
barplot(gold_6_years[,4],main='Распределение в 2012',sub='Страны',ylab='Количество золотых медалей',names=gold_6_years[,1],las=3)
barplot(gold_6_years[,5],main='Распределение в 2008',sub='Страны',ylab='Количество золотых медалей',names=gold_6_years[,1],las=3)
barplot(gold_6_years[,6],main='Распределение в 2004',sub='Страны',ylab='Количество золотых медалей',names=gold_6_years[,1],las=3)
barplot(gold_6_years[,7],main='Распределение в 2000',sub='Страны',ylab='Количество золотых медалей',names=gold_6_years[,1],las=3)

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3))
pie(gold_6_years[,2], labels=gold_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2020')
pie(gold_6_years[,3], labels=gold_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2016')
pie(gold_6_years[,4], labels=gold_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2012')
pie(gold_6_years[,5], labels=gold_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2008')
pie(gold_6_years[,6], labels=gold_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2004')
pie(gold_6_years[,7], labels=gold_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2000')

#Изменения по призовым местам медаляем по 7 странам призерам
countries = c('USA','China','Japan','England','Russia',
              'Australia','Netherlands')
count_prize_20=c(113,88,58,65,71,46,36)
count_prize_16=c(121,70,40,67,56,29,19)
count_prize_12=c(103,89,38,65,79,35,20)
count_prize_8=c(110,100,25,47,73,46,16)
count_prize_4=c(101,63,37,30,90,50,22)
count_prize_0=c(93,58,18,28,89,58,25)
prize_6_years = data.frame(countries,count_prize_20,count_prize_16,count_prize_12,count_prize_8,count_prize_4,count_prize_0)
colnames(prize_6_years)=c('country',c(2020,2016,2012,2008,2004,2000))
prize_6_years

plot.new()
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width
par(new=TRUE)
plot(colnames(prize_6_years),prize_6_years[1,],pch=20,lty=1,type='p',col='blue',ylim=c(0,140),xlab='Года олимпиад',ylab='Количество призовых мест',main='Динамика количества призовых мест 7 стран-призеров олимпиады 2022')
lines(colnames(prize_6_years),prize_6_years[1,],pch=20,lty=1,type='l',col='blue')
lines(colnames(prize_6_years),prize_6_years[2,],pch=20,lty=1,type='p',col='green')
lines(colnames(prize_6_years),prize_6_years[2,],pch=20,lty=1,type='l',col='green')
lines(colnames(prize_6_years),prize_6_years[3,],pch=20,lty=1,type='p',col='red')
lines(colnames(prize_6_years),prize_6_years[3,],pch=20,lty=1,type='l',col='red')
lines(colnames(prize_6_years),prize_6_years[4,],pch=20,lty=1,type='p',col='yellow')
lines(colnames(prize_6_years),prize_6_years[4,],pch=20,lty=1,type='l',col='yellow')
lines(colnames(prize_6_years),prize_6_years[5,],pch=20,lty=1,type='p',col='purple')
lines(colnames(prize_6_years),prize_6_years[5,],pch=20,lty=1,type='l',col='purple')
lines(colnames(prize_6_years),prize_6_years[6,],pch=20,lty=1,type='p',col='black')
lines(colnames(prize_6_years),prize_6_years[6,],pch=20,lty=1,type='l',col='black')
lines(colnames(prize_6_years),prize_6_years[7,],pch=20,lty=1,type='p',col='orange')
lines(colnames(prize_6_years),prize_6_years[7,],pch=20,lty=1,type='l',col='orange')
legend('topleft',inset=.001,cex=0.7,title='Количество призовых мест',c('США','Китай','Япония','Англия','Россия','Австралия','Нидерланды'),lty=c(1,1,1,1,1,1,1),pch=c(20,20,20,20,20,20,20),col=c('blue','green','red','yellow','purple','black','orange'))

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3))
barplot(prize_6_years[,2],main='Распределение в 2020',sub='Страны',ylab='Количество золотых медалей',names=prize_6_years[,1],las=3)
barplot(prize_6_years[,3],main='Распределение в  2016',sub='Страны',ylab='Количество золотых медалей',names=prize_6_years[,1],las=3)
barplot(prize_6_years[,4],main='Распределение в 2012',sub='Страны',ylab='Количество золотых медалей',names=prize_6_years[,1],las=3)
barplot(prize_6_years[,5],main='Распределение в 2008',sub='Страны',ylab='Количество золотых медалей',names=prize_6_years[,1],las=3)
barplot(prize_6_years[,6],main='Распределение в 2004',sub='Страны',ylab='Количество золотых медалей',names=prize_6_years[,1],las=3)
barplot(prize_6_years[,7],main='Распределение в 2000',sub='Страны',ylab='Количество золотых медалей',names=prize_6_years[,1],las=3)

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3))
pie(prize_6_years[,2], labels=prize_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2020')
pie(prize_6_years[,3], labels=prize_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2016')
pie(prize_6_years[,4], labels=prize_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2012')
pie(prize_6_years[,5], labels=prize_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2008')
pie(prize_6_years[,6], labels=prize_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2004')
pie(prize_6_years[,7], labels=prize_6_years[,1], col=rainbow(7),main='Распределение 1-ых мест в 2000')
#4
countries = c('Russia','Belarus','Ukraine','Izrael','CIS')
places_2000_ind=c(2,1,0,0,0)
places_2004_ind=c(2,0,1,0,0)
places_2008_ind=c(1,1,1,0,0)
places_2012_ind=c(2,1,0,0,0)
places_2016_ind=c(2,0,1,0,0)
places_2020_ind=c(0,1,0,1,1)

prize_places_ind = data.frame(countries,places_2000_ind,places_2004_ind,
                          places_2008_ind,places_2012_ind,places_2016_ind,
                          places_2020_ind)
colnames(prize_places_ind)=c('countries',2000,2004,2008,2012,2016,2020)
prize_places_ind


countries_group=c('Russia','Greece','Italy','Bulgaria','PRC','Belarus','Spain','CIS')
places_2000_group=c(1,1,0,1,0,0,0,0)
places_2004_group=c(1,0,1,1,0,0,0,0)
places_2008_group=c(1,0,0,1,1,0,0,0)
places_2012_group=c(1,0,1,1,0,0,0,0)
places_2016_group=c(1,0,0,1,0,0,1,0)
places_2020_group=c(0,0,1,1,0,0,0,1)
prize_places_group = data.frame(countries_group,places_2000_group,places_2004_group,
                              places_2008_group,places_2012_group,places_2016_group,
                              places_2020_group)
colnames(prize_places_group)=c('countries',2000,2004,2008,2012,2016,2020)
prize_places_group

#Групповые
plot.new()
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width
par(new=TRUE)
plot(colnames(prize_places_group)[c(-1)],prize_places_group[1,-1],pch=20,lty=1,type='p',col='blue',ylim=c(0,5),xlab='Года олимпиад',ylab='Количество призовых мест',main='Динамика количества призовых мест 7 стран-призеров олимпиады 2022')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[1,-1],pch=20,lty=1,type='l',col='blue')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[2,-1],pch=20,lty=1,type='p',col='green')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[2,-1],pch=20,lty=1,type='l',col='green')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[3,-1],pch=20,lty=1,type='p',col='red')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[3,-1],pch=20,lty=1,type='l',col='red')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[4,-1],pch=20,lty=1,type='p',col='yellow')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[4,-1],pch=20,lty=1,type='l',col='yellow')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[5,-1],pch=20,lty=1,type='p',col='purple')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[5,-1],pch=20,lty=1,type='l',col='purple')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[6,-1],pch=20,lty=1,type='p',col='black')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[6,-1],pch=20,lty=1,type='l',col='black')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[7,-1],pch=20,lty=1,type='p',col='orange')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[7,-1],pch=20,lty=1,type='l',col='orange')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[7,-1],pch=20,lty=1,type='p',col='gray')
lines(colnames(prize_places_group)[c(-1)],prize_places_group[7,-1],pch=20,lty=1,type='l',col='gray')
legend('topleft',inset=.001,cex=0.7,title='Количество призовых мест',countries_group,lty=c(1,1,1,1,1,1,1,1),pch=c(20,20,20,20,20,20,20,20),col=c('blue','green','red','yellow','purple','black','orange','gray'))

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3))
barplot(prize_places_group[,2],main='Олимпиада 2000',xlab='Страны',ylab='Количество мест',names=prize_places_group[,1],cex.names=0.8,las=3)
barplot(prize_places_group[,3],main='Олимпиада 2004',xlab='Страны',ylab='Количество мест',names=prize_places_group[,1],cex.names=0.8,las=3)
barplot(prize_places_group[,4],main='Олимпиада 2008',xlab='Страны',ylab='Количество мест',names=prize_places_group[,1],cex.names=0.8,las=3)
barplot(prize_places_group[,5],main='Олимпиада 2012',xlab='Страны',ylab='Количество мест',names=prize_places_group[,1],cex.names=0.8,las=3)
barplot(prize_places_group[,6],main='Олимпиада 2016',xlab='Страны',ylab='Количество мест',names=prize_places_group[,1],cex.names=0.8,las=3)
barplot(prize_places_group[,7],main='Олимпиада 2020',xlab='Страны',ylab='Количество мест',names=prize_places_group[,1],cex.names=0.8,las=3)

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3))
pie(prize_places_group[prize_places_group[,2]>0,2], labels=prize_places_group[prize_places_group[,2]>0,1], col=rainbow(length(prize_places_group[prize_places_group[,2]>0,1])),main='Распределение призовых мест в 2000')
pie(prize_places_group[prize_places_group[,3]>0,3], labels=prize_places_group[prize_places_group[,3]>0,1], col=rainbow(length(prize_places_group[prize_places_group[,3]>0,1])),main='Распределение призовых мест в 2004')
pie(prize_places_group[prize_places_group[,4]>0,4], labels=prize_places_group[prize_places_group[,4]>0,1], col=rainbow(length(prize_places_group[prize_places_group[,4]>0,1])),main='Распределение призовых мест в 2008')
pie(prize_places_group[prize_places_group[,5]>0,5], labels=prize_places_group[prize_places_group[,5]>0,1], col=rainbow(length(prize_places_group[prize_places_group[,5]>0,1])),main='Распределение призовых мест в 2012')
pie(prize_places_group[prize_places_group[,6]>0,6], labels=prize_places_group[prize_places_group[,6]>0,1], col=rainbow(length(prize_places_group[prize_places_group[,6]>0,1])),main='Распределение призовых мест в 2016')
pie(prize_places_group[prize_places_group[,7]>0,7], labels=prize_places_group[prize_places_group[,7]>0,1], col=rainbow(length(prize_places_group[prize_places_group[,7]>0,1])),main='Распределение призовых мест в 2020')

#Многоборье

plot.new()
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width
par(new=TRUE)
plot(colnames(prize_places_ind)[c(-1)],prize_places_ind[1,-1],pch=20,lty=1,type='p',col='blue',ylim=c(0,5),xlab='Года олимпиад',ylab='Количество призовых мест',main='Динамика количества призовых мест 7 стран-призеров олимпиады 2022')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[1,-1],pch=20,lty=1,type='l',col='blue')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[2,-1],pch=20,lty=1,type='p',col='green')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[2,-1],pch=20,lty=1,type='l',col='green')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[3,-1],pch=20,lty=1,type='p',col='red')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[3,-1],pch=20,lty=1,type='l',col='red')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[4,-1],pch=20,lty=1,type='p',col='yellow')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[4,-1],pch=20,lty=1,type='l',col='yellow')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[5,-1],pch=20,lty=1,type='p',col='purple')
lines(colnames(prize_places_ind)[c(-1)],prize_places_ind[5,-1],pch=20,lty=1,type='l',col='purple')
legend('topleft',inset=.001,cex=0.7,title='Количество призовых мест',countries,lty=c(1,1,1,1,1),pch=c(20,20,20,20,20),col=c('blue','green','red','yellow','purple'))

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3))
barplot(prize_places_ind[,2],main='Олимпиада 2000',xlab='Страны',ylab='Количество мест',names=prize_places_ind[,1],cex.names=0.8)
barplot(prize_places_ind[,3],main='Олимпиада 2004',xlab='Страны',ylab='Количество мест',names=prize_places_ind[,1],cex.names=0.8)
barplot(prize_places_ind[,4],main='Олимпиада 2008',xlab='Страны',ylab='Количество мест',names=prize_places_ind[,1],cex.names=0.8)
barplot(prize_places_ind[,5],main='Олимпиада 2012',xlab='Страны',ylab='Количество мест',names=prize_places_ind[,1],cex.names=0.8)
barplot(prize_places_ind[,6],main='Олимпиада 2016',xlab='Страны',ylab='Количество мест',names=prize_places_ind[,1],cex.names=0.8)
barplot(prize_places_ind[,7],main='Олимпиада 2020',xlab='Страны',ylab='Количество мест',names=prize_places_ind[,1],cex.names=0.8)

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3))
pie(prize_places_ind[prize_places_ind[,2]>0,2], labels=prize_places_ind[prize_places_ind[,2]>0,1], col=rainbow(length(prize_places_ind[prize_places_ind[,2]>0,1])),main='Распределение 1-ых в 2000')
pie(prize_places_ind[prize_places_ind[,3]>0,3], labels=prize_places_ind[prize_places_ind[,3]>0,1], col=rainbow(length(prize_places_ind[prize_places_ind[,3]>0,1])),main='Распределение 1-ых в 2004')
pie(prize_places_ind[prize_places_ind[,4]>0,4], labels=prize_places_ind[prize_places_ind[,4]>0,1], col=rainbow(length(prize_places_ind[prize_places_ind[,4]>0,1])),main='Распределение 1-ых в 2008')
pie(prize_places_ind[prize_places_ind[,5]>0,5], labels=prize_places_ind[prize_places_ind[,5]>0,1], col=rainbow(length(prize_places_ind[prize_places_ind[,5]>0,1])),main='Распределение 1-ых в 2012')
pie(prize_places_ind[prize_places_ind[,6]>0,6], labels=prize_places_ind[prize_places_ind[,6]>0,1], col=rainbow(length(prize_places_ind[prize_places_ind[,6]>0,1])),main='Распределение 1-ых в 2016')
pie(prize_places_ind[prize_places_ind[,7]>0,7], labels=prize_places_ind[prize_places_ind[,7]>0,1], col=rainbow(length(prize_places_ind[prize_places_ind[,7]>0,1])),main='Распределение 1-ых в 2020')

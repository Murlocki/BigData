#Добавим к набору данных найденные кластеры
#Для 2 кластеров
rm(list=ls())
data_2_new = data.frame(cbind(data_sc_norm[,-14],factor(km2$cluster),data_sc_norm[,14]))
colnames(data_2_new)[c(14,15)]=c("groups","output")
data_2_new$groups
data_2_new$groups = scale(data_2_new$groups,center=min(data_2_new$groups),scale=max(data_2_new$groups)-min(data_2_new$groups))
data_2_new[,14]=factor(data_2_new[,14]/(max(data_2_new[,14])-min(data_2_new[,14])))
data_2_new[,15]=factor(data_2_new[,15])
data_2_new[,14]

#Для 5 кластеров
data_5_new = data.frame(cbind(data_sc_norm[,-14],factor(km5$cluster),data_sc_norm[,14]))
colnames(data_5_new)[c(14,15)]=c("groups","output")
data_5_new$groups
data_5_new$groups = scale(data_5_new$groups,center=min(data_5_new$groups),scale=max(data_5_new$groups)-min(data_5_new$groups))
data_5_new[,14]=factor(data_5_new[,14]/(max(data_5_new[,14])-min(data_5_new[,14])))
data_5_new[,15]=factor(data_5_new[,15])
data_5_new[,14]
data_5_new

#Формируем тренировочные и тестовые данные
set.seed(1234)
ind = sample(2,nrow(data_2_new),replace=TRUE,prob=c(0.7,0.3))
#Для 2 кластеров
trainData2=data.frame(data_2_new[ind==1,])
testData2=data_2_new[ind==2,]
nrow(testData2)
nrow(trainData2)
nrow(data_2_new)
#Для 5 кластеров
trainData5=data_5_new[ind==1,]
testData5=data_5_new[ind==2,]
nrow(testData5)
nrow(trainData5)
nrow(data_5_new)

#Наивный байес
#для 2
library(klaR)
naive_heart_2 <- NaiveBayes(trainData2$output ~ ., data = trainData2)
naive_heart_2$tables
summary(trainData2)
opar=par()
layout(matrix(1:16, 4, 4, byrow = TRUE))
plot(naive_heart_2,lwd = 2)
legend("topleft",legend=c("yes","no"),lty=1:2,cex=0.5)
data
summary(data)
pred <- predict(naive_heart_2, testData2[,-15])$class
(table(Факт = testData2$output, Прогноз = pred))

naive_heart_2 <- NaiveBayes(trainData2$output ~ ., data = trainData2[,-14])
naive_heart_2$tables
opar=par()
layout(matrix(1:16, 4, 4, byrow = TRUE))
plot(naive_heart_2,lwd = 2)
legend("topleft",legend=c("yes","no"),lty=1:2,cex=0.5)
pred <- predict(naive_heart_2, testData2[,c(-14,-15)])$class
(table(Факт = testData2$output, Прогноз = pred))


#для 5
naive_heart_5 <- NaiveBayes(trainData5$output ~ ., data = trainData5)
naive_heart_5$tables
summary(trainData5)
opar=par()
layout(matrix(1:16, 4, 4, byrow = TRUE))
plot(naive_heart_5,lwd = 2)
legend("topleft",legend=c("yes","no"),lty=1:2,cex=0.5)
data
summary(data)
pred <- predict(naive_heart_5, testData5[,-15])$class
(table(Факт = testData5$output, Прогноз = pred))


naive_heart_5 <- NaiveBayes(trainData5$output ~ ., data = trainData5[,-14])
naive_heart_5$tables
opar=par()
layout(matrix(1:16, 4, 4, byrow = TRUE))
plot(naive_heart_5,lwd = 2)
legend("topleft",legend=c("yes","no"),lty=1:2,cex=0.5)
pred <- predict(naive_heart_5, testData5[,c(-14,-15)])$class
(table(Факт = testData5$output, Прогноз = pred))

#Решающие деревья
install.packages("party")
library(party)
heart_2_tree=ctree(trainData2$output~.,data=trainData2)
heart_2_tree
pred <- predict(heart_2_tree, testData2[,-15])
table(pred, testData2[,15])
plot(heart_2_tree,text = "vertical", ymax = 1)

#Аналогично но без групп
heart_2_tree=ctree(trainData2$output~.,data=trainData2[,-14])
heart_2_tree
pred <- predict(heart_2_tree, testData2[,c(-14,-15)])
table(pred, testData2[,15])
plot(heart_2_tree,text = "vertical", ymax = 1)

#Деревья для 5
library(party)
heart_5_tree=ctree(trainData5$output~.,data=trainData5)
heart_5_tree
pred <- predict(heart_5_tree, testData5[,-15])
table(pred, testData5[,15])
plot(heart_5_tree,text = "vertical", ymax = 1)

#Аналогично но без групп
heart_5_tree=ctree(trainData5$output~.,data=trainData5[,-14])
heart_5_tree
pred <- predict(heart_5_tree, testData5[,c(-14,-15)])
table(pred, testData5[,15])
plot(heart_5_tree,text = "vertical", ymax = 1)


#Случайный лес
library(randomForest)
#Для 2
rf=randomForest(trainData2$output ~ .,data=trainData2, ntree=100, proximity=TRUE)
table(predict(rf,testData2[,-15]), testData2$output)
rf=randomForest(trainData2$output ~ .,data=trainData2[,-14], ntree=100, proximity=TRUE)
table(predict(rf,testData2[,c(-14,-15)]), testData2$output)
      
#Для 5
rf=randomForest(trainData5$output ~ .,data=trainData5, ntree=100, proximity=TRUE)
table(predict(rf,testData5[,-15]), testData5$output)
rf=randomForest(trainData5$output ~ .,data=trainData5[,-14], ntree=100, proximity=TRUE)
table(predict(rf,testData5[,c(-14,-15)]), testData5$output)

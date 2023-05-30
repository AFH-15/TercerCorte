library(tidyverse)
library(dplyr)
Prediccion <- read.csv("C:/Users/USER/Desktop/machine 3corte/Prediccion.csv", sep=";")
DAQEntrenamiento <- read.csv("C:/Users/USER/Desktop/machine 3corte/DAQEntrenamiento.csv", sep=";")

plot(DAQKNN[2])
plot(DAQKNN[3:6])
hist(DAQKNN$ultrasonido,breaks=15)

hist(DAQKNN$rojo,breaks=50)
hist(DAQKNN$azul,breaks=50)
hist(DAQKNN$verde,breaks=50)
library(psych)
pairs.panels(DAQKNN[2:5], pch=21, main=("habitat 1 =rojo, convexo=verde, plano=azul")
             , bg=c("red","green2","blue")[unclass(DAQKNN$habitad)])##si se distingen los grupos es util para machine

prop.table((table(DAQKNN$ultrasonido))) ##

prop.table((table(DAQKNN$rojo)))
prop.table((table(DAQKNN$azul)))
prop.table((table(DAQKNN$verde)))##virificar si el dataset es util si el numero es pequeño esta balanceado


##normalizacion z score
standarData$ultrasonido<-scale(normData$ultrasonido)
standarData$rojo<-scale(normData$rojo)
standarData$azul<-scale(normData$azul)
standarData$verde<-scale(normData$verde)

colnames(standarData)[1:4]<-c("ultrasonido[,1]","rojo[,1]","azul[,1]","verde[,1]")
sample.index<-sample(3:nrow(DAQKNN)
                     , nrow(DAQKNN)*0.7
                     , replace=FALSE)
##------------------------------------------
## entrenamiento
k<-5

predictors<-c("ultrasonido","rojo",
              "azul","verde")
train.data<-DAQKNN[sample.index##70%
                 ,c(predictors,"habitat")
                 , drop=F]
test.data<-DAQKNN[-sample.index##30%
                ,c(predictors,"habitat")
                , drop=F]
library(class)
predictors<- knn(train=train.data[predictors]## entrenamiento variables 70% train.data
                  ,test=test.data[predictors]## prueba variables 30% test data
                  ,cl=train.data$habitat ##variable clase
                  ,k=k)

## verificar el rendimiento
library(gmodels)
CrossTable(x=test.data$habitat,y=predictors)


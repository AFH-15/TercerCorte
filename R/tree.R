                                                                #Se implementa para preguntas que tengan varios niveres#Rojo verde amarillo azul, no binario (No es bueno que sea Binario)
library(rpart)
DAQEntrenamiento <- read.csv("C:/Users/USER/Desktop/machine 3corte/DAQEntrenamiento.csv", sep=";")

head(DAQEntrenamiento)

fit <-
  rpart(habitat ~ ultrasonido + rojo + azul+verde,
        method = "class", #class para clasificar anova regresion numericos
        data= DAQEntrenamiento)

#Impresión el arbol de desición
plot(fit,uniform = T,margin = 0.1)
text(fit, use.n=T, all=T, cex=.8)
rpart.plot::rpart.plot(fit, shadow.col = "gray")

#Para poder podar el arbol
fit.pruned <-
  prune(fit, cp=fit$cptable[which.min(fit$cptable[,
                                                  "xerror"]),"CP"])

par(mfrow= c(1, 2))

plot(fit, uniform=T, margin = 0.1, main= "Original Tree")
text(fit, use.n=T, all=T, cex=0.8)

plot(fit.pruned, uniform=T, margin= 0.1, main= "Pruned Tree")
text(fit.pruned, use.n=T, all=T, cex=0.8)

rpart.plot::rpart.plot(fit)
rpart.plot::rpart.plot(fit.pruned)

DAQReal <- read.csv("C:/Users/USER/Desktop/machine 3corte/DAQReal.csv", sep=";")

predictors<-predict(fit, DAQReal, type="prob")
predict(fit, DAQReal, type="prob")
##ERROR 
RMSE.df<-data.frame(predicted=predictors ##error cuadratico medio
                    ,actual=DAQReal$verde+DAQReal$ultrasonido
                    ,RMSE=sqrt(predictors-DAQReal$ultrasonido-DAQReal$verde)^2)


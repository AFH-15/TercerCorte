DAQEntrenamiento <- read.csv("C:/Users/USER/Desktop/machine 3corte/DAQEntrenamiento.csv", sep=";")
DAQReal <- read.csv("C:/Users/USER/Desktop/machine 3corte/DAQReal.csv", sep=";")
library(caret)
fit.control <- trainControl(method = "repeatedcv"    ##x-validation repetido 5 four xvalitation  10 veces o cv 
                            , number = 5, repeats = 10)

fit <- train(habitat ~ ultrasonido + rojo + azul+verde,data = DAQEntrenamiento
             , method = "multinom"
             , trControl = fit.control
             , trace = FALSE)
fit
New<-data.frame(DAQReal)
predict(fit, DAQReal, type="prob")



library(dplyr)
library(tidyr)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(cluster)#Para calcular la silueta
library(e1071)#para cmeans
library(cluster)#Para calcular la silueta
library(mclust) #mixtures of gaussians
library(fpc)#para hacer el plotcluster
library(NbClust)#Para determinar el nÃºmero de clusters Ã³ptimo
library(factoextra)#Para hacer grÃ¡ficos bonitos de clustering
library(e1071)
library(caret)
library(corrplot) 
library(randomcoloR) #genera colores random para graficas
library(readxl)
library(tidyverse)
library(ggplot)
library(haven)
library(foreign)
path_andrea <- "C:/Users/andre/OneDrive/Desktop/Proyecto-MineriaDeDatos/finaldataset.csv"

path_diego <- "C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/finaldataset.csv"
data <-read.csv(path_diego)

#Limpieza de datos
matrifinal <- filter(data, data$EDADHOM < 115, data$EDADMUJ<115)
view(matrifinal)

dataFinal <- matrifinal[,c("EDADHOM","EDADMUJ","DEPREG","MUPREG","PUEMUJ","MESREG","AÑOREG","PUEHOM","ESCHOM","ESCMUJ","CIUOHOM", "CIUOMUJ","DIAOCU")]

#Agregamos columna con diferencia de edades, se aplica valor absoluto para evitar valores negativos


#SVM

train$grupo <- as.factor(train$grupo)
test$grupo <- as.factor(test$grupo)

#Modelo Lineal
modeloSVM_L<-svm(grupo~., data=train, cost=c(0.001,0.01,0.1, 1,5,10,32,100), kernel="linear")
summary(modeloSVM_L)
prediccionL<-predict(modeloSVM_L,newdata=test[,1:13], type="class")
confusionMatrix(test$grupo,prediccionL)

#Modelo Polinomial 
modeloSVM_P<-svm(grupo~., data=train, degree=c(3,4,5), coef0=c(0.1,0.5,1,2,3,4), kernel="polynomial")
prediccionL<-predict(modeloSVM_P,newdata=test[,1:14], type="class")
confusionMatrix(test$grupo,prediccionL)

#Modelo Radial
modeloSVM_R<-svm(grupo~., data=train, gamma=c(0.1,0.5,1,2,3,4), kernel="radial")
prediccionL<-predict(modeloSVM_R,newdata=test[,1:14], type="class")
confusionMatrix(test$grupo,prediccionL)

#Modelo Sigmoidal
modeloTuneado<-svm(grupo~., data=train, gamma=c(0.1,0.5,1,2,3,4), coef0=c(0.1,0.5,1,2,3,4), kernel="sigmoid")
prediccionL<-predict(modeloTuneado,newdata=test[,1:14], type="class")
confusionMatrix(test$grupo,prediccionL)

#Modelo Polinomial 
modelo<-tune.svm(grupo~., data=train, degree=c(3,4,5), coef0=c(0.1,0.5,1,2,3,4), kernel="polynomial")
predMejorModelo<-predict(modelo$best.model,newdata = test[,1:14], type="class")
confusionMatrix(factor(test$grupo),factor(predMejorModelo))
dataFinal$edad_dif = abs(dataFinal$EDADHOM - dataFinal$EDADMUJ)
#Promedio de diferencia de edad entre novio y novia
mean(dataFinal$edad_dif)

#Etiqueta
dataFinal$grupo <- ifelse(dataFinal$edad_dif<10, "1", 
                          ifelse(dataFinal$edad_dif<20, "2",
                                        ifelse(dataFinal$edad_dif<90, "3", NA)))

# Se cambia la variable grupo a tipo factor
dataFinal$grupo <- as.factor(dataFinal$grupo)


#Separación aleatoria, ratio 70:30
dt = sort(sample(nrow(dataFinal), nrow(dataFinal)*.7))
train<-dataFinal[dt,]
test<-dataFinal[-dt,]

head(train, 10)

#-------------------------------------------------
# MODELO REDES NEURONALES
#-------------------------------------------------

nnet_test <- select(test, PUEMUJ,PUEHOM,ESCHOM,ESCMUJ,CIUOHOM, CIUOMUJ,DIAOCU,edad_dif,grupo )
nnet_t <- select(train, PUEMUJ,PUEHOM,ESCHOM,ESCMUJ,CIUOHOM, CIUOMUJ,DIAOCU,edad_dif,grupo)
nnet_t$grupo <- factor(nnet_t$grupo)
nnet_t <- SMOTE(grupo~.,nnet_t,perc.over = 100, perc.under = 500, k=5) # balanceo del dataset  

m_nnet <- nnet(grupo~.,data = nnet_t, size=5, rang=0.00001,
               decay=5e-2, maxit=400) # generaci?n de modelo RNA
prediccion_nnet <- as.data.frame(predict(m_nnet, newdata = nnet_test[,1:9]))
columnaMasAlta<-apply(prediccion_nnet, 1, function(x) colnames(prediccion_nnet)[which.max(x)])
nnet_test$grupo_nnet<-columnaMasAlta #Se le a?ade al grupo de prueba el valor de la predicci?n

cfm_nnet <- confusionMatrix(table((factor(nnet_test$grupo, levels=min(nnet_test$grupo):max(nnet_test$grupo))),
                                  (factor(nnet_test$grupo_nnet, levels=min(nnet_test$grupo):max(nnet_test$grupo)))))

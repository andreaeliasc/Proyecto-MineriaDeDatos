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
                      
                      

                      
                      
 #Arbol de Decision

datosFiltertree <- dataFinal[,c("EDADHOM","EDADMUJ","DEPREG","MUPREG","PUEMUJ","MESREG","AÑOREG","PUEHOM","ESCHOM","ESCMUJ","CIUOHOM", "CIUOMUJ","DIAOCU","edad_dif","grupo")]

datosFiltertree
porciento <- 70/100

porciento <- 70/100


set.seed(321)
trainRowsNumber<-sample(1:nrow(datosFiltertree),porciento*nrow(datosFiltertree))
train<-datosFiltertree[trainRowsNumber,]
test<-datosFiltertree[-trainRowsNumber,]

dt_model<-rpart(train$grupo~PUEHOM+PUEMUJ+ESCMUJ+ESCHOM,train,method = "class",control =rpart.control(minsplit =1,minbucket=1, cp=0))
rpart.plot(dt_model, box.palette = "Blues")

prediccion <- predict(dt_model, newdata = test[1:15])
columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta #Se le a?ade al grupo de prueba el valor de la predicci?n
cfm<-confusionMatrix(as.factor(test$prediccion),as.factor(test$grupo))
cfm



#Anexos

#frecuencia de edad
freqEdad<-table(dataFinal$edad_dif)
view(freqEdad)
barplot(head(freqEdad,15))
head(freqEdad,15)
freqEdad <- sort(freqEdad,                   # Vector atómico
                       decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
                       na.last = TRUE,) 
freqEdad<-(head(freqEdad,15))
barplot(freqEdad,15, col = distinctColorPalette(18), main = "Top 15 diferencias de edad", xlab = "diferencia de edad", ylab="frecuencia")



#Escolaridades mas comunes en diferencias de edades mayores

hey <- table(dataFinal$grupo, dataFinal$ESCMUJ)
hey<- filter(dataFinal, dataFinal$grupo>=3)
holi <- table(hey$ESCMUJ)

barplot(holi, col = distinctColorPalette(18), main = "Escolaridad Mujeres", xlab = "Escolaridad", ylab="frecuencia")#,  names.arg =c("Ninguno","Primaria","Basicos","Diversificado","Universitario","Ignorado"))


hom <- table(dataFinal$grupo, dataFinal$ESCHOM)
hom<- filter(dataFinal, dataFinal$grupo>=3)
homesc <- table(hom$ESCHOM)

barplot(homesc, col = distinctColorPalette(18), main = "Escolaridad Hombres", xlab = "Escolaridad", ylab="frecuencia")#, names.arg =c("Ninguno","Primaria","Basicos","Diversificado","Universitario","Posgrado","Ignorado"))


#diferencia de edades medias

muj <- table(dataFinal$grupo, dataFinal$ESCMUJ)
muj<- filter(dataFinal, dataFinal$grupo==2)
mujer <- table(muj$ESCMUJ)

barplot(mujer, col = distinctColorPalette(18), main = "Escolaridad Mujeres", xlab = "Escolaridad", ylab="frecuencia",  names.arg =c("Ninguno","Primaria","Basicos","Diversificado","Universitario","Posgrado","Ignorado"))


hom <- table(dataFinal$grupo, dataFinal$ESCHOM)
hom<- filter(dataFinal, dataFinal$grupo==2)
homesc <- table(hom$ESCHOM)

barplot(homesc, col = distinctColorPalette(18), main = "Escolaridad Hombres", xlab = "Escolaridad", ylab="frecuencia", names.arg =c("Ninguno","Primaria","Basicos","Diversificado","Universitario","Posgrado","Ignorado"))


#diferencia de edades menores

muj <- table(dataFinal$grupo, dataFinal$ESCMUJ)
muj<- filter(dataFinal, dataFinal$grupo>=1)
mujer <- table(muj$ESCMUJ)

barplot(mujer, col = distinctColorPalette(99), main = "Escolaridad Mujeres", xlab = "Escolaridad", ylab="frecuencia" , names.arg =c("Ninguno","Primaria","Basicos","Diversificado","Universitario","Posgrado","Ignorado"))


hom <- table(dataFinal$grupo, dataFinal$ESCHOM)
hom<- filter(dataFinal, dataFinal$grupo>=1)
homesc <- table(hom$ESCHOM)

barplot(homesc, col = distinctColorPalette(99), main = "Escolaridad Hombres", xlab = "Escolaridad", ylab="frecuencia", names.arg =c("Ninguno","Primaria","Basicos","Diversificado","Universitario","Posgrado","Ignorado"))




#Pueblos menor dif

muj <- table(dataFinal$grupo, dataFinal$PUEMUJ)
muj<- filter(dataFinal, dataFinal$grupo>=1)
mujer <- table(muj$PUEMUJ)

barplot(mujer, col = distinctColorPalette(99), main = "Pueblo de pertenencia de la Mujer", xlab = "Pueblo", ylab="frecuencia", names.arg =c("Maya","Garifuna","Xinca","Mestizo/Ladino","Otro","Ignorado"))


hom <- table(dataFinal$grupo, dataFinal$PUEHOM)
hom<- filter(dataFinal, dataFinal$grupo>=1)
homesc <- table(hom$PUEHOM)

barplot(homesc, col = distinctColorPalette(99), main = "Pueblo de pertenencia del hombre", xlab = "Pueblo", ylab="frecuencia", names.arg =c("Maya","Garifuna","Xinca","Mestizo/Ladino","Otro","Ignorado"))

#Pueblos dif media

muj <- table(dataFinal$grupo, dataFinal$PUEMUJ)
muj<- filter(dataFinal, dataFinal$grupo>=2)
mujer <- table(muj$PUEMUJ)

barplot(mujer, col = distinctColorPalette(99), main = "Pueblo de pertenencia de la Mujer", xlab = "Pueblo", ylab="frecuencia", names.arg =c("Maya","Garifuna","Xinca","Mestizo/Ladino","Otro","Ignorado"))


hom <- table(dataFinal$grupo, dataFinal$PUEHOM)
hom<- filter(dataFinal, dataFinal$grupo>=2)
homesc <- table(hom$PUEHOM)

barplot(homesc, col = distinctColorPalette(99), main = "Pueblo de pertenencia del hombre", xlab = "Pueblo", ylab="frecuencia", names.arg =c("Maya","Garifuna","Xinca","Mestizo/Ladino","Otro","Ignorado"))




#Pueblos dif alta

muj <- table(dataFinal$grupo, dataFinal$PUEMUJ)
muj<- filter(dataFinal, dataFinal$grupo>=3)
mujer <- table(muj$PUEMUJ)

barplot(mujer, col = distinctColorPalette(99), main = "Pueblo de pertenencia de la Mujer", xlab = "Pueblo", ylab="frecuencia", names.arg =c("Maya","Garifuna","Xinca","Mestizo/Ladino","Otro","Ignorado"))


hom <- table(dataFinal$grupo, dataFinal$PUEHOM)
hom<- filter(dataFinal, dataFinal$grupo>=3)
homesc <- table(hom$PUEHOM)

barplot(homesc, col = distinctColorPalette(99), main = "Pueblo de pertenencia del hombre", xlab = "Pueblo", ylab="frecuencia", names.arg =c("Maya","Garifuna","Xinca","Mestizo/Ladino","Otro","Ignorado"))



#Municipios dif alta

muj <- table(dataFinal$grupo, dataFinal$MUPREG)
muj<- filter(dataFinal, dataFinal$grupo>=3)
muj <-table(muj$MUPREG)
muji <- sort(muj,                   # Vector atómico
                 decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
                 na.last = TRUE,) 
view(muji)
muji <- (head(muji,10))

barplot(muji, col = distinctColorPalette(99), main = "Municipios de Registro", xlab = "Municipio", ylab="frecuencia",names.arg =c("Guatemala","Jalapa","Villa Nueva","Pto. Barrios","Morales","Coban","Escuintla", "Huehuetenango", "Jutiapa","San Pedro Carcha" ))


# dif  media
muj <- table(dataFinal$grupo, dataFinal$MUPREG)
muj<- filter(dataFinal, dataFinal$grupo==2)
muj <-table(muj$MUPREG)
muji <- sort(muj,                   # Vector atómico
             decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
             na.last = TRUE,) 
view(muji)
muji <- (head(muji,10))

barplot(muji, col = distinctColorPalette(99), main = "Municipios de Registro", xlab = "Municipio", ylab="frecuencia",names.arg =c("Guatemala","Villa Nueva","Coban","San Pedro Carcha","Quetzaltenango","Huehuetenango","Pto. Barrios", "Jalapa", "Jutiapa","Escuintla" ))

#dif baja
muj <- table(dataFinal$grupo, dataFinal$MUPREG)
muj<- filter(dataFinal, dataFinal$grupo>=1)
muj <-table(muj$MUPREG)
muji <- sort(muj,                   # Vector atómico
             decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
             na.last = TRUE,) 
view(muji)
muji <- (head(muji,10))

barplot(muji, col = distinctColorPalette(99), main = "Municipios de Registro", xlab = "Municipio", ylab="frecuencia",names.arg =c("Guatemala","Villa Nueva","Quetzaltenango","Coban","San Juan Sac","San Pedro Carcha","Totonicapan","Huehuetenango", "Jalapa", "Chimaltenango"))




#Ocupaciones Mujeres dif Baja

# dif  baja
muj <- table(dataFinal$grupo, dataFinal$CIUOMUJ)
muj<- filter(dataFinal, dataFinal$grupo==1)
muj <-table(muj$CIUOMUJ)
muji <- sort(muj,                   # Vector atómico
             decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
             na.last = TRUE,) 
view(muji)
muji <- (head(muji,5))

barplot(muji, col = distinctColorPalette(99), main = "Ocupaciones Mujer", xlab = "Ocupaciones", ylab="frecuencia",names.arg =c("Servicios Personales", "No registrado", "Enseñanza", "Oficinistas","Contabilidad" ))



muj <- table(dataFinal$grupo, dataFinal$CIUOHOM)
muj<- filter(dataFinal, dataFinal$grupo==1)
muj <-table(muj$CIUOHOM)
muji <- sort(muj,                   # Vector atómico
             decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
             na.last = TRUE,) 
view(muji)
muji <- (head(muji,5))

barplot(muji, col = distinctColorPalette(99), main = "Ocupaciones Hombre", xlab = "Ocupaciones", ylab="frecuencia",names.arg =c("Peones", "Agricultor", "Vendedor", "Servicios Personales","Construccion" ))


muj <- table(dataFinal$grupo, dataFinal$CIUOHOM)
muj<- filter(dataFinal, dataFinal$grupo==2)
muj <-table(muj$CIUOHOM)
muji <- sort(muj,                   # Vector atómico
             decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
             na.last = TRUE,) 
view(muji)
muji <- (head(muji,5))

barplot(muji, col = distinctColorPalette(99), main = "Ocupaciones Hombre", xlab = "Ocupaciones", ylab="frecuencia")#,names.arg =c("Peones", "Servicios Personales", "Agricultor", "Vendedor","Construccion" ))


muj <- table(dataFinal$grupo, dataFinal$CIUOMUJ)
muj<- filter(dataFinal, dataFinal$grupo==2)
muj <-table(muj$CIUOMUJ)
muji <- sort(muj,                   # Vector atómico
             decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
             na.last = TRUE,) 
view(muji)
muji <- (head(muji,5))

barplot(muji, col = distinctColorPalette(99), main = "Ocupaciones Mujer", xlab = "Ocupaciones", ylab="frecuencia",names.arg =c("Servicios Personales", "No Registrados", "Enseñanza", "Vendedor","Oficinista" ))




muj <- table(dataFinal$grupo, dataFinal$CIUOHOM)
muj<- filter(dataFinal, dataFinal$grupo==3)
muj <-table(muj$CIUOHOM)
muji <- sort(muj,                   # Vector atómico
             decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
             na.last = TRUE,) 
view(muji)
muji <- (head(muji,5))

barplot(muji, col = distinctColorPalette(99), main = "Ocupaciones Hombre", xlab = "Ocupaciones", ylab="frecuencia",names.arg =c("Peones", "Agricultor", "Vendedor", "Servicios Personales","Construccion" ))


muj <- table(dataFinal$grupo, dataFinal$CIUOMUJ)
muj<- filter(dataFinal, dataFinal$grupo==3)
muj <-table(muj$CIUOMUJ)
muji <- sort(muj,                   # Vector atómico
             decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
             na.last = TRUE,) 
view(muji)
muji <- (head(muji,5))

barplot(muji, col = distinctColorPalette(99), main = "Ocupaciones Mujer", xlab = "Ocupaciones", ylab="frecuencia" ,names.arg =c("Servicios Personales", "No Registrados", "Vendedor", "Enseñanza","Oficinista" ))


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
dataFinal$edad_dif = abs(dataFinal$EDADHOM - dataFinal$EDADMUJ)
#Promedio de diferencia de edad entre novio y novia
mean(dataFinal$edad_dif)



#Separación aleatoria, ratio 70:30
dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]

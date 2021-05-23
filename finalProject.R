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
library(NbClust)#Para determinar el número de clusters óptimo
library(factoextra)#Para hacer gráficos bonitos de clustering
library(e1071)
library(caret)
library(corrplot) 
library(randomcoloR) #genera colores random para graficas
library(readxl)
library(tidyverse)
library(ggplot)
library(haven)
library(foreign)

path_diego <- "C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/finaldataset.csv"
data <-read.csv(path_diego)

#Separaci�n aleatoria, ratio 70:30
dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]
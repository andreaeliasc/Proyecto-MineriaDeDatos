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


path <- "C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data"
ano_ocurrencia2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "A�o y departamento ocurrencia")
grupo_edad <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Grupos de edad novio y novia")


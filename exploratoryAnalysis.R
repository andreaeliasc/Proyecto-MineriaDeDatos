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
library(NbClust)#Para determinar el n√∫mero de clusters √≥ptimo
library(factoextra)#Para hacer gr√°ficos bonitos de clustering
library(e1071)
library(caret)
library(corrplot) 
library(randomcoloR) #genera colores random para graficas
library(readxl)


path <- "C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data"
#2009
ano_ocurrencia2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "AÒo y departamento ocurrencia")
grupo_edad2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Grupos de edad novio y novia")
grupo_etnico2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Grupo Ètnico del novio y novia")
ocupacion_novia2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Ocupaciones del novio")
ocupacion_novio2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Ocupaciones de la novia")
mes_registro2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Mes registro y departamento")

#2010
ano_ocurrencia2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "AÒo y departamento ocurrencia")
grupo_edad2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Grupos de edad novio y novia")
grupo_etnico2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Grupo Ètnico del novio y novia")
ocupacion_novia2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Ocupaciones del novio")
ocupacion_novio2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Ocupaciones de la novia")
mes_registro2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Mes registro y departamento")


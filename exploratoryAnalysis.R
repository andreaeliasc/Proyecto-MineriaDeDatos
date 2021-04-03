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

path <- "C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/"

Mat2009 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.sav")
Mat2010 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.sav")
Mat2011 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2011.sav")
Mat2012 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2012.sav")
Mat2013 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2013.sav")
Mat2014 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2014.sav")
Mat2015 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2015.sav")
Mat2016 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2016.sav")
Mat2017 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2017.sav")
Mat2018 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2018.sav")
Mat2019 <- read_sav("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2019.sav")

#Uppercase 
names(Mat2010)<-toupper(names(Mat2010))
names(Mat2011)<-toupper(names(Mat2011))

#Eliminar año de ocurrencia
Mat2009[19] <- NULL
Mat2010[20] <- NULL
Mat2011[20] <- NULL
Mat2015[5] <- NULL
Mat2016[5] <- NULL
Mat2017[5] <- NULL
Mat2018[5] <- NULL
Mat2019[5] <- NULL

#Eliminar numero de nupcias
Mat2009[14] <- NULL
Mat2009[14] <- NULL
Mat2015[6] <- NULL
Mat2015[6] <- NULL
Mat2016[6] <- NULL
Mat2016[6] <- NULL
Mat2017[6] <- NULL
Mat2017[6] <- NULL
Mat2018[6] <- NULL
Mat2018[6] <- NULL
Mat2019[6] <- NULL
Mat2019[6] <- NULL

#Renombrar 
names(Mat2009)[names(Mat2009) == "AREAG"] <- "AREAGOCU"
names(Mat2010)[names(Mat2010) == "AREAG"] <- "AREAGOCU"
names(Mat2011)[names(Mat2011) == "AREAG"] <- "AREAGOCU"
names(Mat2012)[names(Mat2012) == "AREAG"] <- "AREAGOCU"

names(Mat2009)[names(Mat2009) == "GETHOM"] <- "PUEHOM"
names(Mat2009)[names(Mat2009) == "GETMUJ"] <- "PUEMUJ"
names(Mat2010)[names(Mat2010) == "GETHOM"] <- "PUEHOM"
names(Mat2010)[names(Mat2010) == "GETMUJ"] <- "PUEMUJ"
names(Mat2011)[names(Mat2011) == "GETHOM"] <- "PUEHOM"
names(Mat2011)[names(Mat2011) == "GETMUJ"] <- "PUEMUJ"
names(Mat2012)[names(Mat2012) == "GETHOM"] <- "PUEHOM"
names(Mat2012)[names(Mat2012) == "GETMUJ"] <- "PUEMUJ"

#agregar area de ocurrencia
namevector <- c("AREAGOCU")
Mat2018[ , namevector] <- NA
Mat2019[ , namevector] <- NA

#agregar area de Escolaridades
namevector <- c("ESCHOM", "ESCMUJ")
Mat2009[ , namevector] <- NA
#agregar DIAOCU
namevector <- c("DIAOCU")
Mat2009[ , namevector] <- NA

#cambiar el valor del grupo etnico, de 2 a 4, para los no indigenas (ver diccionario de variables)
Mat2009$PUEHOM[Mat2009$PUEHOM == 2] <- 4
Mat2009$PUEMUJ[Mat2009$PUEMUJ == 2] <- 4
Mat2010$PUEHOM[Mat2010$PUEHOM == 2] <- 4
Mat2010$PUEMUJ[Mat2010$PUEMUJ == 2] <- 4
Mat2011$PUEHOM[Mat2011$PUEHOM == 2] <- 4
Mat2011$PUEMUJ[Mat2011$PUEMUJ == 2] <- 4
Mat2012$PUEHOM[Mat2012$PUEHOM == 2] <- 4
Mat2012$PUEMUJ[Mat2012$PUEMUJ == 2] <- 4

colnames(Mat2009)
colnames(Mat2010)
colnames(Mat2011)
colnames(Mat2012)
colnames(Mat2013)
colnames(Mat2014)
colnames(Mat2015)
colnames(Mat2016)
colnames(Mat2017)
colnames(Mat2018)
colnames(Mat2019)

#grupo_edad2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Grupos de edad novio y novia")
# grupo_edad2009_df <- as.data.frame (grupo_edad2009)
# rownames (grupo_edad2009_df) <- grupo_edad2009_df[,1]
# grupo_edad2009_df <- grupo_edad2009_df    
# df_matrix <- data.matrix(grupo_edad2009_df)
# mosaicplot(df_matrix, main="Health Improvement by Drug Treatment (mosaic plot)")
# barplot(df_matrix, legend=TRUE, beside=TRUE,
#         main='grupo edades')
# install.packages("Hmisc")
# library("Hmisc")
# 
# 
# mydata.rcorr = rcorr(df_matrix)
# mydata.rcorr

#grupo_etnico2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Grupo étnico del novio y novia")
#ocupacion_novia2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Ocupaciones del novio")
#ocupacion_novio2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Ocupaciones de la novia")
#mes_registro2009 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2009.xls", sheet = "Mes registro y departamento")

#2010
#ano_ocurrencia2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Año y departamento ocurrencia")
#grupo_edad2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Grupos de edad novio y novia")
#grupo_etnico2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Grupo étnico del novio y novia")
#ocupacion_novia2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Ocupaciones del novio")
#ocupacion_novio2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Ocupaciones de la novia")
#mes_registro2010 <- read_excel("C:/Users/Diego/Documents/Universidad/Mineria/Proyecto-MineriaDeDatos/data/2010.xls", sheet = "Mes registro y departamento")

#rownames(grupo_edad2009) <- grupo_edad2009[,1] #Assigning row names from 1st column 

#grupo_edad2009[,1] <- NULL #Removing the first column

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

#agregar ciuo
namevector <- c("CIUOHOM","CIUOMUJ")
Mat2009[ , namevector] <- NA
namevector <- c("CIUOHOM","CIUOMUJ")
Mat2010[ , namevector] <- NA
namevector <- c("CIUOHOM","CIUOMUJ")
Mat2011[ , namevector] <- NA
namevector <- c("CIUOHOM","CIUOMUJ")
Mat2012[ , namevector] <- NA

#fix de anio
Mat2009$AÑOREG<- 2009

#cambiar el valor del grupo etnico, de 2 a 4, para los no indigenas (ver diccionario de variables)
Mat2009$PUEHOM[Mat2009$PUEHOM == 2] <- 4
Mat2009$PUEMUJ[Mat2009$PUEMUJ == 2] <- 4
Mat2010$PUEHOM[Mat2010$PUEHOM == 2] <- 4
Mat2010$PUEMUJ[Mat2010$PUEMUJ == 2] <- 4
Mat2011$PUEHOM[Mat2011$PUEHOM == 2] <- 4
Mat2011$PUEMUJ[Mat2011$PUEMUJ == 2] <- 4
Mat2012$PUEHOM[Mat2012$PUEHOM == 2] <- 4
Mat2012$PUEMUJ[Mat2012$PUEMUJ == 2] <- 4


Mat2009<-zap_labels(Mat2009)
Mat2010<-zap_labels(Mat2010)
Mat2011<-zap_labels(Mat2011)
Mat2012<-zap_labels(Mat2012)
Mat2013<-zap_labels(Mat2013)
Mat2014<-zap_labels(Mat2014)
Mat2015<-zap_labels(Mat2015)
Mat2016<-zap_labels(Mat2016)
Mat2017<-zap_labels(Mat2017)
Mat2018<-zap_labels(Mat2018)
Mat2019<-zap_labels(Mat2019)

Matrimonios_General = rbind(Mat2013,Mat2014,Mat2015,Mat2016,Mat2017,Mat2018,Mat2019)
namevector <- c("OCUHOM","OCUMUJ")
Matrimonios_General[ , namevector] <- NA
Matrimonios_General2 = rbind(Mat2009,Mat2010,Mat2011,Mat2012)
final_dataset = rbind(Matrimonios_General, Matrimonios_General2)

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

colnames(Matrimonios_General)
colnames(Matrimonios_General2)
colnames(final_dataset)

#inciso a
glimpse(final_dataset)

#inciso b
library(e1071)
kurtosis(final_dataset$EDADHOM)
kurtosis(final_dataset$EDADMUJ)
hist(final_dataset$EDADHOM, xlim=c(0,100), breaks = "FD")
hist(final_dataset$EDADMUJ, xlim=c(0,100), breaks = "FD")


#Tablas de Frecuencia para variables categoricas
freqDep <- table(final_dataset$DEPREG)
View(freqDep)

freqMun <- table(final_dataset$MUPREG)
View(freqMun)

freqMesReg <- table(final_dataset$MESREG)
View(freqMesReg)

freqAñoReg <- table(final_dataset$AÑOREG)
View(freqAñoReg)

freqClaseUnion <- table(final_dataset$CLAUNI)
View(freqClaseUnion)

freqPueHom <- table(final_dataset$PUEHOM)
View(freqPueHom)

freqPueMuj <- table(final_dataset$PUEMUJ)
View(freqPueMuj)

freqNacHom <- table(final_dataset$NACHOM)
View(freqNacHom)

freqNacMuj <- table(final_dataset$NACMUJ)
View(freqNacMuj)

freqOcupCIUO <- table(final_dataset$CIUOHOM)
View(freqOcupCIUO)

freqOcupCIUOMuj <- table(final_dataset$CIUOMUJ)
View(freqOcupCIUOMuj)


freqMunOcu <- table(final_dataset$MUPOCU)
View(freqMunOcu)

freqOcupHom <- table(final_dataset$OCUHOM)
View(freqOcupHom)

freqOcupMuj <- table(final_dataset$OCUMUJ)
View(freqOcupMuj)

esc_hombre <- data.frame(table(final_dataset$ESCHOM))
esc_mujer <- data.frame(table(final_dataset$ESCMUJ))
depocu <- data.frame(table(final_dataset$DEPOCU))
mupocu <- data.frame(table(final_dataset$MUPOCU))
dia_ocu <- data.frame(table(final_dataset$DIAOCU))
mes_ocu <- data.frame(table(final_dataset$MESOCU))
areag_ocu <- data.frame(table(final_dataset$AREAGOCU))


#Pregunta 1

barplot(freqDep, col=c("orange","blue", "pink", "lightblue", "purple", "royalblue", "green", "red", "white", "lightpink", "yellow", "brown", "magenta", "cyan", "seagreen", "turquoise", "tan", "maroon", "salmon", "beige", "black", "lightyellow"), las = 1, main = "Matrimonios registrados por departamentos", xlab = "departamento", ylab = "matrimonios registrados", names.arg =c("Guatemala", "El Progreso", "Sacatepequez", "Chimaltenango", "Escuintla", "Santa Rosa", "Solola", "Totonicapan", "Quetzaltenango", "Suchitepequez", "Retalhuleu", "San Marcos", "Huehuetenango", "Quiche", "Baja Verapaz", "Alta Verapaz", "Peten", "Izabal", "Zacapa", "Chiquimula", "Jalapa", "Jutiapa"), las =2)

#Pregunta 2
edadMuj<- table(final_dataset$EDADMUJ)
edadMuj <- sort(edadMuj,                   # Vector atómico
     decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
     na.last = TRUE,)     

barplot(head(edadMuj, 15),  col = distinctColorPalette(99), main = "Top 15 edad de mujeres", xlab = "edad", ylab="frecuencia")

# Pregunta 3
edadHom <- table(final_dataset$EDADHOM)
edadHom <- sort(edadHom,                   # Vector atómico
                decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
                na.last = TRUE,)   
View(edadHom)
barplot(head(edadHom, 15),  col = distinctColorPalette(99), main = "Top 15 edad de hombres", xlab = "edad", ylab="frecuencia")

#Pregunta 4

escolaridadMuj <- table(final_dataset$ESCMUJ)
View(escolaridadMuj)
escolaridadMuj <- sort(escolaridadMuj,                   # Vector atómico
                decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
                na.last = TRUE,)     

View(escolaridadMuj)
barplot(escolaridadMuj, col = distinctColorPalette(9), main = "Grado de escolaridad mujeres", xlab = "grado escolaridad", ylab = "frecuencia",  names.arg =c("Ninguno", "Primaria", "Basico", "Diversificado", "Universitario", "Postgrado", "Ignorado") )

#Pregunta #5
escolaridadHombre <- table(final_dataset$ESCHOM)
escolaridadHombre <- sort(escolaridadHombre,                   # Vector atómico
                          decreasing = TRUE, # Ordenar en orden creciente (FALSE) o decrecienete (TRUE)
                          na.last = TRUE,)  

barplot(escolaridadHombre, col = distinctColorPalette(9), main = "Grado de escolaridad de Hombres", xlab = "grado escolaridad", ylab = "frecuencia",  names.arg =c("Ninguno", "Primaria", "Basico", "Diversificado", "Universitario", "Postgrado", "Ignorado") )

#Pregunta #6
puebloMujer <- table(final_dataset$PUEMUJ)
View(puebloMujer)
barplot(puebloMujer, col = distinctColorPalette(6), main = "Pueblo de Origen de la Mujer", xlab = "Pueblo de Origen", ylab = "frecuencia",  names.arg =c("Maya", "Garifuna", "Xinca", "Mestizo/Ladino", "Otro", "Ignorado") )

#Pregunta 7
puebloHombre <- table(final_dataset$PUEHOM)
View(puebloHombre)
barplot(puebloHombre, col = distinctColorPalette(6), main = "Pueblo de Origen del Hombre", xlab = "Pueblo de Origen", ylab = "frecuencia",  names.arg =c("Maya", "Garifuna", "Xinca", "Mestizo/Ladino", "Otro", "Ignorado") )

#Pregunta 8

edadyAñoMujer <- final_dataset[c("EDADMUJ", "AÑOREG")]
hola <- filter(edadyAñoMujer, edadyAñoMujer$EDADMUJ < 18)
View(hola)
barplot(hola)

incurrenciaMenoresEdad <- table(hola$AÑOREG)
View(incurrenciaMenoresEdad)
barplot(incurrenciaMenoresEdad, main = "Matrimonios de menores de edad por año", xlab = "Año de Registro", ylab = "Numero de Matrimonios", col = distinctColorPalette(8))

pie(incurrenciaMenoresEdad, main = "Matrimonios de menores de edad por año", col = distinctColorPalette(8))


#Pregunta 9
edadyAñoHombre <- final_dataset[c("EDADHOM", "AÑOREG")]
homb <- filter(edadyAñoHombre, edadyAñoHombre$EDADHOM < 18)
View(homb)


incurrenciaMenoresEdadHom <- table(homb$AÑOREG)
View(incurrenciaMenoresEdadHom)
barplot(incurrenciaMenoresEdadHom, main = "Matrimonios de menores de edad por año", xlab = "Año de Registro", ylab = "Numero de Matrimonios", col = distinctColorPalette(8))

pie(incurrenciaMenoresEdadHom, main = "Matrimonios de menores de edad por año", col = distinctColorPalette(8))



#Pregunta 10
depEdad <- final_dataset[c("EDADHOM", "EDADMUJ", "DEPREG")]
depEdad2 <- filter(depEdad, depEdad$EDADMUJ < 18, depEdad$EDADHOM < 18)
incurrenciEdad <- table(depEdad2$DEPREG)
View(incurrenciEdad)
barplot(incurrenciEdad, main = "Matrimonios de 2 menores de edad por departamento", xlab = "Departamento de Registro", ylab = "Numero de Matrimonios", col = distinctColorPalette(12) )
pie(incurrenciEdad, main = "Matrimonios de 2 menores de edad por departamento", col = distinctColorPalette(12))


#Pregunta 11
depEdad3 <- filter(depEdad, depEdad$EDADMUJ < 18, depEdad$EDADHOM>17)
incurrencia1 <- table(depEdad3$DEPREG)
View(incurrencia1)
barplot(incurrencia1, main = "Matrimonios de mujer menor de edad y hombre mayor de edad por departamento", xlab = "Departamento de Registro", ylab = "Numero de Matrimonios", col = distinctColorPalette(22) )
pie(incurrencia1, main = "Matrimonios de mujer menor de edad y hombre mayor de edad por departamento", col = distinctColorPalette(22))

#Pregunta 12
depEdad4 <- filter(depEdad, depEdad$EDADMUJ > 17, depEdad$EDADHOM<18)
incurrencia2 <- table(depEdad4$DEPREG)
View(incurrencia2)
barplot(incurrencia2, main = "Matrimonios de mujer mayor de edad y hombre menor de edad por departamento", xlab = "Departamento de Registro", ylab = "Numero de Matrimonios", col = distinctColorPalette(22) )
pie(incurrencia2, main = "Matrimonios de mujer mayor de edad y hombre menor de edad por departamento", col = distinctColorPalette(22))


#Pregunta 13
edadPueblo <- final_dataset[c("EDADHOM", "PUEHOM")]
incurrencua3 <- table(edadPueblo$PUEHOM)
View(incurrencia3)
edadPueblo <- filter(edadPueblo, edadPueblo$EDADHOM<999)
plot(x = edadPueblo$PUEHOM, y = edadPueblo$EDADHOM)

barplot(x = edadPueblo$PUEHOM, y = edadPueblo$EDADHOM)

barplot(x = edadPueblo$PUEHOM, y = edadPueblo$EDADHOM, )


#Pregunta 14
edadPueblo <- final_dataset[c("EDADHOM", "PUEHOM")]
incurrencua3 <- table(edadPueblo$PUEHOM)
View(incurrencia3)
edadPueblo <- filter(edadPueblo, edadPueblo$EDADHOM<999)
plot(x = edadPueblo$PUEHOM, y = edadPueblo$EDADHOM)

barplot(x = edadPueblo$PUEHOM, y = edadPueblo$EDADHOM)

barplot(x = edadPueblo$PUEHOM, y = edadPueblo$EDADHOM, )


#pregunta 15
ocupMayas <- final_dataset[c("OCUHOM", "PUEHOM")]
#1 maya
#4 mestizo
mayas <- filter(ocupMayas, ocupMayas$PUEHOM < 2, !is.na(ocupMayas) )
incuMayas <- table(mayas$OCUHOM)
incuMayas <- (head(incuMayas, 25))
barplot(mayas$OCUHOM)

barplot(incuMayas, main = "Ocupaciones mas comunes en hombres mayas", xlab = "Ocupacion", ylab = "Numero de Hombre", col = distinctColorPalette(1000) )


ladinos <- filter(ocupMayas, ocupMayas$PUEHOM == 4, !is.na(ocupMayas) )
inculadinos <- table(ladinos$OCUHOM)
inculadinos <- (head(inculadinos, 25))
barplot(mayas$OCUHOM)

barplot(inculadinos, main = "Ocupaciones mas comunes en hombres mestizos y ladinos", xlab = "Ocupacion", ylab = "Numero de Hombre", col = distinctColorPalette(1000) )

#pregunta 16
ocupMayasMuj <- final_dataset[c("OCUMUJ", "PUEMUJ")]
#1 maya
#4 mestizo
mayasMuj <- filter(ocupMayasMuj, ocupMayasMuj$PUEMUJ < 2, !is.na(ocupMayasMuj) )
incuMayasMuj <- table(mayasMuj$OCUMUJ)
incuMayasMuj <- (head(incuMayasMuj, 25))


barplot(incuMayasMuj, main = "Ocupaciones mas comunes en mujeres mayas", xlab = "Ocupacion", ylab = "Numero de Mujeres", col = distinctColorPalette(1000) )


ladinosMuj <- filter(ocupMayasMuj, ocupMayasMuj$PUEMUJ == 4, !is.na(ocupMayasMuj) )
inculadinosMuj <- table(ladinosMuj$OCUMUJ)
inculadinosMuj <- (head(inculadinosMuj, 25))
barplot(mayasMuj$OCUMUJ)

barplot(inculadinosMuj, main = "Ocupaciones mas comunes en mujeres mestizas y ladinas", xlab = "Ocupacion", ylab = "Numero de Mujeres", col = distinctColorPalette(1000) )

#escolaridad por pueblo

escolaridadPueblo <- final_dataset[c("ESCMUJ", "PUEMUJ")]
MayasEsc <- filter(escolaridadPueblo, escolaridadPueblo$PUEMUJ == 1)
MayasEsc <- table(MayasEsc$ESCMUJ)
view(MayasEsc)
barplot(MayasEsc, main = "Escolaridad en mujeres mayas", xlab = "Escolaridad", ylab = "Numero de Mujeres", col = distinctColorPalette(1000), names.arg =c("Ninguno", "Primaria", "Basico", "Diversificado", "Universitario", "Postgrado", "Ignorado")  )

escolaridadPueblo2 <- final_dataset[c("ESCMUJ", "PUEMUJ")]
ladinaEsc <- filter(escolaridadPueblo2, escolaridadPueblo2$PUEMUJ == 4)
ladinaEsc <- table(ladinaEsc$ESCMUJ)
View(ladinaEsc)
barplot(ladinaEsc, main = "Escolaridad en mujeres mestizas y ladinas", xlab = "Escolaridad", ylab = "Numero de Mujeres", col = distinctColorPalette(1000), names.arg =c("Ninguno", "Primaria", "Basico", "Diversificado", "Universitario", "Postgrado", "Ignorado")  )
#escolaridad pueblo hombre
escolaridadPuebloHom <- final_dataset[c("ESCHOM", "PUEHOM")]
MayasEscHom <- filter(escolaridadPuebloHom, escolaridadPuebloHom$PUEHOM == 1)
MayasEscHom <- table(MayasEscHom$ESCHOM)
view(MayasEscHom)
barplot(MayasEscHom, main = "Escolaridad en hombres mayas", xlab = "Escolaridad", ylab = "Numero de Mujeres", col = distinctColorPalette(1000), names.arg =c("Ninguno", "Primaria", "Basico", "Diversificado", "Universitario", "Postgrado", "Ignorado")  )


ladinaEscHom <- filter(escolaridadPuebloHom, escolaridadPuebloHom$PUEHOM == 4)
ladinaEscHom <- table(ladinaEscHom$ESCHOM)
View(ladinaEscHom)
barplot(ladinaEscHom, main = "Escolaridad en hombres mestizos y ladinos", xlab = "Escolaridad", ylab = "Numero de Mujeres", col = distinctColorPalette(1000), names.arg =c("Ninguno", "Primaria", "Basico", "Diversificado", "Universitario", "Postgrado", "Ignorado")  )



#Clases de Union por deppto
clasUniDpto <- final_dataset[c("CLAUNI", "DEPOCU")]
freqUni <- table(clasUniDpto$DEPOCU)
View(clasUniDpto)

Union1 <- filter(clasUniDpto, clasUniDpto$CLAUNI == 1 )
View(Union1)
freqUni1 <- table(Union1$DEPOCU)
View(freqUni1)
barplot(freqUni1, main = "Union de Comunidad absoluta por departamento", xlab = "Departamento", ylab = "Numero de Uniones", col = distinctColorPalette(1000))

Union2 <- filter(clasUniDpto, clasUniDpto$CLAUNI == 2 )
View(Union2)
freqUni2 <- table(Union2$DEPOCU)
View(freqUni2)
barplot(freqUni2, main = "Union de separacion absoluta por departamento", xlab = "Departamento", ylab = "Numero de Uniones", col = distinctColorPalette(1000))

Union3 <- filter(clasUniDpto, clasUniDpto$CLAUNI == 3 )
View(Union3)
freqUni3 <- table(Union3$DEPOCU)
View(freqUni3)
barplot(freqUni3, main = "Union de comunidad de gananciales  por departamento", xlab = "Departamento", ylab = "Numero de Uniones", col = distinctColorPalette(1000))

Union4 <- filter(clasUniDpto, clasUniDpto$CLAUNI == 9 )
View(Union4)
freqUni4 <- table(Union4$DEPOCU)
View(freqUni4)
barplot(freqUni4, main = "Union no especificada  por departamento", xlab = "Departamento", ylab = "Numero de Uniones", col = distinctColorPalette(1000))


# matrimonios de extranjeros por departamento
matrimExtran <- final_dataset[c("NACHOM", "DEPOCU")]
matrimExtran <- filter(matrimExtran, matrimExtran$NACHOM != 320)
View(matrimExtran)
freqExtran<- table(matrimExtran$DEPOCU)
View(freqExtran)
barplot(freqExtran, main = "Matrimonios donde el hombre es  extranjero por departamento", xlab = "Departamento", ylab = "Numero de Uniones", col = distinctColorPalette(1000))

matrimExtran <- final_dataset[c("NACMUJ", "DEPOCU")]
matrimExtran <- filter(matrimExtran, matrimExtran$NACMMUJ != 320)
View(matrimExtran)
freqExtran<- table(matrimExtran$DEPOCU)
View(freqExtran)
barplot(freqExtran, main = "Matrimonios donde la mujer es extranjera  por departamento", xlab = "Departamento", ylab = "Numero de Uniones", col = distinctColorPalette(1000))

matrimExtran <- final_dataset[c("NACMUJ","NACHOM", "DEPOCU")]
matrimExtran <- filter(matrimExtran, matrimExtran$NACMMUJ != 320, matrimExtran$NACHOM != 320)
View(matrimExtran)
freqExtran<- table(matrimExtran$DEPOCU)
View(freqExtran)
barplot(freqExtran, main = "Matrimonios donde ambos son extranjeros es extranjera  por departamento", xlab = "Departamento", ylab = "Numero de Uniones", col = distinctColorPalette(1000))




#grado escolaridad por departamento 
escolaridadDepto <- final_dataset[c("ESCMUJ", "ESCHOM", "DEPOCU")]
freqEsc <- filter(escolaridadDepto, escolaridadDepto$ESCMUJ == 1, escolaridadDepto$ESCHOM == 1 )
freqEsc<- table(escolaridadDepto$DEPOCU)
View(freqEsc)
barplot(freqEsc, main = "Matrimonios donde ambos  no poseen grado de escolaridad  por departamento", xlab = "Departamento", ylab = "Numero de Uniones", col = distinctColorPalette(1000))






#Clustering 
clusteringVar <- final_dataset[c("EDADMUJ", "EDADHOM")]
clusteringVar <- final_dataset[c("EDADMUJ", "EDADHOM")]
clusteringVar <- filter(clusteringVar, clusteringVar$EDADMUJ <115, clusteringVar$EDADHOM<115)
wss <- (nrow(clusteringVar)-1)*sum(apply(clusteringVar,2,var))
 for (i in 2:10) 
   wss[i] <- sum(kmeans(clusteringVar, centers=i)$withinss)
 plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")
 
 matrifinal <- filter(final_dataset, final_dataset$EDADHOM < 115, final_dataset$EDADMUJ<115)
 km <- kmeans(clusteringVar, 3)
 matrifinal$grupo<- km$cluster 
 matrifinal$KM<-km$cluster 

plotcluster(clusteringVar[,1:2],km$cluster)
silkm <- silhouette(km$cluster, dist(clusteringVar[,1:2]))

fviz_cluster(km, data = clusteringVar[,1:2],geom = "point", ellipse.type = "norm")

fviz_cluster(km, data = clusteringVar)

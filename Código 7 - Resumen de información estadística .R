# -------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Resumen de información en R
# ------------------------------------------------------------------------- 

# -------------------------------------------------------
# instalación de librerías en R
# -------------------------------------------------------

# install.packages("readr")
# install.packages("dplyr")
# install.packages("moments")

# -------------------------------------------------------
# librerías
# -------------------------------------------------------

library(readr)
library(dplyr)
library(moments)

# -------------------------------------------------------
# Configuración directorio de trabajo
# -------------------------------------------------------

#setwd("C:/Users/ctvas/Documentos Cristian Vásquez/05. Magíster en BA/05. Script Clases/05. Material Complementario Videos/01. Datos")
setwd("aquí debe indicar la carpeta donde se encuentran los datos")
getwd()

# -------------------------------------------------------
# Importa datos desde excel
# -------------------------------------------------------

MallCustomers = read_csv("Mall_Customers.csv")
MallCustomers

attach(MallCustomers)       # almacena las variables en memoria

# -------------------------------------------------------
# Medidas de localización
# -------------------------------------------------------

mean(Age)
median(Age)
quantile(Age)

# -------------------------------------------------------
# Medidas de dispersión
# -------------------------------------------------------

var(Age)
sd(Age)
diff(range(Age))
IQR(Age)

# -------------------------------------------------------
# Medidas de forma
# -------------------------------------------------------

skewness(Age)
kurtosis(Age)

# -------------------------------------------------------
# Frecuencias para datos agrupados
# -------------------------------------------------------

table(Genre)
prop.table(table(Genre))


# -------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Análisis de correlación de variables
# ------------------------------------------------------------------------- 

# -------------------------------------------------------
# instalación de librerías en R
# -------------------------------------------------------

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("corrplot")

# -------------------------------------------------------
# librerías
# -------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

# -------------------------------------------------------
# Configuración directorio de trabajo
# -------------------------------------------------------

#setwd("C:/Users/ctvas/Documentos Cristian Vásquez/05. Magíster en BA/05. Script Clases/05. Material Complementario Videos/01. Datos")
setwd("aquí debe indicar la carpeta donde se encuentran los datos")
getwd()

# -------------------------------------------------------
# Importa datos extensión .txt con librería base
# -------------------------------------------------------

creditos <- read.delim("creditos_uni.txt")
cuanti   <- creditos[c(2,4,6,7,8,9)]


# -------------------------------------------------------
# Análisis set de datos bancarios "creditos_uni.txt"
# -------------------------------------------------------

# Gráfico de dispersión

ggplot(cuanti, aes(plazo,monto)) +
    geom_point(color="red", alpha = 0.3)

# Matriz de correlación

round(cor(cuanti, method = "pearson"),3)

# Heatmap

corrplot(cor(cuanti, method = "pearson"),
         method = "color",
         type = "lower",
         diag = FALSE,
         addCoef.col = "black")

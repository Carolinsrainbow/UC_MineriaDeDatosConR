# -------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Medidas de similaridad 
# ------------------------------------------------------------------------- 

# -------------------------------------------------------
# instalación de librerías en R
# -------------------------------------------------------

# install.packages("readxl")
# install.packages("factoextra")

# -------------------------------------------------------
# librerías
# -------------------------------------------------------

library(readxl)
library(factoextra)

# -------------------------------------------------------
# Configuración directorio de trabajo
# -------------------------------------------------------

#setwd("C:/Users/ctvas/Documentos Cristian Vásquez/05. Magíster en BA/05. Script Clases/05. Material Complementario Videos/01. Datos")
setwd("aquí debe indicar la carpeta donde se encuentran los datos")
getwd()

# -------------------------------------------------------
# Importar datos en formato CSV
# -------------------------------------------------------

rating <- read_xls("muestra_rating.xls")
rating

# ----------------------------------------------------------------------------
# Cálculo de distancias
# ----------------------------------------------------------------------------

datos.centrados <- scale(rating[,4:9], center = T, scale = T) # estandarizar los datos

# Distancia Euclidean

mat_dist1 <- dist(datos.centrados[1:50,],method = "euclidean")
round(as.matrix(mat_dist1)[1:5, 1:5], 2)

fviz_dist(dist.obj = mat_dist1, lab_size = 5) +
    ggtitle("Matriz de Distancia Euclidiana")

# Distancia de Manhattan

mat_dist2 <- dist(datos.centrados[1:50,],method = "manhattan")
round(as.matrix(mat_dist2)[1:5, 1:5], 2)

fviz_dist(dist.obj = mat_dist2, lab_size = 5) +
    ggtitle("Matriz de Distancia de Manhattan")

# Distancia correlación Pearson

mat_dist3 <- get_dist(datos.centrados[1:50,], method = "pearson")
round(as.matrix(mat_dist3)[1:5, 1:5], 2)

fviz_dist(dist.obj = mat_dist3, lab_size = 5) +
    ggtitle("Matriz de Distancia de Correlación Pearson")

#--------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Extracción de información de diferentes fuentes de abastecimiento en R
# Descripción	: El tutorial está orientado gestión y parámetros relevantes 
#               en la lectura de datos de datos y las funciones para almacenar los objetos en R
#-------------------------------------------------------------------------- 


# ----------------------------------------------------------------------------
# Configuración lugar de trabajo
# ----------------------------------------------------------------------------


ruta = "C:/Users/ctvas/Documentos Cristian Vásquez/05. Magíster en BA/09. Tutoriales/02. Material Complementario/01. Datos"
setwd(ruta)

# ----------------------------------------------------------------------------
# Librerías
# ----------------------------------------------------------------------------

# install.packages("readr")
# install.packages("readxl")
# install.packages("dplyr")

# ----------------------------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------------------------

library(readr)
library(readxl)
library(dplyr)


# ----------------------------------------------------------------------------
# Gestión de lectura de datos
# ----------------------------------------------------------------------------

mall.customers01 = read_delim("01.Mall_Customers.csv", delim = ";")
spec(mall.customers01)

mall.customers02 = read_delim("01.Mall_Customers.csv",
                              col_types = cols(col_integer(),
                                               col_character(),
                                               col_character(),
                                               col_integer(),
                                               col_integer()),
                               delim = ";")
spec(mall.customers02)

mall.customers03 = read_delim("02.Mall_Customers.csv",
                              col_types = cols(col_integer(),
                                               col_character(),
                                               col_integer(),
                                               col_integer(),
                                               col_integer()),
                              delim     = ",",
                              na        = "888")
spec(mall.customers03)


excel_sheets("03.Mall_Customers.xlsx")

mall.customers04 = read_excel("03.Mall_Customers.xlsx", sheet = "Male")
mall.customers04

mall.customers05 = read_excel("03.Mall_Customers.xlsx", sheet = 1)
mall.customers05

rm(mall.customers01,
   mall.customers02,
   mall.customers03)

# ----------------------------------------------------------------------------
# Almacenamiento de información
# ----------------------------------------------------------------------------

save(mall.customers04, file = "datos.RData")

save(mall.customers04, mall.customers05, file = "todos_datos.RData")

# ----------------------------------------------------------------------------
# Cargar objetos de R
# ----------------------------------------------------------------------------

mall.customers04
mall.customers05
load("todos_datos.RData")


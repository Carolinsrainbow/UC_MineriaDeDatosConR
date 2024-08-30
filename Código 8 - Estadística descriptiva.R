# -------------------------------------------------------------------------
# Fecha		    : Octubre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Estadísticas descriptivas para describir datos
# Descripción	: Aquí se realizar un análisis descriptivo sobre un set de datos
#               relacionado a la actividad de un retial. Se utilizan las librerías
#               de ciencia de datos para realizar la lectura y la librería "skimr" para
#               calcular los estadísticos de resumen
# ------------------------------------------------------------------------- 

# ----------------------------------------------------------------------------
# Configuración lugar de trabajo
# ----------------------------------------------------------------------------

ruta = "C:/Users/ctvas/Documentos Cristian Vásquez/05. Magíster en BA/09. Tutoriales/02. Material Complementario/01. Datos"
setwd(ruta)

# ----------------------------------------------------------------------------
# Librerías
# ----------------------------------------------------------------------------

# install.packages("readr")
# install.packages("dplyr")
# install.packages("moments")

# ----------------------------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------------------------

library(readr)
library(dplyr)
library(moments)

# ----------------------------------------------------------------------------
# importar datos
# ----------------------------------------------------------------------------

ecommerce = read_delim("Ecommerce.txt",delim = ",")

# ----------------------------------------------------------------------------
# Renombrar los campos
# ----------------------------------------------------------------------------

ecommerce = rename(ecommerce,
                   AvgSessionLength   = 'Avg. Session Length',
                   TimeonApp          = 'Time on App',
                   TimeonWebsite      = 'Time on Website',
                   LengthofMembership = 'Length of Membership',
                   YearlyAmountSpent  = 'Yearly Amount Spent')

# ----------------------------------------------------------------------------
# Estadísticas descritivas utilizando la librería"skimr"
# ----------------------------------------------------------------------------

skimr::skim(select(ecommerce,-c(Email,Address,Avatar)))

aux1 = select(ecommerce ,-c(Email,Address,Avatar))

### Rango y IQR

apply(aux1,2,IQR)
apply(aux1,2,function(x) diff(range(x)))

### Estadísticas de forma

tibble(Variables = names(aux1),
          Asimetria = as.numeric(skewness(aux1)),
          kurtosis  = as.numeric(kurtosis(aux1)))

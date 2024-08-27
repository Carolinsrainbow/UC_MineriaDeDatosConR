# -------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Gestión de datos utilizando la librería dplyr
# ------------------------------------------------------------------------- 

# -------------------------------------------------------
# instalación de librerías en R
# -------------------------------------------------------

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("devtools")
# devtools::install_github("tidyverse/dplyr")

# -------------------------------------------------------
# librerías
# -------------------------------------------------------

library(readxl)
library(dplyr)
library(lubridate)


# -------------------------------------------------------
# Configuración directorio de trabajo
# -------------------------------------------------------

#setwd("C:/Users/ctvas/Documentos Cristian Vásquez/05. Magíster en BA/05. Script Clases/05. Material Complementario Videos/01. Datos")
setwd("aquí debe indicar la carpeta donde se encuentran los datos")
getwd()

#--------------------------------------------------------
# Importa datos desde excel
#--------------------------------------------------------

disneyplus_titles <- read_excel("disneyplus_titles.xlsx")
disneyplus_titles

#--------------------------------------------------------
# Gestión de datos con dplyr
#--------------------------------------------------------

select(disneyplus_titles,1,2,3)


select(disneyplus_titles,contains("tion"))


filter(disneyplus_titles, type == "TV Show")


filter(disneyplus_titles, type == "Movie" & release_year == 2021)


disneyplus_titles %>%
  mutate(recencia = difftime("2022-01-01",date_added,units = "days")) %>%
  select(show_id,type,title,recencia,date_added)
# -------------------------------------------------------------------------
# Fecha		    : Octubre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Problemas con la dimensión de la información
# Descripción	: Analizar un conjunto de datos relacionados a mediciones de 
#               vehículos y estudiar el coeficiente R cuadrado.
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
# install.packages("ggplot2")

# ----------------------------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)


# ----------------------------------------------------------------------------
# importar datos
# ----------------------------------------------------------------------------

datos = read_csv("Auto.csv")

# ----------------------------------------------------------------------------
# Limpieza
# ----------------------------------------------------------------------------

table(datos$horsepower)

datos <- datos                          %>% 
            filter(horsepower != "?")   %>%
            mutate(horsepower = as.numeric(horsepower))

# ----------------------------------------------------------------------------
# Gráfico
# ----------------------------------------------------------------------------

plot1.1 <- ggplot(datos, aes(x = horsepower,y = mpg))           +
            geom_point(color = " slategray4 ",alpha =0.7)       +
            geom_smooth(method = "loess",col=" red")            +
            labs(y = "Millas por galón",
                 x = "Horse power",
                 title = "Scatter Plot entre MPG y Horse power")           +
            theme_bw ()

plot1.1

# ----------------------------------------------------------------------------
# Correlación
# ----------------------------------------------------------------------------

var.ind = c("cylinders","displacement","horsepower","weight","acceleration","year")
select(datos,one_of(var.ind)) %>% cor()

tibble(variable = var.ind, correlacion = as.numeric(cor(select(datos,one_of(var.ind)), datos$mpg))) %>% 
  arrange(correlacion)

# ----------------------------------------------------------------------------
# Modelos
# ----------------------------------------------------------------------------

newdatos <- mutate(datos,horsepower2 = horsepower^2)

modelo1 <- lm(mpg ~ horsepower + horsepower2, data = newdatos)
summary(modelo1)

forecast <- newdatos %>% mutate(prediccion = predict(modelo1))

### Predicción modelo base

plot1.2<- ggplot(forecast, aes(x = horsepower,y = mpg))           +
  geom_point(color = " slategray4 ",alpha =0.7)       +
  geom_line(aes(x = horsepower, y = prediccion), 
             color =" red")            +
  labs(y = "Millas por galón",
       x = "Horse power",
       title = "Scatter Plot entre MPG y Horse power")           +
  theme_bw ()

plot1.2

### nuevos modelos

modelo2 <- lm(mpg ~ horsepower + horsepower2 + cylinders, data = newdatos)
summary(modelo2)


modelo3 <- lm(mpg ~ horsepower + horsepower2 + cylinders + weight, data = newdatos)
summary(modelo3)


modelo4 <- lm(mpg ~ horsepower + horsepower2 + weight, data = newdatos)
summary(modelo4)





modelo5 <- lm(mpg ~ horsepower + horsepower2 + cylinders + weight + year, data = newdatos)
summary(modelo5)

# -------------------------------------------------------------------------
# Fecha		    : Octubre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Gráficos para describir información con ggplot2
# Descripción	: Se analiza un set de datos sobre la demanda de bicicletas p´ublicas del sistema Capital
#               Bikeshare de Washington DC.
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
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("lubridate")
# install.packages("scales")

# ----------------------------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------------------------

library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(scales)

# ----------------------------------------------------------------------------
# importar datos
# ----------------------------------------------------------------------------

datos = read_csv("dataset_bike.csv")

# ----------------------------------------------------------------------------
# Creación de variables factores
# ----------------------------------------------------------------------------

datos$season     <- factor(datos$season, labels = c("Spring", "Summer", "Fall", "Winter"))
datos$weather    <- factor(datos$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
datos$holiday    <- factor(datos$holiday, labels = c("No", "Yes"))
datos$workingday <- factor(datos$workingday, labels = c("No", "Yes"))
datos$weather    <- factor(datos$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
datos$hour       <- factor(hour(ymd_hms(datos$datetime)))
datos$weekday    <- wday(ymd_hms(datos$datetime), label=TRUE, abbr = FALSE) 
datos$month      <- factor(month(datos$datetime, label = TRUE, abbr = FALSE))

# ----------------------------------------------------------------------------
# Extraer información a través de los datos
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Gráficos univariados
# ----------------------------------------------------------------------------

plot1.1 = ggplot(data = datos, aes(x = hour ))			  +
              geom_bar(alpha	= 0.7, 
                       color	= "black", 
                       fill		= "slategray4")         +
              labs(x	= "Hour",  
                   y	= "Frecuencia")									+
              theme_bw()


plot1.2 = ggplot(data = datos, aes(x = holiday))			+
              geom_bar(alpha	= 0.7, 
                       color	= "black", 
                       fill		= "slategray4")         +
              labs(x	= "Holiday",  
                   y	= "Frecuencia")									+
              theme_bw()

plot1.3 = ggplot(data = datos, aes(x = workingday))		+
              geom_bar(alpha	= 0.7, 
                       color	= "black", 
                       fill		= "slategray4")         +
              labs(x	= "Working day",  
                   y	= "Frecuencia")								  +
              theme_bw()

plot1.4 = ggplot(data = datos, aes(x = weekday))		+
              geom_bar(alpha	= 0.7, 
                       color	= "black", 
                       fill		= "slategray4")         +
              labs(x	= "Week day",  
                   y	= "Frecuencia")								  +
              theme_bw()

plot1.5 = ggplot(data = datos, aes(x = season))			  +
              geom_bar(alpha	= 0.7, 
                       color	= "black", 
                       fill		= "slategray4")         +
              labs(x	= "Season",  
                   y	= "Frecuencia")									+
              theme_bw()

plot1.6 = ggplot(data = datos, aes(x = month))			  +
              geom_bar(alpha	= 0.7, 
                       color	= "black", 
                       fill		= "slategray4")         +
              labs(x	= "Month",  
                   y	= "Frecuencia")									+
              theme_bw()                              +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


plot1 <- ggarrange(plot1.1,plot1.2,plot1.3,
                           plot1.4,plot1.5,plot1.6,
                   ncol=3,nrow=2,common.legend = TRUE)

annotate_figure(plot1, top = text_grob("Gráficos de Barra"))

rm(plot1.1,plot1.2,plot1.3,
   plot1.4,plot1.5,plot1.6)


# Resto Variables 


plot1.7 = ggplot(data = datos, aes(x = weather))			  +
              geom_bar(alpha	= 0.7, 
                       color	= "black", 
                       fill		= "slategray4")         +
              labs(x	= "Weather",  
                   y	= "Frecuencia")									+
              theme_bw()

plot1.8  = ggplot(data = datos, aes(x = temp))						        +
              geom_histogram(alpha		= 0.7, 
                             color	= "black", 
                             fill		= "slategray4",
                             bins		= nclass.Sturges(datos$temp))	+
              labs(x	= "Temperature",  
                   y	= "Frecuencia")												      +
              theme_bw()

plot1.9  = ggplot(data = datos, aes(x = atemp))						          +
              geom_histogram(alpha		= 0.7, 
                             color	= "black", 
                             fill		= "slategray4",
                             bins		= nclass.Sturges(datos$atemp))	+
              labs(x	= "Feeling Temperature",  
                   y	= "Frecuencia")												        +
              theme_bw()

plot1.10  = ggplot(data = datos, aes(x = humidity))						          +
              geom_histogram(alpha		= 0.7, 
                             color	= "black", 
                             fill		= "slategray4",
                             bins		= nclass.Sturges(datos$humidity))	+
              labs(x	= "Humidity",  
                   y	= "Frecuencia")												        +
              theme_bw()

plot1.11  = ggplot(data = datos, aes(x = windspeed))						        +
              geom_histogram(alpha		= 0.7, 
                             color	= "black", 
                             fill		= "slategray4",
                             bins		= nclass.Sturges(datos$windspeed))	+
              labs(x	= "Wind Speed",  
                   y	= "Frecuencia")												            +
              theme_bw()

plot1.12  = ggplot(data = datos, aes(x = count))						        +
              geom_histogram(alpha		= 0.7, 
                             color	= "black", 
                             fill		= "slategray4",
                             bins		= nclass.Sturges(datos$count))	+
              labs(x	= "Total number of rentals",  
                   y	= "Frecuencia")												            +
              theme_bw()

plot2 <- ggarrange(plot1.7,plot1.8,plot1.9,
                   plot1.10,plot1.11,plot1.12,
                   ncol=3,nrow=2,common.legend = TRUE)

annotate_figure(plot2, top = text_grob("Gráficos de Distribución"))

rm(plot1.7,plot1.8,plot1.9,
   plot1.10,plot1.11,plot1.12)

rm(plot1,plot2)

# ----------------------------------------------------------------------------
# Gráfico Bivariado 
# ----------------------------------------------------------------------------

plot2.1 <- ggplot(datos, aes(x = season, y = count, fill = season)) +
            geom_boxplot(color = " slategray4 ",alpha =0.7) +
            labs (y = "Count",
                  x = "Season",
                  title = "Boxplot de Count por Season") +
            theme_bw () +
            theme(legend.position = "none")

plot2.2 <- ggplot(datos, aes(x = weather, y = count, fill = weather)) +
            geom_boxplot(color = " slategray4 ",alpha =0.7) +
            labs (y = "Count",
                  x = "Weather",
                  title = "Boxplot de Count por Weather") +
            theme_bw () +
            theme(legend.position = "none")

plot2.3 <- ggplot(datos, aes(x = hour, y = count, fill = hour)) +
              geom_boxplot(color = " slategray4 ",alpha =0.7) +
              labs (y = "Count",
                    x = "Hour",
                    title = "Boxplot de Count por Hour") +
              theme_bw () +
              theme(legend.position = "none")

plot2.4 <- ggplot(datos, aes(x = weekday, y = count, fill = weekday)) +
            geom_boxplot(color = " slategray4 ",alpha =0.7) +
            labs (y = "Count",
                  x = "Week Days",
                  title = "Boxplot de Count por Week Days") +
            theme_bw () +
            theme(legend.position = "none")

plot2.5 <- ggplot(datos, aes(x = month, y = count, fill = month)) +
            geom_boxplot(color = " slategray4 ",alpha =0.7) +
            labs (y = "Count",
                  x = "Months",
                  title = "Boxplot de Count por Months") +
            theme_bw () +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot2.6 <- ggplot(datos, aes(x = holiday, y = count, fill = holiday)) +
            geom_boxplot(color = " slategray4 ",alpha =0.7) +
            labs (y = "Count",
                  x = "Holiday",
                  title = "Boxplot de Count por Holiday") +
            theme_bw () +
            theme(legend.position = "none")

plot3 <- ggarrange(plot2.1,plot2.2,plot2.3,
                   plot2.4,plot2.5,plot2.6,
                   ncol=3,nrow=2,common.legend = FALSE)

annotate_figure(plot3, top = text_grob("Gráficos Boxplot de 2 dimensiones"))

rm(plot2.1,plot2.2,plot2.3,
   plot2.4,plot2.5,plot2.6)

rm(plot3)

# Resto Variables 

plot2.7 <- ggplot(datos, aes(x = temp,y = count))         +
            geom_point(color = " slategray4 ",alpha =0.7) +
            geom_smooth(method = "lm",col=" red")         +
            labs(y = "Count",
                 x = "Temperatura",
                 title = "Scatter Plot entre Count y Temperature")           +
            theme_bw ()

plot2.8 <- ggplot(datos, aes(x = atemp,y = count))         +
            geom_point(color = " slategray4 ",alpha =0.7) +
            geom_smooth(method = "lm",col=" red")         +
            labs(y = "Count",
                 x = "Feeling Temperature",
                 title = "Scatter Plot entre Count y Feeling Temp")           +
            theme_bw ()

plot2.9 <- ggplot(datos, aes(x = humidity,y = count))           +
            geom_point(color = " slategray4 ",alpha =0.7)       +
            geom_smooth(method = "lm",col=" red")               +
            labs(y = "Count",
                 x = "Humidity",
                 title = "Scatter Plot entre Count y Humidity") +
            theme_bw ()

plot2.10 <- ggplot(datos, aes(x = windspeed,y = count))         +
              geom_point(color = " slategray4 ",alpha =0.7) +
              geom_smooth(method = "lm",col=" red")         +
              labs(y = "Count",
                   x = "Wind speed",
                   title = "Scatter Plot entre Count y Wind speed")           +
              theme_bw ()


plot4 <- ggarrange(plot2.7,plot2.8,
                   plot2.9,plot2.10,
                   ncol=2,nrow=2,common.legend = FALSE)

annotate_figure(plot4, top = text_grob("Gráficos Scatter Plot de 2 dimensiones"))

rm(plot2.7,plot2.8,
   plot2.9,plot2.10)

rm(plot4)

# ----------------------------------------------------------------------------
# Casos Especiales 
# ----------------------------------------------------------------------------

plot3.1 <- ggplot(datos, aes(x = hour, y = count, fill = season)) +
            geom_boxplot(color = " slategray4 ",alpha =0.7) +
            labs (y = "Count",
                  x = "Hour",
                  title = "Season: Boxplot de Count por Hour") +
            theme_bw () +
            theme(legend.position = "none") + 
            facet_wrap(~season, nrow = 2)

plot3.2 <- ggplot(datos, aes(x = hour, y = count, fill = weather)) +
            geom_boxplot(color = " slategray4 ",alpha =0.7) +
            labs (y = "Count",
                  x = "Hour",
                  title = "Weather: Boxplot de Count por Hour") +
            theme_bw () +
            theme(legend.position = "none") + 
            facet_wrap(~weather, nrow = 2)

plot3.3 <- ggplot(filter(datos,windspeed>0), aes(x = windspeed,y = count))         +
              geom_point(color = " slategray4 ",alpha =0.7) +
              geom_smooth(method = "loess",col=" red")         +
              labs(y = "Count",
                   x = "Wind speed",
                   title = "Scatter Plot entre Count y Wind speed")           +
              theme_bw ()
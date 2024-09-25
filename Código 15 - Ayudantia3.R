# -----------------------------------------------------------------------------
#
# AYUDANTÍA 3 - MINERÍA DE DATOS CON R 
#
# Autor: Felipe Stuardo Muñoz
# Fecha: 16 de Septiembre 2024


# -------------------------------------------------------
# Instalación de librerías en R
# -------------------------------------------------------

# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("ggpubr")
# install.packages("gtsummary")
# install.packages("corrplot")

# Limpiar entorno
rm(list=ls())

# -------------------------------------------------------
# Librerías
# -------------------------------------------------------

library(readr)
library(dplyr)
library(datos)
library(ggplot2)
library(scales)
library(ggpubr)
library(gtsummary)
library(corrplot)

# -------------------------------------------------------
# Directorio de trabajo
# -------------------------------------------------------

setwd("C:/Users/fastu/OneDrive - uc.cl/UC/UC 2024-2/Minería de Datos con R - 2024-3/Ayudantía 3")
getwd()

#------------------------------------------------------------------------
#  1. COMPONENTES PRINCIPALES
#------------------------------------------------------------------------

# Utilizaremos los datos de diamantes.
?diamantes
View(diamantes)

tbl_summary(diamantes)

skimr::skim(select_all(diamantes))

#------------------------------------------------------------------------
# Realice componentes principales para reducir la dimensionalidad 
# entre quilates, profundidad, tabla y precio
#------------------------------------------------------------------------

#Componentes principales

pca_diam <- prcomp(select(diamantes,-c(corte,color,claridad,x,y,z)), scale = TRUE)
names(pca_diam)

#cargas 

pca_diam$rotation

#varianzas explicadas
pca_diam$sdev^2

prop_var <- pca_diam$sdev^2 / sum(pca_diam$sdev^2)
prop_var

ggplot(data = data.frame(prop_var, pc = 1:4),
       aes(x = pc, y = prop_var)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

#varianzas acumuladas

prop_var_acum <- cumsum(prop_var)
prop_var_acum

ggplot(data = data.frame(prop_var_acum, pc = 1:4),
       aes(x = pc, y = prop_var_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")


NuevasVariables <- data.frame(pca_diam$x)

# -------------------------------------------------------
# 2. Modelo Lineal
# -------------------------------------------------------

# Importamos el dataset
esperanza = read_delim("esperanza.txt",delim = " ")
print(esperanza)


#------------------------------------------------------------------------
# Realice análisis descriptivo mediante una tabla.
#------------------------------------------------------------------------

skimr::skim(select(esperanza, -obs))

# Tarea: Haga un análisis gráfico de las variables

#------------------------------------------------------------------------
# Realice análisis de correlación entre la variable Esperanza de vida y 
# el resto de las variables. Seleccione las variables que tienen 
# suficiente correlación para poder predecir la Esperanza de vida.
#------------------------------------------------------------------------

cor_matrix <- round(cor(select(esperanza, -obs)), 2)
View(cor_matrix)

corrplot(cor_matrix, method = "color", tl.cex = 0.7, addCoef.col = "black", number.cex = 0.5)

#excluyo correlaciones menores a 0.2 (con la variable objetivo)

esperanza_6 = subset(esperanza, select = c(esp_vida ,ingresos, analfabetismo, 
                                           asesinatos,universitarios, heladas))


#------------------------------------------------------------------------
# Realice análisis gráfico bivariado de las variables seleccionadas 
# anteriormente.
#------------------------------------------------------------------------

plotbi1 <- ggplot(esperanza_6, aes(ingresos,esp_vida))+ 
  geom_point(col="slategray4",
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Ingresos")+
  ylab("Esperanza de vida")  +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggtitle("Ingresos vs Esperanza de vida") +
  geom_smooth(method = "lm",se=FALSE) # agregamos la tendencia

plotbi2 <- ggplot(esperanza_6, aes(analfabetismo,esp_vida)) + 
  geom_point(col="slategray4",
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Analfabetismo") +
  ylab("Esperanza de vida") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggtitle("Analfabetismo vs Esperanza de vida") +
  geom_smooth(method = "lm",se=FALSE)

plotbi3 <- ggplot(esperanza_6, aes(asesinatos,esp_vida)) + 
  geom_point(col="slategray4",
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("asesinatos") +
  ylab("Esperanza de vida") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggtitle("Asesinatos vs Esperanza de vida") +
  geom_smooth(method = "lm",se=FALSE)

plotbi4 <- ggplot(esperanza_6, aes(universitarios,esp_vida)) + 
  geom_point(col="slategray4",
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Universitarios") +
  ylab("Esperanza de vida") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggtitle("Universitarios vs Esperanza de vida") +
  geom_smooth(method = "lm",se=FALSE)

plotbi5 <- ggplot(esperanza_6, aes(heladas,esp_vida)) + 
  geom_point(col="slategray4",
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Heladas") +
  ylab("Esperanza de vida") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggtitle("Heladas vs Esperanza de vida") +
  geom_smooth(method = "lm",se=FALSE)


grafico3 <- ggarrange(plotbi1,plotbi2,plotbi3,plotbi4,plotbi5,ncol=3, nrow=2)
annotate_figure(grafico3, top = "Variables Cuantitativas")


#------------------------------------------------------------------------
# Realice 3 modelos lineales. Compárelos y seleccione el mejor.
#------------------------------------------------------------------------

modelo = lm(esp_vida ~ ingresos + analfabetismo + asesinatos + universitarios + heladas, data = esperanza_6)
summary(modelo)

modelo2 = lm(esp_vida ~ ingresos  + asesinatos + universitarios+ heladas, data = esperanza_6)
summary(modelo2)

modelo3 = lm(esp_vida ~ asesinatos + universitarios + heladas, data = esperanza_6)
summary(modelo3)

#------------------------------------------------------------------------
# Calcule los valores de Esperanza de vida estimada con los modelos 
# realizados anteriormente. Compárelos gráficamente.
#------------------------------------------------------------------------

forecast.modelo1 = predict(modelo,esperanza_6)
forecast.modelo2 = predict(modelo2,esperanza_6)
forecast.modelo3 = predict(modelo3,esperanza_6)

# R cuadrado es porcentaje de la variabilidad explicado por las dimensiones independientes
# sobre el fenomeno que se está analizando

contrasta_mod1 <- ggplot(esperanza_6) + 
  geom_point(aes(forecast.modelo1,esp_vida),
             fill = "lightblue", 
             alpha = 0.5, 
             size = 3) +
  labs(x = "esperanza vida estimada",
       y = "esperanza vida real") +
  ggtitle("Esperanza vida real vs estimada modelo1")


contrasta_mod2 <- ggplot(esperanza_6) + 
  geom_point(aes(forecast.modelo2,esp_vida),
             fill = "lightblue", 
             alpha = 0.5, 
             size = 3) +
  labs(x = "esperanza vida estimada",
       y = "esperanza vida real") +
  ggtitle("Esperanza vida real vs estimada modelo2")


contrasta_mod3 <- ggplot(esperanza_6) + 
  geom_point(aes(forecast.modelo3,esp_vida),
             fill = "lightblue", 
             alpha = 0.5, 
             size = 3) +
  labs(x = "esperanza vida estimada",
       y = "esperanza vida real") +
  ggtitle("Esperanza vida real vs estimada modelo3")

grafico_contraste <- ggarrange(contrasta_mod1,contrasta_mod2,contrasta_mod3,ncol=2, nrow=2)
annotate_figure(grafico_contraste, top = "Comparación 3 ajustes")                           

#------------------------------------------------------------------------
###### EJEMPLO MODELO LINEAL + PCA con este dataset
#------------------------------------------------------------------------

pca_esp <- prcomp(select(esperanza,-esp_vida), scale = TRUE)

#cargas 

pca_esp$rotation

#varianzas explicadas

prop_var_esp <- pca_esp$sdev^2 / sum(pca_esp$sdev^2)
prop_var_esp

#varianzas acumuladas

prop_var_acum_esp <- cumsum(prop_var_esp)
prop_var_acum_esp

ggplot(data = data.frame(prop_var_acum_esp, pc = 1:9),
       aes(x = pc, y = prop_var_acum_esp, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")


pc_esp <- data.frame(pca_esp$x)

esperanza_pca <- cbind(esperanza, pc_esp)
View(esperanza_pca)

modelo_lm_pca = lm(esp_vida ~ PC1 + PC2 + PC3 + PC4, data = esperanza_pca)
summary(modelo_lm_pca)

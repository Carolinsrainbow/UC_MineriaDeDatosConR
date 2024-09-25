# -------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Regresón lineal 
# ------------------------------------------------------------------------- 

# -------------------------------------------------------
# instalación de librerías en R
# -------------------------------------------------------

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("ggpubr")
# install.packages("gtsummary")

# -------------------------------------------------------
# librerías
# -------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(gtsummary)

# -------------------------------------------------------
# Configuración directorio de trabajo
# -------------------------------------------------------

#setwd("C:/Users/ctvas/Documentos Cristian Vásquez/05. Magíster en BA/05. Script Clases/05. Material Complementario Videos/01. Datos")
setwd("aquí debe indicar la carpeta donde se encuentran los datos")
getwd()

# -------------------------------------------------------
# Importar datos en formato XLSX
# -------------------------------------------------------

epf = read_excel("gastos_EPF.xlsx")
epf = as_tibble(epf)
print(epf)

# -------------------------------------------------------
# Transformación de variables categóricas
# -------------------------------------------------------

epf$sexo    <- as.factor(epf$sexo)
epf$zona    <- as.factor(epf$zona)
epf$tvp     <- as.factor(epf$tvp)
epf$ecivil  <- as.factor(epf$ecivil)

# -------------------------------------------------------
# Estadísticas descriptivas
# -------------------------------------------------------

skimr::skim(select(epf,ingreso,gastos,npersona,edad))
tbl_summary(select(epf,sexo,zona,tvp,ecivil))


# -------------------------------------------------------
# Gráficos de cada dimensión cuantitativa
# -------------------------------------------------------

plot1 <- ggplot(epf, aes(gastos))                           +
  geom_histogram(alpha  = 0.7, 
                 color  ="black",
                 bins   = nclass.Sturges(epf$gastos), 
                 fill   = "slategray4")                   +
  xlab("Gastos no comprometidos")                         +
  ylab("Frecuencia")                                      +
  scale_x_continuous(labels = comma)                        

plot2 <- ggplot(epf, aes(ingreso))                          +
  geom_histogram(alpha  = 0.7, 
                 color  ="black",
                 bins   = nclass.Sturges(epf$ingreso), 
                 fill   = "slategray4")                   +
  xlab("Ingresos")                                        +
  ylab("Frecuencia")                                      +
  scale_x_continuous(labels = comma)

plot3 <- ggplot(epf, aes(npersona))                         +
  geom_histogram(alpha  = 0.7, 
                 color  ="black",
                 bins   = 9, 
                 fill   = "slategray4")                   +
  xlab("Componentes de la unidad familiar")               +
  ylab("Frecuencia")                                      +
  scale_x_continuous(labels = comma)

plot4 <- ggplot(epf, aes(edad))                             +
  geom_histogram(alpha  = 0.7, 
                 color  ="black",
                 bins   = nclass.Sturges(epf$edad), 
                 fill   = "slategray4")                   +
  xlab("Edad")                                            +
  ylab("Frecuencia")                                      +
  scale_x_continuous(labels = comma)

grafico1 <- ggarrange(plot1,plot2,plot3, plot4, ncol=2, nrow=2)
annotate_figure(grafico1, top = "Variables Cuantitativas")

rm(plot1,plot2,plot3, plot4,grafico1)

# -------------------------------------------------------
# Gráficos de cada dimensión cualitativa
# -------------------------------------------------------

# Agregar etiquetas

labels_tvp <- c("No responde","Propia pagada","Propia pagándose",
                "Arrendada cont", "Arrendada sin con",
                "Cedida trabajo", "Cedida familiar",
                "En litigio", "Herencia")

labels_ecivil <- c("Casado","AUC","Soltero",
                   "Separado","Divorciado","Anulado",
                   "Viudo")

epf$sexo    <- factor(epf$sexo, labels = c("hombre","mujer"))
epf$tvp     <- factor(epf$tvp, labels = labels_tvp)
epf$ecivil  <- factor(epf$ecivil, labels = labels_ecivil)


plot5 <- ggplot(epf, aes(sexo))         +
  geom_bar(alpha  = 0.7, 
           color  ="black",
           fill   = "slategray4")     +
  xlab("Sexo")                        +
  ylab("Frecuencia")                  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot6 <- ggplot(epf, aes(tvp))          +
  geom_bar(alpha  = 0.7, 
           color  ="black",
           fill   = "slategray4")     +
  xlab("Tenencia de la vivienda")     +
  ylab("Frecuencia")                  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot7 <- ggplot(epf, aes(zona))          +
  geom_bar(alpha  = 0.7, 
           color  ="black",
           fill   = "slategray4")     +
  xlab("Zona")                        +
  ylab("Frecuencia")                  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))                  


plot8 <- ggplot(epf, aes(ecivil))       +
  geom_bar(alpha  = 0.7, 
           color  ="black",
           fill   = "slategray4")     +
  xlab("Estado Civil")                +
  ylab("Frecuencia")                  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grafico2 <- ggarrange(plot5,plot6,plot7, plot8, ncol=2, nrow=2)
annotate_figure(grafico2, top = "Variables Cualitativas")

rm(labels_tvp,labels_ecivil)
rm(plot5,plot6,plot7, plot8,grafico2)

# -------------------------------------------------------
# Análisis Bivariado "Correlación"
# -------------------------------------------------------

Variables   = c()
Correlacion = c()

Variables[1]   = "ingreso"
Correlacion[1] = cor(epf$gastos,epf$ingreso)

Variables[2]   = "npersona"
Correlacion[2] = cor(epf$gastos,epf$npersona)

Variables[3]   = "edad"
Correlacion[3] = cor(epf$gastos,epf$edad)

Variables[4]   = "sexo"
Correlacion[4] = sqrt(summary(lm(gastos ~ sexo, data = epf))$r.squared)

Variables[5]   = "zona"
Correlacion[5] = sqrt(summary(lm(gastos ~ zona, data = epf))$r.squared)

Variables[6]   = "ecivil"
Correlacion[6] = sqrt(summary(lm(gastos ~ ecivil, data = epf))$r.squared)

Variables[7]   = "tvp"
Correlacion[7] = sqrt(summary(lm(gastos ~ tvp, data = epf))$r.squared)

Bivariado = tibble(Variable = Variables, Correlacion = Correlacion) %>% 
  arrange(desc(Correlacion))
rm(Variables, Correlacion)


# -------------------------------------------------------
# Análisis Bivariado "Gráficos"
# -------------------------------------------------------

plot1 <- ggplot(epf, aes(ingreso,gastos))                               + 
  geom_point(col="slategray4",
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3)                                        +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Ingreso")                                   +
  ylab("Gastos no comprometidos")                             +
  scale_x_continuous(labels = comma)                          +
  scale_y_continuous(labels = comma)                          +
  ggtitle("Ingresos vs Gastos")                               +
  geom_smooth(method = "lm")

plot2 <- ggplot(epf, aes(npersona,gastos))                              + 
  geom_point(col="slategray4",
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3)                                        +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Componentes unidad familiar")                         +
  ylab("Gastos no comprometidos")                             +
  scale_y_continuous(labels = comma)                          +
  ggtitle("Personas vs Gastos")                               +
  geom_smooth(method = "lm")

plot3 <- ggplot(epf, aes(edad,gastos))                          + 
  geom_point(col="slategray4",
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3)                                        +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Edad")                                                +
  ylab("Gastos no comprometidos")                             +
  scale_y_continuous(labels = comma)                          +
  ggtitle("Edad vs Gastos")                               +
  geom_smooth(method = "lm")


grafico3 <- ggarrange(plot1,plot2,plot3, ncol=3, nrow=1)
annotate_figure(grafico3, top = "Variables Cuantitativas")

rm(plot1,plot2,plot3,grafico3)

plot4 <- ggplot(epf, aes(sexo,gastos))                                  + 
  geom_boxplot(fill="slategray4",
               alpha = 0.2)                                   +
  stat_boxplot(geom = "errorbar", width = 0.3)                +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Sexo")                                                +
  ylab("Ingreso")                                             +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) 	+
  ggtitle("Sexo vs Gastos")                                   

plot5 <- ggplot(epf, aes(tvp,gastos))                                  + 
  geom_boxplot(fill="slategray4",
               alpha = 0.2)                                   +
  stat_boxplot(geom = "errorbar", width = 0.3)                +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Tenencia de la vivienda")                                                +
  ylab("Ingreso")                                             +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) 	+
  ggtitle("TVP vs Gastos") 

plot6 <- ggplot(epf, aes(zona,gastos))                                  + 
  geom_boxplot(fill="slategray4",
               alpha = 0.2)                                   +
  stat_boxplot(geom = "errorbar", width = 0.3)                +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Zona")                                                +
  ylab("Ingreso")                                             +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) 	+
  ggtitle("Zona vs Gastos")

plot7 <- ggplot(epf, aes(ecivil,gastos))                                  + 
  geom_boxplot(fill="slategray4",
               alpha = 0.2)                                   +
  stat_boxplot(geom = "errorbar", width = 0.3)                +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Estado civil")                                                +
  ylab("Ingreso")                                             +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) 	+
  ggtitle("Estado civil vs Gastos")


grafico4 <- ggarrange(plot4,plot5,plot6,plot7,ncol=2, nrow=2)
annotate_figure(grafico4, top = "Variables Cuanlitativas")

rm(plot4,plot5,plot6,plot7,grafico4)

aggregate(gastos ~ sexo, mean,data = epf)
aggregate(gastos ~ tvp, mean,data = epf)
aggregate(gastos ~ zona, mean,data = epf)
aggregate(gastos ~ ecivil, mean,data = epf)

# -------------------------------------------------------
# Corrección análisis bivariado
# -------------------------------------------------------

aggregate(gastos ~ tvp, mean,data = epf) %>% arrange(gastos)
aggregate(gastos ~ ecivil, mean,data = epf) %>% arrange(gastos)

levels(epf$tvp)     <- c("G1.h/nr/asc","G4.prop pagada","G5.prop pagándose", 
                         "G2.acc/ct","G1.h/nr/asc","G2.acc/ct",
                         "G3.lit/cf","G3.lit/cf","G1.h/nr/asc")  

levels(epf$ecivil)  <- c("G4.Cas","G2.AUC/sep","G1.sol/anu/viu",
                         "G2.AUC/sep","G3.Div","G1.sol/anu/viu",
                         "G1.sol/anu/viu")

plot8 <- ggplot(epf, aes(ecivil,gastos))                                  + 
  geom_boxplot(fill="slategray4",
               alpha = 0.2)                                   +
  stat_boxplot(geom = "errorbar", width = 0.3)                +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Estado civil")                                                +
  ylab("Ingreso")                                             +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) 	+
  ggtitle("Estado civil vs Gastos")

plot9 <- ggplot(epf, aes(tvp,gastos))                                  + 
  geom_boxplot(fill="slategray4",
               alpha = 0.2)                                   +
  stat_boxplot(geom = "errorbar", width = 0.3)                +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  xlab("Tenencia vivienda")                                                +
  ylab("Ingreso")                                             +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) 	+
  ggtitle("Tenencia vivienda vs Gastos")

grafico5 <- ggarrange(plot8,plot9,ncol=2, nrow=1)
annotate_figure(grafico5, top = "Agrupaciones ECIVIL y TVP")

epf$ecivil <- relevel(epf$ecivil,"G4.Cas")
epf$tvp    <- relevel(epf$tvp,"G1.h/nr/asc")


# -------------------------------------------------------
# Ajuste regresión lineal 
# -------------------------------------------------------

set.seed(12345)

n     = round(nrow(epf)*0.8)
train = sample(nrow(epf),n)

modelo <- lm(gastos ~ ingreso + npersona + edad + sexo +
               zona + tvp + ecivil, data = epf, subset = train)
summary(modelo)

modelo <- lm(gastos ~ ingreso + zona + tvp + ecivil - 1, data = epf, subset = train)
summary(modelo)


# -------------------------------------------------------
# Consistencia del ajuste
# -------------------------------------------------------

epf.train = epf[train,]
epf.valid = epf[-train,]

forecast.train = predict(modelo,epf[train,])
forecast.valid = predict(modelo,epf[-train,])

R2.train = 1 - var((epf[train,]$gastos-forecast.train))/var(epf[train,]$gastos)
R2.valid = 1 - var((epf[-train,]$gastos-forecast.valid))/var(epf[-train,]$gastos)

R2.train
R2.valid


# -------------------------------------------------------
# Gráfico de ajuste
# -------------------------------------------------------

epf.train$forecast = forecast.train
epf.valid$forecast = forecast.valid

colors <- c("Observado" = "slategray4", "Predicho" = "blue")

plot10 <- ggplot(epf.train)                                     + 
  geom_point(aes(ingreso,gastos,color = "Observado"),
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3)                                        +
  geom_point(aes(ingreso,forecast, color ="Predicho"),
             fill = "blue", 
             alpha = 0.2, 
             size = 3)                                        +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  labs(x = "Ingreso",
       y = "Gastos no comprometidos",
       color = "Gastos")                                      +
  scale_color_manual(values = colors)                         +
  scale_x_continuous(labels = comma)                          +
  scale_y_continuous(labels = comma)                          +
  ggtitle("Train: Gastos observado y predicho")

plot11 <- ggplot(epf.valid)                                     + 
  geom_point(aes(ingreso,gastos,color = "Observado"),
             fill = "slategray4", 
             alpha = 0.2, 
             size = 3)                                        +
  geom_point(aes(ingreso,forecast, color ="Predicho"),
             fill = "blue", 
             alpha = 0.2, 
             size = 3)                                        +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    +
  labs(x = "Ingreso",
       y = "Gastos no comprometidos",
       color = "Gastos")                                      +
  scale_color_manual(values = colors)                         +
  scale_x_continuous(labels = comma)                          +
  scale_y_continuous(labels = comma)                          +
  ggtitle("Valid: Gastos observado y predicho")

grafico6 <- ggarrange(plot10,plot11,ncol=2, nrow=1)
annotate_figure(grafico6, top = "Ajuste en ambas muestras")                           



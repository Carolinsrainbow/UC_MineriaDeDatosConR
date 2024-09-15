# -------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Clasificación de los cliente en un Retail utilizando K-Medias 
# ------------------------------------------------------------------------- 

# -------------------------------------------------------
# instalación de librerías en R
# -------------------------------------------------------

# install.packages("ISLR")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tibble")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("skimr")
# install.packages("factoextra")

# -------------------------------------------------------
# librerías
# -------------------------------------------------------

library(ISLR)
library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(plotly)
library(factoextra)

# -------------------------------------------------------
# Configuración directorio de trabajo
# -------------------------------------------------------

#setwd("C:/Users/ctvas/Documentos Cristian Vásquez/05. Magíster en BA/05. Script Clases/05. Material Complementario Videos/01. Datos")
setwd("aquí debe indicar la carpeta donde se encuentran los datos")
getwd()

# -------------------------------------------------------
# Importar datos desde excel
# -------------------------------------------------------

datos <- read_excel("Online Retail.xlsx")
datos
str(datos)

# -------------------------------------------------------
# Construcción dimensiones RFM
# -------------------------------------------------------

datos.segmentar <- datos										                      %>%
  mutate(InvoiceDate = as.Date(InvoiceDate)) 	  %>%
  group_by(CustomerID)                          %>%
  summarise(Monto	   = sum(UnitPrice*Quantity),
            Frecuencia  = n_distinct(InvoiceNo))

fecha.minima		<- max(as.Date(datos$InvoiceDate))

datos.recencia	<- datos												                        %>%
  mutate(Diff.Date = difftime(fecha.minima, 
                              as.Date(InvoiceDate), 
                              units = "days") )		                        %>%
  group_by(CustomerID)								                %>%
  summarise(Recencia = as.numeric(min(Diff.Date)))	  %>%
  select(CustomerID,Recencia)

datos.finales		<- datos.segmentar %>%
  left_join(datos.recencia, by = "CustomerID")	


datos.finales

# -------------------------------------------------------
# Estadísticas dimensiones RFM
# -------------------------------------------------------

skimr::skim(select(datos.finales,-CustomerID))

# -------------------------------------------------------
# Datos extremos
# -------------------------------------------------------

fig1 <- plot_ly(datos.finales, y = ~ Monto, type = "box", name = "Monto", boxpoints = "all")
fig2 <- plot_ly(datos.finales, y = ~ Frecuencia, type = "box", name = "Frecuencia", boxpoints = "all")
fig3 <- plot_ly(datos.finales, y = ~ Recencia,  type = "box", name = "Recencia", boxpoints = "all")
fig <- subplot(fig1,fig2,fig3)
fig

# -------------------------------------------------------
# Proceso de limpieza
# -------------------------------------------------------

LimInfMonto = quantile(datos.finales$Monto,probs = 0.25) - 1.5*IQR(datos.finales$Monto)
LimSupMonto = quantile(datos.finales$Monto,probs = 0.75) + 1.5*IQR(datos.finales$Monto)

LimInfFrec = quantile(datos.finales$Frecuencia,probs = 0.25) - 1.5*IQR(datos.finales$Frecuencia)
LimSupFrec = quantile(datos.finales$Frecuencia,probs = 0.75) + 1.5*IQR(datos.finales$Frecuencia)

LimInfRecen = quantile(datos.finales$Recencia,probs = 0.25) - 1.5*IQR(datos.finales$Recencia)
LimSupRecen = quantile(datos.finales$Recencia,probs = 0.75) + 1.5*IQR(datos.finales$Recencia)

datos.finales <- datos.finales %>%
  filter(Monto >= LimInfMonto, Monto <= LimSupMonto,
         Frecuencia  >= LimInfFrec, Frecuencia <= LimSupFrec,
         Recencia	  >= LimInfRecen, Recencia  <= LimSupRecen)

skimr::skim(select(datos.finales,-CustomerID))


# -------------------------------------------------------
# Nuevo gráfico 
# -------------------------------------------------------

fig1 <- plot_ly(datos.finales, y = ~ Monto, type = "box", name = "Monto", boxpoints = "all")
fig2 <- plot_ly(datos.finales, y = ~ Frecuencia, type = "box", name = "Frecuencia", boxpoints = "all")
fig3 <- plot_ly(datos.finales, y = ~ Recencia,  type = "box", name = "Recencia", boxpoints = "all")
fig <- subplot(fig1,fig2,fig3)
fig


# -------------------------------------------------------
# Estandarizar variables
# -------------------------------------------------------


datosf.centrados  <- scale(select(datos.finales,-CustomerID), center = TRUE, 
                           scale = TRUE)
round(head(datosf.centrados,10),3)

# -------------------------------------------------------
#  Preparación de los datos
# -------------------------------------------------------

datosf.centrados           <- data.frame(datosf.centrados)
rownames(datosf.centrados) <- datos.finales$CustomerID
round(head(datosf.centrados,10),3)


# -------------------------------------------------------
#  Distancia euclidean
# -------------------------------------------------------

mat_dist <- dist(x = datosf.centrados[1:30,], method = "euclidean")
round(as.matrix(mat_dist)[1:5, 1:5], 2)

# -------------------------------------------------------
#  Distancia manhattan
# -------------------------------------------------------

mat_dist <- dist(x = datosf.centrados[1:30,], method = "manhattan")
round(as.matrix(mat_dist)[1:5, 1:5], 2)

# -------------------------------------------------------
#  Distancia Correlación
# -------------------------------------------------------

mat_dist <- get_dist(x = datosf.centrados[1:30,], method = "pearson")
round(as.matrix(mat_dist)[1:5, 1:5], 2)


# -------------------------------------------------------
#  Heatmaps de la matriz de distancia
# -------------------------------------------------------

mat_dist1 <- dist(x = datosf.centrados[1:50,], method = "euclidean")
mat_dist2 <- dist(x = datosf.centrados[1:50,], method = "manhattan")
mat_dist3 <- get_dist(x = datosf.centrados[1:50,], method = "pearson")

plot1 <- fviz_dist(dist.obj = mat_dist1, lab_size = 5) +
  ggtitle("Matriz de Distancia Euclidiana")
ggplotly(plot1)


plot2 <- fviz_dist(dist.obj = mat_dist2, lab_size = 5) +
  ggtitle("Matriz de Distancia Manhattan")
ggplotly(plot2)

plot3 <- fviz_dist(dist.obj = mat_dist3, lab_size = 5) +
  ggtitle("Matriz de Distancia de Correlación")
ggplotly(plot3)


# -------------------------------------------------------
#  Número óptimo de grupos
# -------------------------------------------------------

fviz_nbclust(x = datosf.centrados, FUNcluster = kmeans, method = "wss", k.max = 25, 
             diss = get_dist(datosf.centrados, method = "euclidean"), nstart = 50) +
  labs(title = "Método Elbow - Número óptimo de conglomerados") +
  theme_grey()


# -------------------------------------------------------
#  Aplicación del algoritmo de k-medias
# -------------------------------------------------------

set.seed(12345)
km_clusters <- kmeans(x = datosf.centrados, centers = 4, nstart = 50)
km_clusters

resultado   <- cbind(datosf.centrados, cluster = km_clusters$cluster)
head(resultado,10)

# -------------------------------------------------------
#  Gráficos de los segmentos
# -------------------------------------------------------

pcaMyData <- princomp(datosf.centrados)
summary(pcaMyData)

datosfinal <- data.frame(datosf.centrados,pcaMyData$scores, factor(km_clusters$cluster))
datosfinal <- transform(datosfinal, cluster_name = paste("Cluster",km_clusters$cluster))

p <- plot_ly(datosfinal, x = datosfinal$Comp.1 , y = datosfinal$Comp.2, 
             text = datosfinal$CustomerID,mode = "markers", color = datosfinal$cluster_name, 
             marker = list(size = 11)) 

p <- layout(p, title = "PCA Clusters from K-Means Clustering", 
            xaxis = list(title = "Axis Title X"),
            yaxis = list(title = "Axis Title Y"))

p

# -------------------------------------------------------
#  Otro gráfico más sencillo
# -------------------------------------------------------

datos.clasificados <- cbind(datos.finales,cluster = km_clusters$cluster)


p <- plot_ly(datos.clasificados, 
             x = datos.clasificados$Frecuencia, 
             y = datos.clasificados$Recencia,
             z = datos.clasificados$Monto,
             text = datos.clasificados$CustomerID,
             mode = "markers", 
             color = datosfinal$cluster_name, 
             marker = list(size = 5)) 

p <- layout(p, title = "Dimensiones RFM K-Means Clustering", 
            scene = list(xaxis = list(title = "Frecuencia"),
                         yaxis = list(title = "Recencia"),
                         zaxis = list(title = "Monto")))

p

#--------------------------------------------------------------
# Descripción de los grupos
#--------------------------------------------------------------

aggregate(cbind(Monto,Frecuencia,Recencia)~ cluster, 
          data = datos.clasificados, mean)




round(prop.table(table(datos.clasificados$cluster)),3)

x     <- c('Grupo 1', 'Grupo 2', 'Grupo 3', 'Grupo 4')
y     <- as.numeric(table(datos.clasificados$cluster))
text  <- c('38% market share', '24% market share', '13% market share','25% market share')
data  <- data.frame(x, y, text)

fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
               text = y, textposition = 'auto',
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% layout(title = "K-means Report",
                      xaxis = list(title = ""),
                      yaxis = list(title = ""))

fig

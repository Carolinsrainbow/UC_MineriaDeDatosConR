#--------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian V?squez
# Curso       : Miner?a de datos con R 
# Archivo 		: Conceptos b?sicos del lenguaje R 
# Descripci?n	: En este tutorial se presentan conceptos y funciones b?sicas
#               que el usuario de R debe conocer para manipular objetos
#-------------------------------------------------------------------------- 


# ----------------------------------------------------------------------------
# Asignaci?n de objetos
# ----------------------------------------------------------------------------

x = 1:10
x

x <- 1:10
x

# ----------------------------------------------------------------------------
#
# 1.0 Funciones B?sicas de R
#
# ----------------------------------------------------------------------------



# ----------------------------------------------------------------------------
# Funciones para vectores
# ----------------------------------------------------------------------------

x = seq(-5,5,1.5)
x[1]
x[1:4]

1:10

rep(3,5)

# operaciones b?sicas sobre vectores

sumas  = c(1,2,3,4) + c(1,2,3,0)
sumas

multi  = c(1,2,3,4)*c(2,2,2,1)
multi

poten  = (1:5)^2
poten

length(c(1,2,3,4,5,5,6))

suma = sum(c(1,2,3,4,5,6))
suma

# ----------------------------------------------------------------------------
# Funciones para matrices
# ----------------------------------------------------------------------------


### Funciones b?sicas sobre matrices

matrix1 <- matrix(1:6,2,3, byrow = TRUE)
matrix1

matrix1[1,3]
matrix1[2,1:2]

t(matrix1)

diag(matrix1)

matrix2 <- diag(c(1,2,3,4))
matrix2

traza = sum(diag(matrix1))
traza

### operaciones b?sicas sobre matrices

matrix1 <- matrix(1:6,2,3, byrow = TRUE)
matrix1

matrix2 <- matrix(c(rep(1,3),rep(2,3)),2,3, byrow = TRUE)
matrix2

matrix1 + matrix2  ## suma

lambda = 5

matrix3 <- lambda * matrix1
matrix3

matrix4 <- matrix1^lambda
matrix4

# ----------------------------------------------------------------------------
# Funciones para dataframe
# ----------------------------------------------------------------------------

vector1 			<- c(1, 2, 3, 4)
vector2 			<- c("rojo", "blanco", "rojo", "rojo") 			
vector3 			<- c(TRUE, TRUE, TRUE, FALSE)
new.data.frame 	<- data.frame(ID2 = vector1, Color = vector2, Passed = vector3)
new.data.frame

names(new.data.frame)

print(Color)

attach(new.data.frame)

print(Color)

data1	<- subset(new.data.frame, select = c(Color,ID2))
data1

data2 <- subset(new.data.frame, Color == "rojo")
data2

data3 <- subset(new.data.frame, Color == "rojo", select = c(ID2, Passed))
data3


# ----------------------------------------------------------------------------
#
# 2.0 Programaci?n condicional e iterativa
#
# ----------------------------------------------------------------------------

precio = c(100,200,300,400,500,600) # media = 350

if(precio[1] > 350){  
    valor <- "precio sobre 350"
  } else {
    valor <- "precio bajo la media"  
  }
valor


x <- c(-2, -1, 1, 2)
ifelse(x >= 0, "Positivo", "Negativo")

ifelse(x >= 0, exp(x), x-5)



resultado = c()

for(i in 1:6){
  resultado[i] = ifelse(precio[i] > 350, "precio mayor a 350", "precio menos a 350")
}
resultado

# ----------------------------------------------------------------------------
#
# 3.0 Creaci?n de funciones
#
# ----------------------------------------------------------------------------


Estadistica <-function(x,medida = "media"){
  if(medida == "media"){
    return(mean(x))
  }
  if(medida == "mediana"){
    return(median(x))
  }
  if(medida == "varianza"){
    return(var(x))
  }
  if(medida == "maximo"){
    return(max(x))
  }
}

Estadistica(precio, "media")
Estadistica(precio, "varianza")
Estadistica(precio, "mediana")
Estadistica(precio, "maximo")

nuevoprecio = c(100,200,300,400,500,600, NA)# media = 350

Resumen = function(x){
    y      = na.omit(x)
    minimo = min(y)
    maximo = max(y)
    media  = mean(y)
    num_na = sum(is.na(x))
    return(list = c(minimo = minimo, maximo = maximo, media= media,
                      num_na = num_na))
}
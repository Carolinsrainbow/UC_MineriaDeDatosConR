# -------------------------------------------------------------------------
# Fecha		    : Septiembre 2022
# Autor		    : Cristian Vásquez
# Curso       : Minería de datos con R 
# Archivo 		: Principales objetos y funciones de R
# ------------------------------------------------------------------------- 

# ----------------------------------------------------------------------------
# Objetos básicos de R
# ----------------------------------------------------------------------------

# vectores

numeric.vector    <- c(-2, -1, 0, 1.2, -5) 
numeric.vector

character.vector  <- c("curso", "minería", "de", "datos", "con", "R") 
character.vector

logical.vector    <- c(FALSE, TRUE, TRUE, TRUE, FALSE)
logical.vector

# matrices

uno.matrix <- matrix( c(1, 2, 3, 4, 5, 6),  nrow = 2,  ncol = 3, byrow = TRUE) 
uno.matrix

dos.matrix <- matrix(as.character(1:9), nrow = 3, ncol = 3)
dos.matrix

# arreglos

uno.array <- array(c(1:4, 11:14, 21:24), dim = c(2, 2, 3))
uno.array

# listas

lista <- list(12345, c("variables","charater"), dos.matrix)
lista

# data frames

vector1 <- c(1, 2, 3, 4)
vector2 <- c("red", "white", "red", NA) 
vector3 <- c(TRUE, TRUE, TRUE, FALSE)

new.data.frame <- data.frame(vector1, vector2, vector3)
new.data.frame

#--------------------------------------------------------------------------------------------
#Este archivo no esta pensado para ejecutarse si no como ser una guia para futuros trabajos de modelado
#Tambien este archivo contendra las cosas mas ultiles de las primera 4 practicas realizadas en la materia de procesos estocasticos
#--------------------------------------------------------------------------------------------

#Cosas Utiles para el trabajo con matrices, como observacion sera util para modelar la matriz de transicion 
library(matrixcalc)
install.packages("matrixcalc")

library(matrixcalc)
Ainv2 <- matrix.inverse(A)

Ainv2 == Ainv

# Multiplicando A por la inversa de A
Ainv%*%A

# Ahora al revés
A%*%Ainv

# Calculemos los valores y vectores propios de A
Lambda <- eigen(A)

# Imprimimos los valores
print(Lambda$values[1])
print(Lambda$values[2])

# Verifica que sí son valores propios
det(Lambda$values[1]*diag(2) - A)
det(Lambda$values[2]*diag(2) - A)

# Imrpimimos los vectores
print(Lambda$vectors[, 1])
print(Lambda$vectors[, 2])

# # Verifica que los vectores propios y valores propios satisfacen que Av - lambda v = 0
Lambda$values[1]*Lambda$vectors[, 1] - A%*%Lambda$vectors[, 1]
Lambda$values[2]*Lambda$vectors[, 2] - A%*%Lambda$vectors[, 2]


#-------------------------------------------------------------
#Cosas a considerar
#Dataframes
#Listas
#Manejo de csv o db
#-------------------------------------------------------------



#aprende a usar ggplot, tienes unas practicas con mtcars, para poder practicar, igual para estadistica te va a funcionar
library(ggplot2)

#Forma mas facil para graficar los datos actuales, solo compara las dos columans dadas
qplot(mtcars$wt, mtcars$hp)

# Revisar los paquetes actuales
search()








#--------------------------------------------------------------------------------------------
#No borres el archivo Practica 1 tiene la primera parte del modelado de cadenas de markov y numero aleatoriaos
#pero aqui hare un resumen de la clase
#--------------------------------------------------------------------------------------------

#Tarea por hacer y para entenderle mejor, has la grafica de una cadena de Markov, me queda duda en lo que significa la grafica, 
#no se si es  como va progresando en tiempo o si son todos los estados que pueden tomar



#--------------------------------------------------------------------------------------------
#Igual la practica 2 tienen lo que te preguntabas de CM solo que mas desarrlloda "Sin tanta basura sobre definir las va"
#--------------------------------------------------------------------------------------------









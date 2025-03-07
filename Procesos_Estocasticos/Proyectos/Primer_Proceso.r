#//////////////////////////////////////////////////////////////////////////////
#Peque;o proyecto para modelar procesos estocasticos
#//////////////////////////////////////////////////////////////////////////////
install.packages("ggplot2")#Instalamos la libreria ggplot2 para hacer graficas

#Cosaspor hacer  para este primer examen -------------------------------------------------------s


#Probar como funciona la varaiable aleatoria continua

va_uniforme_continua <- function(a,b, n){ #a y b son los limites de la variable aleatoria continua y n es el numero de datos que quiero
  return(runif(n, min = a, max = b))
}

#Haciendo una prueba de esta variable aleatoria
vector <- va_uniforme_continua(1,2,10)

print(vector)

#Hacer un dado cargado




#Hacer la funcion para cualquier proceso estocastico 




#Hacer que cualquier matriz la pueda elevar a cualquier potencia que yo quiera

#---Para hacer esto usa el teorema de diagonalizacion de matrices para simplificar los pasos  A = QDQ^(-1)

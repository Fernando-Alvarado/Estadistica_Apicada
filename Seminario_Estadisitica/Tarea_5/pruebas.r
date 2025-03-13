# Usando la funcion sapply 



prueba <- list(c(20, 53, 44, 78, 22, 10, 40, 31, 59, 34, 21, 54,1,1,1,11))


#Funcion que nos saca los n minimos sin repetir de un una lista, para que funciones debe estar en un formato normal, la lista no debe de tener nombres en esus elementos y debe
#ser pasada por una funcion que le cambie el formato para que solo tenga un vector dentro de la lista, si no, no funciona
minimo = function(list, n, exit){#
 if(n==0 || length(list)==0){
    return(exit)
 }else {
    exit = c(exit, min(list))  #Haciendo nuestro vector de salida con los numero mas pequeños
    list = list[list!=min(list)] #Quitando el numero minimo de la lista, aunque esten repetidos
    return(minimo(list, n-1, exit))
    
 }
}

formato <- function(lista){
    listaSal <- unlist(lista)
    return(list(listaSal))
}




#print(minimo(prueba[[1]], 3, c()))

posib_pot <- seq(.1,3,.1)
#Ahora, podemos ver por las posibles potencias de TV
#Por la forma, podemos asumir que tendremos una potencia menor que 1
posib_cov <- seq(.1,.9,.05)



seq1 <- c(1, 2, 3, 4)   # 4 elementos
seq2 <- c(10, 20)       # 2 elementos

combinaciones <- lapply(posib_pot, function(x) (x + posib_cov)) # Vamos a hacer la funcion que haga las combinaciones de los elementos dados



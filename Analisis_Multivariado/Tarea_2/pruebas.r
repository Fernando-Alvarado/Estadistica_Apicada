### Ejercicio 8, simular una distribucion dirichlet


library(ggplot2)
#Parametros

K = 3 #Numero de colores que trendra nuestra urna 




#Definamos a nuestra urna

urna <- function(num_colores, vec_numero){
    #num_colores: numero de bolas de diferente color que tendremos en nuestra urna
    #vec_numero: vector con el numero de bolas de cada color
    pelotas <- lapply(1:num_colores, function(i) rep(i, times = vec_numero[i]))
    return(sample(unlist(pelotas))) #Regresamos una muestra aleatoria de pelotas
}



muestra = urna(K, c(1, 2, 3)) #Creamos una muestra de nuestra urna

print(muestra) 

juego <- function(N, muestra){
    #N: numero de veces que se repetira el juego de la urna
    #muestra: muestra de nuestra urna
    pelotas <- lapply(1:N, function(i) {
        num_aleatorio <- sample(1:length(muestra), 1) #Escogemos un numero aleatorio
        muestra = c(muestra, muestra[num_aleatorio]) #Agregamos una pelota a la urna
    })
    return(sample(unlist(pelotas)))
  
}

print(juego(10, muestra))

df <- data.frame(pelotas = juego(10, muestra))

##Haciendo un histograma de la distribucion de las pelotas 
x11()
ggplot(df, aes(x = pelotas)) +
     geom_histogram(binwidth = 1, fill = "#212f3c", color = "black") + 
     theme_minimal() + 
     labs(title = "Distribucion de pelotas en la urna", x = "Pelotas", y = "Frecuencia")


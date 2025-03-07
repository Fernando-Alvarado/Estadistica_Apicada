#Primer examen de Preocesos Estocásticos
#Realizado por: Alvarado Placios Fernando

#Ejercicio 5 


#Librerias a usar 
library(ggplot2)



#Función que simula el salto del caminante con la misma probabilidad de ir a la derecha o izquierda 
salto_caminante  <- function(){#No tienen parametros pro que el proceso es simetrico
     if(runif(1) > 0.5) { 
        return(1)
    } else {
        return(-1)
    }
}



#Función que simula el tope del caminante en el punto 0 y N (con proba p y 1-p de quedarse o irese)
tope_caminante  <- function(p, estado, N){#Parametro p que es la probabilidad moverse al estado 1 o N-1, estado que es el estado actual del caminante si es 0 o N
    if(runif(1) < p) { 
        if(estado == 0){#Si el estado es 0 se mueve a la derecha, si no a la izquierda
            return(1)
        } else if(estado == N) {
            return(-1)
        }
    } else {
        return(0)
    }
}

#Función que simula el proceso del caminante con topes en 0 y N
proceso_caminante  <- function(N,p, n){#Parametros N: size de nuestra CM, p: probabilidad en los topes, n: numero de pasos que se desee simular, inicioCaminante: punto de inicio de nuestro caminante, este parametro es opcional 
        estadoActual  <-  0 #Fijando el estado inicial de la caminata 
        caminata  <- c( tope_caminante(p, estadoActual, N))
        for(i in 2:n){ #Ciclo que simula los pasos de la caminata
            if(caminata[length(caminata)] == 0 | caminata[length(caminata)] == N){ # nolint: line_length_linter.
                    estadoActual  <- caminata[length(caminata)] +  tope_caminante(p, caminata[length(caminata)], N ) #Redefinimos nuestro estado final  
                     caminata  <- c(caminata,estadoActual) 
            } else if(caminata[length(caminata)] > 0 & caminata[length(caminata)] < N){
                    estadoActual  <- caminata[length(caminata)]+  salto_caminante() #Redefinimos nuestro estado final 
                    caminata  <- c(caminata,estadoActual)            
        }
        
    }
    return(caminata)
}



#Obs, si agrego otra proceso cambie el nombre en la grafica de abajo, para que no haya problemas
#Ejemplo de simulación de la caminata con topes en 0 y N, se puede modificar pero no reenombre la variable, para que la grafica pueda salir sin ningun problema
proceso1  <- proceso_caminante(10, .3, 100) #Simulación de la caminata con topes en 0 y 10 con probabilidad de 0.5 de quedarse en los topes y 100 pasos

proceso2 <- proceso_caminante(10, .5, 100) #Simulación de la caminata con topes en 0 y 10 con probabilidad de 0.5 de quedarse en los topes y 100 pasos

proceso3 <- proceso_caminante(10, .7, 100)



vector_a_graficar <- proceso2 #Vector que se graficara, si se quiere cambiar por otro vector, cambiar el nombre en la grafica de abajo



#Creando la grafica
grafica_Proceso  <- function(data, mensaje = ""){
    ggplot(data, aes(x = Paso, y = Caminata)) + 
    geom_line() + #Parametro para pasar que grafica quiero poner
    labs(title = paste("Caminata aleatoria con topes en 0 y N", mensaje), x = "Tiempo, X_n", y = " Estados") + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
}


#Graficando la caminata aleatoria 1
grafica1 <- grafica_Proceso( data.frame(Paso = 1:length(proceso1), Caminata = proceso1), "Con proba p=.3" ) #Grafica de la caminata aleatoria


#Graficando la caminata aleatoria 1
grafica2 <- grafica_Proceso( data.frame(Paso = 1:length(proceso2), Caminata = proceso2), "Con proba p=.5") #Grafica de la caminata aleatoria

#Graficando la caminata aleatoria 1
grafica3 <- grafica_Proceso( data.frame(Paso = 1:length(proceso3), Caminata = proceso3), "Con proba p=.7") #Grafica de la caminata aleatoria


#Mostrando la grafica
grafica1
grafica2
grafica3







#Usando el metodo de Monte Carlo para obtener la distribucion estacionaria  de nuestro proceso  
#-------- Recomiendo ejecutar esto por separado ya que luego se rompe R y no muestra bien las graficas

#Haciedno muchas iteraciones de nueestras caminatas
iteraciones  <- 100

#Viendo los estados finales de nuestras caminatas
lista_de_estados  <- c()


for(i in 1:iteraciones){

    proceso_CM <- proceso_caminante(10, .3, 100)
    lista_de_estados <- c(lista_de_estados, proceso_CM )
}



tabla  <- table(lista_de_estados)

print(tabla) #Distribucion de estados finales de nuestras caminatas

#Graficando la distribucion de estados final de nuestras caminatas
tabla_df <- as.data.frame(table(lista_de_estados))
names(tabla_df) <- c("Estado", "Frecuencia")

# Crear un gráfico de barras
ggplot(tabla_df, aes(x = Estado, y = Frecuencia)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras de los resultados Finales , donde tomamos p=0.3", x = "Valores", y = "Total")


#Con el metodo de Montecarlo Aproximando una distribucion estacioanriaa de nuestro proceso

frecuencias_relativas <- tabla/(iteraciones*100)

frecuencias_relativas

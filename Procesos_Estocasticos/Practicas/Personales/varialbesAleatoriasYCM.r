#En este archivo voy a practicar lo aprendido de variables aleatorias y cadenad de Marckov

library(ggplot2)#Importando las librerias a usar
library(stats) #libreria para manejar las variables aleatorias

# Generar 100 muestras de una distribución normal con media 0 y desviación estándar 1
datos <- rnorm(100, mean = 0, sd = 1) #general una uniforme con las librerias de R pero creo que stats es la misma notacion 

datosBin <- rbinom(100, size=10, prob=0.5)


vector <- (1:100)


qplot(vector,datos)#graficando las va, normales
qplot(vector, datosBin)#Graficando las va, Binomiales


#Creando una grafica de barras para grafiacar cualquier funcoin que deseemos-----------------

createDataFrame <- function(vector, size){#parametros: vector es un vector que se le pasa de la muestra aleatoria, size: tamano de nuestro vector 
  resultado <- data.frame(
    categorias = c(1:size), #enumerando las realizaciones
    frecuencia =  vector #el vecotr que modela la va
  )
  return(resultado)
}

#Creando la funcion que nos permitira hacer cualquier grafica que deesemos
createGraf <- function(dataFrame,titulo, ejex, ejey){#DataFrame que creemos con la va, y los componentes basicos de la grafica
  grafica <- ggplot(dataFrame, aes(x= categorias, y=frecuencia))+
                    geom_bar(stat="identity", fill="skyblue")+
                    labs(title= titulo, x= ejex, y=ejey)
  return(grafica)
}

#Empezando a hacer la grafica de barras en este caso
#Graficando una va Binomial 

sizeSample <- 100 #tamano deseado de la muestra aleatoria 
createGraf( createDataFrame(rbinom(sizeSample, size=10, prob=0.5), sizeSample), "Graficando una Binomial", "N-esima repeticion", "Valor esperado")


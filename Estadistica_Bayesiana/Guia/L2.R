# Estadística Bayesiana 2025-II ----
# Grupo: 4307
# Dra. Lizbeth Naranjo
# Enrique Reyes

## Laboratorio 2 ----
# 06-02-2025
# Visualizacion de datos en R

### ggplot2 ----
# Este paquete tambien pertenece al tidyverse, se usa para la 
# graficar un conjunto de datos, haciendo graficas "complejas" de 
# forma sencilla y con pocas lineas de codigo

# Instalamos el paquete
install.packages("ggplot2")

# Activamos el paquete
library(ggplot2)

# Continuaremos con el conjunto de datos de Lirios

# Creamos un entorno donde graficaremos
Graf1 = ggplot(iris,aes(Sepal.Length,Petal.Length))

# para graficar un grafico de lineas en nuestro conjunto
# de datos
Graf1 + geom_line()

# podemos ver que esta grafica no es la más representativa para 
# este conjunto de datos, así que plantearemos otra grafica

# Si quisieramos graficar los puntos
Graf1 + geom_point()

# Esta grafica es más representativa para este conjunto de datos

# si quisieramos graficar un suavecimiento a este conjunto de datos
Graf1 + geom_point() + stat_smooth()

# si quisieramos quitar los intervalos de confianza
Graf1 + geom_point() + stat_smooth(se=F)

# Si quisieramos cambiar la opacidad de los puntos
Graf1 + geom_point(alpha=0.3) + stat_smooth(se=F)

# Si quisieramos cambiar el tamaño de los puntos
Graf1 + geom_point(alpha=0.3,aes(size=Species)) + 
  stat_smooth(se=F)

# Si quisieramos cambiar el color por el tipo de especie
Graf1 + geom_point(alpha=0.3,aes(size=Species,color=Species)) + 
  stat_smooth(se=F)

# si quisieramos hacer un ajuste por cada especie
Graf2 = ggplot(iris,aes(Sepal.Length,Petal.Length,color=Species))

Graf2 + geom_point(alpha=0.5,aes(size=Species)) + 
  stat_smooth(se=F)

# si en lugar de un suavizamiento quisieramos un ajuste lineal
# y con intervalos de confianza y con tamaño homogeneo en los puntos
Graf2 + geom_point(alpha=0.5) + stat_smooth(method = "lm")

# y si en lugar de tener una sola grafica quisiera tener una por
# cada especie para visualizar mejor los datos
Graf2 + geom_point(alpha=0.5) + stat_smooth(method = "lm") + 
  facet_wrap(~ Species)

# Para ajustar el campo de vision
Graf2 + geom_point(alpha=0.5) + stat_smooth(method = "lm") + 
  facet_wrap(~ Species, scale="free")

# la grafica de caja, el global con las 150 observaciones seria
Graf1 + geom_boxplot()

# la grafica de caja, para cada especie estaria dado por
Graf2 + geom_boxplot()

# si quisieramos aññadir mas informacion a nuestra grafica
# graficaremos los puntos en x con respecto a la media en y
Graf1 +  geom_boxplot() + 
  stat_summary(fun.y=mean,geom="point",shape=18,size=3,color="black")

# por grupos
Graf2 +  geom_boxplot() + 
  stat_summary(fun.y=mean,geom="point",shape=18,size=3,color="black")

# si en lugar de graficar la media en y qusieramos graficar todos
# los puntos observados
Graf1 +  geom_boxplot() + geom_jitter()

# por grupos
Graf2 +  geom_boxplot() + geom_jitter()

# Podemos combinar ambas graficas
Graf1 + geom_boxplot(notch = TRUE) +
  stat_summary(fun.y = mean, 
               geom="point",shape=18,size=3,color="cyan3")+
  geom_jitter()

# por clase
Graf2 + geom_boxplot(notch = TRUE) +
  stat_summary(fun.y = mean, 
               geom="point",shape=18,size=3,color="cyan3")+
  geom_jitter()

# tenemos el grafico de violin que nos da una idea general
# sobre la dispersion de los datos
Graf1 + geom_violin() +
  stat_summary(fun.y = mean, 
               geom="point",shape=18,size=3,color="cyan3")+
  geom_jitter()

# por clase
Graf2 + geom_violin() +
  stat_summary(fun.y = mean, 
               geom="point",shape=18,size=3,color="cyan3")+
  geom_jitter()

# grafica de barras
ggplot(iris,aes(Petal.Length)) + 
  geom_bar()

# Por clases
ggplot(iris,aes(Petal.Length,fill=Species)) + 
  geom_bar()

# grafica de puntos
ggplot(iris,aes(Petal.Length)) + 
  geom_dotplot()

# Por clases
ggplot(iris,aes(Petal.Length,fill=Species)) + 
  geom_dotplot()

# Histograma
ggplot(iris,aes(Petal.Length)) + 
  geom_histogram()

# frecuencia
ggplot(iris,aes(Petal.Length)) + 
  geom_freqpoly()

# histograma mas frecuencia
ggplot(iris,aes(Petal.Length)) + 
  geom_histogram(alpha=0.7) + geom_freqpoly(color="red2")

# histograma y frecuencia por specie
ggplot(iris,aes(Petal.Length,fill=Species)) + 
  geom_histogram(alpha=0.7) + geom_freqpoly(color="red2")

# histograma de densidad
ggplot(iris,aes(Petal.Length)) + 
  geom_histogram(aes(y=..density..)) 

# histograma de densidad mas densidad empirica por especie
ggplot(data=iris, aes(Petal.Length,fill=Species)) + 
  geom_histogram(aes(y=..density..)) + geom_density(alpha=0.7)

# podemos cambiar el tema
ggplot(data=iris, aes(Petal.Length,fill=Species)) + 
  geom_histogram(aes(y=..density..)) + geom_density(alpha=0.7) +
  theme_bw()

# podemos añadir titulo
ggplot(data=iris, aes(Petal.Length,fill=Species)) + 
  geom_histogram(aes(y=..density..)) + geom_density(alpha=0.7) +
  theme_bw() + ggtitle("Ddensidad e histograma empirico")

# podemos modificar el nombre de los ejes
ggplot(data=iris, aes(Petal.Length,fill=Species)) + 
  geom_histogram(aes(y=..density..)) + geom_density(alpha=0.7) +
  theme_bw() + ggtitle("Ddensidad e histograma empirico") +
  labs(y="Densidad",x="Longitud del petalo") 

# podemos modificar el nombre de la variable Species
ggplot(data=iris, aes(Petal.Length,fill=Species)) + 
  geom_histogram(aes(y=..density..)) + geom_density(alpha=0.7) +
  theme_bw() + ggtitle("Ddensidad e histograma empirico") +
  labs(y="Densidad",x="Longitud del petalo",fill="Especies") 

# podemos añadir un subtitulo
ggplot(data=iris, aes(Petal.Length,fill=Species)) + 
  geom_histogram(aes(y=..density..)) + geom_density(alpha=0.7) +
  theme_bw() + ggtitle("Ddensidad e histograma empirico",
                       subtitle = "Estadística Bayesiana") +
  labs(y="Densidad",x="Longitud del petalo",fill="Especies") 

# podemos añadir una descripcion
ggplot(data=iris, aes(Petal.Length,fill=Species)) + 
  geom_histogram(aes(y=..density..)) + geom_density(alpha=0.7) +
  theme_bw() + ggtitle("Ddensidad e histograma empirico",
                       subtitle = "Estadística Bayesiana") +
  labs(y="Densidad",x="Longitud del petalo",fill="Especies",
       caption = "Densidad de la longitud del petalo por especie") 

# Podemos añadir una numeracion a las graficas
ggplot(data=iris, aes(Petal.Length,fill=Species)) + 
  geom_histogram(aes(y=..density..)) + geom_density(alpha=0.7) +
  theme_bw() + ggtitle("Ddensidad e histograma empirico",
                       subtitle = "Estadística Bayesiana") +
  labs(y="Densidad",x="Longitud del petalo",fill="Especies",
       caption = "Densidad de la longitud del petalo por especie",
       tag = "Figura 1") 


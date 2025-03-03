# Etadistica Bayesiana 2025-II ----
# Grupo: 4307
# Dra. Lizbeth Naranjo
# Enrique Reyes

## Laboratorio 1 ----
# 30-01-2025
# Manipulacion de datos en R

### dply ----
# Es un paquete muy util que pertenece al entorno "tidyverse"
# con el cual se puden manipular datos de forma muy sencilla
# de manera intuitiva sin necesidad de tantas linea de codigo

# Instalamos el paquete
install.packages("dplyr")

# Activamos el paquete
library(dplyr)

# las funciones mas utiles en esta paqueteria son:
# filter, select, arrange, mutate, group_by summarise

# para poder ejemplificar el uso eficiente de este paquete 
# usaremos un conjunto de datos muy comun, los lirios de Fisher

Lirios = iris

# Observamos el tipo de variables que contiene esta en esta base
glimpse(Lirios)

# filter: genera un filtro para obtener la informacion deseada
filter(Lirios,Species=="virginica")

filter(Lirios,Species=="virginica" | Species=="versicolor")

filter(Lirios,Species=="virginica",Petal.Length>=6)

# Otros filtros, son:
Lirios1 = rbind(Lirios,Lirios[seq(1,150,5),])

# distinct: quita los valores repetidos en la base de datos
# segun un patron (puede ser una variable o de forma global)
distinct(Lirios1)

# top_n: sirve para visualizar los valores mas altos (o mas bajos)
# segun una variable especificada
top_n(Lirios,6,Sepal.Length)
top_n(Lirios,-1,Sepal.Length)

# top_frac: devuelve una fraccion especifica de la muestra dada
# una proporcion
top_frac(Lirios,0.1,Sepal.Width)

#select: selecciona las variables de interes
select(Lirios,Sepal.Length)

select(Lirios,Sepal.Length,Sepal.Width)

select(Lirios,Sepal.Length:Petal.Length)

select(Lirios,-Petal.Width)

select(Lirios, contains('Width'))

select(Lirios, starts_with('P'))

select(Lirios, ends_with('h'))

select(Lirios, matches('.ec.'))

# arrange: ordena los datos (por default: menor a mayor)
arrange(Lirios, Sepal.Length)

arrange(Lirios, -Sepal.Length)

arrange(Lirios, Species, Sepal.Length)

# mutate: crea nuevas variables

mutate(Lirios,petalos=Petal.Width/Petal.Length)

mutate(Lirios,petalos=Petal.Width/Petal.Length,
       sepalos=Sepal.Width/Sepal.Length)

mutate(Lirios,ID_Especie=ifelse(Species=="setosa",1,
                                ifelse(Species=="versicolor",2,3)))

# rename: renombra una variable

rename(Lirios,Longitud.Sepalo=Sepal.Length)

rename(Lirios,Longitud.Sepalo=Sepal.Length,Ancho_Sepalo=Sepal.Width)

# Pipe
# A partir de R 4.1.0 se implemento una nueva estructura para
# encadenar operaciones ( |> ) llamada pipe nativo, este
# brinda una reduccion en la sintaxis de codigo, pero
# pierde generalidades contra el pipe ( %>% ) implementado
# desde la paqueteria magrittr, pero pata este curso en donde
# estudiaremos de forma general el codigo, ambos pipes
# serviran para lo mismo

Lirios %>% select(contains('Petal'),Species)

Lirios %>% select(contains('Petal'),Species) %>% filter(Species=="versicolor")

Lirios %>% select(contains('Petal'),Species) %>% 
           filter(Species=="versicolor") %>%
           arrange(Petal.Width)

Lirios %>% select(contains('Petal'),Species) %>% 
           filter(Species=="versicolor") %>%
           arrange(Petal.Width) %>%
           mutate(forma=Petal.Width/Petal.Length)

Lirios |> select(contains('Sepal'),Species) |> 
          filter(Species=="virginica") |>
          arrange(Sepal.Length) |>
          mutate(forma=Sepal.Width/Sepal.Length)

# summarise: nos devuelve el resultado de una operacion mas compleja
# esta puede ser asociada a un grupo o no

# group_by: sirve para agrupar los datos, dada una variable categorica

Lirios %>% group_by(Species) %>%
           summarise(media_lp=mean(Petal.Length)) %>%
           ungroup()

Lirios |> group_by(Species) |>
          summarise(var_as=var(Sepal.Width)) |>
          ungroup()

Lirios |> group_by(Species) |>
          summarise(n=n()) |>
          ungroup()

Lirios %>% group_by(Species) %>%
           summarise(media_lp=mean(Petal.Length),
                     var_lp=var(Petal.Length),
                     Q1_lp=quantile(Petal.Length,0.25),
                     Q2_lp=median(Petal.Length),
                     Q3_lp=quantile(Petal.Length,0.75)) %>%
           ungroup()

Lirios %>% group_by(Species) %>%
           mutate(d_e_c_lp=Petal.Length - mean(Petal.Length)) %>%
           ungroup()

# Joins: sirven para unir dos o mas tablas que se pueden relacionar

# Tomaremos por ejemplo las bases:

band_members

band_instruments

# left_join: une dos tablas manteniendo la estructura de la primera base
# y solo añade la informacion adicional que encuentre en la segunda base
band_members %>% left_join(band_instruments)

# right_join: une dos tablas manteniendo la estructura de la segunda base
# y solo añade la informacion adicional que encuentre en la primera base
band_members %>% right_join(band_instruments)

# inner_join: une la informacion coincidente entre las dos bases
band_members %>% inner_join(band_instruments)

# full_join: une las dos bases en su totalidad
band_members %>% full_join(band_instruments)

# Ejemplo del uso
# de la técnica de Análisis Factorial Exploratorio en R

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2025-2/ApreEstAut")
library(tidyverse)
# Datos
# Los datos corresponden a los scores que se han dado a 329 comunidades
# de acuerdo con los siguientes aspectos

#Climate and Terrain
#Housing
#Health Care & the Environment
#Crime
#Transportation
#Education
#The Arts
#Recreation
#Economics

# Más detalles
#Climate & Terrain: very hot and very cold months, seasonal temperature variation, heating- and cooling-degree days, freezing days, zero-degree days, ninety-degree days.
#Housing: utility bills, property taxes, mortgage payments.
#Health Care & Environment: per capita physicians, teaching hospitals, medical schools, cardiac rehabilitation centers, comprehensive cancer treatment centers, hospices, insurance/hospitalization costs index, flouridation of drinking water, air pollution.
#Crime: violent crime rate, property crime rate.
#Transportation: daily commute, public transportation, Interstate highways, air service, passenger rail service.
#Education: pupil/teacher ratio in the public K-12 system, effort index in K-12, accademic options in higher education.
#The Arts: museums, fine arts and public radio stations, public television stations, universities offering a degree or degrees in the arts, symphony orchestras, theatres, opera companies, dance companies, public libraries.
#Recreation: good restaurants, public golf courses, certified lanes for tenpin bowling, movie theatres, zoos, aquariums, family theme parks, sanctioned automobile race tracks, pari-mutuel betting attractions, major- and minor- league professional sports teams, NCAA Division I football and basketball teams, miles of ocean or Great Lakes coastline, inland water, national forests, national parks, or national wildlife refuges, Consolidated Metropolitan Statistical Area access.
#Economics: average household income adjusted for taxes and living costs, income growth, job growth.

#Excepto para Housing y Crime, todos los scores están en el sentido
# a mayor valor mejor calificación
#Para Housing y Crime el sentido es el contrario
#es decir, a menor valor, mejor calificación

Datos=read.delim("places.txt",
                  header = FALSE, sep='')
names(Datos)=c("Clim", "Housing", "HealthEnv", "Crime", "Transp",  "Educ", "Arts", "Recre",  "Econ", "Id")
summary(Datos)

# Las escalas de los datos no son iguales
X11()
library(GGally)
ggpairs(data=Datos[,-10], title="Datos")


#quitemos el dato extraño en Arts
Datos$Arts[which.max(Datos$Arts)]
Datos=Datos[-which.max(Datos$Arts),] 
summary(Datos)


# El objetivo es tratar de encontrar uno o varios índices
# para resumir la información

# Se podrían usar los datos en la escala original, otra opción es
# usar una escala logarítmica  (o cualquier otra estandarización)
# para trata de estandarizar las escalas de las variables
X11()
ggpairs(data=log10(Datos[,-10]), title="Datos")

# Uso del paquete psych para EFA
library(psych)

Efa1 <- fa(log10(Datos[,-10]),nfactors=3)  #Forzosamente se debe
                                  #indicar el número de factores
#Factores correlacionados (por default) rotate="oblimin", method="minres"
#Usa la función de correlación (estandariza los datos) por default
Efa1 
#standarized loadings son las correlaciones entre los factores y las
#variables originales, muestran el peso de cada variable
#sirven para interpretar.
#Notar que los factores no están en orden como en PCA, pues 
#el orden en el que se encuentran no necesariamente
#coincide con su varianza.


# De la varianza total, estos 3 factores sólo explican el 49%

# communalities:
# Estos porcentajes reflejan que tan bien están modeladas las variables 
# a través de los factores (tipo coef det, R^2 en reg):
Efa1$communalities #similar a h2 de la salida principal
# E.g. un modelo muy pobre para Crime y Econ

#La columna com o complexity refleja qué tantos factores
#están involucrados en la explicación de una variable
Efa1$complexity
#por ejemplo, Transp está correlacionado con dos factores de forma similar

#Notar que también aparece la correlación entre factores,
#que a diferencia de pca puede ser diferente de cero.
Efa1$Phi


# El resultado se puede visualizar con las siguientes 
# herramientas
X11()
biplot(Efa1,choose=c(1,2,3)) #lectura similar a pca
#Otra gráfica más ilustrativa para interpretar es la siguiente.
fa.diagram(Efa1, cut=.5) # por default cut=.3

# Para imprimir una salida más fácil
# de interpretar se puede usar la función print()
print(Efa1, cut = .5, digits=2, sort=TRUE)


# Algunas herramientas de diagnóstico,
# aunque lo más importante es encontrar alguna
# interpretación útil.
# Algunas se basan en supuestos distribucionales,
# que no verificaremos y sólo usaremos de referencia.

summary(Efa1) #también al final de la salida anterior
# Las pruebas Chi-square consideran
# H0: el modelo parece plausible
# Ha: parece que se requiere un modelo más complejo (más factores)
# Aquí se rechaza H0. 
Efa1$PVAL
# Nota. Esta prueba sólo es una guía, pues
# para su desarrollo necesita el cumplimiento de 
# supuestos distribucionales

#Tucker Lewis Index, al menos de .9

# RMSEA (Root mean square error of approximation)
# deseable que sea pequeño, e.g. menor a .01

# BIC para comparar entre modelos, aunque asume supuesto distribucional.

#########
# Una herramienta para tener una idea 
# del número de factores es fa.parallel.
# fm="minres" es por default y se puede modificar el método
X11()
set.seed(123)
parallel <- fa.parallel(log10(Datos[,-10]), fa="fa", n.iter=100)


Efa1b <- fa(log10(Datos[,-10]),nfactors=5) 
Efa1b 
fa.diagram(Efa1b)

#Si la interpretación no es conveniente,
#se pueden probar diferentes rotaciones
Efa2 <- fa(log10(Datos[,-10]),nfactors=5, rotate="varimax") 
Efa2 
fa.diagram(Efa2)
#Notar que la varianza explicada no cambia con una rotación diferente.
#Ni el BIC

Efa3 <- fa(log10(Datos[,-10]),nfactors=4, rotate="varimax") 
Efa3
fa.diagram(Efa3)

# También diferentes métodos de estimación
X11() 
# si se usa ml
set.seed(123)
parallel <- fa.parallel(log10(Datos[,-10]), fa="fa", n.iter=100, fm="ml")


Efa4 <- fa(log10(Datos[,-10]),nfactors=5, rotate="oblimin", fm="ml") 
Efa4
fa.diagram(Efa4)

# Quizás estos tiene mejor interpretación
Efa5 <- fa(log10(Datos[,-10]),nfactors=5, rotate="biquartimin", fm="ml") 
Efa5
fa.diagram(Efa5)
summary(Efa5)

# La función principal también permite rotar y especificar número de factores
CP5 <- principal(log10(Datos[,-10]),nfactors=5, rotate="varimax") 
CP5
fa.diagram(CP5) # Esta gráfica ayuda también para interpretar 
# componentes principales, aunque sólo sirve con datos estandarizados (media 0 y var 1)
summary(CP5)

# Esta es una buena solución también, 
# pues se puede interpretar bien.
# Además, las variables están bien representadas
# y la varianza explicada es adecuada.


#Nota
#Una vez que se decide una solución
#quizás se quiera usar esa proyección para una nueva
#observación. Como en pca, predict sólo sirve cuando se usan
#los datos estandarizados.
Efa5 <- fa(log10(Datos[,-10]),nfactors=5, rotate="biquartimin", fm="ml") 
Efa5$scores[2,] #observación 2 en términos de los factores
predict.psych(Efa5, log10(Datos[2,-10]), log10(Datos[,-10]))

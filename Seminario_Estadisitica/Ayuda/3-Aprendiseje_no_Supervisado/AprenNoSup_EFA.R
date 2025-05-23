# Ejemplo del uso
# de la t�cnica de An�lisis Factorial Exploratorio en R

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

# M�s detalles
#Climate & Terrain: very hot and very cold months, seasonal temperature variation, heating- and cooling-degree days, freezing days, zero-degree days, ninety-degree days.
#Housing: utility bills, property taxes, mortgage payments.
#Health Care & Environment: per capita physicians, teaching hospitals, medical schools, cardiac rehabilitation centers, comprehensive cancer treatment centers, hospices, insurance/hospitalization costs index, flouridation of drinking water, air pollution.
#Crime: violent crime rate, property crime rate.
#Transportation: daily commute, public transportation, Interstate highways, air service, passenger rail service.
#Education: pupil/teacher ratio in the public K-12 system, effort index in K-12, accademic options in higher education.
#The Arts: museums, fine arts and public radio stations, public television stations, universities offering a degree or degrees in the arts, symphony orchestras, theatres, opera companies, dance companies, public libraries.
#Recreation: good restaurants, public golf courses, certified lanes for tenpin bowling, movie theatres, zoos, aquariums, family theme parks, sanctioned automobile race tracks, pari-mutuel betting attractions, major- and minor- league professional sports teams, NCAA Division I football and basketball teams, miles of ocean or Great Lakes coastline, inland water, national forests, national parks, or national wildlife refuges, Consolidated Metropolitan Statistical Area access.
#Economics: average household income adjusted for taxes and living costs, income growth, job growth.

#Excepto para Housing y Crime, todos los scores est�n en el sentido
# a mayor valor mejor calificaci�n
#Para Housing y Crime el sentido es el contrario
#es decir, a menor valor, mejor calificaci�n

Datos=read.delim("places.txt",
                  header = FALSE, sep='')
names(Datos)=c("Clim", "Housing", "HealthEnv", "Crime", "Transp",  "Educ", "Arts", "Recre",  "Econ", "Id")
summary(Datos)

# Las escalas de los datos no son iguales
X11()
library(GGally)
ggpairs(data=Datos[,-10], title="Datos")


#quitemos el dato extra�o en Arts
Datos$Arts[which.max(Datos$Arts)]
Datos=Datos[-which.max(Datos$Arts),] 
summary(Datos)


# El objetivo es tratar de encontrar uno o varios �ndices
# para resumir la informaci�n

# Se podr�an usar los datos en la escala original, otra opci�n es
# usar una escala logar�tmica  (o cualquier otra estandarizaci�n)
# para trata de estandarizar las escalas de las variables
X11()
ggpairs(data=log10(Datos[,-10]), title="Datos")

# Uso del paquete psych para EFA
library(psych)

Efa1 <- fa(log10(Datos[,-10]),nfactors=3)  #Forzosamente se debe
                                  #indicar el n�mero de factores
#Factores correlacionados (por default) rotate="oblimin", method="minres"
#Usa la funci�n de correlaci�n (estandariza los datos) por default
Efa1 
#standarized loadings son las correlaciones entre los factores y las
#variables originales, muestran el peso de cada variable
#sirven para interpretar.
#Notar que los factores no est�n en orden como en PCA, pues 
#el orden en el que se encuentran no necesariamente
#coincide con su varianza.


# De la varianza total, estos 3 factores s�lo explican el 49%

# communalities:
# Estos porcentajes reflejan que tan bien est�n modeladas las variables 
# a trav�s de los factores (tipo coef det, R^2 en reg):
Efa1$communalities #similar a h2 de la salida principal
# E.g. un modelo muy pobre para Crime y Econ

#La columna com o complexity refleja qu� tantos factores
#est�n involucrados en la explicaci�n de una variable
Efa1$complexity
#por ejemplo, Transp est� correlacionado con dos factores de forma similar

#Notar que tambi�n aparece la correlaci�n entre factores,
#que a diferencia de pca puede ser diferente de cero.
Efa1$Phi


# El resultado se puede visualizar con las siguientes 
# herramientas
X11()
biplot(Efa1,choose=c(1,2,3)) #lectura similar a pca
#Otra gr�fica m�s ilustrativa para interpretar es la siguiente.
fa.diagram(Efa1, cut=.5) # por default cut=.3

# Para imprimir una salida m�s f�cil
# de interpretar se puede usar la funci�n print()
print(Efa1, cut = .5, digits=2, sort=TRUE)


# Algunas herramientas de diagn�stico,
# aunque lo m�s importante es encontrar alguna
# interpretaci�n �til.
# Algunas se basan en supuestos distribucionales,
# que no verificaremos y s�lo usaremos de referencia.

summary(Efa1) #tambi�n al final de la salida anterior
# Las pruebas Chi-square consideran
# H0: el modelo parece plausible
# Ha: parece que se requiere un modelo m�s complejo (m�s factores)
# Aqu� se rechaza H0. 
Efa1$PVAL
# Nota. Esta prueba s�lo es una gu�a, pues
# para su desarrollo necesita el cumplimiento de 
# supuestos distribucionales

#Tucker Lewis Index, al menos de .9

# RMSEA (Root mean square error of approximation)
# deseable que sea peque�o, e.g. menor a .01

# BIC para comparar entre modelos, aunque asume supuesto distribucional.

#########
# Una herramienta para tener una idea 
# del n�mero de factores es fa.parallel.
# fm="minres" es por default y se puede modificar el m�todo
X11()
set.seed(123)
parallel <- fa.parallel(log10(Datos[,-10]), fa="fa", n.iter=100)


Efa1b <- fa(log10(Datos[,-10]),nfactors=5) 
Efa1b 
fa.diagram(Efa1b)

#Si la interpretaci�n no es conveniente,
#se pueden probar diferentes rotaciones
Efa2 <- fa(log10(Datos[,-10]),nfactors=5, rotate="varimax") 
Efa2 
fa.diagram(Efa2)
#Notar que la varianza explicada no cambia con una rotaci�n diferente.
#Ni el BIC

Efa3 <- fa(log10(Datos[,-10]),nfactors=4, rotate="varimax") 
Efa3
fa.diagram(Efa3)

# Tambi�n diferentes m�todos de estimaci�n
X11() 
# si se usa ml
set.seed(123)
parallel <- fa.parallel(log10(Datos[,-10]), fa="fa", n.iter=100, fm="ml")


Efa4 <- fa(log10(Datos[,-10]),nfactors=5, rotate="oblimin", fm="ml") 
Efa4
fa.diagram(Efa4)

# Quiz�s estos tiene mejor interpretaci�n
Efa5 <- fa(log10(Datos[,-10]),nfactors=5, rotate="biquartimin", fm="ml") 
Efa5
fa.diagram(Efa5)
summary(Efa5)

# La funci�n principal tambi�n permite rotar y especificar n�mero de factores
CP5 <- principal(log10(Datos[,-10]),nfactors=5, rotate="varimax") 
CP5
fa.diagram(CP5) # Esta gr�fica ayuda tambi�n para interpretar 
# componentes principales, aunque s�lo sirve con datos estandarizados (media 0 y var 1)
summary(CP5)

# Esta es una buena soluci�n tambi�n, 
# pues se puede interpretar bien.
# Adem�s, las variables est�n bien representadas
# y la varianza explicada es adecuada.


#Nota
#Una vez que se decide una soluci�n
#quiz�s se quiera usar esa proyecci�n para una nueva
#observaci�n. Como en pca, predict s�lo sirve cuando se usan
#los datos estandarizados.
Efa5 <- fa(log10(Datos[,-10]),nfactors=5, rotate="biquartimin", fm="ml") 
Efa5$scores[2,] #observaci�n 2 en t�rminos de los factores
predict.psych(Efa5, log10(Datos[2,-10]), log10(Datos[,-10]))

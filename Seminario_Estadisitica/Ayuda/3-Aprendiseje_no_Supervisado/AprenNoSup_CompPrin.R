# Ejemplo del uso
# de la t�cnica de componentes principales en R

rm(list = ls(all.names = TRUE))
gc()

setwd("C:/Users/ferna/Documents/Estadistica_Aplicada/Seminario_Estadisitica/Ayuda/3-Aprendiseje_no_Supervisado")
library(tidyverse)
# Datos
# Los datos corresponden a los scores que se 
# han dado a 329 comunidades
# de acuerdo con los siguientes aspectos

#Climate and Terrain
#Housing cost
#Health Care & the Environment
#Crime 
#Transportation
#Education
#The Arts
#Recreation
#Economics

#Excepto para Housing y Crime, todos los scores 
# est�n en el sentido
# a mayor valor mejor calificaci�n
#Para Housing y Crime el sentido es el contrario
#es decir, a menor valor, mejor calificaci�n

Datos=read.delim("./places.txt",
                  header = FALSE, sep='')
names(Datos)=c("Clim", "Housing", "HealthEnv", "Crime", "Transp",  "Educ", "Arts", "Recre",  "Econ", "Id")
summary(Datos)

# Las escalas de los datos no son iguales (Clim vs Housing)
X11()
library(GGally)
ggpairs(data=Datos[,-10], title="Datos")

# El objetivo es tratar de encontrar uno o varios �ndices
# para resumir la informaci�n

# Se podr�an usar los datos en la escala original, otra opci�n es
# usar una escala logar�tmica para trata de estandarizar
X11()
ggpairs(data=log10(Datos[,-10]), title="Datos")


# Una de las funciones para encontrar los componentes principales en R base
# es prcomp(), la otra es princomp()
# esta �ltima permite dar la estimaci�n de matriz de varianzas
# y covarianzas a usar

# scale es el argumento que permite escalar a varianza 1
# (por default se centran los datos a media cero)

# Los datos se pueden usar en formato dataframe con las columnas de X
# o bien f�rmula sin variable dependiente

# Ejemplo usando la escala original
R.CP=prcomp(Datos[,-10], scale = FALSE)
R.CPb=prcomp(~.,Datos[,-10],  scale = FALSE)

X11()
par(mfrow=c(1,2))
biplot(R.CP)

biplot(R.CPb)


# Para visualizar los resultados se puede usar el siguiente paquete
library(factoextra)
X11()
fviz_eig(R.CP)
fviz_pca_var(R.CP,
             col.var = "contrib")
fviz_pca_ind(R.CP)
X11()
fviz_eig(R.CPb)
fviz_pca_var(R.CPb,
             col.var = "contrib")

# M�s detalles en

print(summary(R.CP), digits=3)
round(R.CP$sdev^2, 2) #Varianzas de los CP, eigenvalores
round(R.CP$rotation, 2) #Matriz con eigenvectores
#Informaci�n usada en caso de estandarizar
round(R.CP$center, 2) #medias de las variables originales usadas para centrar los datos
round(R.CP$scale, 2)  #desviacion estandar de las variables originales, 0 si scale=FALSE
round(R.CP$x, 2) # Componentes principales de cada observaci�n

# Se pueden calcular las CP para nuevas observaciones
# usando la funci�n predict (ser� �til en predicci�n)
predict(R.CP, newdata=Datos[2,-10])

#Por ejemplo, si quisieramos calcular el CP1
#para la observaci�n 2, debemos notar que hay
#que restar la media y despu�s usar el eigenvector del CP1:
R.CP$rotation[,1]%*%(t(Datos[2,-10])-R.CP$center) #c�lculo de CP1 para obs 2
t(R.CP$rotation[,1])%*%R.CP$rotation[,1] #verificaci�n de condici�n a^ta=1

#### Para entender lo que se gr�fica sobre contribuciones,
#### se consideran los eigenvectores multiplicados
#### por la ra�z de la varianza de cada CP
# Ver gr�fica de contribuciones, el CP1:
R.CP$rotation[,1]*R.CP$sdev[1]

###Ahora usando la escala logar�tmica
R.CPlog=prcomp(log10(Datos[,-10]), scale = FALSE)
print(summary(R.CPlog), digits=3)
round(R.CPlog$sdev^2, 5)
round(R.CPlog$rotation, 3)
round(R.CPlog$x[1:1,], 3)
round(R.CPlog$center, 2)
predict(R.CPlog, newdata=log10(Datos[1,-10]))

##################
# Lo anterior corresponde a la parte num�rica.
# Pero en el caso de creaci�n de �ndices, ser�a
# muy importante la interpretaci�n.

# Esto se hace a partir de analizar las correlaciones
# aunque se puede usar de apoyo la gr�fica de contribuciones

fviz_pca_var(R.CPlog,
             col.var = "contrib", axes = c(1, 2))
options("scipen"=5, "digits"=2) 
cor(R.CPlog$x[,1:3],log10(Datos[,-10]))

# Posible interpretaci�n del primer CP

# Notar la alta correlaci�n (>.5) con cinco variables originales
# Arts, HealthEnv, Transp, Housing, Recre
# En particular, esa correlaci�n es positiva, es decir,
# a mayor valor de estos scores mayor valor del CP1.
# Un posible resumen, es que este CP representa
# un score de calidad sobre Arts, HealthEnv, Transp y Recre
# al mismo tiempo que se observa un costo alto de Housing.

# Esta interpretaci�n podr�a ser d�ficil de entender,
# una m�s simple es s�lo considerar las dos variables
# con correlaciones m�s fuertes: Arts y HealthEnv, 
# entonces, el CP1 refleja en mayor medida la calidad
# de acceso a sitios culturales y de salud.

# Posible interpretaci�n del segundo CP

#Este componente s�lo tiene una variable con fuerte correlaci�n:
# HealthEnv
# Dado que la correlaci�n es negativa se podr�a interpretar como
# a mayor valor se tiene una menor calidad en acceso a la salud


fviz_pca_var(R.CPlog,
             col.var = "contrib", axes = c(1, 3))
# Posible interpretaci�n del tercer CP
# Las mayores correlaciones son con Crime y Recre,
# ambas son negativas.
# Este score se puede interpretar como un valor alto en el CP3
# se asocia con un alto control de 
# crimen aunque con mal acceso a espacios recreativos



#########
##########
# Uso del paquete psych
library(psych)
p=9
pca <- principal(log10(Datos[,-10]), cor="cov", covar = TRUE, nfactor = p, rotate = "none",scores=TRUE)
pca
# standarized loadings son las correlaciones 
# entre los componentes y las
# variables originales, es decir, nos ayudan directamente
# en la interpretaci�n

#De lo no estandarizado notar
# SS loadings es la varianza de los CP1,
# adem�s:
pca$loadings[,1] #no son directamente los eigenvectores, 
                 #est�n multiplicados por una constante (ra�z varianza)

t(pca$loadings[,1])%*%pca$loadings[,1]
pca$loadings[,1]/sqrt(pca[["Vaccounted"]][1,1]) #los eigenvectores

#En general,
#los eigenvectores originales, se pueden obtener como:
pca$loadings%*%diag(1/sqrt(pca$values))

#Notar que los eigenvectores pueden diferir por el signo
#ver CP3 de prcomp vs los de principal
pca$loadings%*%diag(1/sqrt(pca$values))[,3]
R.CPlog$rotation[,3]


head(pca$scores)
#Los scores o CP que se guardan en $scores
#se calculan con los standarized loadings
#para tener varianza 1, es decir, no son los que se
#obtienen directamente con los eigenvectores
#Estos se puede obtener realizando la siguiente modificaci�n:
round(pca$scores[1,]*sqrt(pca$values), 2)


##Una gr�fica similar a las anteriores
X11()
biplot(pca,choose=c(1,2,3))

########
# Nota, los CP que se obtienen con la funci�n predict de psych
# no son los mismos que los que se obtendr�an usando los eigenvectores
# originales, sin embargo, la direcci�n de �stos casi es la misma,
# pues usa una matriz de errores


CPpsych=factor.scores(x = as.matrix(log10(Datos[,-10])), f = pca, method="components" )
CPpsych$scores[1,] #s�lo datos con los que se calcula pca

CPpsych2=predict.psych(pca, log10(Datos[,-10]), options="best.keys")
CPpsych2[1,] #nuevos datos, pero no exactamente igual en direcci�n

pairs(cbind(CPpsfs=CPpsych$scores[,1], CPpspred=CPpsych2[,1], CPpssc=pca$scores[,1], CPor=R.CPlog$x[,1]))

summary(cbind(CPpsfs=CPpsych$scores[,1], CPpspred=CPpsych2[,1], CPpssc=pca$scores[,1], CPor=R.CPlog$x[,1]))
cor(cbind(CPpsfs=CPpsych$scores[,1], CPpspred=CPpsych2[,1], CPpssc=pca$scores[,1], CPor=R.CPlog$x[,1]))


##########
# Tambi�n, sin ning�n paquete es posible obtener
# los eigenvectores:
eigen(cov(log10(Datos[,-10])))

########
# Para datos binarios u ordinales, 
# se puede usar con el paquete psych, cor="mixed" que usa la funci�n mixedCor
# Otra opci�n es usar hetcor en el paquete polycor para calcular una matrix
# de correlaci�n.

pcam <- principal(cov(log10(Datos[,-10])), cor="cov", covar = TRUE, nfactor = p, rotate = "none",scores=TRUE)
pcam

# Componentes principales usando la matriz de correlaci�n
#S�lo da un resultado, Standardized loadings
#Da diferente resultado que usar matriz de covarianzas
pcamcor <- principal(cov(log10(Datos[,-10])), covar = FALSE, nfactor = p, rotate = "none",scores=TRUE)
pcamcor

pcamcorb <- principal(log10(Datos[,-10]), covar = FALSE, nfactor = p, rotate = "none",scores=TRUE)
pcamcorb




###########################################
###Nota. Si se usa la matriz de correlaci�n, 
# las gr�ficas de contribuciones
# son m�s f�ciles de entender.


R.CP=prcomp(Datos[,-10], scale = TRUE)
library(factoextra)
X11()
fviz_eig(R.CP)
fviz_pca_var(R.CP,
             col.var = "contrib") # son directamente 
                           #las correlaciones con las variables originales

var <- get_pca_var(R.CP)
var
var$coord[,1]  ### las correlaciones son las coordenadas
cor(R.CP$x[,1],Datos[,1:9])


var$cor[,1]

var$cos2[,1]  #Correlaciones al cuadrado
cor(R.CP$x[,1],Datos[,1:9])^2

sum(var$cos2[,1]) #esto es igual a la varianza del primer CP
R.CP$sdev^2

var$contrib[,1] #corresponden al porcentaje de varianza de cada variable
                #al CP1
var$cos2[,1]/sum(var$cos2[,1])*100 #contribuci�n de cada variable a la varianza del CP1

options("scipen"=10, "digits"=2) 
var$cos2*100 #Tambi�n se puede leer por variable.



### Tidymodels
#pca como preprocesamiento
predict(R.CP, newdata=Datos[2,-10]) #obtener proyecci�n
                                    #considerando nueva observaci�n
                                    #al dato 2

library(tidyverse)
library(tidymodels)
rec <- recipe(~., data = Datos[,-10])
pca_trans <- rec %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp = 9)  #s�lo se define lo que se har�
pca_estimates <- prep(pca_trans, training = Datos[,-10]) #se obtienen los CP
(pca_data <- bake(pca_estimates, Datos[2,-10])) #se predice

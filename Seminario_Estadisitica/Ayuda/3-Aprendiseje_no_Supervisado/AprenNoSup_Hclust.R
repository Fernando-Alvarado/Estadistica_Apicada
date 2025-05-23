# Ejemplo del uso
# de la t�cnica para conglomeraci�n
# jer�rquica aglomerativa en R
rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2025-2/ApreEstAut")
library(tidyverse)
datos=mtcars
help("mtcars")

# 32 autos de 1974
#	mpg	Miles/(US) gallon    -- Millas por gal�n
#	cyl	Number of cylinders     -- Num. de cilindros
#	disp	Displacement (cu.in.) -- Desplazamiento del motor
#	hp	Gross horsepower -- Caballos de fuerza
#	drat	Rear axle ratio  -- Relaci�n de eje trasero 
#	wt	Weight (1000 lbs)  -- Peso
#	qsec	1/4 mile time    -- velocidad en segundos para recorrer 402 metros
#	vs	Engine (0 = V-shaped, 1 = straight)  -- Tipo de motor 
#	am	Transmission (0 = automatic, 1 = manual) -- Tipo de transmisi�n
#	gear	Number of forward gears  -- Num. de cambios de velocidades
#	carb	Number of carburetors  -- Num. de carburadores

summary(datos) #usaremos nuevamente los datos
              # sin preprocesar, aunque la recomendaci�n
              # es preprocesar usando alguna estandarizaci�n.


# Distancias (disimilaridades) cl�sicas (variables en formato num�rico)
dis_datos <- dist(x = datos, method="canberra")
str(dis_datos)
print( as.matrix(dist(dis_datos))[1:3,1:3] )

dis_datos2 <- dist(x = datos, method="euclidian")

# Recordar que otra funci�n con disimilaridades es daisy, help(daisy)
# que tiene a Gower para datos mixtos.

# Funci�n hclust acepta cualquier matriz de distancias
# Se debe indicar el m�todo de enlace (disimilaridad entre clusters)
#help(hclust)
#"ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" o "centroid".
clust.jer <- hclust(dis_datos, method="complete")

clust.jer2 <- hclust(dis_datos2, method="ward.D")

# Visualizaci�n del resultado
# dendograma (s�lo es �til con pocas observaciones)
X11()
par(mfrow=c(1,2))
plot(clust.jer)

# Se puede agregar la identificaci�n de los clusters
plot(clust.jer2)
rect.hclust(clust.jer2 , k = 3, border = 2:6) #border con colores

# Para graficar s�lo parte del dendograma
# por ejemplo a la altura 250 del eje vertical
X11()
par(mfrow=c(1,2))
dend.clust.jer2 <- cut( as.dendrogram(clust.jer2), h = 250)
plot(dend.clust.jer2$upper, center=TRUE)

plot(clust.jer2)

# Se puede obtener una conglomeraci�n en particular
# para realizar su interpretaci�n
# Supongamos que nos hemos decidido por tres clusters
datosv2=datos
datosv2$c3= cutree(clust.jer2, k = 3) #Se crea variable con los clusters
table(datosv2$c3)



# Por ejemplo, para interpretar usando CP
# Similar a lo que hicimos con k.means u otros m�todos
library(psych)
p=2
pca <- principal(datos, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca,group=datosv2$c3, pch=c(0,21,4)[datosv2$c3])

#Se pueden calcular �ndices para evaluar la conglomeraci�n

# Por ejemplo, �ndice "Silhouette" para los tres clusters:
library(clusterCrit)
intCriteria(as.matrix(datos),as.integer(datosv2$c3),c("Silhouette"))

# Tambi�n see puede usar NbClust para analizar varios
# �ndices para definir K
library(NbClust) 
# Notar que se usar� una distancia calculada, aunque se podr�a
# indicar como argumento
k_clus <- NbClust(diss = dis_datos, distance = NULL, min.nc = 2,
                  max.nc = 5, method = "complete", index = "silhouette") #incluyendo distancia previamente calculada
# Todos los �ndices calculados por valor de k evaludado
k_clus$All.index
# El valor de k seleccionado
k_clus$Best.nc


k_clus2 <- NbClust(data = datos, distance = "canberra", min.nc = 2,
                  max.nc = 5, method = "complete", index = "silhouette") #indicando distancia a calcular
# Todos los �ndices calculados por valor de k evaludado
k_clus2$All.index
# El valor de k seleccionado
k_clus2$Best.nc


# Tambi�n se puede usar lo comentado 
# para k-means sobre preprocesamiento y 
# uso de la disimilaridad "gower"


# Otras opciones en R son
# diana y agnes
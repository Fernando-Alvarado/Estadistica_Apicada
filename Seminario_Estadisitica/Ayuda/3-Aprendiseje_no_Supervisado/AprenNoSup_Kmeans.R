# Ejemplo del uso
# de la t?cnica de k-means para
# obtener conglomerados en R

rm(list = ls(all.names = TRUE))
gc()

#setwd("~/GitHub/Notas 2025-2/ApreEstAut")
library(tidyverse)
datos=mtcars
help("mtcars")

# 32 autos de 1974
#	mpg	Miles/(US) gallon    -- Millas por gal?n
#	cyl	Number of cylinders     -- Num. de cilindros
#	disp	Displacement (cu.in.) -- Desplazamiento del motor
#	hp	Gross horsepower -- Caballos de fuerza
#	drat	Rear axle ratio  -- Relaci?n de eje trasero 
#	wt	Weight (1000 lbs)  -- Peso
#	qsec	1/4 mile time    -- velocidad en segundos para recorrer 402 metros
#	vs	Engine (0 = V-shaped, 1 = straight)  -- Tipo de motor 
#	am	Transmission (0 = automatic, 1 = manual) -- Tipo de transmisi?n
#	gear	Number of forward gears  -- Num. de cambios de velocidades
#	carb	Number of carburetors  -- Num. de carburadores

summary(datos)

#Realmente
# cyl, gear y carb se pueden trabajar como variables ordinales
# vs y am son variables categ?ricas binarias

# Por ahora consideremos todas como variables continuas


######################
# La funci?n kmeans
set.seed(2)
# K-means, K = 3 y 20 asignaciones aleatorias de cl?steres iniciales 
# Aqu? x corresponde a los datos que ya deben estar 
# preprocesados para mejores resultados
k.means <- kmeans(x = datos, centers = 3, nstart = 20)
# Como no se estandariz?, las variables
# en la mayor escala dominan la creaci?n de 
# conglomerados


# La asignaci?n a un cluster se puede obtener con $cluster
table(k.means$cluster)

# Es de inter?s analizar qu? distingue a los clusters
# e interpretar los clusters
library(GGally)
datosc3=datos
datosc3$k3=factor(k.means$cluster)

# Se puede hacer un an?lisis gr?fico o bien con estad?sticas
# por cluster
X11()
ggpairs(data=datosc3, title="Datos", aes(colour = k3))

datosc3_Res <- datosc3 %>%
  group_by(k3) %>%
  do(estadisticas = summary(.))

datosc3_Res$estadisticas

library(psych)
describeBy(datosc3 ~ k3,mat=TRUE)

# Una posible interpretaci?n.
# Los m?s ahorradores en 1, mientras que 
# en 2 los m?s r?pidos aunque no tan ahorradores.
# El 3 se parece m?s al 2, pero con todos con 
# transmisi?n autom?tica.

# A veces los CP o alguna proyecci?n de inter?s sirve 
# de apoyo para describir el resultado
R.CP=prcomp(datos, scale = TRUE)
library(factoextra)
X11()
fviz_eig(R.CP)
fviz_pca_var(R.CP,
             col.var = "contrib")
X11()
fviz_pca_ind(R.CP, geom.ind = "point", 
             col.ind = as.factor(datosc3$k3), 
             axes = c(1, 2), 
             pointsize = 3.5)

p=2
pca <- principal(datos, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca,group=datosc3$k3, pch=c(0,21,4)[datosc3$k3])


### Otros aspectos importantes
# ?cu?l es el valor de K a elegir?

# Usando un paquete para calcular alg?n ?ndice 
# para varios valores de k,
# por ejemplo, "silhouette" (se elige el m?ximo)

set.seed(2)
X11()
figS=fviz_nbclust(datos, FUNcluster = kmeans, method = c("silhouette"), k.max = 8, nstart = 20)
figS
figS$data

# Otro paquete.
# Usando una salida en particular calculada a mano
# Difiere un poco en la definici?n, as? que se debe usar
# para calcular para los diferentes valores de k a explorar
# (para Silhouette, usa la media del ?ndice por cluster) 
library(clusterCrit)
intCriteria(as.matrix(datos),as.integer(datosc3$k3),c("Silhouette"))
intCriteria(as.matrix(datos),kmeans(x = datos, centers = 2, nstart = 20)$cluster,c("Silhouette"))

# Otro paquete.
# Una versi?n de k-means en un paquete que tiene muchos ?ndices
# La opci?n "alllong" puede ser muy tardada, pero arroja un resumen
# de los ?ndices implementados
library(NbClust)
X11()
k_clus <- NbClust(data = datos, distance = "euclidean", min.nc = 2,
                  max.nc = 6, method = "kmeans", index = "alllong")
# Todos los ?ndices calculados por valor de k evaludado
k_clus$All.index

# El valor de k seleccionado por cada ?ndice
k_clus$Best.nc

# Tambi?n se puede calcular s?lo un ?ndice
k_clus_Sil <- NbClust(data = datos, distance = "euclidean", min.nc = 2,
                      max.nc = 6, method = "kmeans", index = "silhouette")
k_clus_Sil$All.index
k_clus_Sil$Best.nc
# Esta funci?n corre la funci?n kmeans, as? que puede tambi?n
# dar la partici?n asociada al mejor valor de k
k_clus_Sil$Best.partition

# comparando con el c?lculo en intCriteria, hay una diferencia
intCriteria(as.matrix(datos),as.integer(k_clus_Sil$Best.partition),c("Silhouette"))


# Otra forma de calcular el ?ndice "Silhouette"
library(cluster)
ss=silhouette(k_clus_Sil$Best.partition, dist(datos))
ssdf=ss[,1:3]
# Replicar lo de NbClust
mean(ss[,3])
# Replicar lo de intCriteria
ssdf=as.data.frame(ssdf)
ssmean=ssdf%>% group_by(cluster) %>% summarise(mean=mean(sil_width))
mean(ssmean$mean)


#################################
# Otros aspectos b?sicos a considerar

#### Preprocesamiento b?sico

# Usar datos con media cero y varianza 1

datosst=scale(datos, scale = TRUE)
#Repetir an?lisis, pero ahora con datosst
summary(datosst)

# La funci?n preProcess en paquete caret tambi?n tiene 
# herramientas para preprocesar (muy ?tiles para predicci?n)
# La usaremos de varias formas s?lo para mostrar opciones
library(caret)
str(datos)
# Guarda los valores para realizar el preprocesamiento
preProcCont = preProcess(datos[,c("mpg", "disp", "drat","hp","wt","qsec")], method = c("center", "scale"))
preProcOrd = preProcess(datos[,c("cyl", "gear","carb")], method = c("range"))

# Aplica el preprocesamiento
datosCont <- predict(preProcCont, datos[,c("mpg", "disp", "drat","hp","wt","qsec")])
datosOrd <- predict(preProcOrd, datos[,c("cyl", "gear","carb")])

# Los datos preprocesados
datosv2=as.data.frame(cbind(datosCont, datosOrd, datos[,c("vs", "am")]))
# Repetir an?lisis con datosv2
summary(datosv2)

###Tidymodels tambi?n sirve para preprocesar los datos
library(tidyverse)
library(tidymodels)
rec <- recipe(~., data = datos) #se define lo b?sico de los datos
regla_trans <- rec %>% 
  step_normalize(all_numeric()) #se agregan reglas de preprocesamiento
datos_estimates <- prep(regla_trans, training = datos)  #se calcula c?mo se har?n esas transformaciones
datos_trans <- bake(datos_estimates, datos) #se aplica el preprocesamiento
summary(datos_trans)


######################
# Usar los componentes principales. 
# Varias opciones de acuerdo
# con el uso de matriz de covarianza o correlaci?n,
# as? como de rotaci?n.
# Tambi?n cu?ntos componentes usar, 
# por ejemplo s?lo tres
summary(R.CP)
datosCP=R.CP$x[,1:3]

# Repetir an?lisis con datosCP
summary(datosCP)

# O s?lo los CP que acumulen 80% de varianza

R.CPsum=summary(R.CP)
R.CPsum$importance
numCP=min(which(R.CPsum$importance[3,]>.8))
datosCP80=R.CP$x[,1:numCP]

# Repetir an?lisis con datosCP
summary(datosCP80)



#################################
# Extensiones para usar el tipo de variable (binarias, ordinales)

# Modificamos datos, incluyendo el metadato asociado 
# al tipo de variables
# Muchas funciones que trabajan con variables ordinales
# requieren que se haga expl?cita esta definici?n

# En este caso, la funci?n ordered() 
# es una abreviaci?n de factor(),
# en donde se incluye la informaci?n del orden
# y se hace expl?cito que es una variable
# categ?rica ordinal.

# Hay que revisar que el orden es adecuado
datos2 <- within(datos, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})

str(datos2)

#Para automatizar con los metadatos
is.ordered(datos2$vs)
is.factor(datos2$vs)
is.numeric(datos2$vs)



### Una opci?n es calcular a mano la 
# matriz de disimilaridades entre todos 
# los pares de observaciones

# daisy() del paquete library(cluster)
# opci?n "gower" toma en cuenta el tipo de variable
diss_mat <- daisy(datos2, metric = "gower")
summary(diss_mat)
#31*(32)/2  # la diagonal es cero

# funci?n pam() en paquete cluster
# versi?n similar a kmeans que permite
# modificar la disimilaridad
# Tienen como opciones "euclidean" y "manhattan"
# Aunque es m?s general al permitir incluir una
# calculada por otras funciones, daisy() o dist()

# Tambi?n usa medoids en lugar de medias 
# (una observaci?n actua como representante)


pam_clust <- pam(diss_mat, 3)
summary(pam_clust)

# calcula el ?ndice silhouette para tomar una decisi?n
# con $clustering se obtiene la agrupaci?n

X11()
biplot(pca,group=pam_clust$clustering, pch=c(0,21,4)[pam_clust$clustering])

# Funci?n para automatizar valor de k
library(fpc)
pc = pamk(diss_mat, krange=2:6, criterion="asw") #aqu? se prefieren 4 grupos con el average silhouette width
pc[2:3]



# Otra opci?n es usar kproto() del paquete clustMixType

library(clustMixType)

kpr_clust = kproto(datos2, k = 3)
# proporciona un buen resumen para analizar los datos
summary(kpr_clust)
# Para analizar valor de k
valk <- validation_kproto(method = "silhouette", data = datos2, k = 2:6, nstart = 5)
valk$indices 

X11()
biplot(pca,group=kpr_clust$cluster, pch=c(0,21,4)[kpr_clust$cluster])
#Quiz?s m?s f?cil de interpretar, pues la separaci?n
#se explica casi por el CP1


#Otras opciones, para datos mixtos, ver paquetes kamila y ClustOfVar
#para datos continuos, ver cmeans() en e1071, fanny() en cluster 
#   clara() en cluster y mkmeans() en VBmix

#########
# usar CP calculados con una versi?n para datos mixtos
# e.g. funci?n FAMD en FactoMineR. Otra opci?n paquete PCAmixdata
library(FactoMineR)
CP_datos2 <- FAMD(datos2, ncp=5, graph=FALSE)
summary(CP_datos2)
datosCP=CP_datos2$ind$coord
summary(datosCP)
# Repetir an?lisis con datosCP

# Tambi?n se podr?an usar esas dimensiones para visualizar
X11()
plot(CP_datos2, choix = c("quanti"), axes = c(1, 2))
X11()
plot(CP_datos2, choix = c("quali"), axes = c(1, 2))

datosCPwC=as.data.frame(datosCP)
datosCPwC$clus=factor(kpr_clust$cluster)
summary(datosCPwC)
X11()
datosCPwC %>%
  ggplot(aes(Dim.1,Dim.2, color=clus))+
  geom_point( size=3)

datosc3$clus=factor(kpr_clust$cluster)
describeBy(datosc3 ~ clus,mat=TRUE)



# Usando principal de pysch para los componentes

# Arreglar los datos
# binarias 0,1
# ordinales 1,2,3,...
datosPol <- within(datos, {
  cyl  <- as.numeric(ordered(cyl))
  gear <- as.numeric(ordered(gear))
  carb <- as.numeric(ordered(carb))
})

r <- mixedCor(data=datosPol,correct=0)

CPmix=principal(datosPol, nfactors = 3, rotate = "none", cor = "mixed",correct=0)
CPmix

CPmixb=principal(r$rho, nfactors = 3, rotate = "none", cor = "mixed")
CPmixb

X11()
biplot(CPmix,choose=c(1,2,3))

datosCPmix=CPmix$scores
summary(datosCPmix)

#repetir an?lisis con datosCPmix

# o visualizar usando datosCPmix
datosCPmixwC=as.data.frame(datosCPmix)
datosCPmixwC$clus=factor(kpr_clust$cluster)
summary(datosCPmixwC)
X11()
datosCPmixwC %>%
  ggplot(aes(PC1,PC2, color=clus))+
  geom_point( size=3)


#Ejemplo del c?lculo del poder predictivo de un
#modelo de regresi?n lineal m?ltiple

#Parte 1. M?todos b?sicos de remuestreo

rm(list = ls(all.names = TRUE))
gc()

library (ISLR)
help(Hitters)

Datos=Hitters
Datos = na.omit(Datos)
str(Datos)

# Supongamos que queremos investigar o 
# atinarle al salario de algunos de los
# jugadores y para esto tenemos la informaci?n hist?rica 
# (del a?o pasado aunque aqu? es de 1986)
# Estamos asumiendo que tendremos la informaci?n 
# asociada a los jugadores a los 
# que aplicaremos este modelo

##############################
### Primer modelo a explorar
##############################

# Por simplicidad, por ahora no realizaremos nada de preprocesamiento
# nos centraremos en obtener algunas m?tricas de predicci?n
# Modelo con todas las variables, s?lo efectos principales
mod1=lm(Salary ~ ., Datos) 
summary(mod1)

# La regla final ser?a el modelo ajustado 
# con todas las observaciones, usando como 
# predicci?n el valor esperado estimado de y
# para valores dados de x

# Para usar la regla dar?amos la expresi?n con coeficientes estimados 
# al usuario o bien se usar?a la funci?n predict

########
## C?lculo del poder predictivo del modelo 1

###
# Supongamos que s?lo queremos dividir la muestra
# en dos: train (80%) y test (20%)
# y calcular el MSE como medida de poder predictivo

# Una opci?n para dividir la muestra es usar sample() 
set.seed (1) #Fijar semilla para replicar c?lculos
n=dim(Datos)[1]
train <- sample(1:n, n*.8) #muestra aleatoria simple sin reemplazo
head(train) #resultado es conjunto de ?ndices correspondiente al train
length(train) #n_train
test = (-train) #?ndices que se quitar?an para obtener el train


# El resto es replicar el proceso de entrenamiento 
# de la regla a evaluar 
# con el conjunto train 
# y calcular 
# algunas m?tricas, e.g. el MSE, MAE o R2 
# usando la regla (obtenida con n_train) 
# para predecir los valores de y del test
mod1t=lm(Salary ~ ., Datos[train,]) #entrenar s?lo con train
predm1t=predict(mod1t, Datos[test,]) #evaluar s?lo con test
MSE.mod1=mean((Datos$Salary[test]-predm1t)^2) #MSE
MSE.mod1

MAE.mod1=mean(abs(Datos$Salary[test]-predm1t)) #MAE
MAE.mod1

# Por lo general, tanto el MSE como el MAE
# por s? mismas no tienen mucha interpretaci?n,
# aunque se prefieren con un valor peque?o.

# Estas medidas se usan m?s para comparar 
# posibles reglas obtenidas con otros modelos o m?todos 
# de entrenamiento (Redes Neuronales vs 
# Regresi?n vs Regresi?n polinomial vs Regresi?n lasso)

# Una medida alternativa que s? puede ser m?s informativa es una
# versi?n de la R2, es decir, la correlaci?n al cuadrado
# de "y" y "y_gorrito", o tambi?n la correlaci?n directamente:
 
R2.mod1 =cor(Datos$Salary[test],predm1t)^2 # se prefiere cerca de 1
R2.mod1


# Antes de pasar a otros m?todos de remuestreo
# alternativos para medir el
# poder predictivo veamos algunos conceptos 
# adicionales que aparecen en los textos


# Medida de error aparente (poder predictivo aparente)
# Se calcula con la regla entrenada
# usando las n observaciones, 
# considerando una m?trica definida
predm1=predict(mod1, Datos)
MSE.mod1.ap=mean((Datos$Salary-predm1)^2)
MSE.mod1.ap #vs 160150.4

# Es una MUY MALA medida de aproximaci?n 
# del poder de predicci?n:
# subestima el verdadero error de predicci?n

R2.mod1.ap=cor(Datos$Salary,predm1)^2
R2.mod1.ap  #vs 0.4268716


# Medida de error de entrenamiento (poder predictivo de entrenamiento)
# Se calcula s?lo con el conjunto de entrenamiento
predm1t=predict(mod1t, Datos[train,])
MSE.mod1.tr=mean((Datos$Salary[train]-predm1t)^2)
MSE.mod1.tr #vs 160150.4 #vs 92017.87

# Esta ?ltima s?lo aproximar?a al error aparente 
# (muy mala aproximaci?n para el poder predictivo)
# Ninguna de estas dos refleja el escenario de 
# NUEVAS observaciones

R2.mod1.tr= cor(Datos$Salary[train],predm1t)^2
R2.mod1.tr #vs 0.4268716 #vs 0.5461159



# Notar la variabilidad en la estimaci?n 
# del poder predictivo con diferentes particiones
set.seed (2) #correr con varias semillas, e.g. 2, 20
n=dim(Datos)[1]
train <- sample (1:n, n*.8)
test = (-train)
mod1t=lm(Salary ~ ., Datos[train,])
predm1t=predict(mod1t, Datos[test,])
MSE.mod1=mean((Datos$Salary[test]-predm1t)^2)
MSE.mod1 #mucha variabilidad  [1] 160150.4
R2.mod1 =cor(Datos$Salary[test],predm1t)^2 
R2.mod1 #moderada variabilidad  [1] 0.4268716

#notar que cada m?trica var?a de forma diferente


### Se busca una estimaci?n del poder
### predictivo m?s estable (menos varianza)

###
# Supongamos que queremos aplicar el m?todo
# repeated holdout method
# B=100, train (80%) y test (20%)
# y calcular MSE y R2 como medidas de poder predictivo

# Podr?amos usar un ciclo: for o while

# Pero tambi?n podemos aprovechar algunas funciones de R
# Por ejemplo, con una sola instrucci?n en R obtener 
#  los ?ndices de los B conjuntos train
set.seed(321)
B=100
IndexTrain = replicate(B, sample(1:n, n*.8))
# Se obtiene matriz donde cada columna corresponde 
#  a los ?ndices del train

# Definamos una funci?n que replica el proceso
#  de entrenamiento con el train y
#  el c?lculo de las m?tricas
#  sobre el test,
#  en cada repetici?n, b=1,...,B.

# mod1RHM es una funci?n con los siguentes argumentos:
    #Dat: Datos
    #IndTrain: Matriz, en donde cada columna 
      #tiene los ?ndices del train de la iteraci?n b
    #x es el valor de la iteraci?n b
mod1RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  mod1t=lm(Salary ~ ., Dat[train,])
  predm1t=predict(mod1t, Dat[test,])
  MSE=mean((Dat$Salary[test]-predm1t)^2)
  R2 =cor(Datos$Salary[test],predm1t)^2
  return(c(MSE, R2))
}

MSE.B.mod1= sapply(1:B,mod1RHM, IndTrain=IndexTrain, Dat=Datos)
# primer rengl?n es el MSE y segundo el R2

summary(t(MSE.B.mod1)) #ver la variabilidad entre las 100 repeticiones


# Estimaci?n del poder predictivo usando
# MSE, R2 y Repeated holdout method es
(MSE.RHM.mod1=mean(MSE.B.mod1[1,]))
(R2.RHM.mod1=mean(MSE.B.mod1[2,]))



# La variabilidad de la estimaci?n es menor, 
# e.g. correr todo el proceso del c?lculo del poder predictivo de nuevo



###
# Supongamos que queremos aplicar el m?todo
# K-Cross Validation
# K=5, train (aprox 80%) y test (aprox 20%)
# y calcular el MSE o R2 como medida de poder predictivo

# usamos un vector con valores del 1 a K
K=5
(labK=rep(1:K, length.out = n))
table(labK)

# realizamos una permutaci?n aleatoria de los pliegues
# a partir del vector labK
set.seed(1234)
(Pliegues <- sample(labK))  #seleccionamos n de n, pero orden aleatorio

# Funci?n mod1KCV con los siguientes argumentos
   #Dat: Datos
   #Plie: vector indicando a que pliegue pertenece
         #cada observaci?n
   #x es la iteraci?n k, k=1,...,K

mod1KCV=function(x, Plie, Dat){
  train <- which(Plie != x) #da los ?ndices del train
  test = (-train)
  mod1t=lm(Salary ~ ., Dat[train,])
  predm1t=predict(mod1t, Dat[test,])
  MSE=mean((Dat$Salary[test]-predm1t)^2)
  R2 =cor(Datos$Salary[test],predm1t)^2
  return(c(MSE, R2))
}

MSE.K.mod1= sapply(1:K,mod1KCV, Plie=Pliegues, Dat=Datos)
summary(t(MSE.K.mod1))


# Estimaci?n del poder predictivo usando
# MSE, R2 y K-Cross Validation es
(MSE.KCV.mod1=mean(MSE.K.mod1[1,]))
(R2.KCV.mod1=mean(MSE.K.mod1[2,]))


###
# Supongamos que queremos aplicar el m?todo
# Repeated K-Cross Validation
# B=20, K=5, train (aprox 80%) y test (aprox 20%)
# y calcular el MSE como medida de poder predictivo

# Funci?n BKCV que replica 
# todo lo hecho para K-CV.
# Mismos argumentos que Funci?n mod1KCV, 
# pues usar? esa funci?n.

BKCV=function(x, labK, Dat, K){
Pliegues <- sample(labK)
MSE.K.mod1= sapply(1:K,mod1KCV, Plie=Pliegues, Dat=Dat)
MSE.KCV.mod1=mean(MSE.K.mod1[1,])
R2.KCV.mod1=mean(MSE.K.mod1[2,])
return(c(MSE.KCV.mod1, R2.KCV.mod1))
}

set.seed(1)
B=20
MSE.BK.mod1= sapply(1:B,BKCV, labK=labK, Dat=Datos, K=K)
summary(t(MSE.BK.mod1))
(MSE.BKCV.mod1=mean(MSE.BK.mod1[1,]))  # Repeated K-Cross Validation
(R2.BKCV.mod1=mean(MSE.BK.mod1[2,]))

# comparable con 
MSE.RHM.mod1 #Repeated holdout method
R2.RHM.mod1

# Los dos m?todos Repeated tienen menor variabilidad
# al ser promedios de varias repeticiones

# Notar que a falta de poder de c?mputo, K-Cross Validation
# no es tan variable, as? que muchas veces se toma como 
# una moderada aproximaci?n del poder predictivo,
# aunque es mucho m?s variable cuando los m?todos de 
# entrenamiento son m?s complejos 
# (muchos par?metros e hiperpar?metros)

# Se recomienda hacer al menos K-CV, sobre todo
# cuando n no es tan grande.





### Nota 1.
# Cuando hay varias reglas (m?todos de entrenamiento)
# a comparar en t?rminos de poder predictivo,
# se debe aplicar el mismo m?todo de remuestreo 
# para calcular el poder predictivo 
# que tambi?n debe calcularse de la misma forma (misma m?trica).

# Buena pr?tica:
# Si es posible, tambi?n usar las mismas divisiones train-test.


### Nota 2.
# En los paquetes, para las reglas m?s com?nes hay funciones que
# calculan medidas de predicci?n
# para algunos de los m?todos de remuestreo de forma "eficiente"

# Para modelos lineales generalizados, cv.glm en boot para K-CV
library(boot)
set.seed(123)
K=5
B=20 #repetiremos el K-CV

mod1glm=glm(Salary ~ ., data=Datos)

cv=rep(NA,B)
for (i in 1:B){
  cv[i]=cv.glm(Datos, mod1glm, K=K)$delta[1]
}
(mean(cv)) #s?lo MSE


library(cvTools) #Repeated K-CV
set.seed(123)
K=5
B=20
folds <- cvFolds(nrow(Datos), K = K, R = B)
repCV(mod1, cost = mspe, folds = folds)



### Nota 3.
# Algunos paquetes usan estos m?todos de remuestreo
# pero con m?s ?nfasis en el entrenamiento y tuneo de hiperpar?metros

# Los m?s populares en R son 
# caret, funci?n train (muchos m?todos, pero ya no se mantiene para los actuales)
# https://topepo.github.io/caret/model-training-and-tuning.html#model-training-and-parameter-tuning

# tidymodels, funci?n tune_grid (se podr?a decir que es el reemplazo de caret, 
# en proceso de desarrollo, con varios ejemplos)
# https://www.tidymodels.org/start/tuning/
# https://www.tidymodels.org/
# https://www.tmwr.org/

# e1071 con su funci?n tune para los m?todos 
# m?s com?nes, eg. svm y nnet
# https://cran.r-project.org/web/packages/e1071/


# Tambi?n paquete mlr3:
# https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html
# https://mlr3.mlr-org.com/





##########################################
# Algunos aspectos de inter?s de caret
library(caret)
# tiene su propia funci?n para particionar la muestra
# supongamos que s?lo la dividiremos en 2
set.seed(1234)
(Partition <- createDataPartition(Datos$Salary,p = .80, groups =1, list = FALSE)) #groups para particionar de acuerdo con Y
# regresa s?lo los ?ndices
TrainDatos <- Datos[ Partition,]
TestDatos  <- Datos[-Partition,]
# Por ejemplo, aqu? no se tune? nada
tc <- trainControl(method = "none") # aqu? permite indicar m?todo de remuestreo para tunear
# por ejemplo, para un modelo de regresi?n se podr?a tunear el intercepto (define una malla)
# Ajustamos el modelo de regresi?n con el train
lm1_tr_caret = caret::train(Salary ~ ., data = TrainDatos, method = "lm",
                trControl = tc)
lm1_tr_caret
summary(lm1_tr_caret)
# la ventaja es que tiene medidas interesantes para poder predictivo
# al aplicar en el test
lm1_test_caret = predict(lm1_tr_caret, TestDatos)
(MedPred=postResample(pred = lm1_test_caret, obs = TestDatos$Salary))
# MAE-Mean absolute error, media de la diferencia en valor absoluto |y-\hat{y}|
# Rsquared, correlaci?n al cuadrado entre y y \hat{y}
#Notar que RMSE es la ra?z de MSE
MedPred[1]^2

# replica de las otras medidas de poder predictivo:
mean(abs(lm1_test_caret-TestDatos$Salary))
cor(lm1_test_caret,TestDatos$Salary)^2


#Otra ventaja de caret es que tiene funciones que realizan la divisi?n
# considerando muestreo aleatorio simple con estratificaci?n.
# La estratificaci?n la realizan usando los valores de la variable
# dependiente y (usa los percentiles en el caso continuo 
# y los niveles en caso categ?rico)

Partition2 <- createDataPartition(Datos$Salary,p = .80, groups =5, list = FALSE)

# Tambi?n repite la partici?n B veces
# cada columna son los ?ndices del train de cada partici?n.
Partition3 <- createDataPartition(Datos$Salary,p = .80, groups =5, list = FALSE, times=5)


# Tambi?n tiene lo correspondiente a K-CV con estratificaci?n
createFolds(Datos$Salary, k = 5, list = FALSE, returnTrain = FALSE) #repetir B veces, por ejemplo con replicate



# En la mayor?a de las aplicaciones o ejemplos de los paquetes
# caret o tidymodels se pone m?s atenci?n en el entrenamiento,
# en particular en el tuneo. 

# Para la evaluaci?n del poder predictivo muchos ejemplos
# muestran s?lo la divisi?n en dos grupos, 
# pero como ya lo vimos la estimaci?n 
# por ese m?todo tiene mucha variabilidad.

# En la actualidad ya hay m?s apoyo para m?todos de remuestreo
# y el c?lculo del poder predictivo

# https://www.tidymodels.org/learn/work/nested-resampling/
# Sec 4.3 en https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html


# Como ya se mencion?, se recomienda al menos usar un K-CV
# para estimar el poder predictivo

# Algunas referencias sobre el tema son
# i) Cap. 2 en Hyperparameter Tuning for Machine and Deep Learning with R
# ii) Cap?tulo 5 en Data Mining:Practical Machine Learning Tools and Techniques
# iii) Performance-Estimation Properties of Cross-Validation-Based Protocols with Simultaneous Hyper-Parameter Optimization
#    Tsamardinos et. al. (2015)
# iv) A bias correction for the minimum error rate in cross-validation
#    Tibshirani y Tibshirani (2009)
# v) Cross-validation pitfalls when selecting and assessing regression and classification models
#    Krstajic et. al. (2014), 

#MSE, no tiene punto de referencia, solo se compara 
#R^2, nos dice la corrwlacion entre la y a la y gorrito, quiere decir que minetras mas cercano a 1 mejor 

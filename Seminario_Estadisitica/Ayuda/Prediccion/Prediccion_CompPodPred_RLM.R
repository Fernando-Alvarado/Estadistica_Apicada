#######################
# Predicci�n, Y continua.

#Ejemplo sobre la comparaci�n del poder predictivo
# para varias reglas basadas en regresi�n lineal m�ltiple:

# 1. S�lo efectos principales
# 2. Incluyendo las variables continuas al cuadrado
# 3. Incluyendo las variables continuas al cuadrado y selecci�n por
#    pasos usando criterio BIC
# 4. Incluyendo las variables continuas al cuadrado y selecci�n usando
#    lasso con K-CV y MSE para tunear par�metro lambda

rm(list = ls(all.names = TRUE))
gc()


# Datos a usar
library (ISLR)
help(Hitters)

Datos=Hitters
Datos = na.omit(Datos)
str(Datos)


# Por simplicidad se usar�:
# Repeated holdout method
# B=50, train (80%) y test (20%)
# y c�lculo del MSE como medida de poder predictivo

# La partici�n se realizar� con caret y 
# ser� la misma para todos los modelos
library(caret)
set.seed(1)
B=50
Partition<- createDataPartition(Datos$Salary, p = .80, groups =4, list = FALSE, times = B)
# Notar que la partici�n considera que el test tendr� en la medida de lo
# posible observaciones de 4 grupos formados por los percentiles .25,.5,.75 de Salary


##############################
### Primer modelo a explorar
### S�lo efectos principales
##############################
##
# Descripci�n del m�todo de entrenamiento y regla final
mod1=lm(Salary ~ ., Datos)
summary(mod1)  # regla final, la que se usar�a en producci�n

##
# Medici�n del poder predictivo de la regla final

mod1RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  modtr=lm(Salary ~ ., Dat[train,])
  predte=predict(modtr, Dat[test,])
  MSE=mean((Dat$Salary[test]-predte)^2)
  return(MSE)
}

MSE.B.mod1= sapply(1:B,mod1RHM, IndTrain=Partition, Dat=Datos)
(MSE.RHM.mod1=mean(MSE.B.mod1))
#[1] 118964.9

##############################
### Segundo modelo a explorar
### Incluyendo las variables continuas al cuadrado
##############################

##
# Descripci�n del m�todo de entrenamiento y regla final

# Analicemos cuales son las variables continuas
str(Datos)

# r base con sapply
(continuas = names(which(sapply(Datos, is.numeric))) )
# tambi�n se puede usar tidyverse
library(tidyverse)
(categoricas = names(Datos %>% select_if(~!is.numeric(.x))) )

# definir variables predictoras continuas
xnames=continuas[!continuas %in%  c("Salary")]
forexp=as.formula(  paste('Salary ~.',"+", paste(paste('I(',xnames,'^2)',collapse = ' + ')  ) )) 
forexp
mod2=lm(forexp, Datos)  # regla final, la que se usar�a en producci�n
summary(mod2)

# Notar que la f�rmula que se defini� 
# no depende para nada de las observaciones,
# as� que aunque es parte del preprocesamiento
# no cambiar�a en la divisi�n train-test

##
# Medici�n del poder predictivo

mod2RHM=function(x, IndTrain, Dat, forme){
  train= IndTrain[,x]
  test = (-train)
  modtr=lm(forme, Dat[train,])
  predte=predict(modtr, Dat[test,])
  MSE=mean((Dat$Salary[test]-predte)^2)
  return(MSE)
}

MSE.B.mod2= sapply(1:B,mod2RHM, IndTrain=Partition, Dat=Datos, forme=forexp)
(MSE.RHM.mod2=mean(MSE.B.mod2))
# [1] 124168.7  vs # mod 1: 118964.9

##############################
### Tercer modelo a explorar
### Incluyendo las variables continuas al cuadrado  
###  y selecci�n por pasos usando criterio BIC
##############################

##
# Descripci�n del m�todo de entrenamiento y regla final

# se requiere una f�rmula para definir el modelo m�s complejo
upperfor=as.formula(  paste('~.',"+", paste('I(',xnames,'^2)',collapse = ' + ') ) ) 
upperfor
# se requiere definir la penalizaci�n para BIC
pen=log(dim(Datos)[1])  #depende de los datos

# Adicional al ajuste mod2 se realiza la selecci�n
# con step o stepAIC se empieza en mod2 y se busca entre los modelos definidos por scope
library(MASS)
mod3 <- stepAIC(mod2, scope =list(upper = upperfor, lower = ~1), trace = FALSE,direction ="both", k=pen)
summary(mod3)

# Notar que la f�rmula que se defini� 
# no depende para nada de las observaciones.
# Pero hay que tomar en cuenta la definici�n 
# del modelo inicial: mod2,
# as� como la penalizaci�n que s� depende de n.

# Adem�s la selecci�n de variables siempre es
# parte del entrenamiento cuando el criterio
# se basa en las observaciones, como el AIC o BIC


##
# Medici�n del poder predictivo

mod3RHM=function(x, IndTrain, Dat, forme, upform){
  train= IndTrain[,x]
  test = (-train)
  assign("DatosAux", Dat[train,], envir = .GlobalEnv) #Cuidado stepAIC o step buscan la base de datos en el environment global cuando se usa scope 
  modAux=lm(forme, data=DatosAux)
  penAux=log(dim(DatosAux)[1])
  modtr=stepAIC(modAux, scope =list(upper = upform, lower = ~1), trace = FALSE,direction ="both", k=penAux)
  predte=predict(modtr, Dat[test,])
  MSE=mean((Dat$Salary[test]-predte)^2)
  return(MSE)
}


#MSE.B.mod3= sapply(1:B,mod3RHM, IndTrain=Partition, Dat=Datos, forme=forexp, upform=upperfor)
#Mejor asegurar usando un for en orden, pues DatosAux estar� en el environment global
MSE.B.mod3=NA
for(ik in 1:B){
   MSE.B.mod3[ik]=mod3RHM(ik,IndTrain=Partition, Dat=Datos, forme=forexp, upform=upperfor)
  }
(MSE.RHM.mod3=mean(MSE.B.mod3))
# [1] 108758.1 vs # mod2: 124168.7  vs # mod 1: 118964.9

# Pausa para observar variabilidad si s�lo se usar� una 
# partici�n

summary(MSE.B.mod3)


##############################
### Cuarto modelo a explorar
### Incluyendo las variables continuas al cuadrado  
###  y selecci�n usando lasso con glmnet
##############################

##
# Descripci�n del m�todo de entrenamiento y regla final

# Usaremos la f�rmula con variables al cuadrado para crear matrix X 
forexp
Xmod4 <- model.matrix(forexp, data=Datos)[,-1]
Ymod4 <- Datos[,"Salary"] 

#recordar que para lasso alpha=1 (est� por default)
#por otro lado hay dos opciones de estimadores, 
# relax = FALSE est� por default (lo usaremos por ahora)
# recordar que los betas estimados no son EMV
library(glmnet)
mod4.lasso = glmnet(Xmod4, Ymod4, family = gaussian("identity"), nlambda = 50)
#Faltar�a tunear (definir valor) de lambda
# podr�a ser con AIC, BIC, pero ahora usaremos 
# un m�todo de remuestreo basado en poder predictvo

#glmnet tiene una opci�n para tunear lambda 
# usando K-CV con base en poder predictivo
set.seed(1) #dado que se usar� un m�todo de remuestreo 
mod4.lasso.tun=cv.glmnet(Xmod4, Ymod4, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = gaussian("identity"), nlambda = 50)
X11()
plot(mod4.lasso.tun)
print.cv.glmnet(mod4.lasso.tun)
mod4.lasso.tun$lambda.min   #mod4.lasso.tun$lambda.1se
coef(mod4.lasso.tun, s = "lambda.min")

# Regla final es aquella que usa el lambda seleccionado
# estimando los betas con todos los datos. 
help("cv.glmnet")

# Por ejemplo

#usando la salida de cv.glmnet
predict(mod4.lasso.tun, newx = Xmod4[1:5,], type = "response", s = "lambda.min")

#usando la salida de glmnet, pero especificando un valor de lambda
predict(mod4.lasso, newx = Xmod4[1:5,], type = "response", s = mod4.lasso.tun$lambda.min)



##
# Medici�n del poder predictivo
# aqu� s� es sumamente importante 
# incluir el tuneo dentro del entrenamiento

mod4RHM=function(x, IndTrain, Dat, forme){
  train= IndTrain[,x]
  test = (-train)
  Xmod4ttotal = model.matrix(forme, data=Dat)[,-1]
  Xmod4t = Xmod4ttotal[train, ]
  Ymod4t = Dat[train,"Salary"] 
  mod4t.lasso.tun=cv.glmnet(Xmod4t, Ymod4t, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = gaussian("identity"), nlambda = 50)
  predte=predict(mod4t.lasso.tun, newx = Xmod4ttotal[test,], type = "response", s = "lambda.min")
  MSE=mean((Dat$Salary[test]-predte)^2)
  return(MSE)
}

set.seed(1)
MSE.B.mod4= sapply(1:B,mod4RHM, IndTrain=Partition, Dat=Datos, forme=forexp)
(MSE.RHM.mod4=mean(MSE.B.mod4))
# [1]  101678.8  vs # mod3: 108758.1 vs # mod2: 124168.7  vs # mod 1: 118964.9

# Comparaci�n final
MSE.RHM.mod1 # [1] 118964.9
MSE.RHM.mod2 # [1] 124168.7
MSE.RHM.mod3 # [1] 108758.1
MSE.RHM.mod4 # [1] 101678.8

# Para este ejemplo, la regla con mejor poder predictivo es la 4, es decir
# la que usa lasso sobre los efectos principales y variables al cuadrado

# Notar la diferencia de la estimaci�n del MSE usado para tunear en el m�todo lasso
# en el proceso de entrenamiento: MSE 88637 vs 
# la estimaci�n del poder predictivo correcta: MSE 101678.8





# Nota sobre el preprocesamiento para el usuario final.
# Se usa model.matrix, as� que para nuevos datos y
# cuando hay variables categ�ricas
# se deben guardar 
# los niveles de todas las variables categ�ricas y su orden
# as� se puede construir de forma adecuada la matrix

xlevs <- lapply(Datos[,sapply(Datos, is.factor), drop = F], function(j){
  levels(j)
})

# As�, para datos nuevos, por ejemplo Datos[2:2,]
datosnew=Datos[2:2,]
str(datosnew) #Aqu� se guarda la informaci�n, pues la obtuvimos al filtrar, pero en un caso real
# un usuario nuevo deber�a indicar de la misma forma
# el dataframe, incluyendo los niveles para las variables factor
# si s�lo hay un valor podr�a incluirlo como categ�rica
datosnew$Division=factor(as.character(datosnew$Division)) #aqu� se podr�an indicar los niveles
datosnew$League=factor(as.character(datosnew$League))
datosnew$NewLeague=factor(as.character(datosnew$NewLeague))
str(datosnew)  #notar que esto es incorrecto
X_new <- model.matrix(forexp, data=datosnew, xlev = xlevs)[,-1] #aqu� se pasan los niveles

Xmod4[2:2,]
X_new

# Error sin indicar los niveles
model.matrix(forexp, data=datosnew)[,-1]








#############################################
####### Repliquemos el tuneo con caret para lasso
### (por tiempo no se calcula poder predictivo)

# Construcci�n de la regla

# Forma para realizar el tuneo con 5 K-CV
Tuneo <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = defaultSummary,
  selectionFunction = "best"
)

set.seed(1)
ReglaTuneada <- train(Xmod4, Ymod4, method = "glmnet", 
                             trControl = Tuneo,metric = "RMSE",
                             tuneGrid = expand.grid(alpha = 1,
                                                    lambda = seq(0.1,20,by = 0.1)))

ReglaTuneada

ReglaTuneada$bestTune 

coef(ReglaTuneada$finalModel, ReglaTuneada$bestTune$lambda)

modGLMNET=glmnet(Xmod4, Ymod4, family = "gaussian", alpha = 1)
coef(modGLMNET,ReglaTuneada$bestTune$lambda)
cbind(coef(ReglaTuneada$finalModel, ReglaTuneada$bestTune$lambda), coef(modGLMNET,ReglaTuneada$bestTune$lambda))

# es decir, el objeto $finalModel de caret considera la soluci�n global
# que se evaluar� en lambda=5
(ReglaTuneada_uso = predict(ReglaTuneada, Xmod4[1:5,]))

(predict(modGLMNET, newx = Xmod4[1:5,], type = "response", s = ReglaTuneada$bestTune$lambda))

# Notar que la funci�n train de caret deber�a usarse
# con el conjunto train al calcular el poder predictivo.
# Es decir, corresponde al tuneo y
# realiza una partici�n al interior del conjunto train.



#############################################
####### Repliquemos el tuneo con tidymodels para lasso
### (por tiempo no se calcula poder predictivo)


library(tidyverse)
library(tidymodels)

#alpha es mixture, usar 1.
#lambda es penalty

DataTidymodels=as.data.frame(cbind(Xmod4, Ymod4))
names(DataTidymodels)

#Definir los datos, qu� variable es de inter�s y preprocesamiento
rec <- recipe(Ymod4 ~ ., data = DataTidymodels) 

##Caracter�sticas del modelo y lo que se tunear� con tune()
lasso_mod <- linear_reg(mode = "regression",
                        penalty = tune(),
                        mixture = 1) %>% 
                        set_engine("glmnet")
lasso_mod%>%translate() #Para verificar el uso de glmnet

#Definici�n de trabajo
wf <- workflow() %>%
  add_model(lasso_mod) %>% add_recipe(rec)

#Se define c�mo se har� el tuneo (m�todo de remuestreo)
folds <- rsample::vfold_cv(DataTidymodels, v = 5, strata = Ymod4)

#Se define la malla a usar para cada hiperpar�metro definido con tune()
my_grid <- tibble(penalty = 10^seq(-3, 2, length.out = 50))

#Se define y ejecuta la tarea de tunear
set.seed(1)
my_res <- wf %>% 
           tune_grid(resamples = folds,
            grid = my_grid,
            control = control_grid(verbose = FALSE, save_pred = TRUE),
            metrics = metric_set(rmse))

show_best(my_res,metric ="rmse")

best_mod <- my_res %>% select_best(metric ="rmse")
best_mod

#Regla tuneada con las n observaciones
final_fitted <- finalize_workflow(wf, best_mod) %>%
  fit(data = DataTidymodels)

#Predecir con el modelo entrenado
predict(final_fitted, DataTidymodels[1:5,])

(predict(modGLMNET, newx = Xmod4[1:5,], type = "response", s = best_mod$penalty))

coef(final_fitted$fit$fit$fit, best_mod$penalty)
coef(modGLMNET, best_mod$penalty )

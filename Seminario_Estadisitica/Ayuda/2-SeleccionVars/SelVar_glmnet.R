# Ejemplo para el problema de selecci�n de variables
# como un problema de optimizaci�n con penalizaciones

# Uso del paquete glmnet

rm(list = ls(all.names = TRUE))
gc()

# Datos (mismos que los usados para optimizaci�n discreta)
library(MASS)
help("birthwt")

str(birthwt)
summary(birthwt)
# Mismo Preprocesamiento
bwtMod <- with(birthwt, {
  race <- factor(race, levels=c(1,2,3),labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0, levels=c(TRUE,FALSE),labels=c("TRUE", "FALSE"))
  ftv <- factor(ftv) # por default los niveles son 0,1,2,3,4,5,6
  levels(ftv)[-(1:2)] <- "2+"        #convertir todo los niveles con 2, 3, 4, 5 y 6 en 2+. Forma f�cil para recodificar
  data.frame(low = factor(low), age, lwt, race, smoke = factor(smoke > 0),
             ptd, ht = factor(ht > 0), ui = factor(ui > 0), ftv)
})

summary(bwtMod)
str(bwtMod)


library(glmnet)

#### No usa formato tipo formula.
# Se necesita ingresar la matriz dise�o X sin 
# columna asociada al intercepto.
# Por simplicidad, se mostrar� primero el modelo con efectos principales:

# model.matrix permite construir 
# la matriz dise�o X a partir de una f�rmula
XbwtMod <- model.matrix(low ~ ., data=bwtMod)[,-1]
Y <- bwtMod[,"low"] 

### Primero algunos aspectos computaciones.
### Obtener una estimaci�n equivalente a glm,
### es decir, sin penalizaciones.

# La funci�n glmnet est� programada para ElasticNet,
# usando por default alpha=1, es decir, el m�todo Lasso.
# Tambi�n relax = FALSE por default, es decir, 
# se realiza la estimaci�n usando gamma=1.
#
fit0.glmnet <- glmnet(XbwtMod, Y, family = binomial("logit"), lambda = 0.
                      , standardize = FALSE   #por default es TRUE
                      ,  intercept = TRUE)    #por default es TRUE
coef(fit0.glmnet)

fit0.glm <- glm(low ~ ., family = binomial("logit"), data = bwtMod)
summary(fit0.glm)

cbind(coef(fit0.glmnet),coef(fit0.glm)) #peque�as diferencias por 
                                        #los m�todos num�ricos

#�Por qu� glmnet estandariza internamente?
### En general, cuando se trabaja con un glm.
#Los betas en la escala original 
#de cada variable no son comparables:
#hay betas muy chicos, pero significativos (e.g asociado a lwt)
#mientras que hay betas grandes no significativos (e.g asociado a raceother),
#es decir, el significado de los betas es muy diferente seg�n la escala
#y tipo de variable.

#Una forma de tratar de hacerlos comparables es estandarizar
#i.e. Transformar las variables (media 0 y varianza 1)

# Internamente el paquete glmnet estandariza, 
# pero en la salida los coeficientes se regresan
# a la escala original. 
# En este caso relax = TRUE (gamma=0) tambi�n da lo mismo
fit0b.glmnet <- glmnet(XbwtMod, Y, family = binomial("logit"), lambda = 0,
                       standardize = TRUE, 
                      intercept = TRUE, 
                      relax = TRUE) #por default FALSE
cbind(coef(fit0b.glmnet),coef(fit0b.glmnet$relaxed)) #se guardan dos betas
                                                     #betas lasso
                                                     #betas por MV para los valores distintos de cero

################
#Uso de penalizaciones
# Si no se incluye el argumento lambda, se resuelve 
#    el problema para una malla de valores de lambda.
# Se puede indicar la malla (sucesi�n de valores a evaluar) o
# bien el n�mero de valores que incluya la malla (la da el paquete) 
# (s�lo ajusta hasta que ya no ve cambios importantes)

#Recordar que alpha=1 y relax = FALSE por default
#es decir, m�todo lasso.
# Probemos con una malla de 200 valores:
fit1.glmnet <- glmnet(XbwtMod, Y, family = binomial("logit"), nlambda = 200)
print(fit1.glmnet)
coef(fit1.glmnet) #los betas_lambda, cada columna un valor de lambda diferente

# Notar que se obtienen tantos coeficientes estimados
# como valores de lambda efectivos.

############# Ahora se debe seleccionar un lambda para trabajar,
#### se puede usar el AIC o el BIC, aunque para predicci�n hay otros criterios.

# Ejemplo del c�lculo de valores proporcionales al BIC o al AIC
# Se calculan a mano usando informaci�n que se guarda con 
# la funci�n glmnet como sigue:
# -((fit1.glmnet$nulldev)-fit1.glmnet$nulldev * (1 - fit1.glmnet$dev.ratio))
# que representa un valor proporcional -2ln(Lik)
AICfit1=-((fit1.glmnet$nulldev)-fit1.glmnet$nulldev * (1 - fit1.glmnet$dev.ratio))+
         2*fit1.glmnet$df #la �ltima parte corresponde a la penalizaci�n, de ah� el 2
MinAICfit1=which.min(AICfit1)
AICfit1[MinAICfit1] #Nota. no es el AIC exacto, pero sirve para ordenar
coef(fit1.glmnet)[,MinAICfit1]

#otras formas de calcular el valor proporcional -2ln(Lik)
# -((fit1.glmnet$nulldev)-fit1.glmnet$nulldev * (1 - fit1.glmnet$dev.ratio))[MinAICfit1]
#-(fit1.glmnet$nulldev - deviance(fit1.glmnet))[MinAICfit1]
#-((fit1.glmnet$nulldev)-fit1.glmnet$nulldev * (1 - fit1.glmnet$dev.ratio))[MinAICfit1]

#########################
#Ejemplo para verificar que lo anterior es el AIC o un valor proporcional

#Empecemos con el modelo ajustado y sus componentes:
coef(fit1.glmnet)[,MinAICfit1] #coeficientes del modelo
fit1.glmnet$lambda[MinAICfit1] #valor de lambda
# probabilidades estimadas con los par�metros lasso
probs=predict(fit1.glmnet,newx=XbwtMod,s=fit1.glmnet$lambda[MinAICfit1],type="response") #estimaci�n de la probabilidad y=1 para cada observaci�n en la muestra
loglik=sum( (Y==0)*log(1-probs)+(Y==1)*log(probs)) #verosimilitud
-2*loglik+2*(fit1.glmnet$df[MinAICfit1]+1)  #AIC exacto, +1 es por b0

AICfit1[MinAICfit1] #notar que el usado fue este
                    #�cu�l es la constante que le falta o sobra?
# La modificaci�n calculada con herramientas de glmnet
# no considera la penalizaci�n por el intercepto y usa una constante adicional
# asociada al modelo NULL, aquel con s�lo el intercepto

loglikNULL=sum( (Y==0)*log(1-mean(Y==1))+(Y==1)*log(mean(Y==1)))
AICfit1[MinAICfit1] - 2*loglikNULL+ 2


###########Nota. Lo anterior usa betas_lambda, de los cuales no
###########sabemos su distribuci�n

# Para el enfoque de estimaci�n o inferencia
# quiz�s es m�s f�cil y conveniente usar la opci�n
# relax=TRUE (EMV)

# Es decir, los dos pasos para seleccionar variables
# 1) obtener betas_lambda
# 2) obtener los betas usando MV para las variables 
#     con betas diferentes de cero

fit2.glmnet <- glmnet(XbwtMod, Y, family = binomial("logit"), nlambda = 200, relax = TRUE)
print(fit2.glmnet)
#Se guardan dos resultados, los de lasso y los EMV
coef(fit2.glmnet)
coef(fit2.glmnet$relaxed)

# C�lculo del AIC usando los EMV, aumentar $relaxed:
AICfit2=-((fit2.glmnet$relaxed$nulldev)-fit2.glmnet$relaxed$nulldev * (1 - fit2.glmnet$relaxed$dev.ratio))+2*fit2.glmnet$relaxed$df
MinAICfit2=which.min(AICfit2)
coef(fit2.glmnet$relaxed)[,MinAICfit2]
AICfit2[MinAICfit2]-2*loglikNULL+2


# Este an�lisis se puede hacer directamente con glm
# para relax=TRUE,
# pues los betas relaxed de glmnet son estimados por MV


ModelList=list(NA)
AICList=list(NA)

nlam=length(fit2.glmnet$lambda)
XbwtModcint <- model.matrix(low ~ ., data=bwtMod) #matriz dise�o completa

for(jk in 1:nlam){
  Xjk=XbwtModcint[, coef(fit2.glmnet)[,jk]!=0]
  modeljk=glm.fit(Xjk, Y, family = binomial("logit"))
  ModelList[[jk]]=modeljk
  AICList[[jk]]=modeljk$aic  #solo guarda aic, no bic
}

MinAIC=which.min(unlist(AICList))
ModMinAIC=ModelList[[MinAIC]]
coefficients(ModMinAIC)
AICList[[MinAIC]]  #valor exacto del AIC


# Con el BIC, se reemplaza 2 por log(dim(bwtMod)[1])
BICfit2=-((fit2.glmnet$relaxed$nulldev)-fit2.glmnet$relaxed$nulldev * (1 - fit2.glmnet$relaxed$dev.ratio))+
       log(dim(bwtMod)[1])*fit2.glmnet$relaxed$df
MinBICfit2=which.min(BICfit2)
coef(fit2.glmnet$relaxed)[,MinBICfit2] #muchos m�s ceros
BICfit2[MinBICfit2]-2*loglikNULL+log(dim(bwtMod)[1])

# Se puede ejecutar con glm para obtener el modelo
fitBIC.glm <- glm(low ~ ptd+lwt+ht, family = binomial("logit"), data = bwtMod)
summary(fitBIC.glm)
BIC(fitBIC.glm) #mismo modelo que con step y BIC

# Nota. glmnet tiene la funci�n cv.glmnet que ser� muy �til para
# definir el valor de lambda con base en m�tricas asociadas al poder predictivo

#### Modelos m�s complejos con glmnet.
# Dado que la matriz dise�o se construye posiblemente con una f�rmula,
# se pueden incluir modelos m�s complejos, e.g. con interacciones

XbwtModc <- model.matrix(low ~ .^2 + I(age^2)
                         + I(lwt^2), data=bwtMod)[,-1]
summary(XbwtModc)
# Hay una columna con puros ceros, de una interacci�n. Se debe eliminar
# en las versiones viejas de glmnet, en las nuevas hay que tener cuidado
#XbwtModc=XbwtModc[, !(colnames(XbwtModc) %in% c("htTRUE:uiTRUE"))]

fit3.glmnet <- glmnet(XbwtModc, Y, family = binomial("logit"), nlambda=200  , relax = TRUE)
print(fit3.glmnet)


# Con el BIC
BICfit3=-((fit3.glmnet$relaxed$nulldev)-fit3.glmnet$relaxed$nulldev * (1 - fit3.glmnet$relaxed$dev.ratio))+log(dim(bwtMod)[1])*fit3.glmnet$relaxed$df
MinBICfit3=which.min(BICfit3)
coef(fit3.glmnet$relaxed)[,MinBICfit3]

fitBICc.glm <- glm(low ~ I(lwt*(ptd==FALSE)), family = binomial("logit"), data = bwtMod)
summary(fitBICc.glm)
BIC(fitBICc.glm)

#Nota. En glmnet se puede usar el argumento penalty.factor con un
#valor igual a cero para forzar a que el modelo 
#siempre incluya ciertas variables.
 


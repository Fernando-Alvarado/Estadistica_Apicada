### Ejemplo de problema tipo ANOVA (dos factores) 
### con datos binarios

### Parte 2. Comparación de modelos con diferentes funciones ligas,
### verificación de supuestos e interpretación. 

rm(list = ls(all.names = TRUE))
gc()

# El NYT reportó resultados sobre el efecto del medicamento AZT 
#(Zidovudina o Azidotimidina fue el primer medicamento antirretroviral para el desarrollo de SIDA)
# para disminuir el desarrollo de los síntomas de SIDA (Feb 15, 1991).
# Un grupo de 338 veteranos cuyo sistema inmune empezaba a mostrar signos de la
# enfermedad causada por el VIH fueron considerados para el estudio.
# De forma aleatoria se asignó la aplicación de AZT, de manera que
# a un grupo se le aplicó de forma inmediata
# y al otro grupo se le aplicó hasta que sus células T mostraban que el
# sistema inmune estaba muy débil.
# Para ajustar los grupos se consideró la raza de los veteranos

# Las tres variables que se consideraron son:
# X= 1 si se aplicó el AZT de forma inmediata
# Z= raza (blanca, negra)
# Y= desarrollo de síntomas de SIDA (1-Yes, 0-No)

# Los resultaron fueron los siguientes.
# Durante los primeros 3 años del estudio,
# de los que recibieron de forma inmediata AZT
# 11 de 63 veteranos de raza negra desarrollaron síntomas de SIDA
# y que 14 de 107 de raza blanca.
# De los que NO recibieron de forma inmediata AZT
# 12 de 55 veteranos de raza negra desarrollaron síntomas de SIDA
# y que 32 de 113 de raza blanca

# ¿Qué se puede inferir con estos datos sobre el uso o no de AZT?
# ¿Existe evidencia de un beneficio al suministrar AZT?

n=c(11,63-11,14, 107-14, 12, 55-12, 32, 113-32)
Y=c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")
Z=c("b", "b", "w", "w", "b", "b", "w", "w")
X=c("AZT", "AZT", "AZT", "AZT", "NoAZT", "NoAZT", "NoAZT", "NoAZT")

#Datos agrupados forma 1
DatosAg= data.frame(cbind(n,Y,Z,X))
summary(DatosAg)
DatosAg$n=as.numeric(as.character(DatosAg$n))
summary(DatosAg)

#Datos desagrupados (más común en la práctica)
head(DatosAg)
library(tidyverse)
Datos=DatosAg %>% group_by(Y,Z,X) %>%
  do( data.frame(unos= rep(1, .$n)) )
head(Datos)
sum(DatosAg$n)
summary(Datos)
Datos=as.data.frame(Datos)
Datos[sapply(Datos, is.character)] <- lapply(Datos[sapply(Datos, is.character)], 
                                             as.factor)
summary(Datos)

#En lo que sigue usaremos los datos no agrupados

#Comenzaremos con el modelo más completo, es decir
#incluyendo interacciones

#Hay 4 ligas que se podrían usar:
fitlogit=glm(Y~X*Z, family = binomial(link="logit"), data=Datos)
summary(fitlogit)

fitprob=glm(Y~X*Z, family = binomial(link="probit"), data=Datos)
summary(fitprob)

fitlog=glm(Y~X*Z, family = binomial(link="log"), data=Datos)
summary(fitlog)

fitcll=glm(Y~X*Z, family = binomial(link="cloglog"), data=Datos)
summary(fitcll)


#Notar que las estimaciones son diferentes, pues cada modelo
#tiene una distinta forma de representar la relación entre la media
#y las covariables


#Sin embargo, dado que las covariables son categóricas,
#el modelo más completo 
#(incluye todas las interacciones entre las covariables que se considera),
#lleva a que las estimaciones de las probabilidades son las mismas
#sin importar la liga que se use

newdata <- data.frame(X = c("AZT", "NoAZT","AZT", "NoAZT"), Z = c("b", "b","w", "w") )
newdata$logit <- predict(fitlogit, newdata[,1:2], type = c("response"), se.fit=TRUE)$fit
newdata$prob <- predict(fitprob, newdata[,1:2], type = c("response"), se.fit=TRUE)$fit
newdata$log <- predict(fitlog, newdata[,1:2], type = c("response"), se.fit=TRUE)$fit
newdata$cll <- predict(fitcll, newdata[,1:2], type = c("response"), se.fit=TRUE)$fit
newdata

#Además con el modelo completo también los criterios AIC y BIC coinciden
c(AIC(fitlogit), AIC(fitprob), AIC(fitlog), AIC(fitcll))
c(BIC(fitlogit), BIC(fitprob), BIC(fitlog), BIC(fitcll))

#Las diferencias se observarán al considerar modelos reducidos que 
#no incluyan ciertas interacciones entre las variables

#Tomemos en lo que sigue de referencia el modelo logit
summary(fitlogit)

#La primera prueba que se debe realizar es 
#la similar a la prueba asociada a la tabla ANOVA en regresión lineal múltiple
library(multcomp)
K=matrix(c(0,1,0,0,
           0,0,1,0,
           0,0,0,1), ncol=4, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fitlogit, linfct=K, rhs=m), test=Chisqtest())  #Chisqtest() es apropiada para datos donde y no es continua
# Se rechaza H0, lo que implica que se puede proceder
# al análisis del modelo


#Por lo general, en un problema tipo ANOVA
# se desea un modelo reducido antes de hacer
# la inferencia sobre la variable de interés (X)


#Por ejemplo, se procede a eliminar las interacciones.
#En este caso correspondería sólo al parámetro beta3.

#Dado que aquí sólo hay una variable asociada a la interacción
#se puede leer directamente del summary

#El p-value es .240, por lo que no se rechaza H0

#Un modelo reducido con sólo efectos principales es plausible

fitlogitb=glm(Y~X+Z, family = binomial(link="logit"), data=Datos)
summary(fitlogitb)

#Notar que también a partir de este modelo se puede
#reducir aún más el modelo, pues parece que la variable Z
#no agrega información relevante en la modelación
#de la esperanza una vez que se agrega la variable X (p-value =.84755)

fitlogitc=glm(Y~X, family = binomial(link="logit"), data=Datos)
summary(fitlogitc)

#Con este modelo podríamos ya responder las preguntas
#sobre el efecto de la variable X

#Notar que con la prueba de bondad de ajuste 
#no se rechaza H0
#es decir, el modelo es plausible para los datos
anova(fitlogit, fitlogitc, test = "Chisq")

#Algo similar se debería realizar con el resto de modelos

fitprobc=glm(Y~X, family = binomial(link="probit"), data=Datos)
fitlogc=glm(Y~X, family = binomial(link="log"), data=Datos)
fitcllc=glm(Y~X, family = binomial(link="cloglog"), data=Datos)
c(AIC(fitlogitc), AIC(fitprobc), AIC(fitlogc), AIC(fitcllc))

#Nuevamente es un modelo que incluye sólo una variable (sería el completo)
#Por eso la coincidencia. En general, con variables continuas no se
#obtiene la coincidencia y se debería seleccionar una liga.

#Uno de los aspectos más relevantes es analizar si el modelo era adecuado
#antes de realizar las pruebas de hipótesis
#Es decir, con el modelo con interacciones

X11()
library(ggplot2)
library(ggResidpanel)
resid_panel(fitlogit, plots=c("all"))
#Notar que para datos binarios, la mayoría de gráficas no tiene sentido
#sobre todo porque también las covariables son categóricas

library(statmod)
fitlogitqr <- qresid( fitlogit) #Notar que estos son diferentes a los del
                                #panel de arriba, incluye cierta aleatorización
                                #para datos binarios
X11()
qqnorm( fitlogitqr, las=1 ); qqline( fitlogitqr) 
nortest::lillie.test(fitlogitqr)
shapiro.test(fitlogitqr)
library(DHARMa)  #Los residuales simulados también son útiles en este caso
set.seed(123)
fitlogitres <- simulateResiduals(fittedModel = fitlogit)
X11()
plot(fitlogitres )  #Aquí no tenemos variables continuas,
                    # y estas gráficas son más informativas en general.
                    #El principal problema en un modelo bernoulli
                    # podría ser sobre
                    # el parámetro de dispersión, en ese caso
                    # por lo general se rechaza la prueba
                    # correspondiente al test de dispersión.
                    #Una alternativa es considerar el 
                    # modelo quasibinomial.
# No se observan problemas con los supuestos del modelo


# Regla de dedo para ver si un modelo es adecuado en cuanto al supuesto
# sobre el parámetro de dispersión que se asume igual a 1.
# Residual deviance / degrees of freedom debe ser similar a 1

deviance(fitlogit)/df.residual(fitlogit)

# O usando el que sería el estimador del parámetro de dispersión

# Estimador de phi
sum(residuals(fitlogit, "pearson")^2)/(dim(Datos)[1]-summary(fitlogit)$df[3])

# No se tienen problemas con los supuestos y ya 
# se puede usar el modelo completo y el reducido.

# Dado que el modelo es muy sencillo,
# una opción que sale directamente del summary
# es la prueba sobre diferencia entre aplicar AZT o no
summary(fitlogitc)

# eta(Y;X)=b0+b1 NoAZT
# eta(Y;X=AZT)=b0
# eta(Y;X=NoAZT)=b0+b1

#H0:b1=0 vs Ha:b1!=0
# Dado que se rechaza H0, hay evidencia
# para concluir beta1 != 0 (0.00961 **)
# Es decir: Sí hay un efecto con la aplicación de AZT


# La respuesta a la pregunta de los investigadores
# en este caso incluye dirección
# pues se habla de evidencia de un beneficio.

# Ha: eta(Y;X=AZT)=b0 < eta(Y;X=NoAZT)=b0+b1
#H0:b1<=0 vs Ha:b1>0

library(multcomp)
K=matrix(c(0,1), ncol=2, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fitlogitc, linfct=K, rhs=m, alternative="greater"))
#Se rechaza H0, p-value=0.00481
#Por lo que se puede concluir que sí existe un beneficio.


#### Otros aspectos de interpretación
# Para b1, la lectura se hace contra el nivel
# de referencia, en este caso
# beta1 se estima como .7218,
# es decir, como es positivo hay un
# incremento en la probabilidad de desarrollar
# enfermedad grave al no aplicar el AZT comparado
# con la aplicación de AZT

# Muchas veces se usa el cociente de momios para interpretar
# Para variables binarias se compara contra el nivel de referencia
# Este corresponde a: exp(parámetro)

exp(fitlogitc$coeff[2])
exp(confint(fitlogitc))

# El cociente entre las probabilidades 
# de desarrollar una enfermedad grave y no desarrollarla
# aumenta entre 1.2 y 3.6 veces cuando no se aplica el AZT comparado
# con aplicar el AZT al 95% de confianza.

# Notar que aquí es más fácil presentar las probabilidades, pues
# sólo hay dos grupos (incluso si se tuvieran los 4 grupos usando Z)

# Quizás usar intervalos de confianza simultáneos
library(multcomp)
K=matrix(c(1,0,
           1,1), ncol=2, nrow=2, byrow=TRUE)
ICeta=confint(glht(fitlogitc, linfct=K), level=.95)

# Aquí es creciente la función liga
Fg_1 <- family(fitlogitc)$linkinv
ICmuLI=Fg_1(ICeta$confint[1:2,2])
ICmuLS=Fg_1(ICeta$confint[1:2,3])
Estmu=Fg_1(ICeta$confint[1:2,1])
rbind(ICmuLI,Estmu,ICmuLS) #Col 1 - AZT
                           #Col 2 - NoAZT
                           #Notar que el traslape se da, pues
                           #estos intervalos no son los más adecuados
                           #para comparar, para eso están las pruebas de 
                           #hipótesis

# Notar que el cociente de momios o odds ratio aquí es fácil de 
# calcular
Estmu[1]/(1-Estmu[1]) #Cociente para AZT
Estmu[2]/(1-Estmu[2]) #Cociente para No AZT

Estmu[2]/(1-Estmu[2])/(Estmu[1]/(1-Estmu[1])) #Cociente de momios


#Una gráfica resumen

DatosIC=data.frame(t(rbind(ICmuLI,Estmu,ICmuLS)))
DatosIC$x=c(0,1)
DatosIC$X=c("AZT","NoAZT")

ggplot(DatosIC, aes(X, Estmu)) + geom_point() + 
  geom_errorbar(aes(ymin = ICmuLI, ymax = ICmuLS))+ theme_bw()


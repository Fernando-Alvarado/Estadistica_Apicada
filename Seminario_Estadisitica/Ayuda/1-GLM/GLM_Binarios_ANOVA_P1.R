### Ejemplo de problema tipo ANOVA (dos factores) 
### con datos binarios

### Parte 1. Algunos comentarios sobre el ajuste 
### y pruebas de hipótesis cuando todas las variables son categóricas.

### Para ejemplificar esta parte se considera una regresión logística,
### es decir, distribución Bernoulli con liga logit

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



#Formato de los datos

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


### Datos agrupados forma 2 
### (común cuando se definen experimentos binomiales)
nY1=c(11,14,  12,  32)
nY0=c(63-11,107-14, 55-12, 113-32)
Z1=c("b", "w", "b", "w")
X1=c("AZT", "AZT", "NoAZT", "NoAZT")
DatosAg2=as.data.frame(cbind(nY1, nY0, Z1,X1 ))
data1 <- matrix(append(nY1, nY0),ncol=2)

# Para ejemplificar el uso de la función glm,
# consideraremos un modelo de efectos principales,
# es decir, sin interacciones entre Z1 y X1

#####
### Igualdad en la estimación y pruebas de hipótesis
### Notar que R por default transforma a tipo factor
### Aunque es deseable definir esto desde antes

str(DatosAg)
fit1=glm(I(Y=="Yes")~X+Z, family = binomial(link="logit"), weights= n, data=DatosAg)
summary(fit1)

str(Datos)
fit2=glm(Y~X+Z, family = binomial(link="logit"), data=Datos)
summary(fit2)

# No se usa un dataframe, en su lugar matrices y vectores
fit3=glm(data1~X1+Z1, family = binomial(link="logit"))
summary(fit3)  

###Notar que no hay igualdad en la devianza ni en el AIC
###El caso más diferente es el 3, pues éste asume que 
###para cada cruce entre las variables categóricas
###se fijó desde el inicio el tamaño de la muestra,
###haciendo que sólo existan 4 observaciones correspondientes
###a experimentos binomiales.

###Sin embargo, todas las estimaciones puntuales son iguales,
###así como sus varianzas. 


### Una prueba que comúnmente se usa
### para modelos donde todas las X's con categóricas 
# se llama "Prueba de bondad de ajuste Ji-cuadrada".
# Se usa mucho en estos modelos para analizar 
# la conveniencia de un modelo reducido.

#H0: es plausible el modelo reducido vs
#Ha: se requiere el modelo completo

#Modelo completo: aquél que considera una probabilidad
#diferente para cada cruce de las variables categóricas
#(el más complejo posible).

# Sólo es posible aplicarla en algunos casos.
# Las categorías que se usan en esta prueba
# se forman a partir del número de patrones
# que tienen las variables explicativas.
# Notar que esto sólo se puede realizar con pocos valores en 
# las variables explicativas -variables continuas categorizadas
# o variables categóricas de origen.

# En este caso la prueba tiene una distribución ji-cuadrada
# Donde los grados de libertad son m-m'
# con m como # de patrones
# en este caso hay 2 etiquetas en Z1 y 2 en X1
# con lo que se forman 4 patrones, de donde
# m= 4

# m' son el número de parámetros estimados en el modelo reducido
# en este modelo no hay parámetro de dispersión, sólo beta0, beta1 y beta2
# m'= 3

### En algunos casos, es posible obtener esta prueba
# a partir de la devianza.

# La devianza sólo es válida para esta prueba 
# cuando los grados de libertad son igual a
# Número de patrones en las variables explicativas - número de parámetros en el modelo. 
# El contraste de bondad de ajuste en estos casos se realiza comparando con una 
# Ji-cuadrada con grados de libertad = número de patrones - número de parámetros en el modelo
# Ho: Los datos parecen provenir del modelo ajustado vs Ha: los datos no provienen del modelo ajustado

# En este caso sólo con fit3 se obtiene la devianza adecuada para la prueba de bondad de ajuste

# P-value
pchisq(fit3$deviance, df=fit3$df.residual, lower.tail=FALSE)

#Notar que no se rechaza H0, por lo que es plausible el modelo reducido


# Fuera de lo anterior no hay problema con el formato de los datos
# que se usa para el ajuste.
# Sin embargo, la comparación de los modelos (cambiando la liga) siempre 
# se debe realizar considerando el mismo formato de datos a usar

# Nota. La prueba de bondad de ajuste siempre se puede obtener fácilmente
# del modelo más complejo vs modelo reducido
# con las herramientas sobre pruebas de hipótesis
# que están disponibles.

fit3c=glm(data1~X1*Z1, family = binomial(link="logit"))
summary(fit3c) 
anova(fit3c, fit3, test = "Chisq")
#también se podría usar multcomp

# Otro ejemplo, con un modelo aún más sencillo
fit3r2=glm(data1~X1, family = binomial(link="logit"))
summary(fit3r2)
# P-value
pchisq(fit3r2$deviance, df=fit3r2$df.residual, lower.tail=FALSE)
anova(fit3c, fit3r2, test = "Chisq")

s# No importa el formato de los datos
fit1c=glm(I(Y=="Yes")~X*Z, family = binomial(link="logit"), weights= n, data=DatosAg)
fit1r2=glm(I(Y=="Yes")~X, family = binomial(link="logit"), weights= n, data=DatosAg)
anova(fit1c, fit1r2, test = "Chisq")



# Por otro lado, todas las pruebas de hipótesis son
# equivalentes entre las tres formas de ajuste


### Usando el modelo de efectos principales

# Supongamos que queremos realizar la prueba
# similar a la asociada a la tabla anova
# H0: beta1=0 y beta2=0
library(multcomp)
K=matrix(c(0,1,0, 
           0,0,1), ncol=3, nrow=2, byrow=TRUE)
m=c(0, 0)
summary(glht(fit1, linfct=K, rhs=m), test=Chisqtest() ) 
# Para datos donde Y es categórica no se recomienda la 
# aproximación usando la prueba F:
summary(glht(fit1, linfct=K, rhs=m), test=Ftest() )
#dado que la aproximación F es mala y hay más error tipo II.


# Son equivalentes sin importar qué modelo de datos se usó
summary(glht(fit2, linfct=K, rhs=m), test=Chisqtest() )
summary(glht(fit3, linfct=K, rhs=m), test=Chisqtest() )



## Esta igualdad en pruebas de hipótesis
## se debe a que las varianzas y estimaciones son las mismas

coef(fit1)
coef(fit2)
coef(fit3)
vcov(fit1)
vcov(fit2)
vcov(fit3)


# Algunas gráficas
X11()
library(lessR)
Datos$ZX=factor(paste(Datos$Z,Datos$X, sep="."))
summary(Datos)
v.Z.X=Datos$ZX
v.Y=Datos$Y
BarChart(x=v.Z.X , by=v.Y, stack100=TRUE)

library(vcd)
mosaic(~X+Y|Z,  data = Datos, split = TRUE)

library(ggmosaic)
ggplot(data = Datos)+
  geom_mosaic(aes(x = product(Y), fill = Y, conds = product(X, Z)), 
              divider = c("vspine", "hspine", "hspine"))+
  labs(x = "AZT:Raza", y = "Y", 
       title = "Frecuencias relativas por Raza y AZT")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Datos)+
  geom_mosaic(aes(x = product(Y, X), fill = Y))+
  facet_grid(. ~Z)+
  labs(x="AZT", y = "Y", title = "Frecuencias relativas por Raza y AZT")+
  theme(plot.title = element_text(hjust = 0.5))


# ¿ La verificación de supuestos es igual ?
# No es igual, con datos desagrupados hay más
# información para trabajar y es el formato deseable.

# Aunque en el caso de experimentos binomiales
# se podrían revisar los supuestos en ambos
# formatos (desagrupado y agrupado formato 2), 
# aunque deberían existir más muestras binomiales
# para que tenga sentido el resultado de las gráficas
# (en ninguno de los dos formatos debería ver problemas)

library(DHARMa)  #Los residuales simulados también son útiles en este caso
set.seed(123)
fit1res <- simulateResiduals(fittedModel = fit1)
X11()
plot(fit1res )

set.seed(123)
fit2res <- simulateResiduals(fittedModel = fit2)
X11()
plot(fit2res )


set.seed(123)
fit3res <- simulateResiduals(fittedModel = fit3)
X11()
plot(fit3res )

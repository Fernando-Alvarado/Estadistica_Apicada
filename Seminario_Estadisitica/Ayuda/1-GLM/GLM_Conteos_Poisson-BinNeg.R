### Ejemplo de modelado básico para conteos  

rm(list = ls(all.names = TRUE))
gc()
library(lme4)
data(grouseticks)
help(grouseticks)
#### Datos que intentan estudiar el número de sheep ticks (garrapatas) en las cabezas de 403
#### aves "red grouse" (polluelos)

#### La pregunta de investigación es si estas garrapatas varían 
#### en promedio de acuerdo 
#### con la altura sobre el nivel del mar donde están las aves 

# Sin embargo, una variable confusora puede ser el año de 
# recolección de la información, pues las condiciones climáticas
# fueron muy diferentes.

#### Este problema tiene como objetivo la estimación y descripción de las
#### relaciones del número promedio de garrapatas con dos variables: una categórica
#### y una continua (similar a un problema tipo ANCOVA, aunque no lo es)



head(grouseticks)
summary(grouseticks) ### base a nivel polluelo 
#### ¿el número de garrapatas es comparable entre las
#### observaciones o se debe hacer un ajuste (término offset)?
#### Dado que son polluelos todos (misma superficie), 
#### se espera que
#### sí sean comparables y no es necesario el ajuste.

str(grouseticks)

#TICKS
#number of ticks sampled

#HEIGHT
#height above sea level (meters)

#YEAR
#year (-1900)

X11()
hist(grouseticks$TICKS, col="grey", border=NA, las=1, breaks=0:90)

#### Se deben analizar si hay muchos ceros o pocos, 
#### pues eso sería signo de sobre o sub dispersión (sobre todo cuando se ve un histograma bimodal)

#### En ocasiones es adecuado observar ceros, aunque si son muchos
#### se debe proceder al uso de otros modelos



library(ggplot2)
ggplot(data=grouseticks, aes(x=HEIGHT,y=TICKS, colour=YEAR))+ geom_point()+
 theme_classic()

### ¿La variable Y 
### es realmente independiente entre los polluelos?
### ¿son variables independientes?



#### Primer modelo, con todas las interacciones
levels(grouseticks$YEAR)
fit1 <- glm(TICKS ~ YEAR*HEIGHT, family=poisson(link="log"), data=grouseticks)
summary(fit1)

# Modelo teórico:
# Distribución Poisson con
# eta(YEAR, HEIGHT)=b0+ b1 YEAR96 + b2 YEAR97 + b3 HEIGHT +
#                      b4 YEAR96:HEIGHT + b5 YEAR97:HEIGHT
#  log(E(TICKS;YEAR, HEIGHT))= eta(YEAR, HEIGHT)


# Regla de dedo para ver si es un modelo adecuado en cuanto al supuesto de
# la media igual a la varianza que se hace en un modelo Poisson
# Traducido en checar si el parámetro de dispersión es igual a 1.

# Se revisan los posible estimadores del parámetro de dispersión: 
# Residual deviance / degrees of freedom    (debe ser similar a 1)

deviance(fit1)/df.residual(fit1) # Se ve muy diferente de 1

# O usando el que sería el estimador del parámetro de dispersión de Pearson

sum(residuals(fit1, "pearson")^2)/(dim(grouseticks)[1]-summary(fit1)$df[3])

# En este caso es muy diferente de 1, lo que muestra problemas
# con el supuesto de media igual a varianza

# Veamos cómo se ve la verificación de supuestos

X11()
library(ggplot2)
library(ggResidpanel)
resid_panel(fit1, plots=c("all")) # sólo son útiles las dos
                                  # gráficas superiores, en los extremos

# Parece que hay problemas con la QQplot (checar otras opciones),
# aunque también 
# en la forma de modelar, ver figura superior izquierda

library(DHARMa)  
set.seed(123)
fit1res <- simulateResiduals(fittedModel = fit1)
X11()
plot(fit1res )

# Se observan muchos problemas.



### Dos alternativas básicas

### I. quasipoisson
### Permitir ajustar un parámetro de dispersión al modelo Poisson
### Mismos parámetros, pero diferente dispersión y pruebas de hipótesis

fit2 <- glm(TICKS ~ YEAR*HEIGHT, family=quasipoisson, data=grouseticks)
summary(fit2)

# Desventaja. -No se puede realizar mayor verificación de supuestos
#             -No permite comparación de modelos usando AIC o BIC    

### II. Usar un modelo binomial negativo (recomendado)
library(MASS)
fit3 <- glm.nb(TICKS ~ YEAR*HEIGHT, data=grouseticks, link="log")
summary(fit3)
# Modelo teórico:
# Distribución Binomial Negativa con
# eta(YEAR, HEIGHT)=b0+ b1 YEAR96 + b2 YEAR97 + b3 HEIGHT +
#                      b4 YEAR96:HEIGHT + b5 YEAR97:HEIGHT
#  log(E(TICKS;YEAR, HEIGHT))= eta(YEAR, HEIGHT)

# sólo se pueden verificar supuestos con DHARMa

set.seed(123)
fit3res <- simulateResiduals(fittedModel = fit3)
X11()
plot(fit3res )

# este modelo ya parece mucho más adecuado


# comparación de modelos, Poisson vs NB. AIC o BIC:
c(AIC(fit1), AIC(fit3)) #ya nos sugería la preferencia por BN
c(BIC(fit1), BIC(fit3))

#Aquí ya se podría comenzar con la interpretación del modelo
#y realizar pruebas de hipótesis. 

#####################################
# En este ejemplo se puede observar que hay muchos ceros
# quizás un modelo de ceros inflados o hurdle podrían ser
# una opción

# Modelo con ceros inflados

# Paquete pscl es muy fácil de usar, aunque sólo está limitado
# a ciertos modelos.
library(pscl)

fit4_zero=zeroinfl(TICKS ~ YEAR*HEIGHT | 1, data=grouseticks, dist="negbin")
summary(fit4_zero) # Log(theta)=log(0.9001) 
c(AIC(fit3), AIC(fit4_zero)) # no parece necesario
c(BIC(fit3), BIC(fit4_zero))


fit5_zero=zeroinfl(TICKS ~ YEAR*HEIGHT | 1, data=grouseticks, dist="poisson")
summary(fit5_zero)
c(AIC(fit1), AIC(fit5_zero)) # sí parece necesario
c(BIC(fit1), BIC(fit5_zero))

# El paquete VGAM, también permite usar modelos comunes glm para conteos
# además de traer algunas herramientas de apoyo.

library(VGAM)
fit4_zerob <- vglm(TICKS ~ YEAR*HEIGHT, zinegbinomialff, grouseticks)
summary(fit4_zerob) #theta=exp(-1.05e-01) y (Intercept):3 es -coef del modelo logit (diferencias numéricas)
summary(fit4_zero)
AIC(fit4_zerob)
AIC(fit4_zero)

#fit5_zerob <- vglm(TICKS ~ YEAR*HEIGHT, zipoissonff, grouseticks)
#summary(fit5_zerob)


# Una forma visual de ver si el ajuste parece correcto en conteos
rootogram4(fit4_zerob)

## El modelo que parece adecuado en vglm para obtener rootogram4
fit3b <- vglm(TICKS ~ YEAR*HEIGHT, negbinomial, grouseticks)
summary(fit3b)   #exp(-0.10526)
summary(fit3)   
rootogram4(fit3b)

# Ejemplo de un caso donde el modelo no es del todo adecuado
fit1b <- vglm(TICKS ~ YEAR*HEIGHT, poissonff, grouseticks)
rootogram4(fit1b)
summary(fit1b)
summary(fit1)

# Ya no se puede usar DHARMa con vgam, ni con pscl


######## Nos quedamos con modelo BN sin ceros inflados.
# Visualización e interpretación cuando se usa la liga log

dat95= data.frame(HEIGHT=seq(400, 540, by=1), TICKS=predict(fit3, newdata=data.frame(HEIGHT=seq(400, 540, by=1), YEAR=rep("95", 141) ), type="response" )   )
dat96= data.frame(HEIGHT=seq(400, 540, by=1), TICKS=predict(fit3, newdata=data.frame(HEIGHT=seq(400, 540, by=1), YEAR=rep("96", 141) ), type="response" )   )
dat97= data.frame(HEIGHT=seq(400, 540, by=1), TICKS=predict(fit3, newdata=data.frame(HEIGHT=seq(400, 540, by=1), YEAR=rep("97", 141) ), type="response" )   )

X11()
ggplot(data=grouseticks, aes(x=HEIGHT,y=TICKS, colour=YEAR))+ geom_point()+
  geom_line(data= dat95, colour="red" )+
  geom_line(data= dat96, colour="green" )+
  geom_line(data= dat97, colour="blue" )+ theme_classic()
  
summary(fit3)
# eta(YEAR, HEIGHT)=b0+ b1 YEAR96 + b2 YEAR97 + b3 HEIGHT +
#                      b4 YEAR96:HEIGHT + b5 YEAR97:HEIGHT
#  log(E(TICKS;YEAR, HEIGHT))= eta(YEAR, HEIGHT)

#prueba similar a la asociada a la tabla anova
library(multcomp)
K=matrix(c(0,1,0,0,0,0,
           0,0,1,0,0,0,
           0,0,0,1,0,0,
           0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=5, byrow=TRUE)
m=c(0,0,0,0,0)
summary(glht(fit3, linfct=K, rhs=m), test=Chisqtest())  #Wald asumiendo normalidad
# Se rechaza H0, al menos una covariable ayuda a modelar el número de garrapatas


# Se podrían plantear diferentes preguntas
# ¿hay un efecto del año de estudio en el número promedio de garrapatas, 
# una vez que se considera en el modelo a la altura sobre el nivel del mar?
# Ni hay efecto es igual a mismo eta para diferentes años
K=matrix(c(0,1,0,0,0,0,
           0,0,1,0,0,0,
           0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=4, byrow=TRUE)
m=c(0,0,0,0)
summary(glht(fit3, linfct=K, rhs=m), test=Chisqtest())  #Wald asumiendo normalidad
# Se rechaza H0, por lo que  considerando la altura del nivel del mar en el modelo,
# hay un efecto significativo del año de estudio 
# en la modelación del número promedio de garrapatas. 
# Es decir,  la asociación
# entre la altura sobre el nivel del mar con el promedio de garrapatas
# es diferente en al menos un par de años.



# ¿la altura sobre el nivel del mar afecta al número promedio de garrapatas, 
# una vez que se considera en el modelo el año de estudio?
# Si no afecta no debería existir relación en ningún año
K=matrix(c(0,0,0,1,0,0,
           0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit3, linfct=K, rhs=m), test=Chisqtest())  #Wald asumiendo normalidad
# Se rechaza H0, por lo que  considerando el año de estudio en el modelo,
# hay un efecto significativo de la altura sobre el nivel del mar 
# en la modelación del número promedio de garrapatas. 
# Es decir, la inclusión de la altura sobre el nivel del mar 
# ayuda a modelar el promedio de garrapatas en al menos un
# año de estudio


# Para complementar la prueba de hipótesis anterior,
# se puede proceder con las simultáneas
# Notar que aquí se deben poner las hipótesis
# usando las combinaciones lineales específicas para cada año
K=matrix(c(0,0,0,1,0,0,
           0,0,0,1,1,0,
           0,0,0,1,0,1), ncol=6, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit3, linfct=K, rhs=m))  


# Notar que la global es equivalente, pero las simultáneas no (requieren combinaciones lineales específicas)
# Para la global, comparar la siguiente salida con la obtenida en líneas 259-263
summary(glht(fit3, linfct=K, rhs=m), test=Chisqtest())  #Wald asumiendo normalidad



#¿el efecto de la altura sobre el nivel del mar en 
# el número de garrapatas es el mismo en cada año de estudio?
K=matrix(c(0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(fit3, linfct=K, rhs=m), test=Chisqtest())  #Wald asumiendo normalidad
#Se rechaza H0, por lo que el efecto es diferenciado entre al menos 
#dos años de estudio.


#ejemplos de interpretación

summary(fit3)
#En el año 95, al aumentar un metro adicional en la altura sobre el nivel del mar,
#el número promedio de garrapatas se reduce en 4% (exp(-0.041308)=0.9595335)

# Considerando una altura de 450 metros sobre el nivel del mar, 
# el número promedio de garrapatas en el año 96 
# representa el exp(-10.820259+450*0.026132)*100=2.557783*100=255.7783% del
# número promedio en el año 95

Means95vs96=predict(fit3, newdata=data.frame(HEIGHT=c(450, 450), YEAR=c("95", "96") ), type="response" )
Means95vs96[2]/Means95vs96[1]

###################################
# Extra. Ejemplo de un modelo mixto simple
library(lme4)
fit3mix <- glmer.nb(TICKS ~ YEAR*HEIGHT+(1|BROOD), data=grouseticks)
summary(fit3mix)

set.seed(123)
fit3mixres <- simulateResiduals(fittedModel = fit3mix)
X11()
plot(fit3mixres )


dat95m= data.frame(HEIGHT=seq(400, 540, by=1), TICKS=predict(fit3mix, newdata=data.frame(HEIGHT=seq(400, 540, by=1), YEAR=rep("95", 141) ), type="response",re.form=NA )   )
dat96m= data.frame(HEIGHT=seq(400, 540, by=1), TICKS=predict(fit3mix, newdata=data.frame(HEIGHT=seq(400, 540, by=1), YEAR=rep("96", 141) ), type="response",re.form=NA )   )
dat97m= data.frame(HEIGHT=seq(400, 540, by=1), TICKS=predict(fit3mix, newdata=data.frame(HEIGHT=seq(400, 540, by=1), YEAR=rep("97", 141) ), type="response",re.form=NA )   )

X11()

ggplot(data=grouseticks, aes(x=HEIGHT,y=TICKS, colour=YEAR))+ geom_point()+
  geom_line(data= dat95m, colour="red" )+
  geom_line(data= dat96m, colour="green" )+
  geom_line(data= dat97m, colour="blue" )+ theme_classic()


# ¿la altura sobre el nivel del mar afecta al número promedio de garrapatas, 
# una vez que se considera en el modelo el año de estudio?
K=matrix(c(0,0,0,1,0,0,
           0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit3mix, linfct=K, rhs=m), test=Chisqtest())  #Wald asumiendo normalidad

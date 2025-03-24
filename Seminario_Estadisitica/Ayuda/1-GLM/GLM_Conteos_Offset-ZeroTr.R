### Ejemplos de modelado para conteos, donde se usa un término offset y
### es necesario usar un modelo especial dado la ausencia de ceros



rm(list = ls(all.names = TRUE))
gc()
###########################
# Uso de un término offset (tasas)
library(mdhglm)
data("train")
help("train")
head(train)

#### Los datos corresponden a accidentes anuales (y) entre trenes
#### y carros en Inglaterra entre 1975 y 2003 (x, x=0 es 1975).

#### Además de los accidentes, la base de datos contiene
#### los millones de kilometros (t) que los trenes
#### viajaron en cada uno de los años

#### La pregunta de investigación era analizar
#### si los accidentes se han incrementado en los últimos años

#### Para lo anterior era necesario estandarizar por el
#### número de millones de kilometros que los trenes viajaron al año
#### es decir, es conveniente hablar de tasas (incidencia) en lugar de conteos

#### En este caso, el interés es ver si
#### log(mu_y/t)=b0+b1x
#### donde mu_y representa el promedio de accidentes (y-número de accidentes),
#### x el año y t el total de kilómetros.
#### Es decir, los conteos se estandarizan por el número de kilómetros
#### y la variable explicativa son los años,
#### considerando x=el número de años desde 1975
#### x = 0 para 1975 y x=28 para 2003

summary(train)

X11()
plot(train$x, train$y/train$t)

# Notar que para dejar en términos de la variable con conteos,
# al considerar la liga log se debe incluir log(train$t) [offset]

#### log(mu_y/t)=b0+b1x
####    log(mu_y)=log(t)+b0+b1x

train$logtkil=log(train$t)

fit1 <- glm(y ~ x+offset(logtkil) , family=poisson(link="log"), data=train)
summary(fit1)

# Regla de dedo para analizar si hay un problema por 
# considerar el parámetro de dispersión igual a 1
deviance(fit1)/df.residual(fit1)

# En este caso no es muy diferente de 1

library(DHARMa)  
set.seed(1234)
fit1res <- simulateResiduals(fittedModel = fit1)
X11()
plot(fit1res )

# Tampoco se observan evidencias muy fuertes en contra de los supuestos de este modelo
# Éste se podría usar para analizar las preguntas de investigación

# Antes, veamos si tiene alguna ventaja considerar un modelo
# binomial negativo, de la misma forma considerando la liga log
library(MASS)
fit2 <- glm.nb(y ~ x+offset(logtkil), data=train, link="log")
summary(fit2)

set.seed(123)
fit2res <- simulateResiduals(fittedModel = fit2)
X11()
plot(fit2res)

# También parece un buen modelo, de hecho un poco mejor. Veamos los criterios
# AIC y BIC

c(AIC(fit1), AIC(fit2))
# Usando el AIC también se podría optar por el modelo binomial negativo
c(BIC(fit1), BIC(fit2))
# Pero con el BIC se prefiere el Poisson (menos parámetros)

# Realmente la interpretación de ambos modelos llevará a las mismas conclusiones
# pues se parecen mucho.


### Interpretación

summary(fit2)

#log(mu/t)=b0+b1x
#-4.2-.0337x

# Se observa que b1 es diferente de cero (p-value=0.00893), 
# De hecho 
# se podría hacer la prueba con dirección donde Ha: b1<0
# ya que la función liga log es creciente y 
# hay decremento en los accidentes a través de los años si
# Ha: b1<0

library(multcomp)
K=matrix(c(0,1), ncol=2, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit2, linfct=K, rhs=m, alternative="less"))  
# Por lo que se concluye que sí hay un decremento

# De forma puntual también se observaba que
# la tasa de accidentes ha decrecido, ya que beta1 es negativo y 
#Además:
exp(coef(fit2)[2]) #es el factor asociado
#Es decir, por cada año se ha reducido un (1-.966689164)*100%
# =3.331084%

# Por ejemplo, comparemos las tasas promedio para 1975 y 2003
# Supongamos que además de la estimación puntual también queremos
# intervalos de confianza.
# Recomendación, usar intervalos simultáneos para resumir modelo. 

# Si el interés sólo son las tasas, ya no es necesario tener el
# término offset, pues ese sólo modifica los conteos.
# Basta con la parte del componente sistemático asociado a los
# parámetros. Multcomp puede ser más útil para tasas


library(multcomp)
K=matrix(c(1,0,
           1,28), ncol=2, nrow=2, byrow=TRUE)

fitE <- glht(fit2, linfct = K)
fitci <- confint(fitE, level = 0.90)

exp(fitci$confint) # aplicar función inversa de la función liga
#La tasa de accidentes pasó de .015 (.010,.022) en 1975 a .0058 (.004,.009) en 2003







### Ejemplo con datos con ceros truncados

library(foreign)
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ztp.dta")


# En este conjunto de datos se trabajan los días de estancia en un hospital (stay)
# Por su naturaleza, esta variable empieza en 1

# HMO es una variable dicotómica asociada a si tiene o no seguro
# died es otra variable dicotómica asociada a si el paciente murió durante la estancia o no
# age es la variable edad dividida en 9 grupos
dat <- within(dat, {
  hmo <- factor(hmo)
  died <- factor(died)
  age <- factor(age)
})

summary(dat)

X11()
hist(dat$stay)
tapply(dat$stay, dat$age, summary)
tapply(dat$stay, dat$hmo, summary)
tapply(dat$stay, dat$died, summary)


# Dos opciones.
# Con Poisson
# 1. modelo especial para ceros truncados
library(VGAM)
m1 <- vglm(stay ~ age + hmo + died, family = pospoisson(), data = dat)
summary(m1)

# 2. modelo modificando la variable y con glm
m2 <- glm(I(stay-1) ~ age + hmo + died , family=poisson(link="log"), data=dat)
summary(m2)

c(AIC(m1), AIC(m2))

# Con Binomial negativa
# 1. modelo especial para ceros truncados
# intercepto 2 se refiere al parámetro adicional de la binomial negativa
m1nb <- vglm(stay ~ age + hmo + died, family = posnegbinomial(), data = dat)
summary(m1nb)

# 2. modelo modificando la variable y con glm.nb
m2nb <- glm.nb(I(stay-1) ~ age + hmo + died , link="log", data=dat)
summary(m2nb)

c(AIC(m1nb), AIC(m2nb))

# Notar que sólo los modelos ajustados con glm y glm.bn pueden ser revisados
# en sus supuestos con DHARMa


library(DHARMa)  
set.seed(123)

m2res <- simulateResiduals(fittedModel = m2)
X11()
plot(m2res )


m2nbres <- simulateResiduals(fittedModel = m2nb)
X11()
plot(m2nbres )

# Parece que se comporta mejor una binomial negativa

# Los modelos vglm pueden revisarse de acuerdo con su desempeño
X11()
rootogram4(m1) #parece que se les olvido incluir esta opción
rootogram4(m1nb)
#el ajuste no parece del todo adecuado, falta modelar un poco más


m2nbvglm <- vglm(I(stay-1) ~ age + hmo + died, negbinomial, dat)
rootogram4(m2nbvglm)
summary(m2nbvglm)
summary(m2nb)

# Parece que falta modelar, el problema siguen siendo los ceros (unos originales)
m3nb <- vglm(I(stay-1) ~ age + hmo + died, zinegbinomialff, dat)
rootogram4(m3nb)



c(AIC(m1nb), AIC(m2nb), AIC(m3nb))


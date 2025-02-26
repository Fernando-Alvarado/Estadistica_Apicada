#Ejemplo ANCOVA
#Exposición a cadmio

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2025-2/ApreEstAut")
options(digits=4)  


CADdata <- read.table("cadmium.txt",header=TRUE, sep=" ", dec=".")

str(CADdata)

#La variable grupo debe ser convertida a factor

CADdata$group=factor(CADdata$group, levels=c(1,2,3), labels=c("High","Low","No") )
str(CADdata)
summary(CADdata)

#Una gráfica global que representan pares de variables
X11()
library(GGally)
ggpairs(data=CADdata, title="Datos", aes(colour = group))


X11()
plot(CADdata$age, CADdata$vitcap, col=c("green","red","blue")[CADdata$group],xlab = "Age", 
     ylab = "Vital Capacity (L)",xlim=c(20,68), ylim=c(2.5,6), 
     pch=c(16,16,16), main = "Exposure to cadmium")

legend(60,5.8, levels(CADdata$group),
       col=c("green","red","blue"), pch = c(16,16,16) )



#Otras gráficas
X11()
par(mfrow=c(1,2)); par(mar=c(4,4,2,1.5))
boxplot(vitcap ~ group, data = CADdata, col = "white", outline=FALSE)
stripchart(vitcap ~ group, data = CADdata,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)
boxplot(age ~ group, data = CADdata, col = "white", outline=FALSE)
stripchart(age ~ group, data = CADdata,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)

# Filtramos los datos por edad para 
# definir el estudio en una población con edades similares
# Esto podría ser debatible, dado que no sabemos 
# cómo se podría trasladar 
# la exposición High en edad menor a 39 (no hay datos),
# además que por construcción sería díficil obtener
# observaciones, pues High se asocia a 
# más de 10 años de exposición
summary(CADdata[CADdata$group=="High",])
CADdata=CADdata[CADdata$age>=39,]

#La gráfica de interés
X11()
par(mfrow=c(1,2)); par(mar=c(4,4,2,1.5))

plot(CADdata$age, CADdata$vitcap, col=c("green","red","blue")[CADdata$group],xlab = "Age", 
     ylab = "Vital Capacity (L)",xlim=c(35,68), ylim=c(2.5,5.6), 
     pch=c(16,16,16), main = "Exposure to cadmium")

legend(60,5.8, levels(CADdata$group),
       col=c("green","red","blue"), pch = c(16,16,16) )

boxplot(vitcap ~ group, data = CADdata, col = "white", outline=FALSE)
stripchart(vitcap ~ group, data = CADdata,
           method = "jitter",
           pch = 19,
           col = c("green","red","blue"),
           vertical = TRUE,
           add = TRUE)

# Revisar con cuidado dos observaciones del grupo No expuesto,
# sin estos datos quizás no haya evidencia de diferencia entre
# los grupos!
# Por ahora realizaremos el análisis usando esas observaciones.
# En la práctica se sugiere ir al origen de los datos y
# analizar si no hay un error de captura.


#Importante analizar el metadato de niveles, pues
#el primero será el nivel de referencia en los modelos
#Lo anterior es el criterio "por default" que usa R sobre
#variables tipo factor


levels(CADdata$group)


#Aquí la referencia es High

#Ajuste del modelo con interacciones
# E(y;x)= b0 + b1 age + b2 Low + b3 No + b4(age*Low) + b5(age*No) # +b6 (age^2*Low)
fit <- lm(vitcap ~ age * group, data = CADdata) 
summary(fit)

#E(Y;group=High, age)=  b0 + b1 age
#E(Y;group= Low, age)=  b0 + b1 age +b2 + b4 age = (b0 + b2) + (b1 + b4) age
#E(Y;group=  No, age)=  b0 + b1 age +b3 + b5 age = (b0 + b3) + (b1 + b5) age

#Este modelo considera tres diferentes rectas, una para cada grupo

#Se rechaza H0 en la prueba asociada
#a la tabla ANOVA (p-value: 0.000592), 
#entonces el modelo tiene sentido.
#Es decir, la edad o el nivel de exposición ayudan a modelar la E(Y).

#Seguimos con prueba de igualdad de pendientes 
#(coeficientes asociados a las interacciones)
#Si no se rechaza, podríamos optar por un modelo con igualdad de 
#pendientes o rectas paralelas (facilita la interpretación)
#H0: b4=0 y b5=0 vs Ha: b4!=0 o b5!=0

# Se realiza una prueba lineal general
library(multcomp)
K=matrix(c(0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

# Se rechaza H0 (p-value: 0.0091). Es decir, hay evidencia
# de rectas con diferente pendiente.
# Esta prueba no nos ayuda a identificar qué rectas tienen
# pendientes diferentes.

# Para analizar lo anterior 
# realizamos una prueba simultánea (sigue a la 
# prueba lineal general)

#Notar que aquí se incluyen
#las tres hipótesis individuales asociadas a

# H0_1: pendientes de High y Low iguales (b4=0)
# H0_2: pendientes de High y No  iguales (b5=0)
# H0_3: pendientes de Low y No   iguales (b4-b5=0)

#Esta prueba sólo detecta las diferencias más evidentes, 
#la lectura se debe hacer con cuidado considerando
#que esta prueba tiene más chance de cometer el error
#tipo II

#¿Todas las pendientes difieren?
K=matrix(c(0,0,0,0,1,0,
           0,0,0,0,0,1,
           0,0,0,0,1,-1), ncol=6, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit, linfct=K, rhs=m))
#De forma simultánea podemos identificar que
#Se rechaza H0 (la hipótesis nula global) por dos diferencias
#1. Se rechaza b5=0, pues p-value < 0.026. 
#Hay evidencia sobre pendientes de High y No diferentes
#2. Se rechaza b4-b5=0, pues p-value < 0.046. 
#Hay evidencia sobre pendientes de Low y No diferentes

#Con esta prueba no se rechaza b4=0, por lo que podríamos optar 
#por considerar un modelo con b4=0.


#Ajustemos el modelo reducido que sólo considera a (age*No)
#Este nuevo modelo corresponde a
# E(y;x)= b0 + b1 age + b2 Low + b3 No + b4(age*No)

fitred <- lm(vitcap ~ age + group + I(age*(group=="No")), data = CADdata) 
summary(fitred)

#E(Y;group=High, age)=  b0 + b1 age
#E(Y;group= Low, age)=  b0 + b1 age +b2 = (b0 + b2) + b1 age
#E(Y;group=  No, age)=  b0 + b1 age +b3 + b4 age = (b0 + b3) + (b1 + b4) age

#Observar que con este modelo, las rectas
# de los grupos High y Low tienen la misma pendiente

#Se rechaza H0 en la prueba asociada
#a la tabla ANOVA (p-value: 0.000245)

# Ahora podríamos proceder a reducir un poco más el modelo
# para tratar de facilitar la interpretación
# o bien a contestar las preguntas sobre los investigadores

# De la salida de fitred, se puede observar
# que una opción es considerar b2=0

#El modelo reducido quedaría como
# E(y;x)= b0 + b1 age  + b2 No + b3(age*No)

#E(Y;group=High, age)=  b0 + b1 age
#E(Y;group= Low, age)=  b0 + b1 age 
#E(Y;group=  No, age)=  b0 + b1 age +b2 + b3 age = (b0 + b2) + (b1 + b3) age


# Con esto se tendrían rectas iguales para High y Low
# por otro lado, se tiene otra recta para No

fitred2 <- lm(vitcap ~ age + I(group=="No") + I(age*(group=="No")), data = CADdata) 
summary(fitred2)

#Se rechaza H0 en la prueba asociada
#a la tabla ANOVA (p-value: 7.26e-05)
#También se puede observar que no se podría reducir más el modelo

#Las rectas ajustadas son

#\hat{E}(Y;group=High, age)=\hat{E}(Y;group=Low, age)= 8.4499 -0.0910 age
#\hat{E}(Y;group=No, age)= (8.4499-3.6605)+(-0.0910+0.0781) age =4.789-0.0129 age

#Se puede observar que la pendiente de los expuestos a cadmio 
# es mayor en valor absoluto y negativa  
# con lo que podemos interpretar que la capacidad vital
# decrece más rápido en ese grupo comparado con los no expuestos.

#Esto se puede observar más fácilmente en una gráfica


#Modelo fitred2

fitredHyL <- function(X2) {fitred2$coefficients[1]+ fitred2$coefficients[2]*X2}
fitredN <- function(X2) {fitred2$coefficients[1]+fitred2$coefficients[3]+ (fitred2$coefficients[2]+fitred2$coefficients[4])*X2}

X11()
with(CADdata, plot(age, vitcap, col=c("red", "green", "blue")[CADdata[,1]] , pch = c(16,16,16)))
legend("topright",levels(CADdata[,1]), col=c("red", "green", "blue"), pch = c(16,16,16), pt.cex=1.5,cex = .9, y.intersp = 1.4 , bty="n" )

curve(fitredHyL, from = min(CADdata$age), to = max(CADdata$age),
      col = "red", add = T)
curve(fitredN, from = min(CADdata$age), to = max(CADdata$age),
      col = "blue", add = T)


####
# Aunque lo ideal es realizar las pruebas de hipótesis correspondientes.
# Para esto, recordemos que todas las conclusiones que se basan en pruebas 
# de hipótesis deben acompañarse con la revisión de los supuestos

####
#Una revisión rápida de los supuestos del modelo inicial 
X11()
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))
plot(fit, 1)   #linealidad
plot(fit, 3)   #homocedasticidad
plot(fit, 2)   #normalidad
plot(fit, 5, cook.levels = c(4/(dim(CADdata)[1]-2), 0.5, 1.0))   #Outliers 

####
#Una revisión rápida de los supuestos del modelo final 
# Objetivo. No encontrar evidencia muy fuerte en contra

X11()
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))
plot(fitred2, 1)   #linealidad
plot(fitred2, 3)   #homocedasticidad
plot(fitred2, 2)   #normalidad
plot(fitred2, 5, cook.levels = c(4/(dim(CADdata)[1]-2), 0.5, 1.0))   #Outliers 

#Homocedasticidad
library(car)
#H0: la varianza es constante 
car::ncvTest(fitred2)
#No se rechaza H0, no hay evidencia en contra de la homocedasticidad

#Normalidad 
#Se basa en los residuales estandarizados o estudentizados
#H0: los datos provienen de una distribución normal
library(broom)
Datosfitred2=augment(fitred2)
# Shapiro-Wilk
shapiro.test(Datosfitred2$.std.resid)
library(nortest)
# Anderson-Darling
ad.test(Datosfitred2$.std.resid)
# Shapiro-Francia
sf.test(Datosfitred2$.std.resid)

#No se rechaza H0, no hay evidencia en contra de la normalidad

#Linealidad
X11()
library(car)
residualPlots(fit)


### Uso del modelo

# Algunas pruebas genéricas de interés en este problema

# ¿Realmente los niveles de cadmio proporcionan un diferente
# comportamiento de la capacidad vital?

# Dado el modelo
# E(y;x)= b0 + b1 age  + b2 No + b3(age*No)
# La pregunta anterior se traduce a contrastar si todos los parámetros
# asociados con las variables binarias relacionadas con los
# niveles de exposición a cadmio son iguales a cero vs al menos uno es 
# diferente de cero, es decir

#H0: b2=0 y b3=0
#vs
#Ha: b2!=0 o b3!=0

K=matrix(c(0,0,1,0,
           0,0,0,1), ncol=4, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(fitred2, linfct=K, rhs=m), test=Ftest())

# Se rechaza H0, lo que implica que se encontró 
# evidencia sobre un diferente impacto de los niveles
# del cadmio, sin embargo, esta prueba no nos ayuda a
# interpretar los resultados y dado que tenemos una 
# variable de ajuste y rectas que no son paralelas,
# conviene realizar las preguntas
# considerando valores particulares de ésta. 


# Por ejemplo, si a los 50 años ya se nota en promedio
# una diferencia entre los que están expuestos a cadmio y los que no,
# es decir,

#H0: E(Y;group= High(Low), age=50)=E(Y;group=  No, age=50)
#vs
#Ha: E(Y;group= High(Low), age=50)!=E(Y;group=  No, age=50)

#Notar que estas hipótesis se pueden escribir en términos de 
#los parámetros:

#H0: b0 + b1 50 = (b0 + b2) + (b1 + b3) 50
#Se pasan los parámetros de un lado de la igualdad 
#H0: b2+b3 (50) = 0

K=matrix(c(0,0,1,50), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fitred2, linfct=K, rhs=m))

#No se rechaza H0, es decir, a los 50 años no se encuentra evidencia
#que indique una diferencia promedio en la capacidad vital
#entre los que fueron expuestos y los que no


#A veces es importante incluir la dirección,
#es decir, los investigadores sospechan que 
#a los 55 años la capacidad vital es en promedio
#mayor en los no expuestos comparados con los expuestos

#Aquí tomamos información del contexto del problema
#con base en lo que sospechan los investigadores.
#Esto se expresa con una dirección en la forma de
#plantear las hipótesis

#H0: E(Y;group= High(Low), age=55) >= E(Y;group=  No, age=55)
#vs
#Ha: E(Y;group= High(Low), age=55)  < E(Y;group=  No, age=55)

#Notar que en la Ha sólo se permiten los casos !=, < o >

#Esto se puede escribir en términos de los parámetros
#H0: b0 + b1 55 >= (b0 + b2) + (b1 + b3) 55

#Es decir, pasando los parámetros a un sólo lado de la 
#desigualdad
#H0: 0 >= b2 + b3 55
#vs
#Ha: 0 < b2 + b3 55   

#La alternativa se usa para definir la dirección
# < f(b0,b1,...,bp) es greater
# > f(b0,b1,...,bp) es less

K=matrix(c(0,0,1,55), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fitred2, linfct=K, rhs=m, alternative="greater"))

#Se rechaza H0, es decir,
#hay evidencia para concluir que el promedio de capacidad vital
#es menor en los expuestos comparados con los no expuestos
#a la edad de 55 años.
#La diferencia promedio estimada es .635


#Una forma de resumir los resultados es presentar una gráfica 
#con los intervalos de confianza simultáneos. Con esto
#es posible identificar en qué punto ya se podría observar 
#una diferencia entre los promedios en la capacidad vital



#Hay al menos dos opciones.
#Nota. También se podrían usar pruebas de hipótesis simultáneas.

#I. La más fácil de interpretar corresponde a incluir los datos,
#las rectas ajustadas y los intervalos de confianza simultáneos de E(Y)
#en una misma gráfica, identificando por color a cada grupo.
#Dado que en esta opción no se considera información adicional (dirección),
#se podría evitar distinguir claramente algo (mayor error tipo II)

X11()
par(mfrow = c(1,2))
with(CADdata, plot(age, vitcap, col=c("red", "green", "blue")[CADdata[,1]] , pch = c(16,16,16)))
legend("topright",levels(CADdata[,1]), col=c("red", "green", "blue"), pch = c(16,16,16), pt.cex=1.5,cex = .9, y.intersp = 1.4 , bty="n" )

summary(CADdata)

#Creamos una malla de valores asociados a la edad
age <- seq(from = 39, to = 65, by = .5)
length(age)

#Calculares los intervalos de confianza simultáneos para la esperanza
#de la capacidad vital, para expuestos y no expuestos
#Bajaré la confianza a 90%, pues serán intervalos simultáneos
#de la esperanza y estos suelen ser muy conservadores

# E(y;x)= b0 + b1 age  + b2 No + b3(age*No)

#Para la banda del grupo expuesto (high o low)
#E(Y;group=High (low), age)= b0 + b1 age
KH <- cbind(1, age, 0, 0)
(KH)
#Para del grupo no expuesto
#E(Y;group=No, age)= (b0 + b2) + (b1 + b3) age
KN <- cbind(1, age, 1,age)
(KN)

K=rbind(KH, KN)

fitE <- glht(fitred2, linfct = K)
fitci <- confint(fitE, level = 0.90)

lines(age, coef(fitE)[1:53], col="red")
lines(age, fitci$confint[1:53,"upr"], col="red")
lines(age, fitci$confint[1:53,"lwr"], col="red")

lines(age, coef(fitE)[54:106], col="blue")
lines(age, fitci$confint[54:106,"upr"], col="blue")
lines(age, fitci$confint[54:106,"lwr"], col="blue")


#Estas bandas de confianza o intervalos de confianza simultáneos sirven para identificar
#las diferencias más evidentes con los datos, pero son muy conservadoras

#Al presentar estas bandas de confianza se debe aclarar el nivel y que son
#simultáneos.


#II. Un poco más difícil de interpretar para los usuarios
#y corresponde a incluir el intervalo de confianza directamente
#de la diferencia de esperanzas para una edad fija entre
#expuestos y no expuestos, es decir

#E(Y;group=High (low), age)-E(Y;group=No, age)
#= [b0 + b1 age] - [(b0 + b2) + (b1 + b3) age]
#= -b2-b3 age

#Notar que ahora nos interesan aquellos valores 
#cuyos intervalos estén por abajo de 0
#pues con eso conluiremos que para esas edades 
#se observa en promedio una menor capacidad vital en los
#expuestos


#Creamos una malla de valores asociados a la edad
age <- seq(from = 39, to = 65, by = .5)
length(age)
K <- cbind(0, 0, -1,-age)

fitE <- glht(fitred2, linfct = K)
fitci <- confint(fitE, level = 0.90)

plot(age, coef(fitE), col="black", type="l", main="Diferencia de E(vitcap) entre expuestos y no", ylab="E(Y;High-Low) - E(Y;No)")
lines(age, fitci$confint[,"upr"], col="black")
lines(age, fitci$confint[,"lwr"], col="black")
abline(h=0, col="blue")

#A partir de los 54 años se observa que 
#la esperanza de la capacidad vital es menor 
#en las personas expuestas a cadmio en comparación
#con las no expuestas. Esta diferencia aumenta con la edad


# Tambiés es posible imprimir las pruebas de hipótesis simultáneas
# asociadas a igualdad vs diferencia
summary(glht(fitred2, linfct = K))
K[32,]

# Incluso se pueden obtener pruebas de hipótesis con
# dirección, que serían más apropiadas (con menos error tipo II)

summary(glht(fitred2, linfct = K,alternative="less"))
K[29,]

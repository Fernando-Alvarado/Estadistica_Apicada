#Herramientas para verificar supuestos
#Ejemplo para modelar las ventas a partir de la publicidad en TV

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2025-2/ApreEstAut")
options(digits=4)  
Datos=read.csv("Advertising.csv", header=TRUE, sep="," )

head(Datos)
summary(Datos)
require(ggplot2)
ggplot(Datos, aes(x=TV,y=sales)) +
  geom_point()

#Modelo inicial (glm1)
glm1=glm(sales~TV, data=Datos, family = Gamma(link="inverse"))
summary(glm1)

#Panel de varias gráficas de utilidad para verificar supuestos
# algunas sólo sirven cuando "y" es continua
X11()
library(ggplot2)
library(ggResidpanel)
resid_panel(glm1, plots=c("all"))

#En la primera gráfica se observa un problema con
#la forma de definir el componente lineal en este modelo

#En estos casos es conveniente revisar gráficas usando cada covariable
#para identificar qué variable requiere alguna transformación

#La siguiente herramienta proporciona las gráficas por cada covariable
X11()
library(car)
residualPlots(glm1)

# Es posible observar que hay un patrón en los datos con respecto
# a la variable TV, por lo que sería necesario realizar alguna 
# transformación a esta variable.

#El test que se presenta de forma adicional,
#sirve para verificar si
#hay un problema en el componente lineal,
#pero es limitado, pues sólo revisa la posible
#necesidad de incluir un término cuadrático de
#la variable en cuestión al componente sistemático

#Se busca No rechazar H0, en caso de rechazar
#se podría optar por incluir el término cuadrático (polinomio de grado 2)

##############
# Otras gráficas y herramientas

# Para analizar el supuesto del componente aleatorio y función liga
# usando residuales quantile

library(statmod)
glm1qr <- qresid( glm1 )
X11()
qqnorm( glm1qr, las=1 ); qqline( glm1qr)
nortest::lillie.test(glm1qr)
shapiro.test(glm1qr)

# Notar que buscar normalidad de estos residuales podría ser
# muy exigente, aunque también el rechazo se puede deber a que
# no se ha logrado modelar de forma correcta el componente
# lineal.

# Análisis usando los residuales obtenidos usando simulaciones
# La primer gráfica se enfoca en el componente aleatorio y función liga
# La segunda se enfoca más en la parte del componente lineal

#####
####Nota. En la práctica sería suficiente con revisar estas gráficas
#####
library(DHARMa)
set.seed(123)
glm1res <- simulateResiduals(fittedModel = glm1)
X11()
plot(glm1res)

#En este caso se observa en la segunda gráfica un fuerte
#problema en el componente lineal

# Cuando hay más de una covariable continua, se 
# recomienda realizar una gráfica por covariable,
# esto puede ayudar a detectar qué variable necesita
# posiblemente una transformación o incluir algún polinomio

# Aquí un ejemplo obteniendo una gráfica con la variable TV
plotResiduals(glm1res, Datos$TV)


#Analicemos los otros modelos encontrados 

glm2=glm(sales~I(TV^(-1/2)), data=Datos, family = inverse.gaussian(link="inverse"))
glm3=glm(sales~I(log(TV)), data=Datos, family = Gamma(link="log"))

# Para glm2
X11()
resid_panel(glm2, plots=c("all"))
glm2qr <- qresid( glm2 )
X11()
qqnorm( glm2qr, las=1 ); qqline( glm2qr)
nortest::lillie.test(glm2qr)
shapiro.test(glm2qr)

glm2res <- simulateResiduals(fittedModel = glm2)
X11()
plot(glm2res)

X11()
plotResiduals(glm2res, Datos$TV)



# Existe una mejora, pero el componente lineal sigue con problemas

#Para glm3
X11()
resid_panel(glm3, plots=c("all"))
glm3qr <- qresid( glm3 )
X11()
qqnorm( glm3qr, las=1 ); qqline( glm3qr)
nortest::lillie.test(glm3qr)
shapiro.test(glm3qr)

glm3res <- simulateResiduals(fittedModel = glm3)
X11()
plot(glm3res)

X11()
plotResiduals(glm3res, Datos$TV)


## Parece que el mejor modelo es el glm3, en el sentido de que tiene 
## un AIC competitivo y además NO hay evidencia FUERTE contra los supuestos


# Notar que el AIC o BIC son sólo índices. En muchos casos podría
# ser necesario incluir algún término adicional al modelo 
# (término cuadrático o polinomio de mayor orden), aunque esto
# será penalizado por estos índices.


#Interpretación
glm2=glm(sales~I(TV^(-1/2)), data=Datos, family = inverse.gaussian(link="inverse"))
glm3=glm(sales~I(log(TV)), data=Datos, family = Gamma(link="log"))
summary(glm2)
summary(glm3)


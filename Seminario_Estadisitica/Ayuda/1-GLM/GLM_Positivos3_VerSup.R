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

#Panel de varias gr�ficas de utilidad para verificar supuestos
# algunas s�lo sirven cuando "y" es continua
X11()
library(ggplot2)
library(ggResidpanel)
resid_panel(glm1, plots=c("all"))

#En la primera gr�fica se observa un problema con
#la forma de definir el componente lineal en este modelo

#En estos casos es conveniente revisar gr�ficas usando cada covariable
#para identificar qu� variable requiere alguna transformaci�n

#La siguiente herramienta proporciona las gr�ficas por cada covariable
X11()
library(car)
residualPlots(glm1)

# Es posible observar que hay un patr�n en los datos con respecto
# a la variable TV, por lo que ser�a necesario realizar alguna 
# transformaci�n a esta variable.

#El test que se presenta de forma adicional,
#sirve para verificar si
#hay un problema en el componente lineal,
#pero es limitado, pues s�lo revisa la posible
#necesidad de incluir un t�rmino cuadr�tico de
#la variable en cuesti�n al componente sistem�tico

#Se busca No rechazar H0, en caso de rechazar
#se podr�a optar por incluir el t�rmino cuadr�tico (polinomio de grado 2)

##############
# Otras gr�ficas y herramientas

# Para analizar el supuesto del componente aleatorio y funci�n liga
# usando residuales quantile

library(statmod)
glm1qr <- qresid( glm1 )
X11()
qqnorm( glm1qr, las=1 ); qqline( glm1qr)
nortest::lillie.test(glm1qr)
shapiro.test(glm1qr)

# Notar que buscar normalidad de estos residuales podr�a ser
# muy exigente, aunque tambi�n el rechazo se puede deber a que
# no se ha logrado modelar de forma correcta el componente
# lineal.

# An�lisis usando los residuales obtenidos usando simulaciones
# La primer gr�fica se enfoca en el componente aleatorio y funci�n liga
# La segunda se enfoca m�s en la parte del componente lineal

#####
####Nota. En la pr�ctica ser�a suficiente con revisar estas gr�ficas
#####
library(DHARMa)
set.seed(123)
glm1res <- simulateResiduals(fittedModel = glm1)
X11()
plot(glm1res)

#En este caso se observa en la segunda gr�fica un fuerte
#problema en el componente lineal

# Cuando hay m�s de una covariable continua, se 
# recomienda realizar una gr�fica por covariable,
# esto puede ayudar a detectar qu� variable necesita
# posiblemente una transformaci�n o incluir alg�n polinomio

# Aqu� un ejemplo obteniendo una gr�fica con la variable TV
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
## un AIC competitivo y adem�s NO hay evidencia FUERTE contra los supuestos


# Notar que el AIC o BIC son s�lo �ndices. En muchos casos podr�a
# ser necesario incluir alg�n t�rmino adicional al modelo 
# (t�rmino cuadr�tico o polinomio de mayor orden), aunque esto
# ser� penalizado por estos �ndices.


#Interpretaci�n
glm2=glm(sales~I(TV^(-1/2)), data=Datos, family = inverse.gaussian(link="inverse"))
glm3=glm(sales~I(log(TV)), data=Datos, family = Gamma(link="log"))
summary(glm2)
summary(glm3)


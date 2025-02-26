#Ejemplo Modelo Gamma
#Publicidad
#Estimación y pruebas de hipótesis

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2025-2/ApreEstAut")
options(digits=4)  
Datos=read.csv("Advertising.csv", header=TRUE, sep="," )

head(Datos)
summary(Datos)
require(ggplot2)
ggplot(Datos, aes(x=TV,y=sales)) +
  geom_point() +
  geom_smooth(method = glm, method.args = list(family = Gamma(link="inverse")), se = FALSE)


glm1=glm(sales~TV, data=Datos, family = Gamma(link="inverse"))
summary(glm1)

# Estimador de phi
sum(residuals(glm1, "pearson")^2)/(dim(Datos)[1]-2)
summary(glm1)$disp

#Tests que se basan en el cociente de verosimilitud
# tipo prueba F asociada a la tabla anova de un modelo de reg múltiple
library(car)
Anova(glm1, test.statistic=c("F"))
drop1(glm1, test="LRT")
anova(glm1, test = "F")

#Tests tipo Wald
#El del summary
Anova(glm1, test.statistic=c("Wald"))

#se puede usar multcomp
library(multcomp)
K=matrix(c(0,1), ncol=2, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(glm1, linfct=K, rhs=m))  #Wald asumiendo normalidad (para cada combinación lineal)
summary(glht(glm1, linfct=K, rhs=m), test=Ftest())  #F, LRT (formato prueba lineal general)
summary(glht(glm1, linfct=K, rhs=m), test=Chisqtest())  #Wald asumiendo normalidad (formato prueba lineal general)


#Para intervalos de confianza, aun cuando sean no simultáneos, 
#la función predict ya no los proporciona (lo hacía con lm)

#Supongamos que queremos estimar el valor promedio de ventas
#dado un gasto en publicidad de 100


#Primero obtener intervalo para eta (dos opciones) y luego convertirlo a
#uno para mu
#Esto es lo que se sugiere hacer

#Intervalos tipo Wald, se asume normalidad
# library(multcomp)
K=matrix(c(1,100), ncol=2, nrow=1, byrow=TRUE)
ICeta=confint(glht(glm1, linfct=K), level=.95)
#Para pasarla a mu, notar que la función g^{-1}() es decreciente 
ICmuLS=1/ICeta$confint[1,2]
ICmuLI=1/ICeta$confint[1,3]
c(ICmuLI,ICmuLS)

#En general, si se nos olvida como se ve la función inversa de 
#la función liga, ésta se puede obtener como:
Fg_1 <- family(glm1)$linkinv
ICmuLSb=Fg_1(ICeta$confint[1,2])
ICmuLIb=Fg_1(ICeta$confint[1,3])
c(ICmuLIb,ICmuLSb)

#II. library(car)
#Se asume normalidad, 
ICdeta=deltaMethod(glm1, "b0+100*b1", parameterNames= paste("b", 0:1, sep=""), level=.95)
ICdmuLS=Fg_1(ICdeta[,3])
ICdmuLI=Fg_1(ICdeta[,4])
c(ICdmuLI,ICdmuLS)


#Para mu directamente se puede usar el método delta
#Esto no se recomienda mucho cuando hay poca muestra,
#sólo se recomienda cuando
#no se puede obtener primero un intervalo
#para alguna combinación lineal de parámetros

deltaMethod(glm1, "1/(b0+100*b1)", parameterNames= paste("b", 0:1, sep=""), level=.95)



#Otras gráficas
#predict con "response" es sobre la "media"
#y sobre "eta" es la opción "link"
#Datos en donde evaluar
datoseval=data.frame(TV=seq(0, 300, by=.2))
datglm1= data.frame(TV=datoseval$TV, sales=predict(glm1, newdata=datoseval, type="response" ))
ggplot(data=Datos, aes(x=TV,y=sales))+
  geom_point(colour="black")+
  geom_line(data= datglm1, colour="green" )

fitglm1 <- function(X2) {1/(coefficients(glm1)[1]+coefficients(glm1)[2]*X2)}
X11()
plot(Datos$TV,Datos$sales, xlab="TV", ylab="sales")

curve(fitglm1, from = min(Datos$TV), to = max(Datos$TV),
      col = "red", add = T)
lines(datglm1$TV, datglm1$sales, col="blue")



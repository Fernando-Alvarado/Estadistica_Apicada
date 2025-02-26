#Comparación y Selección de Modelos
#Un enfoque de aprendizaje automatizado
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

#Modelo inicial
glm1=glm(sales~TV, data=Datos, family = Gamma(link="inverse"))


#Ahora buscaremos y seleccionarmos un modelo
# entre un conjunto de posibles glm
#Varios aspectos que podríamos considerar

#Componente lineal: 
# i) Transformaciones Box Tidwell (potencias) a x
# ii) Polinomio sobre x 


# Ejemplo de una fórmula genérica para usar en glm
# tanto para una potencia como usando polinomios
#i)
potencia= -1
if(potencia==0){
  paste0("sales ~ I(log(TV))")}else
{
  paste0("sales ~ I(TV^(",  potencia, "))")}

#ii)
grado=2
paste0("sales ~ poly(TV,",  grado, ", raw=TRUE)")

# El uso de mallas es muy común en aprendizaje automatizado
# La definición de las mallas (diferentes valores a probar)
#  responde al poder de cómputo y a la interpretación
# Mallas para el valor de potencia o grado:
malla=seq(from = 1, to = 5, by = 1)
(Poli <- cbind("poly", malla))

malla=seq(from = -3, to = 3, by = .5)
(Pot <- cbind("pot", malla))

# diferentes opciones a probar para el componente lineal:
(CompLin=rbind(Poli, Pot))

#Componente aleatorio:
# Y es continua y positiva, tenemos tres opciones:
# i) Distribución Normal
# ii) Distribución Gamma
# iii) Distribución Inversa Gaussiana
help("family")

# Malla con las diferentes opciones a probar:
(Distribuciones=c("gaussian", "Gamma", "inverse.gaussian"))

#Función liga
# i) inverse
# ii) identity
# iii) log
# iv) 1/mu^2 (sólo IG)

# Malla con las diferentes opciones a probar:
(FunLigas=c("identity", "log", "inverse", "1/mu^2"))


nFunLigas=length(FunLigas)
nDist=length(Distribuciones)
nCompLin=dim(CompLin)[1]


ModelList=list(NA)  #guardar resultados del ajuste, objeto glm
AICList=list(NA)    #guardar el AIC del modelo
BICList=list(NA)    #guardar el BIC del modelo
FormList=list(NA)   #guardar la fórmula usada para el ajuste
#Total modelos 18*2*3+18*4 =180 (tres funciones ligas para 2 dist y 4 para una, IG)
index=0
for(k in 1:nCompLin){
  #definimos componente lineal y formula
  if(CompLin[k,1]=="poly"){
    formstring=paste0("sales ~ poly(TV,",  CompLin[k,2], ", raw=TRUE)")
  }else{
    if(CompLin[k,2]==0){
      formstring=paste0("sales ~ I(log(TV))")}else
      {
        formstring=paste0("sales ~ I(TV^(",  CompLin[k,2], "))")}
  }
  form <- as.formula(formstring)
  for(j in 1:nDist){
    for(l in 1:nFunLigas){
      #definición del argumento family
      if(FunLigas[l]=="1/mu^2"){
        if(Distribuciones[j]=="inverse.gaussian"){
          index=index+1
          Dist=get(Distribuciones[j])  #obtener la función a usar
          Mod.A.Prueba=glm(form, data=Datos, family = Dist(link=FunLigas[l]))
          ModelList[[index]]=Mod.A.Prueba
          AICList[[index]]=AIC(Mod.A.Prueba)
          BICList[[index]]=BIC(Mod.A.Prueba)
          FormList[[index]]=formstring
        }
      }else{
        index=index+1
        Dist=get(Distribuciones[j])
        Mod.A.Prueba=glm(form, data=Datos, family = Dist(link=FunLigas[l]))
        ModelList[[index]]=Mod.A.Prueba
        AICList[[index]]=AIC(Mod.A.Prueba)
        BICList[[index]]=BIC(Mod.A.Prueba)
        FormList[[index]]=formstring
      }
    }
  }
}

#Índice del modelo con menor AIC

MinAIC=which.min(unlist(AICList))
ModMinAIC=ModelList[[MinAIC]]
summary(ModMinAIC)
ModMinAIC$family

AICList[[MinAIC]]
BICList[[MinAIC]]
FormList[[MinAIC]]


#Índice del modelo con menor BIC

MinBIC=which.min(unlist(BICList))
ModMinBIC=ModelList[[MinBIC]]
summary(ModMinBIC)
ModMinBIC$family

AICList[[MinBIC]]
BICList[[MinBIC]]
FormList[[MinBIC]]


#Los otros modelos
AICs=unlist(AICList)
DatAICs=cbind(Index=1:length(AICs), AICs)
DatAICs=DatAICs[order(AICs),]


ModAIC2=ModelList[[DatAICs[2,1]]]
summary(ModAIC2)
ModAIC2$family

AICList[[DatAICs[2,1]]]
BICList[[DatAICs[2,1]]]
FormList[[DatAICs[2,1]]]

#######################
### Comparación con un modelo de regresión lineal normal
###Buscamos posibles transformaciones
###Es necesario transformar a $y$ pues la varianza no es constante
library(car)

fitlm1=lm(sales~TV, data=Datos)
summary(fitlm1)
summary(powerTransform(fitlm1)) #Transformaciones BoxCox

#Por facilidad se considera sólo la potencia de la transformación BoxCox
#Se busca ahora una posible transformación Boxtidwell para TV
boxTidwell(I(sales^(1/2))~TV, data=Datos)

fitlm2=lm(I(sales^(1/2))~I(TV^(1/3)), data=Datos)
summary(fitlm2)

#Notar que el AIC de este modelo no es comparable con los AIC
#de los otros modelos, pues éste se calcula en la escala raíz cuadrada
AIC(fitlm2)

#a mano
loglikY=sum( log( (dnorm(sqrt(Datos$sales), fitlm2$fitted.values, sigma(fitlm2))+dnorm(-sqrt(Datos$sales), fitlm2$fitted.values, sigma(fitlm2))  )*(1/(2*sqrt(Datos$sales)))  ) )
(AICY=-2*(loglikY)+2*(2+1))



#Los datos que se grafican para este modelo en la escala original
#no corresponden a la media, pero sí a la mediana
datoseval=data.frame(TV=seq(0, 300, by=.2))
datfitlm2= data.frame(TV=datoseval$TV, sales=predict(fitlm2, newdata=datoseval)^2 )


X11()
ggplot(Datos, aes(x=TV,y=sales)) +
  geom_point() +
  geom_smooth(method = glm, formula = y~ I(x^(-0.5)), method.args = list(family = inverse.gaussian(link="inverse")), se = FALSE, color = "red")+
  geom_smooth(method = glm, formula = y~ I(log(x)), method.args = list(family = Gamma(link="log")), se = FALSE, color = "green")+
  geom_smooth(method = glm, method.args = list(family = Gamma(link="inverse")), se = FALSE)+
  geom_line(data= datfitlm2, colour="black" )+ theme_classic()

c(AIC(glm1), AIC(ModMinAIC), AIC(ModAIC2), AICY)

# Aún falta un detalle a considerar
#¿cuál cumple los supuestos del modelo?




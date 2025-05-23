# Regresi�n lineal m�ltiple
# Selecci�n variables.

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2025-2/ApreEstAut")
Datos=read.csv("ejemplo1RLM.csv", header=TRUE )

summary(Datos)

#Dos variables explicativas X1 y X2

X11()
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
pairs(Datos)
#El scatterplot es m�s d�ficil de usar en la regresi�n lineal m�ltiple
#pues s�lo se muestran relaciones entre pares de variables.
#En la regresi�n lineal m�ltiple nos preguntamos, por ejemplo,
# c�mo es el efecto de X2 en E(Y;X1,X2), as� que debemos
#considerar que en E(Y;X1,X2) aparecen tanto X1 y X2 variables.

#Un posible modelo es:
# E(y;X1, X2)=b0+b1X1+b2X2 

fit=lm(y~X1+X2, data=Datos)

summary(fit)

#Con la prueba asociada a la tabla Anova
# se RECHAZA H0.

#La siguiente pregunta es si ambas variables 
# aportan informaci�n al modelado.

#Para esto podemos hacer pruebas individuales sobre cada coeficiente
#usando las pruebas t o bien la prueba lineal general

#Las pruebas t que se obtienen del summary
#est�n asociadas a cada coeficiente de forma "no simult�nea"
#Para su interpretaci�n se sugiere identificar qu� 
#modelo resulta de considerar el par�metro asociado igual a cero.

#En este caso tenemos b0, b1 y b2. 

#Por ejemplo.

#El modelo que considera que b1=0, ser�a uno en el que s�lo est� X2
#es decir, 
#el modelo reducido E(y;X1, X2)=b0+b2X2 
#vs el completo     E(y;X1, X2)=b0+b1X1+b2X2

#La prueba t del summary para
#H0: b1=0 vs Ha: b1 != 0, nos lleva a concluir que no hay evidencia 
#para rechazar H0 con una significancia de alpha=.05
#Se toma el p-value del summary, rengl�n asociado a X1:
# p-value=0.29 > .05


#En este caso, la prueba est� elaborada para responder
#�La inclusi�n de la variable X1 una vez que se tiene 
#el modelo y=b0+b2X2+e 
#nos est� o no agregando informaci�n adicional?;
#en otras palabras,
#�condicional en la inclusi�n de X2 en el modelo,
#X1 nos agrega informaci�n adicional para modelar E(Y;x)?

#Cuando NO se rechaza Ho, parece que los datos nos sugieren que
#es plausible considerar
# el modelo "reducido"
#y=bo+b2X2+e contra el modelo completo y=bo+b1X1+b2X2+e,
# mientras que si se rechaza H0, entonces X1 s� nos agrega 
#informaci�n al modelo y se prefiere y=bo+b1X1+b2X2+e.


#Un an�lisis similar se puede hacer con b2. 
#Lo importante es notar que estos an�lisis
#corresponden a preguntas independientes sobre cada par�metro en el modelo 
#condicionando en la inclusi�n del resto de variables

#Las pruebas t se enfocan en una s�la combinaci�n de los par�metros.

#Ajustemos los modelos reducidos que son plausibles
#quitando a X1
fitred1=lm(y~X2, data=Datos)
summary(fitred1)

#quitando a X2
fitred2=lm(y~X1, data=Datos)
summary(fitred2)

## Selecci�n entre los modelos. Aqu� casi los tres modelos
## indican un mismo coeficiente de determinaci�n, 
## incluso el ajustado. Con base en este se seleccionar�a 
## el modelo 
## con el mayor coeficiente de determinaci�n.
## En este caso el que tiene s�lo a X1. 

#Si se usa el AIC o BIC
c(AIC(fit), AIC(fitred1), AIC(fitred2))


# Notar que de forma marginal, X1 y X2 se asocian con E(y),
# pero de acuerdo con el AIC se prefiere el modelo
# que s�lo incluye a X1

c(BIC(fit), BIC(fitred1), BIC(fitred2))

# Lo mismo con el BIC
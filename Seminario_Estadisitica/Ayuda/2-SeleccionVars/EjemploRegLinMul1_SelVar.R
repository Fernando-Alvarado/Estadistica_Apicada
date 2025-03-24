# Regresión lineal múltiple
# Selección variables.

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2025-2/ApreEstAut")
Datos=read.csv("ejemplo1RLM.csv", header=TRUE )

summary(Datos)

#Dos variables explicativas X1 y X2

X11()
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
pairs(Datos)
#El scatterplot es más díficil de usar en la regresión lineal múltiple
#pues sólo se muestran relaciones entre pares de variables.
#En la regresión lineal múltiple nos preguntamos, por ejemplo,
# cómo es el efecto de X2 en E(Y;X1,X2), así que debemos
#considerar que en E(Y;X1,X2) aparecen tanto X1 y X2 variables.

#Un posible modelo es:
# E(y;X1, X2)=b0+b1X1+b2X2 

fit=lm(y~X1+X2, data=Datos)

summary(fit)

#Con la prueba asociada a la tabla Anova
# se RECHAZA H0.

#La siguiente pregunta es si ambas variables 
# aportan información al modelado.

#Para esto podemos hacer pruebas individuales sobre cada coeficiente
#usando las pruebas t o bien la prueba lineal general

#Las pruebas t que se obtienen del summary
#están asociadas a cada coeficiente de forma "no simultánea"
#Para su interpretación se sugiere identificar qué 
#modelo resulta de considerar el parámetro asociado igual a cero.

#En este caso tenemos b0, b1 y b2. 

#Por ejemplo.

#El modelo que considera que b1=0, sería uno en el que sólo está X2
#es decir, 
#el modelo reducido E(y;X1, X2)=b0+b2X2 
#vs el completo     E(y;X1, X2)=b0+b1X1+b2X2

#La prueba t del summary para
#H0: b1=0 vs Ha: b1 != 0, nos lleva a concluir que no hay evidencia 
#para rechazar H0 con una significancia de alpha=.05
#Se toma el p-value del summary, renglón asociado a X1:
# p-value=0.29 > .05


#En este caso, la prueba está elaborada para responder
#¿La inclusión de la variable X1 una vez que se tiene 
#el modelo y=b0+b2X2+e 
#nos está o no agregando información adicional?;
#en otras palabras,
#¿condicional en la inclusión de X2 en el modelo,
#X1 nos agrega información adicional para modelar E(Y;x)?

#Cuando NO se rechaza Ho, parece que los datos nos sugieren que
#es plausible considerar
# el modelo "reducido"
#y=bo+b2X2+e contra el modelo completo y=bo+b1X1+b2X2+e,
# mientras que si se rechaza H0, entonces X1 sí nos agrega 
#información al modelo y se prefiere y=bo+b1X1+b2X2+e.


#Un análisis similar se puede hacer con b2. 
#Lo importante es notar que estos análisis
#corresponden a preguntas independientes sobre cada parámetro en el modelo 
#condicionando en la inclusión del resto de variables

#Las pruebas t se enfocan en una sóla combinación de los parámetros.

#Ajustemos los modelos reducidos que son plausibles
#quitando a X1
fitred1=lm(y~X2, data=Datos)
summary(fitred1)

#quitando a X2
fitred2=lm(y~X1, data=Datos)
summary(fitred2)

## Selección entre los modelos. Aquí casi los tres modelos
## indican un mismo coeficiente de determinación, 
## incluso el ajustado. Con base en este se seleccionaría 
## el modelo 
## con el mayor coeficiente de determinación.
## En este caso el que tiene sólo a X1. 

#Si se usa el AIC o BIC
c(AIC(fit), AIC(fitred1), AIC(fitred2))


# Notar que de forma marginal, X1 y X2 se asocian con E(y),
# pero de acuerdo con el AIC se prefiere el modelo
# que sólo incluye a X1

c(BIC(fit), BIC(fitred1), BIC(fitred2))

# Lo mismo con el BIC
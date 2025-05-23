# Ejemplo para selecci�n de variables
# como un problema de optimizaci�n discreta

# Dos opciones. 
#       1) Por pasos (p peque�a o moderada) 
#       2) Mejor subconjunto (s�lo para p peque�a)


rm(list = ls(all.names = TRUE))
gc()


# Datos
library(MASS)
help("birthwt")

str(birthwt)
summary(birthwt)
levels(factor(birthwt$ftv)) #e.g. podr�a considerar como categ�rica
# Preprocesamiento
bwtMod <- with(birthwt, {
  race <- factor(race, levels=c(1,2,3),labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0, levels=c(TRUE,FALSE),labels=c("TRUE", "FALSE"))
  ftv <- factor(ftv) # por default los niveles son 0,1,2,3,4,5,6
  levels(ftv)[-(1:2)] <- "2+"        #convertir todo los niveles con 2, 3, 4, 5 y 6 en 2+. Forma f�cil para recodificar
  data.frame(low = factor(low), age, lwt, race, smoke = factor(smoke > 0),
             ptd, ht = factor(ht > 0), ui = factor(ui > 0), ftv)
})

summary(bwtMod)
str(bwtMod)

#####################
################# M�todos por pasos


birthwt.glm <- glm(low ~ ., family = binomial("logit"), data = bwtMod)
summary(birthwt.glm)
# Funci�n stepAIC, elimina-agrega variable por variable en su versi�n original en el dataframe,
# es decir, toma como conjunto todas las binarias de una variable categ�rica.
# k=2 para AIC (default) y k = log(n) para BIC
# Por default se tiene el m�todo "backward" cuando no se indica nada en el 
# argumento scope
birthwt.step <- stepAIC(birthwt.glm, trace = TRUE)  #Poner trace = FALSE en la pr�ctica
summary(birthwt.step)
AIC(birthwt.step)

# Tambi�n puede restringir la b�squeda entre dos opciones de modelos, 
# uno muy simple y otro m�s complejo, por ejemplo, incluyendo opciones
# como interacciones. Esto se indica en el argumento scope
# empezando con backward a partir de birthwt.glm cuando la opci�n es 
# "both" o "backward"
birthwt.step2 <- stepAIC(birthwt.glm, scope =list(upper = ~ .^2 + I(age^2)
                                                  + I(lwt^2), lower = ~1), trace = TRUE,direction ="both")
AIC(birthwt.step2)

# comparando con BIC, se penalizan m�s las interacciones al incluir muchos par�metros
birthwt.step3 <- stepAIC(birthwt.glm, scope =list(upper = ~ .^2 + I(age^2)
                         + I(lwt^2), lower = ~1), trace = TRUE,direction ="both", k=log(dim(bwtMod)[1]))
BIC(birthwt.step3) #Notar que la impresi�n de stepAIC indica AIC, aunque es el BIC
AIC(birthwt.step3)

# Nota. La funci�n step de stats en R base hace exactamente lo mismo.

birthwt.step4 <- step(birthwt.glm, scope =list(upper = ~ .^2 + I(age^2)
                                                  + I(lwt^2), lower = ~1), trace = TRUE,direction ="both", k=log(dim(bwtMod)[1]))

# La salida es un objeto similar al glm
birthwt.step4$formula
birthwt.step2$formula

# se pueden verificar los supuestos directamente
library(DHARMa)
set.seed(123)
birthwt.step4res <- simulateResiduals(fittedModel = birthwt.step4)
X11()
plot(birthwt.step4res)
summary(birthwt.step4)


birthwt.step2res <- simulateResiduals(fittedModel = birthwt.step2)
X11()
plot(birthwt.step2res)

summary(birthwt.step2)

# Los modelos est�n anidados, se podr�an comparar
anova(birthwt.step2, birthwt.step4, test="Chisq")
# parece que hay t�rminos significativos que salieron al usar el BIC

# En general, las variables categ�ricas son penalizadas m�s fuerte, 
# pues se consideran en bloque todas las binarias que se necesitan.
# Una opci�n en el caso de tener muchas categor�as, es concatenarlas (unirlas)
# para reducir par�metros.

# De birthwt.step2 se podr�a estudiar si 
# es conveniente concatenar ftv1 con el nivel de referencia

######
# Nota. Otra versi�n m�s robusta de usar stepAIC o step (pues
# a veces hay problemas con algunas definiciones del scope, eg con forward)
# es definiendo los modelos del scope al inicio

# modelo auxiliar (nulo, para empezar con selecci�n forward)
mod3auxN <- glm(low ~ 1, family = binomial("logit"), data = bwtMod)
summary(mod3auxN)

# modelo auxiliar (Full, el m�s completo a explorar)
mod3auxF <- glm(low ~ .^2 + I(age^2)
                + I(lwt^2), family = binomial("logit"), data = bwtMod)
summary(mod3auxF)


mod3 <- stepAIC(mod3auxN, scope =list(upper = mod3auxF, lower = mod3auxN), trace =TRUE,direction="forward", k=log(dim(bwtMod)[1]))
summary(mod3)
AIC(birthwt.step3)
AIC(mod3)

### Nota
# Para no quitar toda una variable categ�rica, es posible definir a mano las
# binarias y usar el dataframe ya con estas variables.
# La desventaja es que el m�todo depender� de qu� categor�a queda como 
# referencia.

### Para mantener la variable race

mod3auxrace <- glm(low ~ race, family = binomial("logit"), data = bwtMod)
summary(mod3auxrace)

modkeep <- stepAIC(birthwt.glm, scope =list(upper = mod3auxF, lower = mod3auxrace), trace =TRUE,direction="both", k=log(dim(bwtMod)[1]))

summary(modkeep)

#####################
################# Mejor subconjunto

##### Paquete bestglm

# Este paquete sirve para los modelos b�sicos de glm
# Requiere un formato especial para los datos: Xy
# Es decir, un dataframe donde X significa todas las variables a usar y 
# al final la columna con la variable dependiente y
# En este dataframe al considerar las variables categ�ricas
# se pueden incluir a mano las variables binarias (k-1, k #categor�as)
# o bien se deben declarar como tipo factor
# en el �ltimo caso, elimina toda la variable categ�rica como �nica opci�n

summary(bwtMod)

bwtModXy=bwtMod[,c(2:9,1)]
library(bestglm)

# La b�squeda s�lo considera modelos con efectos principales
# de las variables en el dataframe
# desde una variable hasta el modelo con todas las variables
best.logit <- bestglm(bwtModXy,
                      IC = "AIC",    #BIC y otras versiones, tambi�n CV para predicci�n                 
                      family=binomial("logit"),
                      method = "exhaustive")
summary(best.logit$BestModel) #objeto similar al obtenido con la funci�n glm

# Para revisar m�s opciones
best.logit$Subsets

c(AIC(best.logit$BestModel), AIC(birthwt.step2), AIC(birthwt.step))
# Perdi� s�lo cuando se aplica una b�squeda por pasos sobre modelos m�s complejos
# con interacciones

# Dentro del dataframe se pueden construir algunas interacciones de inter�s
# pero la b�squeda es por cada variable, as� que eso podr�a tener repercusi�n
# con las variables categ�ricas.



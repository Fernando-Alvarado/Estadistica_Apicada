# Ejemplo para selección de variables
# como un problema de optimización discreta

# Dos opciones. 
#       1) Por pasos (p pequeña o moderada) 
#       2) Mejor subconjunto (sólo para p pequeña)


rm(list = ls(all.names = TRUE))
gc()


# Datos
library(MASS)
help("birthwt")

str(birthwt)
summary(birthwt)
levels(factor(birthwt$ftv)) #e.g. podría considerar como categórica
# Preprocesamiento
bwtMod <- with(birthwt, {
  race <- factor(race, levels=c(1,2,3),labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0, levels=c(TRUE,FALSE),labels=c("TRUE", "FALSE"))
  ftv <- factor(ftv) # por default los niveles son 0,1,2,3,4,5,6
  levels(ftv)[-(1:2)] <- "2+"        #convertir todo los niveles con 2, 3, 4, 5 y 6 en 2+. Forma fácil para recodificar
  data.frame(low = factor(low), age, lwt, race, smoke = factor(smoke > 0),
             ptd, ht = factor(ht > 0), ui = factor(ui > 0), ftv)
})

summary(bwtMod)
str(bwtMod)

#####################
################# Métodos por pasos


birthwt.glm <- glm(low ~ ., family = binomial("logit"), data = bwtMod)
summary(birthwt.glm)
# Función stepAIC, elimina-agrega variable por variable en su versión original en el dataframe,
# es decir, toma como conjunto todas las binarias de una variable categórica.
# k=2 para AIC (default) y k = log(n) para BIC
# Por default se tiene el método "backward" cuando no se indica nada en el 
# argumento scope
birthwt.step <- stepAIC(birthwt.glm, trace = TRUE)  #Poner trace = FALSE en la práctica
summary(birthwt.step)
AIC(birthwt.step)

# También puede restringir la búsqueda entre dos opciones de modelos, 
# uno muy simple y otro más complejo, por ejemplo, incluyendo opciones
# como interacciones. Esto se indica en el argumento scope
# empezando con backward a partir de birthwt.glm cuando la opción es 
# "both" o "backward"
birthwt.step2 <- stepAIC(birthwt.glm, scope =list(upper = ~ .^2 + I(age^2)
                                                  + I(lwt^2), lower = ~1), trace = TRUE,direction ="both")
AIC(birthwt.step2)

# comparando con BIC, se penalizan más las interacciones al incluir muchos parámetros
birthwt.step3 <- stepAIC(birthwt.glm, scope =list(upper = ~ .^2 + I(age^2)
                         + I(lwt^2), lower = ~1), trace = TRUE,direction ="both", k=log(dim(bwtMod)[1]))
BIC(birthwt.step3) #Notar que la impresión de stepAIC indica AIC, aunque es el BIC
AIC(birthwt.step3)

# Nota. La función step de stats en R base hace exactamente lo mismo.

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

# Los modelos están anidados, se podrían comparar
anova(birthwt.step2, birthwt.step4, test="Chisq")
# parece que hay términos significativos que salieron al usar el BIC

# En general, las variables categóricas son penalizadas más fuerte, 
# pues se consideran en bloque todas las binarias que se necesitan.
# Una opción en el caso de tener muchas categorías, es concatenarlas (unirlas)
# para reducir parámetros.

# De birthwt.step2 se podría estudiar si 
# es conveniente concatenar ftv1 con el nivel de referencia

######
# Nota. Otra versión más robusta de usar stepAIC o step (pues
# a veces hay problemas con algunas definiciones del scope, eg con forward)
# es definiendo los modelos del scope al inicio

# modelo auxiliar (nulo, para empezar con selección forward)
mod3auxN <- glm(low ~ 1, family = binomial("logit"), data = bwtMod)
summary(mod3auxN)

# modelo auxiliar (Full, el más completo a explorar)
mod3auxF <- glm(low ~ .^2 + I(age^2)
                + I(lwt^2), family = binomial("logit"), data = bwtMod)
summary(mod3auxF)


mod3 <- stepAIC(mod3auxN, scope =list(upper = mod3auxF, lower = mod3auxN), trace =TRUE,direction="forward", k=log(dim(bwtMod)[1]))
summary(mod3)
AIC(birthwt.step3)
AIC(mod3)

### Nota
# Para no quitar toda una variable categórica, es posible definir a mano las
# binarias y usar el dataframe ya con estas variables.
# La desventaja es que el método dependerá de qué categoría queda como 
# referencia.

### Para mantener la variable race

mod3auxrace <- glm(low ~ race, family = binomial("logit"), data = bwtMod)
summary(mod3auxrace)

modkeep <- stepAIC(birthwt.glm, scope =list(upper = mod3auxF, lower = mod3auxrace), trace =TRUE,direction="both", k=log(dim(bwtMod)[1]))

summary(modkeep)

#####################
################# Mejor subconjunto

##### Paquete bestglm

# Este paquete sirve para los modelos básicos de glm
# Requiere un formato especial para los datos: Xy
# Es decir, un dataframe donde X significa todas las variables a usar y 
# al final la columna con la variable dependiente y
# En este dataframe al considerar las variables categóricas
# se pueden incluir a mano las variables binarias (k-1, k #categorías)
# o bien se deben declarar como tipo factor
# en el último caso, elimina toda la variable categórica como única opción

summary(bwtMod)

bwtModXy=bwtMod[,c(2:9,1)]
library(bestglm)

# La búsqueda sólo considera modelos con efectos principales
# de las variables en el dataframe
# desde una variable hasta el modelo con todas las variables
best.logit <- bestglm(bwtModXy,
                      IC = "AIC",    #BIC y otras versiones, también CV para predicción                 
                      family=binomial("logit"),
                      method = "exhaustive")
summary(best.logit$BestModel) #objeto similar al obtenido con la función glm

# Para revisar más opciones
best.logit$Subsets

c(AIC(best.logit$BestModel), AIC(birthwt.step2), AIC(birthwt.step))
# Perdió sólo cuando se aplica una búsqueda por pasos sobre modelos más complejos
# con interacciones

# Dentro del dataframe se pueden construir algunas interacciones de interés
# pero la búsqueda es por cada variable, así que eso podría tener repercusión
# con las variables categóricas.



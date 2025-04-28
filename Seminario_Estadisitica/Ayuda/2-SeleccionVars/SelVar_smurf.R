# Ejemplo para selección de variables
# como un problema de optimización con penalizaciones

# Uso del paquete smurf 

rm(list = ls(all.names = TRUE))
gc()

# Datos (mismos que los usados para optimización discreta)
library(MASS)
help("birthwt")

str(birthwt)
summary(birthwt)
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


library(smurf)

# Smurf requiere que para cada variable se indique 
# el tipo de penalización que se debe usar.
# Esto puede ser lo más tedioso, aunque se podría automatizar.

# El tipo de penalización se debe indicar en un objeto tipo fórmula

# Recomendación
# Var. cat. nominales (#cat>=2) usar generalized fused lasso "gflasso"
# Var. cat. ordinales (#cat>2) usar fused lasso "flasso", ya debe estar indicado el orden 
# Variables continuas con lasso

formu <- low ~ p(age, pen = "lasso") + p(lwt, pen = "lasso") +
              p(race, pen = "gflasso") + p(smoke, pen = "gflasso") +
              p(ptd, pen = "gflasso") + p(ht, pen = "gflasso") +
              p(ui, pen = "gflasso") + p(ftv, pen = "flasso")

# i) glmsmurf sí tiene implementada la búsqueda con aic o bic,
# también tiene métricas sobre poder predictivo

# ii) argumento family con las mismas opciones que glm

# iii) varios tipos de estandarización, se recomienda "glm.stand" 
#   glmnet estandariza variables vs smurf penaliza de forma diferente los betas 

# lambda, un valor en particular 
#       o bien el criterio que se usará para seleccionar
#         el valor de lambda sobre una malla

# para aumentar el tamaño de la malla o indicarla a mano
# se deben usar argumentos de función glmsmurf.control
# en el argumento control, donde se pueden pedir estimaciones por MV

ejemplo.fit <- glmsmurf(formula = formu, family = binomial("logit"), data = bwtMod, 
                        pen.weights = "glm.stand", lambda = "is.bic", control=list(lambda.length=200, reest = TRUE, lambda.reest=TRUE))
#gráfica mostrando los diferentes BIC y el valor
#de lambda donde se obtiene el menor
X11()
plot_lambda(ejemplo.fit)
ejemplo.fit$lambda
log(ejemplo.fit$lambda)


# La siguiente gráfica resume por variable (divididas por líneas verticales)
# los coeficientes que se consideran cero (en cuadrados grises)
# para concatenar niveles de variables categóricas se usan los mismos colores

plot(ejemplo.fit, cex=3)

# el orden de las variables es el que se usa en la fórmula
# el primer espacio corresponde al intercepto

# age coeficiente igual a cero
# lwt coeficiente diferente de cero, aunque pequeño
# race dos variables binarias quitando la referencia, ambos coeficientes iguales
# smoke coeficiente de la binaria que entra igual a cero
# pth   coeficiente dif de cero, asociado a la binaria que entra
# ht    coeficiente dif de cero, asociado a la binaria que entra
# ui    coeficiente de la binaria cero
# ftv   cat ordinal, pero todas las igual a cero

summary(ejemplo.fit)
# las estimaciones se presentan en dos columnas
# Estimated corresponde a los betas estimadas que resuelven el problema con penalizaciones
# Re-estimated es la versión relax o las estimaciones que se obtiene al usar
#             máxima verosimilitud considerando sólo las variables asociadas a estimaciones
#             de betas diferentes de cero


# equivalencia con un glm
str(bwtMod)
fitBIC.glm <- glm(low ~ ptd+lwt+ht, family = binomial("logit"), data = bwtMod)
summary(fitBIC.glm)
BIC(fitBIC.glm)

# Es más lento que glmnet, también es un poco más complejo definir modelos
# pero permite concatenar categorías

# Con AIC para ver lo correspondiente a concatenar categorías:
ejemplo.fitAIC <- glmsmurf(formula = formu, family = binomial("logit"), data = bwtMod, 
                        pen.weights = "glm.stand", lambda = "is.aic", control=list(lambda.length=200, reest = TRUE, lambda.reest=TRUE))
plot(ejemplo.fitAIC, cex=3)
summary(ejemplo.fitAIC)
# Notar que aquí se concatenan los niveles black y other


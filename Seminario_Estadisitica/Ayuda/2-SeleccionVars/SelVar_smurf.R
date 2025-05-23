# Ejemplo para selecci�n de variables
# como un problema de optimizaci�n con penalizaciones

# Uso del paquete smurf 

rm(list = ls(all.names = TRUE))
gc()

# Datos (mismos que los usados para optimizaci�n discreta)
library(MASS)
help("birthwt")

str(birthwt)
summary(birthwt)
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


library(smurf)

# Smurf requiere que para cada variable se indique 
# el tipo de penalizaci�n que se debe usar.
# Esto puede ser lo m�s tedioso, aunque se podr�a automatizar.

# El tipo de penalizaci�n se debe indicar en un objeto tipo f�rmula

# Recomendaci�n
# Var. cat. nominales (#cat>=2) usar generalized fused lasso "gflasso"
# Var. cat. ordinales (#cat>2) usar fused lasso "flasso", ya debe estar indicado el orden 
# Variables continuas con lasso

formu <- low ~ p(age, pen = "lasso") + p(lwt, pen = "lasso") +
              p(race, pen = "gflasso") + p(smoke, pen = "gflasso") +
              p(ptd, pen = "gflasso") + p(ht, pen = "gflasso") +
              p(ui, pen = "gflasso") + p(ftv, pen = "flasso")

# i) glmsmurf s� tiene implementada la b�squeda con aic o bic,
# tambi�n tiene m�tricas sobre poder predictivo

# ii) argumento family con las mismas opciones que glm

# iii) varios tipos de estandarizaci�n, se recomienda "glm.stand" 
#   glmnet estandariza variables vs smurf penaliza de forma diferente los betas 

# lambda, un valor en particular 
#       o bien el criterio que se usar� para seleccionar
#         el valor de lambda sobre una malla

# para aumentar el tama�o de la malla o indicarla a mano
# se deben usar argumentos de funci�n glmsmurf.control
# en el argumento control, donde se pueden pedir estimaciones por MV

ejemplo.fit <- glmsmurf(formula = formu, family = binomial("logit"), data = bwtMod, 
                        pen.weights = "glm.stand", lambda = "is.bic", control=list(lambda.length=200, reest = TRUE, lambda.reest=TRUE))
#gr�fica mostrando los diferentes BIC y el valor
#de lambda donde se obtiene el menor
X11()
plot_lambda(ejemplo.fit)
ejemplo.fit$lambda
log(ejemplo.fit$lambda)


# La siguiente gr�fica resume por variable (divididas por l�neas verticales)
# los coeficientes que se consideran cero (en cuadrados grises)
# para concatenar niveles de variables categ�ricas se usan los mismos colores

plot(ejemplo.fit, cex=3)

# el orden de las variables es el que se usa en la f�rmula
# el primer espacio corresponde al intercepto

# age coeficiente igual a cero
# lwt coeficiente diferente de cero, aunque peque�o
# race dos variables binarias quitando la referencia, ambos coeficientes iguales
# smoke coeficiente de la binaria que entra igual a cero
# pth   coeficiente dif de cero, asociado a la binaria que entra
# ht    coeficiente dif de cero, asociado a la binaria que entra
# ui    coeficiente de la binaria cero
# ftv   cat ordinal, pero todas las igual a cero

summary(ejemplo.fit)
# las estimaciones se presentan en dos columnas
# Estimated corresponde a los betas estimadas que resuelven el problema con penalizaciones
# Re-estimated es la versi�n relax o las estimaciones que se obtiene al usar
#             m�xima verosimilitud considerando s�lo las variables asociadas a estimaciones
#             de betas diferentes de cero


# equivalencia con un glm
str(bwtMod)
fitBIC.glm <- glm(low ~ ptd+lwt+ht, family = binomial("logit"), data = bwtMod)
summary(fitBIC.glm)
BIC(fitBIC.glm)

# Es m�s lento que glmnet, tambi�n es un poco m�s complejo definir modelos
# pero permite concatenar categor�as

# Con AIC para ver lo correspondiente a concatenar categor�as:
ejemplo.fitAIC <- glmsmurf(formula = formu, family = binomial("logit"), data = bwtMod, 
                        pen.weights = "glm.stand", lambda = "is.aic", control=list(lambda.length=200, reest = TRUE, lambda.reest=TRUE))
plot(ejemplo.fitAIC, cex=3)
summary(ejemplo.fitAIC)
# Notar que aqu� se concatenan los niveles black y other


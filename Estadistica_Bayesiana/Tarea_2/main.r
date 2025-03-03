#Codigo de la segunda tarea, ejercicio 1
#Autor: Fernando Alvarado Palacios
#Fecha: 2021/09/15

library(ggplot2)
library(tidyr)
#Inciso A
alpha <- 1/2
beta <- 7 / 2

# Calcular P(X > 10) de la distribucion gamma
p_10 <- pgamma(10, shape = alpha, scale = 1 / beta)
lam_10 <- 1 - p_10

#print(p_10)
# Mostrar resultado
#print(lam_10)



#Inciso B

p_b <- pgamma(10, shape = 300 * alpha, scale = 1 / beta)


print(p_b)

print(1 - p_b)


#Graficas 

observaciones <- c(879, 903, 948, 939, 893) #Dtos que nos da las observaciones

#Graficas
valores = seq(0, 1000, length.out = 500)

# Crear data frame con la distribución gamma inicial
df_sim <- data.frame(
  x = valores,
  priori = dgamma(valores, shape = alpha, rate = beta)
)

# Agregar columnas de observaciones iterativamente
for (i in 1:5) {
  df_sim[, paste0("obs_", i)] <- dgamma(df_sim$x, 
                                        shape = 1 * (alpha + sum(observaciones[1:i])),
                                        rate = beta + i)
}

df_mod <- df_sim %>%
  pivot_longer(cols = -x, names_to = "grupo", values_to = "y")

head(df_mod)

# Mostrar gráfico
#x11()  # Abrir ventana de gráficos (opcional)
ggplot(df_mod, aes(x = x, y = y, color = grupo)) +
  geom_line() +
  theme_minimal()



tamaño = 500
valores2 = seq(0, 100, length.out = tamaño)
lambdaSim = rgamma(1, shape = alpha, rate = beta)

print(lambdaSim)

  df_prueba <- data.frame(
    x = valores2,
    y = dpois(valores2, lambda = lambdaSim)
  )

  ggplot(df_prueba, aes(x = x, y = y)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Distribución Poisson",
         x = "x",
         y = "y") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  
  
  #-----------------------Codigo Oswaldo --------------
  
  x <- 0:15
  curve(dgamma(x, 7/2, rate = 1/2), 0, 16)
  
  
  
  x <- 0:1000
  curve(dgamma(x, 79.53, rate = 5.5), 0, 30)
  
  
  
  
  
  #-------------Codigo chat
  # Fijar semilla para reproducibilidad
  set.seed(123)
  
  # Parámetros de la distribución a priori Gamma(7/2, 1/2)
  alpha_prior <- 7/2
  beta_prior <- 1/2
  
  # Parámetro "real" de lambda (para la simulación)
  lambda_real <- 7
  
  # Generar 3 observaciones Poisson(lambda_real)
  X <- rpois(10, lambda_real)
  print(paste("Observaciones:", paste(X, collapse = ", ")))
  
  # Actualizar los parámetros de la distribución posterior
  alpha_posterior <- alpha_prior + sum(X)
  beta_posterior <- beta_prior + length(X)
  
  # Simular muestras de la distribución a priori y a posteriori
  samples_prior <- rgamma(10000, shape = alpha_prior, rate = beta_prior)
  samples_posterior <- rgamma(10000, shape = alpha_posterior, rate = beta_posterior)
  
  
  
  
  # Graficar la comparación entre la distribución a priori y a posteriori
  hist(samples_prior, probability = TRUE, col = rgb(0,0,1,0.5), 
       main = "Distribución a Priori vs Posteriori", xlab = "Lambda", breaks = 50)
  hist(samples_posterior, probability = TRUE, col = rgb(1,0,0,0.5), add = TRUE, breaks = 50)
  legend("topright", legend = c("A Priori", "Posteriori"), 
         fill = c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))
  
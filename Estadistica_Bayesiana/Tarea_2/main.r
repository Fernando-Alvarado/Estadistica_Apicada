#Codigo de la segunda tarea, ejercicio 1
#Autor: Fernando Alvarado Palacios
#Fecha: 2021/09/15

library(ggplot2)
library(tidyr)
#Cosas para fijar 
  # Fijar semilla para reproducibilidad
  set.seed(123)
  
  tamaño = 10000
  

  alpha_prior <- 7/2
  beta_prior <- 1/2



#Ejercicio 1
#Inciso A ---------------------------------------------------------------

 # Parámetros de la distribución a priori Gamma(7/2, 1/2)

#Print esperanza simulada
muestras <- rgamma(tamaño, shape = alpha_prior, rate = beta_prior)
esperanza_simulada <- mean(muestras)
print(esperanza_simulada)


# Calcular P(X > 10) de la distribucion gamma
p_10 <- pgamma(10, shape = alpha_prior, rate = beta_prior)
lam_10 <- 1 - p_10

print(p_10)
# Mostrar resultado
print(lam_10)




#Inciso B ---------------------------------------------------------------

  # Parámetro "real" de lambda (para la simulación)
  lambda_real <- 7
  
  # Generar 3 observaciones Poisson(lambda_real)
  X <- rpois(10, lambda_real)
  print(paste("Observaciones:", paste(X, collapse = ", ")))
  
  # Actualizar los parámetros de la distribución posterior
  alpha_posterior <- alpha_prior + sum(X)
  beta_posterior <- beta_prior + length(X)
  
  # Simular muestras de la distribución a priori y a posteriori originales de nuestro problema
  samples_prior <- rgamma(tamaño, shape = alpha_prior, rate = beta_prior)
  samples_posterior <- rgamma(tamaño, shape = alpha_posterior, rate = beta_posterior)


  #Haciendo la suma de las observaciones

  observaciones <- c(879, 903, 948, 939, 893) / 300 #Dtos que nos da las observaciones
  observaciones 

for (i in 1:5) {
  assign(paste0("obs_", i), 
         rgamma(tamaño, shape = alpha + sum(observaciones[1:i]), rate = beta + i))
}


# Calcular P(X > 10) de la distribucion gamma dasd las observaciones 
proba_10 <- pgamma(10, shape = alpha + sum(observaciones[1:5]), rate = beta + 5)
proba_10_datos <- 1 - proba_10

print(proba_10)
# Mostrar resultado
print(proba_10_datos) #Probabilidad de que sea mayor a 10

#Esta proba me da 0, que que con las cobservaciones la muestra se cargo a valores mas peuqeños

  
 # Crear el dataframe de valores simulados
df_datos <- data.frame(
  value = c(samples_prior, samples_posterior, obs_1, obs_2, obs_3, obs_4, obs_5),
  distribucion = rep(c("Prior", "Posterior", "Con 1 obs", "Con 2 obs", "Con 3 obs", "Con 4 obs", "Con 5 obs"), 
                     each = tamaño)  # Asegurar que cada grupo tiene `tamaño` elementos
)



# Crear un dataframe separado para las medias
medias <- data.frame(
  distribucion = c("Prior", "Posterior", "Con 1 obs", "Con 2 obs", "Con 3 obs", "Con 4 obs", "Con 5 obs"),
  media = c(mean(samples_prior), mean(samples_posterior), mean(obs_1), mean(obs_2), mean(obs_3), mean(obs_4), mean(obs_5))
)



ggplot(df_datos, aes(x = value, fill = distribucion)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = medias, aes(xintercept = media, color = distribucion), 
             linetype = "dashed", linewidth = 1) +  
  geom_text(data = medias, aes(x = media, y = -0.1, label = round(media, 2)),  
            angle = 90, vjust = 1, size = 4) + 
  labs(title = "Distribuciones a Priori y Posteriori con sus Medias",
       x = "Valor",
       y = "Densidad") +
  xlim(0, 16) +#Para que se vean mejor la grafica
  theme_minimal()

  
  
  
  
  
  
  
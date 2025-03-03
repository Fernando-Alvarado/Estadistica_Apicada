#Codigo de la segunda tarea, ejercicio 1
#Autor: Fernando Alvarado Palacios
#Fecha: 2021/09/15

library(ggplot2)
library(tidyr)
#Inciso A
alpha <- 49 / 196
beta <- 7 / 196

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
valores = seq(0, 1500, length.out = 750)

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
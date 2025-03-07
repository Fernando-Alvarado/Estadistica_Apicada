#Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(VGAM)  # Para la función dpareto


#Definoemdp nuestra semnilla
set.seed(42) 
# Definir hiperparámetros a trabajar
#Valores de nuestra distribucion Pareto
a <- 2
b <- 1

# Tamaño de la muestra
n <- 20
#Tamaño de la grafica 
num = 200 

# Simular Theta a partir de una Pareto(a, b)
theta_real <- rpareto(1, scale = b, shape = a)


# Generar una muestra X ~ Uniforme(0, Theta)
x <- runif(n, min = 0, max = theta_real)


# Calcular los parámetros de la distribución posterior de Theta
a_post <- a + n
b_post <- b
#b_post <- max(x) 

# Graficando  la distribución posterior -------------------------------------------------------------- # nolint
valores_theta <- seq(b_post, theta_real * 1.5, length.out = num)
densidad_posterior <- (a_post * b_post^a_post) / (valores_theta^(a_post + 1))
#Graficando la maxima verosimilitud ----------------------------------------------------------------- # nolint
verosimulitd <- (1/theta_real)^n

#Distribución predictiva inicial---------------------------------------------------------------------- # nolint: line_length_linter.
z_values <- seq(b, theta_real * 1.5, length.out = num)
predictiva_inicial <- rep(a / ((a + 1) * b), length(z_values))

#Distribución predictiva final ---------------------------------------------------------------------- # nolint
predictiva_final <- rep(a_post / ((a_post + 1) * b_post), length(z_values))

# Creando DataFrames para graficar
df_posterior <- data.frame(theta = valores_theta, density = densidad_posterior, Distribution = "Distribucion Posterior")  # nolint
df_verosimilitud <- data.frame(theta = valores_theta, density = verosimulitd, Distribution = "Verosimilitud")  # nolint
df_graf <- bind_rows(df_posterior, df_verosimilitud)  # nolint

print(head(df_graf))

df_pred_init <- data.frame(z = z_values, density = predictiva_inicial, Distribution = "Predictiva Inicial") # nolint
df_pred_final <- data.frame(z = z_values, density = predictiva_final, Distribution = "Predictiva Final") # nolint
df_pred <- bind_rows(df_pred_init, df_pred_final) #Unir la predictiva final y la inicial # nolint

print(head(df_pred))


# Graficar la distribución posterior de Theta
ggplot(df_graf, aes(x = theta, y = density, color = Distribution)) +
  geom_line(size = 1.2) +
  labs(title = "Distribución Posterior", x = expression(theta), y = "Densidad") + # nolint
  theme_minimal()


# Graficar las distribuciones predictivas
ggplot(df_pred, aes(x = z, y = density, color = Distribution)) +
  geom_line(size = 1.2) +
  labs(title = "Distribuciones Predictivas", x = expression(z), y = "Densidad") + # nolint
  theme_minimal()

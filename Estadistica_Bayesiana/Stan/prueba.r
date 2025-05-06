# Instalar si aún no tienes cmdstanr (puedes omitir si ya está instalado)
# pak::pak("stan-dev/cmdstanr")

# Cargar la librería
library(cmdstanr)
library(posterior)

# Crear archivo del modelo bernoulli.stan
writeLines("
data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  theta ~ beta(1, 1);        // prior
  y ~ bernoulli(theta);      // likelihood
}
", con = "bernoulli.stan")

# Simular datos
set.seed(123)
N <- 100
theta_real <- 0.7
y <- rbinom(N, size = 1, prob = theta_real)
datos <- list(N = N, y = y)

# Compilar el modelo
modelo <- cmdstan_model("bernoulli.stan")

# Ajustar el modelo con muestreo MCMC
ajuste <- modelo$sample(
  data = datos,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)

# Resumen de los parámetros
print(ajuste$summary())

# Graficar posterior de theta
posterior <- as_draws_df(ajuste)
hist(posterior$theta, 
     main = "Posterior de theta", 
     xlab = "theta", 
     breaks = 20,
     col = "skyblue")

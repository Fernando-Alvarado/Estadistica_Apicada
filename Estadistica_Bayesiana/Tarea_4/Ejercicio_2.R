
library(rstan)

# Parámetros dador por el problemea
n <- 10
y <- 7
mu <- 1
sigma2 <- 0.16
nsim <- 10000
theta <- numeric(nsim) #Cconrener donde ire guardando mis thetas
theta[1] <- rnorm(1, mu, sqrt(sigma2))  # inicialización

# Función de la posterior (sin constante)
ln_post <- function(theta) {
  ln_ver <- (7 * theta) - (10*log(1 + exp(theta)))
  ln_prior <- - (theta - mu)^2 / (2 * sigma2)
  return(ln_ver + ln_prior)
}


a<- ln_post(theta[1])

# Metropolis-Hastings
for (t in 2:nsim) {
  theta_inicio <- rnorm(1, theta[t-1], 0.5)  # propuesta
  log_r <- ln_post(theta_inicio) - ln_post(theta[t-1]) # Criterio decision
  if (log(runif(1)) < log_r) { 
    theta[t] <- theta_inicio
  } else {
    theta[t] <- theta[t-1]
  }
}



# Regresando a nuesta p original
p <- exp(theta) / (1 + exp(theta))


# a) Simulando nuestra distribucion final usando M-H 
hist(p, breaks=50, main="Posterior de p, con 10,000 iteraciones", col="#00838f")





# b) Calculando la estimacion con STAN



data_stan <- list(
  n = 10,
  y = 7,
  mu = 1,
  sigma = sqrt(0.16)
)

setwd("C:/Users/ferna/Documents/Estadistica_Aplicada/Estadistica_Bayesiana/Tarea_4")

fit <- stan(file = "./simulacion.stan", data = data_stan)
print(fit, pars=c("theta", "p"))
s






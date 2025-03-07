########################### ALGUNAS VARIABLES ALEATORIAS Y CADENAS DE MARKOV ########################### 

########################### NÚMEROS PSEUDO ALEATORIOS

# Definimos nuestra función del método congruencial lineal
lcm <- function(N, x0, a, c, m){
  x <- rep(0,N)
  x[1] <- x0
  for (i in 2:N) x[i] <- (a*x[i-1]+c)%%m
  u <- x/m
  return(u)
}

N <- 100000 # Número de iteraciones

LCM <- lcm(N, x0 = 1, a = 1664525, c = 1013904223, m = 2^32)

# Una forma de visualizar los datos 

# Guardamos las coordenadas X y Y
X <- rep(0,N-1)
Y <- rep(0,N-1)
for (i in 1:length(LCM)-1) {
  X[i] <- LCM[i]
  Y[i] <- LCM[i+1]
}

# Las graficamos
plot(X,Y,
     type = "p", 
     cex = .05,
     xlab="X",
     ylab="Y")

########################### VARIABLES ALEATORIAS BERNOULLI

# Definimos la función
Bernoulli <- function(p, u){
  if (u<p) {
    return(1)
  } else {
    return(0)
  }
}

# Generamos valores
p = 0.75
ValoresBernoulli <- rep(0,1000)
for (i in 1:1000) ValoresBernoulli[i] <- Bernoulli(p,LCM[i])

# Un primer histograma
hist(ValoresBernoulli,border="blue",xlim=c(0,1), main="Histograma v.a's Benroulli")

# Un segundo histograma
h = hist(ValoresBernoulli, plot=F)
bp = barplot(h$counts/sum(h$counts), col="blue")
axis(1, at=c(bp), labels=h$mids)
title(main="Histograma v.a's Benroulli")
title(xlab="Resultados")
title(ylab="Proporción")


# Comparamos con la función de R
BernoulliR <- rbinom(1000, 1, p) 

h2 = hist(BernoulliR, plot=F)
bp2 = barplot(h2$counts/sum(h2$counts), col="red")
axis(1, at=c(bp2), labels=h2$mids)
title(main="Histograma v.a's Benroulli de R")
title(xlab="Resultados")
title(ylab="Proporción")

########################### VARIABLES ALEATORIAS UNIFORMES
Uniformes <- runif(100000)

hist(Uniformes, freq = FALSE, col="green", xlim=c(-0.1,1.1), main="Histograma v.a's Uniformes",
     xlab="Resultados", ylab="Densidad")

########################### VARIABLES ALEATORIAS BINOMIALES

# Definimos la función
Binomial <- function(n,p){
  S <- 0
  for (i in 1:n) S <- S + Bernoulli(p, runif(1)) 
  return (S)
}

# Generamos valores
p = 0.75
n = 20
ValoresBinomial <- rep(0,1000)
for (i in 1:1000) ValoresBinomial[i] <- Binomial(n,p) 

# Un primer histograma
hist(ValoresBinomial, border="blue", main="Histograma v.a's Benroulli")

# Un segundo histograma
h = hist(ValoresBinomial, plot=F)
bp = barplot(h$counts/sum(h$counts), col="blue")
axis(1, at=c(bp), labels=h$mids)
title(main="Histograma v.a's Binomial")
title(xlab="Resultados")
title(ylab="Proporción")

# Comparamos con la función de R
BinomialR <- rbinom(1000, n, p) 

h2 = hist(BinomialR, plot=F)
bp2 = barplot(h2$counts/sum(h2$counts), col="red")
axis(1, at=c(bp2), labels=h2$mids)
title(main="Histograma v.a's Binomial de R")
title(xlab="Resultados")
title(ylab="Proporción")

########################### UN DADO
Dado <- function(){
  P = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
  Pacumulada = cumsum(P)
  r = runif(1)
  return(min(which(Pacumulada > r)))
}

########################### UN DADO DE N CARAS
DadoN <- function(n){
  P = replicate(n, 1/n)
  Pacumulada = cumsum(P)
  r = runif(1)
  return(min(which(Pacumulada > r)))
}

# La version resumida
DadoN2 <- function(n){
  return(ceiling(n*runif(1)))
}

########################### UN DADO CARGADO
DadoCargado <- function(v){
  Pacumulada = cumsum(v)
  r = runif(1)
  return(min(which(Pacumulada>r)))
}

########################### UN RESUMEN DE LO QUE HEMOS PROGRAMADO
Bernoulli(0.5, runif(1))
Binomial(10,0.5)
Dado()
DadoN(10)
DadoN2(10)
DadoCargado(c(0.1,0.2,0.3,0.4))


########################### SIMULACION DE CADENAS DE MARKOV

# Definimos la función para generar una realización
Realizacion <- function(n,P,Pi0){     ##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Tengo una duda sobre lo que se refiere una graficacion de una CM
  estados <- c(DadoCargado(Pi0))
  for (i in 2:n){
    estados[i] = DadoCargado(P[estados[i-1],])
  }
  return(estados)
}

# Definimos una matriz de transición y una distribución inicial
P = matrix(c(0,1,0,0.5,0,0.5,0,1,0), nrow = 3, byrow = TRUE)
Pi_0 = c(0.25,0.5,0.25)

# Graficamos la realización
plot(c(1:100), Realizacion(100,P,Pi_0), type = "l", col="red", 
     main = "Realización Cadena de Markov", ylab="Xn", xlab="n")

# Graficamos la proporción de permanencia en los estados
h = hist(Realizacion(10000,P,Pi_0), plot=F)
bp = barplot(h$counts/sum(h$counts), col="gold")
axis(1, at=c(bp), labels=h$mids)
title(main="Proporción de permanencia")
title(xlab="Estados")
title(ylab="Proporción")



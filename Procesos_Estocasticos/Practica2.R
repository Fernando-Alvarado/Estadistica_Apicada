############# CADENAS DE MARKOV, MATRICES DE TRANSICION Y MUCHOS PASOS #############

########################### RECUERDO: UN DADO CARGADO
DadoCargado <- function(v){
  Pacumulada = cumsum(v)
  r = runif(1)
  return(min(which(Pacumulada>r)))
}

########################### RECUERDO: SIMULACION DE CADENAS DE MARKOV

# Definimos la función para generar una realización
Realizacion <- function(n,P,Pi0){
  estados <- c(DadoCargado(Pi0))
  for (i in 2:n){
    estados[i] = DadoCargado(P[estados[i-1],])
  }
  return(estados)
}

########################### LA CADENA DEL ADN

# Definimos la matriz de trnasicion
P = matrix(c(0.25,0.5,0.25,0,
             0,0.25,0.5,0.25,
             0.5,0,0,0.5,
             0.25,0.25,0.25,0.25), nrow = 4, byrow = TRUE)
colnames(P) <- c("A","G","C","T")
rownames(P) <- c("A","G","C","T")
P

Pi_0 = c(0.25,0.25,0.25,0.25)

print(Realizacion(100,P,Pi_0))

plot(c(1:100), Realizacion(100,P,Pi_0), type = "l", col="red", 
     main = "Realización Cadena de Markov", ylab="Xn", xlab="n")

# Elevamos su matriz de transición a la n
n <- 6

eig_vectors <- eigen(P)$vectors
eig_values <- eigen(P)$values

Pn <- eig_vectors %*% diag(eig_values)^n %*% solve(eig_vectors)

colnames(Pn) <- c("A","G","C","T")
rownames(Pn) <- c("A","G","C","T")

Pn

# Vamos a corroborar por medio de simulaciones que esto pasa
Pi_A = c(1,0,0,0)
Pi_G = c(0,1,0,0)
Pi_C = c(0,0,1,0)
Pi_T = c(0,0,0,1)

RA <- c()
RG <- c()
RC <- c()
RT <- c()

NSim <- 10000

for (i in 1:NSim) {
  RA <- c(RA, tail(Realizacion(n+1,P,Pi_A),n=1))
  RG <- c(RG, tail(Realizacion(n+1,P,Pi_G),n=1))
  RC <- c(RC, tail(Realizacion(n+1,P,Pi_C),n=1))
  RT <- c(RT, tail(Realizacion(n+1,P,Pi_T),n=1))
}

PA <- c(length(RA[RA == 1])/NSim,length(RA[RA == 2])/NSim,
        length(RA[RA == 3])/NSim,length(RA[RA == 4])/NSim)
PG <- c(length(RG[RG == 1])/NSim,length(RG[RG == 2])/NSim,
        length(RG[RG == 3])/NSim,length(RG[RG == 4])/NSim)
PC <- c(length(RC[RC == 1])/NSim,length(RC[RC == 2])/NSim,
        length(RC[RC == 3])/NSim,length(RC[RC == 4])/NSim)
PT <- c(length(RT[RT == 1])/NSim,length(RT[RT == 2])/NSim,
        length(RT[RT == 3])/NSim,length(RT[RT == 4])/NSim)

Props <- c(PA,PG,PC,PT)
PnSim <- matrix(Props, byrow = TRUE, nrow=4)
colnames(PnSim) <- c("A","G","C","T")
rownames(PnSim) <- c("A","G","C","T")

Pn
PnSim

# Hagamos una funcion para esto
ProbRow <- function(P, i, n, NSim){
  Pi <- replicate(nrow(P), 0)
  Pi[i] <- 1
  
  R <- replicate(NSim,0)
  
  for (i in 1:NSim) R[i] <- tail(Realizacion(n+1,P,Pi),n=1)
  
  Prob <- replicate(nrow(P),0)
  for (j in 1:nrow(P)) Prob[j] <- length(R[R == j])/NSim
  
  return(Prob)
}

# Probemos nuestra funcion
PA <- ProbRow(P, 1, n, NSim)
PG <- ProbRow(P, 2, n, NSim)
PC <- ProbRow(P, 3, n, NSim)
PT <- ProbRow(P, 4, n, NSim)
Props <- c(PA,PG,PC,PT)
PnSim <- matrix(Props, byrow = TRUE, nrow=4)
colnames(PnSim) <- c("A","G","C","T")
rownames(PnSim) <- c("A","G","C","T")

########################### CAMINATA EN EL HIPERCUBO

# Definimos la matriz de la caminata en el hipercubo
P = matrix(c(0,0.5,0.5,0,
             0.5,0,0,0.5,
             0.5,0,0,0.5,
             0,0.5,0.5,0), nrow = 4, byrow = TRUE)

# Elevamos la matriz a la n
n <- 1

eig_vectors <- eigen(P)$vectors
eig_values <- eigen(P)$values

Pn <- round(eig_vectors %*% diag(eig_values)^n %*% solve(eig_vectors), 2)

Pn

# Usemos nuestra función para simular
NSim <- 10000

P1 <- ProbRow(P, 1, n, NSim)
P2 <- ProbRow(P, 2, n, NSim)
P3 <- ProbRow(P, 3, n, NSim)
P4 <- ProbRow(P, 4, n, NSim)
Props <- c(P1,P2,P3,P4)
PnSim <- matrix(Props, byrow = TRUE, nrow=4)

# Comparemos
Pn
PnSim

#Codigo de la tarea-exmane de procesos estocasticos, 
#codigo echo por: Fernando Alvaraado Palacios

#Limpiando la consola para poder trabajar mejor
rm(list = ls(all.names = TRUE))
gc()

#Librerias a usar
library(ggplot2) #Graficar
#set.seed(123) # Fijando la semilla

#Ejercicio 1

#a) Generando nuestra muestra aleatoria de nuestos datos

N_muestra = 100 #Tamaño de la muestra

p_estimada= 0.0603272 #Parametro obtenido en la parte teorica de nuestra muestra 

alpha = 0.04 #Parametro encontrado en la parte teorica que se usara a la hora de simular


#a) ------------------------------------------------------------------------
modelado_Xi = function(N,p){ #Haciendo el modelo de las variables X_i, como un tipo de dado cargado
  X = numeric(N) #Creando un vecotor de longitud N, para podeder meter las variables x_i
  for (i in 1:N){
    U = runif(1)
    if (U < p){ #comparadno con la relacion de p
      X[i] = 0.09
    }else{
      X[i] = -1.1
    }
  }
  return(X)
}



#Definiendo la funcion para generar nuestrar realizacion de S_k
Realizacion_S_k <- function(X) {#Funcion para calcular la realizacion de S_k, que es la suma de los valores de X_i
  N = length(X)
  S = numeric(N)
  S[1] = 0
  for (i in 2:N) {
    S[i] = S[i - 1] + X[i] #Sumando los valores de X_i
  }
  return(S)
}

# Función para calcular la realización de Z_k
Realizacion_Z_k <- function(X, p) {
  N = length(X)
  Z = numeric(N)
  S = Realizacion_S_k(X) #Haciendo las realizaciones de S_k, para poder hacer la realizacion de Z_k
  E = 0.09 * p + (-1.1) * (1 - p) # Esperanza de X_i, es lo que haces que nuestro porceso sea una martingala
  Z[1] = 0
  for (i in 2:N) {
    Z[i] = S[i] - E * (i - 1) # Ajuste de la martingala
  }
  return(Z)
}


# Obtener las trayectorias de S_k y Z_k
#Modelado de la muestras X_i 
X = modelado_Xi(N_muestra, p_estimada)
S_k = Realizacion_S_k(X)
Z_k = Realizacion_Z_k(X, p_estimada)

# Graficar las trayectorias
plot(S_k, type = "l", col = "blue", ylab = "Valor", xlab = "Tiempo", main = "Trayectoria de S_k")
lines(Z_k, type = "l", col = "red")
legend("topright", legend = c("S_k", "Z_k"), col = c("blue", "red"), lty = 1)




#b)  ---------------------------------------------Ahora vamos a graficar algunos brownianos ---------------------------------------------------
#Numero de repeticiones
r_b = c(10, 20, 50) #Repeticiones con las que vamos a calcular nuestras aproximaciones de brownianos brownianos

#Creando un arreglo para donde va a pertener k 
k_array <- function(N, r) {
  k = seq(0, (N-1)/r, by = 1/r) #Haciendo la secuencia de numeros con el que vamos a hacer la grafcia
  return(k)
}


#Primero vamos a crear el modelo de los W_r_k, y luego aplicarelos la interpolacion, para poder graficarlos

realizacion_W_r_k <- function(N, r, Z_k) {#Creando el modelo de los W_r_k, para un ciero intervalo 
  W_rk = numeric(N)
  for (i in 1:N) {
    W_rk[i] = 1 / sqrt(r) * Z_k[i]
  }
  return(W_rk)
}


# Graficar cada uno de los procesos  W_r_k para cada r en r_b
print(length(Z_k))
for (r in r_b) {
  W_r_k = realizacion_W_r_k(N_muestra, r, Z_k)
  k_points = k_array(N_muestra, r)
  print(length(k_points))
  
  plot(k_points, W_r_k, type = "l", col = "black", #Graficando los brownianos
       main = paste("Realización del Proceso W^{(r)}_k, con r =", r), 
       ylab = "W_r_k", xlab = "k")
}



#C) -----------------------Graficando el otro proceso simulando el browniano ----------------------------------------------------

k_modificado <- function(r) {
  # Generar una secuencia de puntos de tiempo dentro del intervalo [0, 1] con 'r' divisiones
  k = seq(0, 1, length.out = r)
  return(k)
}

realizacion_Brown <- function(N, r, p, a) {#Graficando nuestro modelo Browniano
    B = numeric(N)
    # Generamos la secuencia de puntos k dentro del intervalo [0, 1], donde todos midan lo mismo
    k = k_modificado(r)
    # Genarando las trayectorias de W_r_k utilizando realizacion_W_r_k
    Z_k = Realizacion_Z_k(modelado_Xi(N, p), p)
    W_rk = realizacion_W_r_k(N, r, Z_k)
    # Crear el movimiento Browniano geométrico `B`
    for (i in 1:N) {
        B[i] = 110 * exp(-a * k[i] + W_rk[i]) #Aplicndo la formula dada del browniano
    }
    
    return(B)
}

plot( k_modificado(100), realizacion_Brown(100, 100, p_estimada, alpha), type = "l", col="black", 
     main = "Simulacion del Browniano Geometrico", ylab="Xn", xlab="n")


#d), repitnedo los incisos anteriores usando diferetne valos de N

#repidiento los items con mas iteraciones 
N_muestras <- c(500, 1000, 5000) 

#Para la parte a) de los incisos

for (N in N_muestras) { #generalizando el codigo con mas iteraciones
  X = modelado_Xi(N, p_estimada)
  S_k = Realizacion_S_k(X)
  Z_k = Realizacion_Z_k(X, p_estimada)
  plot(S_k, type = "l", col = "blue", ylab = "Valor", xlab = "Tiempo", main = paste("Trayectoria de S_k con N =", N))
  lines(Z_k, type = "l", col = "red")
  legend("topright", legend = c("S_k", "Z_k"), col = c("blue", "red"), lty = 1)
}
mat <- matrix(c(50, 100, 250, 100, 200, 500, 500, 1000, 2500), nrow = 3, ncol = 3, byrow = TRUE)

#Para la parte b) de los incisos
for (i in 1:length(N_muestras)) {
  N <- N_muestras[i]
  
  for (j in 1:ncol(mat)) {
    r <- mat[i, j]
    
    # Generar la trayectoria W_r_k
    W_r_k <- realizacion_W_r_k(N, r, Z_k)
    
    # Generar los puntos de tiempo en el intervalo [0, 1]
    k_points <- k_array(N, r)
    
    # Asegúrate de que las longitudes de k_points y W_r_k sean iguales
      plot(k_points, W_r_k, type = "l", col = "red",
           main = paste("Realización del Proceso W^{(r)}_k, con r =", r, "y N =", N),
           ylab = "W_r_k", xlab = "k")
  }
}

#Para la parte c, donde simulamos el browniano_geometrico
for (N in N_muestras) {
    plot(k_modificado(N), realizacion_Brown(N, N, p_estimada, alpha), type = "l", col="black", 
         main = paste("Simulacion del Browniano Geometrico con N =", N, "y r =", r), ylab="Xn", xlab="n")
}

##Conclusiones
##Podemos observar que mientras mas vamos haciendo iteraciones, nuestro modelo se va hacercando mas a un movieminto browniano, geomtrico, como se esperaba en clase
##Tambien podemos observar que el ajuste en la S_k, que hace que sea martngala, provoca que la simulacion haga un linea recta, como se esperaba en la teoria
##Finalmente, podemos observar que el modelo de browniano geometrico, se ajusta a la formula dada en la teoria y lo que me sorprendio, fue que no necesitamos usar una normal 
##para aproxima el modelo.

#Ejercicio 2

sigma2 <- 2 * alpha # Varianza log-normal
sigma <- sqrt(sigma2) # Desviación estándar

# Simulando 100 variables lognormales(0, 2alpha)



accion_call <- function(X_i, alpha, sigma) {
  X_i <- rlnorm(100, meanlog = 0, sdlog = sigma) #Obteniendo las variables lognormales
  vec_precios <- numeric(length(X_i)) #Haciendo el vector donde iran los precios de las acciones
  for (i in 1:length(X_i)) {
    d <-  110 * X_i[i] - 113 #Formula dada en la teoria
    discounted_value <-    max(exp(-alpha * d), 0)        # e^(-alpha * (110 * X_i - 113)^{+})
    vec_precios[i] <- discounted_value
  }
  return(vec_precios)
}


print(sum(accion_call(X_i, alpha, sigma))/100)


# Definiendo la función para calcular el precio de una opción call según el teorema 112 de las notas vistas en clase
call_option_price <- function(x0, K, alpha, sigma, T) {
  d1 <- (log(x0 / K) + (alpha + (sigma^2) / 2) * T) / (sigma * sqrt(T)) #Caclulando d1
  d2 <- d1 - sigma * sqrt(T) #Calculando d2
  #Checando los valores de la Phi, para poder calcular el precio de la opcion
  Phi_d1 <- pnorm(d1) #Distribuciones dadas por R
  Phi_d2 <- pnorm(d2)
  # Formula del precio de la accion
  v0 <- x0 * Phi_d1 - exp(alpha * T) * K * Phi_d2
  return(v0)
}

# Parametros a introducir, para comparar con la simulacion anteriror
x0 <- 110    
K <- 113     
alpha <- 0.04 
sigma2 <- 2 * alpha 
sigma <- sqrt(sigma2)       

# Calcular el precio de la opción call
call_price <- call_option_price(x0, K, alpha, sigma, 1)

# Mostrar el precio de la opción
print(call_price)



#Comparando los precios, por el momento no son muy parecdios, pero ya no pude ver que salia mal con la simulacion, 
#fue un semestre muy pesado para mi, pero puro trabajo honesto 

























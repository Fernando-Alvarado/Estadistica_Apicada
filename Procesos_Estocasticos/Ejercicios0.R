# Función para simular el juego del caos
JuegoCaos <- function(X0, n, A=c(0,0), B=c(0.5,(0.75)^(1/2)), C=c(1,0)){
  vertices <- list("A" = A,
                   "B" = B,
                   "C" = C)
  x <- c(X0[1])
  y <- c(X0[2])
  for (i in 1:n){
    r <- runif(1)
    if (r < 1/3){
      v <- vertices$A
    } else if (r >= 1/3 & r < 2/3){
      v <- vertices$B
    } else{
      v <- vertices$C
    }
    x <- c(x,(v[1]+x[i])/2)
    y <- c(y,(v[2]+y[i])/2)
  }
  return(list("X" = x,
              "Y" = y))
}

# Simulación
L <- JuegoCaos(c(0.5,0.5), 10000)

# Graficamos el resultado
plot(L$X,L$Y,
     type = "p", 
     cex = .05,
     main="Juego del Caos",
     xlab="X",
     ylab="Y",
     col="blue")

install.packages("ggplot2")

# Lo graficamos de otra manera usando ggplot2
library(ggplot2)

df <- data.frame(L)
df

ggplot(data = df, aes(X,Y),
       xlab="X",
       ylab="Y") + geom_point(size=0.01)

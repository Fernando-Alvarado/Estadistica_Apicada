#Codigo de la segunda tarea, ejercicio 1
#Autor: Fernando Alvarado Palacios
#Fecha: 2021/09/15

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
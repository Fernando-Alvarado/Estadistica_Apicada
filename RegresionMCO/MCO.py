# Datos de ejemplo
x = [1, 2, 3, 4, 5]
y = [2, 4, 5, 4, 5]
# Calcular promedios
n = len(x)
x_mean = sum(x) / n
y_mean = sum(y) / n
# Calcular la pendiente (beta_1)
numerador = 0
denominador = 0
for i in range(n):
    numerador += (x[i] - x_mean) * (y[i] - y_mean)
    
for i in range(n):
    denominador += (x[i] - x_mean) ** 2

# Calcular beta_0
beta_1 = numerador / denominador

# Calcular beta_0
beta_0 = y_mean - beta_1 * x_mean

#Veamos como se ve nuestro modelo de regresion lineal
print(f"El modelo es: y = {beta_0:.2f} + {beta_1:.2f} * x")


def Proyeccion(x): #Funcion que nos ayudara a hacer una proyeccion a futuro con los datos que tenemos
    return beta_0 + beta_1 * x



print(f"Para x = {6}, la proyección de y es: {Proyeccion(6)}")
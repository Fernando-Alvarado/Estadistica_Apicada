#Echo por: Alvarado Palacios Fernando
#Fecha: 2021/10/14

library(purrr) #Libreria para trabajar con listas de forma mas eficiente

print(mtcars$cyl) # Veamos que cilindros tenemos en nuestro dataframe

by_cyl <- split(mtcars, mtcars$cyl) #divide nuestro dataframe, segun el numero de cilindros que tenemos en este caso 4,6 y 8

print(by_cyl)

print(
    by_cyl %>%
        map(~ lm(mpg ~ wt, data = .x)) %>% #Ajuscmos el modelo de regresion para cada subconjunto de by_cyl y con data = .x, estamos indicando que se ajuste a cada subconjunto
        map(coef) %>% #Extraemos los coeficientes de cada modelo
        map_dbl(2) #Extraemos el segundo coeficiente de cada modelo
)
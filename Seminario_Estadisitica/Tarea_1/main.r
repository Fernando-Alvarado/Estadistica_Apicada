#Codigo de Fernando Alvarado Placios 
#Analisis de salarios 

setwd("C:/Users/ferna/Documents/Seminario Estadistica/EjercicioExtra1")

#Cargando las librerias a usar
library(ggplot2)
library(dplyr)

#Cargando los datos
datos <- read.csv("./salarios.csv")
datos$sexo <- factor(datos$sexo, levels = c(1, 2), labels = c("Hombre", "Mujer"))


#Agrupando los datos por sexo
datos %>% 
    group_by(sexo) %>%
    summarise_all(mean) -> datos.summary




#Haciendo un analisis estadistico de los datos 
mean(datos$salario) #Media de los salarios
var(datos$salario) #Varianza de los salarios

#Separando por hombres y mujeres 
datos.summary  # 1 significa hombre y 2 mujer

#Grafica para poder visualizar los resultado anteriores

ggplot(datos, aes(x = anios_trabajo, y =  salario, color = sexo)) +
  geom_point() +
  geom_point(data = datos.summary, shape=15, size=5)  +
  labs(
    title = "Comparacion años de trabajo y salario por sexo"
  )
#En esta grafica podemos ver la media de salarios y de años de trabajo por sexo, donde en promedio a los hombre les pagan mas con menos años de experiencia que a las mujeres


#Hacineod un boxpllot 

ggplot(datos, aes(x = sexo, y = salario, fill = sexo)) +
  geom_boxplot() +
  geom_point(data = datos, aes(x = sexo, y = salario)) +
  labs(
    title = "Salarios por sexo"
  )
 # En esta grafica podemos ver que aunque la media de los salarios sean parecidos, los hombres tienen un mayor rango de salarios que las mujeres

ggplot(datos, aes(x = sexo, y = anios_trabajo, fill = sexo)) +
  geom_boxplot() +
  geom_point(data = datos, aes(x = sexo, y = anios_trabajo)) +
  labs(
    title = "Años de trabajo por sexo"
  )
#Con esta grafica podemos ver que en promedio los hombres  contratados cuantan con menos años de experiencia que las mujeres

#Conclusion
#Con los datos proporcionados por la empresa, podemos ver que en general los los salarios no dependen de los años de antiguedad, si no por el sexo ya que los hombre gana mas con menos años de experiencia que las mujeres
#Politica que debe cambiar para tener una equidad de genero en la empresa

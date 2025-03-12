#Codigo de r para ejecutar en chunks
library(ggplot2)
library(dplyr)

setwd("C:/Users/ferna/Documents/Estadistica_Aplicada/Seminario_Estadisitica/Tarea_4/Data")

data <- read.csv("./initech.csv")

modelo <- lm(salary~ years,  data = data)

#Graficar mis datos junto con mi modelo

ggplot(data, aes(x = years, y = salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Salario vs Años de Experiencia",
       x = "Años de Experiencia",
       y = "Salario") +
  theme_minimal()
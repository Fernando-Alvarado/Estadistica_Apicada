#Para hacer mis pruebas antes de hacer el markdown
#Librerias que se van a usar en esta tarea
library(readxl)
library(ggplot2)
library(dplyr)

#Manejo de Formato de mi MD
library(GGally)
library(gridExtra)
library(grid)
library(cowplot)

library(corrplot)


print("Hola mundo")
#--------------------------------------------------------------------------------





#chunks general --------------------------------------------------------------------------------------------------------------

data <- read.csv("Data/wine.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)


matriz_correlacion <- cor(data[, -1], use = "pairwise.complete.obs")



#-----------------------------------------------------------------------------------------------------------------------------





#x11()  # Abre una nueva ventana de gráficos (opcional)
#print(ggpairs(data))  # Asegura que la gráfica se muestre
vars_selesccionadas <- c("Flavanoids", "Total_phenols", "OD", "Proline", "Alcohol", "Proanthocyanins")

data_subset <- data[, vars_selesccionadas]

x11()
print(ggpairs(data_subset))  # Asegura que la gráfica se muestre

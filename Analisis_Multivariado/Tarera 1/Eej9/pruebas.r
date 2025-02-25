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


library(TeachingDemos)
print("Hola mundo")
#--------------------------------------------------------------------------------





#chunks general --------------------------------------------------------------------------------------------------------------

data <- read.csv("Data/wine.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
data$Class <- as.factor(data$Class)

matriz_correlacion <- cor(data[, -1], use = "pairwise.complete.obs")



#-----------------------------------------------------------------------------------------------------------------------------
orden_1 <- c("Total_phenols", "Proanthocyanins", "Proline", "Alcohol", "Flavanoids", "OD")
orden_2 <- c("Total_phenols", "Alcohol", "OD", "Flavanoids", "Proanthocyanins", "Proline")
orden_3 <- c("Flavanoids", "Alcohol", "Proline", "OD", "Proanthocyanins", "Total_phenols")
orden_4 <- c("Flavanoids", "Alcohol", "Proanthocyanins", "Proline", "OD", "Total_phenols")
orden_5 <- c("Flavanoids", "Alcohol", "Proline", "OD", "Proanthocyanins", "Total_phenols")
orden_6 <- c("Flavanoids", "Alcohol", "Proline",  "OD", "Proanthocyanins", "Total_phenols")
orden_7 <- c("Proanthocyanins", "Alcohol", "Total_phenols", "Proline", "OD", "Flavanoids")
orden_8 <- c("Proline", "Flavanoids", "Proanthocyanins", "Total_phenols", "OD", "Alcohol")
orden_9 <- c("Proanthocyanins", "Proline", "Total_phenols", "OD", "Flavanoids", "Alcohol")
orden_10 <- c("Alcohol", "Flavanoids", "OD", "Proanthocyanins", "Total_phenols", "Proline")



orden_total <- list(
  orden_1, orden_2, orden_3, orden_4, orden_5, orden_6, orden_7, orden_8, orden_9, orden_10
)

#--------------------------------------------------------------------------------


#x11()  # Abre una nueva ventana de gráficos (opcional)
#print(ggpairs(data))  # Asegura que la gráfica se muestre
vars_selesccionadas <- c( "Flavanoids", "Total_phenols", "OD", "Proline", "Alcohol", "Proanthocyanins")

data_subset <- data[, vars_selesccionadas]

graficas_estrellas <- function(set, texto) {
  x11()  # Abre nueva ventana de gráficos
  data_subset <- data[, set, drop = FALSE]  # Asegurar que sea data frame
  stars(data_subset, draw.segments = TRUE, col.stars = "black", main = texto) 
}


a <- c("Flavanoids", "Alcohol", "Proline",  "OD", "Proanthocyanins", "Total_phenols")
b <- c("Proanthocyanins", "Proline", "Total_phenols", "OD", "Flavanoids", "Alcohol")

#graficas_estrellas(a, "Opcion 1")
#graficas_estrellas(b, "Opcion 2")


graficas_caras <- function(set, texto) {
  x11()  # Abre nueva ventana de gráficos
  data_subset <- data[, set, drop = FALSE]  # Asegurar que sea data frame
 
  faces2(data_subset)
   title(main = texto)
}

graficas_caras(a, "Opcion 1")
graficas_caras(b, "Opcion 2")


library(andrews)

curvas_de_Andrews <- function(set, texto) {
  x11()  # Abre nueva ventana de gráficos
  data_subset <- data[, set, drop = FALSE]  # Asegurar que sea data frame
  andrews(data_subset, clr  = data$Class, , main = texto)
}

#curvas_de_Andrews(a, "Opcion 1")
#curvas_de_Andrews(b, "Opcion 2")

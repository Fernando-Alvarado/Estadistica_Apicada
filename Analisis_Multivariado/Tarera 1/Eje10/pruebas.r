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
library(andrews) #Libreria para las caras de chernoff
library(TeachingDemos)

#Cargar los arvhivos

data <- read.csv("Data/Diabetes.txt", header = TRUE, sep = " ", stringsAsFactors = FALSE)

com_1 <- c("relwt", "glufast", "glutest", "instest", "sspg")
com_2 <- c("glufast", "relwt", "sspg", "instest", "glutest")
com_3 <- c("sspg", "instest", "glutest", "glufast", "relwt")
com_4 <- c("instest", "sspg", "glufast", "glutest", "relwt")
com_5 <- c("glutest", "glufast", "relwt", "sspg", "instest")
com_6 <- c("sspg", "relwt", "instest", "glutest", "glufast")
com_7 <- c("glutest", "sspg", "instest", "glufast", "relwt")
com_8 <- c("instest", "glutest", "sspg", "relwt", "glufast")
com_9 <- c("glufast", "instest", "relwt", "sspg", "glutest")
com_10 <- c("relwt", "glutest", "glufast", "sspg", "instest")
com_11 <- c("instest", "glufast", "sspg", "relwt", "glutest")
com_12 <- c("glufast", "glutest", "relwt", "sspg", "instest")
com_13 <- c("sspg", "glutest", "glufast", "instest", "relwt")
com_14 <- c("relwt", "instest", "glufast", "sspg", "glutest")
com_15 <- c("glutest", "sspg", "relwt", "glufast", "instest")
com_16 <- c("glufast", "relwt", "instest", "glutest", "sspg")
com_17 <- c("instest", "sspg", "glutest", "relwt", "glufast")
com_18 <- c("relwt", "glufast", "instest", "sspg", "glutest")
com_19 <- c("sspg", "relwt", "glutest", "glufast", "instest")
com_20 <- c("glutest", "instest", "sspg", "glufast", "relwt")



graficas_estrellas <- function(set, texto) {
  data_subset <- data[, set, drop = FALSE]  # Asegurar que sea data frame
  stars(data_subset, draw.segments = TRUE, col.stars = "black", main = texto) 
}


for(i in 1:20) {
  set <- get(paste0("com_", i))
  texto <- paste("Conjunto", i)
  graficas_estrellas(set, texto)
}


graficas_caras <- function(set) {
  data_subset <- data[, set, drop = FALSE]  # Asegurar que sea data frame
  x11()
  faces2(data_subset)
}

graficas_caras(com_16)
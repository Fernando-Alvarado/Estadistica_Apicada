# Estadística Bayesiana 2025-II ----
# Grupo: 4307
# Dra. Lizbeth Naranjo
# Enrique Reyes

## Laboratorio 4 ----
# 28-02-2025

rm(list=ls())
library(ModEstM)
library(ggplot2)

# Gamma-Poisson ----

Estimadores_T = function(a,b,x,c){
  n=length(x)
  media_T = (a+sum(x))/(b+n)
  mediana_T = qgamma(0.5,a+sum(x),b+n)
  Moda_T = (a+sum(x)-1)/(b+n)
  I_cre = qgamma(c(c/2,1-c/2),a+sum(x),b+n)
  list(Media=media_T,Mediana=mediana_T,
       Moda=Moda_T,
       Intervalo=I_cre)
}

Estimadores_S = function(a,b,x,m,c){
  n=length(x)
  x1=rgamma(m,a+sum(x),b+n)
  media_S = mean(x1)
  mediana_S = quantile(x1,0.5)
  Moda_S = ModEstM(x1)
  I_cre = quantile(x1,c(c/2,1-c/2))
  list(Media=media_S,Mediana=mediana_S,
       Moda=Moda_S[[1]][1],
       Intervalo=I_cre)
}

set.seed(4638)
a=20;b=2
x=rpois(104,10)

Teoricos = Estimadores_T(a,b,x,0.05)
Simulados = Estimadores_S(a,b,x,1000,0.05)

Intervalos = as.data.frame(rbind(Teoricos$Intervalo,
                                 Simulados$Intervalo))
colnames(Intervalos)=c("LI","LS")
Intervalos$Real=10
Intervalos$Clase=as.factor(c(1,2))



ggplot(Intervalos,aes(x=Clase,y=Real,colour=Clase)) +
  geom_errorbar(aes(ymin = LI, ymax = LS))+
  theme_bw()

# Vayamos con un caso real
# Cargamos la base de datos
#granja = read.csv("C:/Users/ferna/Documents/Estadistica_Aplicada/Estadistica_Bayesiana/Guia/Inferencia/Huevos.csv")

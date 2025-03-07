# Estadística Bayesiana 2025-II ----
# Grupo: 4307
# Dra. Lizbeth Naranjo
# Enrique Reyes

## Laboratorio 2 ----
# 14-02-2025
# Modelo Poisson-Gamma
library(ggplot2)

# Distribuciones inicial y final
# Hiperparametros
a<-100
b<-2
# Inicial
set.seed(767)
lambda<-rgamma(1,shape = a, rate = b)
x0<-seq(0.1,150,0.1)
Y0<-dgamma(x0,shape = a,rate = b)
# Verosimilitud
x<-0:150
Y<-dpois(x,lambda = lambda)
# Una muestra seria
n<-20
muestra<-rpois(n,lambda)
Sn<-sum(muestra)
# Posterior
lambdaS<-dgamma(muestra,shape=a+Sn,rate=b+n)
Ys<-dgamma(x0,shape=a+Sn,rate=b+n)

plot0<-ggplot() + aes(x=x,y=Y,colour="Verosimilitud") + 
  geom_line(size=1) +
  geom_line(aes(x=x0,y=Y0,colour="Inicial")) +
  geom_line(aes(x=x0,y=Ys,colour="Final")) +
  scale_colour_manual(breaks=c("Verosimilitud","Inicial",
                               "Final"),
                      values=c("#6C3483","#117A65",
                               "#A93226"),
                      labels=c("Verosimilitud","Inicial",
                               "Final"))+
  ggtitle("Distribuciones con a=100 and b=2") +
  labs(x="x", y="Densidad") +
  theme_bw()
plot0

###################
library(bayesrules)
plot_gamma_poisson(shape = a,rate = b,sum_y = Sn,n=n)+
  theme_bw()

# Distribución predictiva inicial y final:
# Hiperparametros de la distribucion de lambda
a<-100
b<-2
x<-0:150
z<-dnbinom(x,a,b/(b+1))

# Vector de observaciones y la distribucion predictiva
# posterior
X<-muestra
n<-n
Sn<-sum(X)
x<-0:150
au<-a+Sn
bu<-b+n
z1<-dnbinom(x,size=au,prob=bu/(bu+1))


plot1<-ggplot() + aes(x=x,y=z,colour="Inicial") + 
  geom_line(size=1) +
  geom_line(aes(x=x,y=z1,colour="Final"))+
  scale_colour_manual(breaks=c("Inicial", "Final"),
                      values=c("#cd7118","#1874cd"),
                      labels=c("Predictiva Inicial",
                               "Predictiva Final"))+
  ggtitle("Distribuciones predictivas con a=100 and b=2") +
  labs(x="x", y="Densidad") +
  theme_bw()
plot1




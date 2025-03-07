##################################################
### Enfoque Bayesiano
##################################################

library(VGAM)

##################################################
### Bernoulli
##################################################
theta_true = 0.3 ### valor verdadero

### Simular una muestra
### Likelihood   Binomial(y|size=1,theta) = Bernoulli(theta)  
n = 100 ### tamaño de muestra 
k = 1
Xi = rbinom(n=n, size=1, prob=theta_true)

(y = sum(Xi))
y

### Prior   Beta(p|,a,b)
a = 2   ### hiperparametro de Beta
b = 2   ### hiperparametro de Beta 

### Una sola muestra observada
p = seq(0,1,0.01)

### Posterior

X11()
	plot(p,dbeta(p,y+1,n-y+1), 
	     main=paste0("Muestra Y=",y),xlab=expression(theta), ylab="Densidad", 
	     lty=1, lwd=8, type="l", col="black")
	lines(p, dbeta(p,a,b), 
	      lty=2, lwd=8, col="red")
	lines(p, dbeta(p,a+y,b+n-y), 
	      lty=3, lwd=8, col="blue")
	legend("topright", legend=c("Prior","Verosimilitud","Posterior"), 
	       lty=c(1,2,3), col=c("red","black","blue"), lwd=6,cex=6.7)

	
	
	
	### Predictiva  Prior & Posterior
X11()
	x = (0:k)
	plot(x,dbetabinom.ab(x, size=1, shape1=a+sum(y), shape2=b+1*n-sum(y)), 
	     main=paste0("Muestra suma Y=",sum(Xi)," media Y=",mean(Xi)),xlab="Y",ylab="Densidad",
	     type="h",col="blue",lwd=2)
	points(x+jitter(0.01),dbetabinom.ab(x, size=1, shape1=a, shape2=b), 
	       type="h",col="black",pch=19,cex=1.5,lwd=2)
	legend("topright", legend=c("Predictiva Prior","Predictiva Posterior"), 
	       lty=c(1,1), col=c("black","blue"), pch=c(19,19), cex=0.7)
	
##################################################
### Binomial
##################################################

#dev.new(width=4,height=4) 

	theta_true = 0.3 ### valor verdadero
	### Muestra observada de tamanio k
	k = 10
	
	### Simular una muestra
	### Likelihood   Binomial(y|k,theta)  
	n = 100   ### tamaño de muestra 
	Xi = rbinom(n=n, size=k, prob=theta_true)
	
	y = sum(Xi)
	
### Prior   Beta(p|,a,b)
a = 2
b = 2

### Posterior
p = seq(0,1,0.01)

	plot(p,dbeta(p,sum(y)+1,k*n-sum(y)+1), 
	     main=paste0("Muestra suma Y=",sum(Xi)," media Y=",mean(Xi)), xlab=expression(theta), ylab="Densidad", 
	     lty=1, lwd=4, type="l", col="black")
	lines(p, dbeta(p,a,b), 
	      lty=2, lwd=4, col="red")
	lines(p, dbeta(p,a+sum(y),b+k*n-sum(y)), 
	      lty=3, lwd=4, col="blue")
	legend("topright", legend=c("Prior","Verosimilitud","Posterior"), 
	       lty=c(1,2,3), col=c("red","black","blue"), lwd=2,cex=0.7)

### Predictiva  Prior & Posterior

x = (0:k)
	plot(x,dbetabinom.ab(x, size=k, shape1=a+sum(y), shape2=b+k*n-sum(y)), 
	     main=paste0("Muestra suma Y=",sum(Xi)," media Y=",mean(Xi)),xlab="Y",ylab="Densidad",
	     type="h",col="blue",lwd=2)
	points(x+jitter(0.1),dbetabinom.ab(x, size=k, shape1=a, shape2=b), 
	       type="h",col="black",pch=19,cex=1.5,lwd=2)
	legend("topright", legend=c("Predictiva Prior","Predictiva Posterior"), 
	       lty=c(1,1), col=c("black","blue"), pch=c(19,19), cex=0.7)

##################################################
###	JAGS model
##################################################
	#fit a JAGS model. 
	require(rjags)
	
jags.model = "
model {  
for(i in 1:n){  
   Xi[i] ~ dbinom(theta,k); ### Likelihood  
}  
theta ~ dbeta(a,b);  ### Prior   
Xnew ~ dbinom(theta,k);  ### Predictive   
}
"
#setup data as list
data = list(Xi=Xi, k=k, n=length(Xi), 
            a=a,b=b)

#run JAGS model
j.model <- jags.model(file = textConnection(jags.model),
                      data=data,
                      n.chains=3)

#sample from the posterior
jags.out  <- coda.samples (model = j.model,
                            variable.names = c('theta','Xnew'),
                            n.iter = 1000)

#grab coefficients, compare fitted vs observed of z to prove this fits. 
summary(jags.out)
plot(jags.out)

################################################## 

##################################################
### Usando paquete de R: MCMCpack

library(MCMCpack)

## Not run:
posterior <- MCbinomialbeta(y=sum(Xi), n=n*k, 
                            alpha=a, beta=b, mc=5000)

summary(posterior)
plot(posterior)

grid <- seq(0,1,0.01)
plot(grid, dbeta(grid, a, b), type="l", col="red", lwd=3, ylim=c(0,3.6),
     xlab="pi", ylab="density")
lines(density(posterior), col="blue", lwd=3)
legend("topright", c("prior", "posterior"), lwd=3, col=c("red", "blue"))

##################################################
##################################################

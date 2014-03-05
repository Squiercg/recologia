### Modelo com  Zeros Inflados
set.seed(123)
### Gerando os dados
psi <- 0.7
n.amostras <- 30
x <- gl(n = 2, k = n.amostras, labels = c("Sem Flor", "Com Flor"))
w <- rbinom(n = 2*n.amostras, size = 1, prob = psi)
lambda <- exp(0.69 +(0.92*(as.numeric(x)-1)) )
C <- rpois(n = 2*n.amostras, lambda = w * lambda)
cbind(x, w, C)

#Figura 01
boxplot(C~x,frame=F,ylab="Abundância de aranhas",xlab="Arbusto")

### Analise Bayesiana
# Definindo o modelo
sink("ZIP.txt")
cat("
model {
# Priors
 psi ~ dunif(0,1)
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)

# Likelihood
 for (i in 1:n) {
    w[i] ~ dbern(psi)
    C[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- w[i]*lambda[i]
    log(lambda[i]) <- alpha + beta *x[i]
 }

# Quantidade derivada (Para comparar com o resultado frequentista)
 R.lpsi <- logit(1-psi)
}
",fill=TRUE)
sink()

# Juntando os dados
bugs.data <- list(C = C, x = as.numeric(x)-1, n = length(x))

# Função para gerar o inicio da cadeia
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1), w = rep(1, 2*n.amostras))}

# Parametros a Estimar
params <- c("alpha","beta","psi","R.lpsi")

# Caracteristicas do MCMC
nc <- 3					# Número de cadeias
ni <- 50000				# Número de amostras da distribuição posterior
nb <- 10000				# Numero de amostras a ser descartadas do inicio da cadeia como burn-in
nt <- 4					# Thinning rate

# Iniciando o Gibbs Sampler
library(R2OpenBUGS)
out <- bugs(data=bugs.data, inits=inits, parameters.to.save=params, model.file="ZIP.txt",
n.thin=nt,n.chains=nc,n.burnin=nb, n.iter=ni)

print(out, dig = 3)

#figura 2
layout(matrix(c(1,1,2,3),ncol=2,nrow=2,byrow=T))
hist(out$sims.list$psi,breaks=seq(0,1,by=0.025),freq=F,xlab="Probabilidade",main="Chance do arbusto ser habitável"
     ,ylab="Frequência")
abline(v=out$summary[3,1],col="red",lty=2,lwd=2)
lines(x=c(out$summary[3,3],out$summary[3,3]), y=c(0,1),lwd=4,col="red")
lines(x=c(out$summary[3,7],out$summary[3,7]), y=c(0,1),lwd=4,col="red")
legend("topleft",lty=c(2,1),lwd=c(2,4),col="red",legend=c("Média","Intervalo de confiança de 95%"),bty="n")


hist(exp(out$sims.list$alpha),freq=F,breaks=seq(0,8,by=1),main=expression(paste(lambda," Sem flor")),
     xlab="Lambda Esperado",ylab="Frequência",xaxt="n")
axis(1,at=0:8)
hist(exp(out$sims.list$alpha+out$sims.list$beta),freq=F,breaks=seq(0,8,by=1),main=expression(paste(lambda," Com flor")),
     xlab="Lambda Esperado",ylab="Frequência",xaxt="n")
axis(1,at=0:8)




### Analise Frequentista
#install.packages("pscl")
library(pscl)
modelo.z <- zeroinfl(C ~ x | 1, dist = "poisson")
summary(modelo.z)

modelo.glm <- glm(C ~ x , family = "poisson")
summary(modelo.glm)

logLik(modelo.z)
logLik(modelo.glm)

extractAIC(modelo.z)
extractAIC(modelo.glm)

#Figura 3
layout(matrix(c(1,2,3,4),ncol=2,nrow=2,byrow=T))

menor<-min(c(resid(modelo.z),resid(modelo.glm)))
maior<-max(c(resid(modelo.z),resid(modelo.glm)))

plot(resid(modelo.z),frame=F,main="Modelo com Zeros-Inflados",ylim=c(menor,maior))
abline(h=0,lty=2,lwd=2)
hist(resid(modelo.z),main="Modelo com Zeros-Inflados",breaks=seq(menor,maior+0.5,by=0.25),freq=F)
lines(density(resid(modelo.z)),lwd=3,lty=3,col="red")


plot(resid(modelo.glm),frame=F,main="GLM",ylim=c(menor,maior))
abline(h=0,lty=2,lwd=2)
hist(resid(modelo.glm),main="GLM",breaks=seq(menor,maior+0.5,by=0.25),freq=F)
lines(density(resid(modelo.glm)),lwd=3,lty=3,col="red")


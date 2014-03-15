### Gerando dados
set.seed(1234)
n.locais <- 10
A <- runif(n = 2*n.locais, 2,5)		# Area de 2 a 5 km2 
x <- gl(n = 2, k = n.locais, labels = c("Pasto", "Plantação"))
preditor.linear <- log(A) + 0.69 +(0.92*(as.numeric(x)-1))
lambda <- exp(preditor.linear)
C <- rpois(n = 2*n.locais, lambda = lambda)

#Figura 1
stripchart(C~x,vertical=T,pch=19,method="stack",offset=0.8,at=c(1.2,2.2),frame=F,xlab="Contagem",ylab="Local")

#Figura 2
stripchart(C~x,vertical=T,pch=19,method="jitter",offset=0.8,at=c(1,2),frame=F,xlab="Contagem",ylab="Local",cex=A/2)

### Análise com OpenBugs
# Definindo o modelo
sink("Offset.txt")
cat("
model {
# Priors
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)

# Likelihood
 for (i in 1:n) {
    C[i] ~ dpois(lambda[i])
    log(lambda[i]) <- 1 * logA[i] + alpha + beta *x[i]
 }
}
",fill=TRUE)
sink()

# Juntando os dados numa lista para o OpenBugs
bugs.data <- list(C = C, x = as.numeric(x)-1, logA = log(A), n = length(x))

# Função para iniciar as cadeias com valores ao acaso
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1))}

# Parametros a estimar
params <- c("alpha","beta")

# MCMC settings
nc <- 3    # Número de cadeias
ni <- 1100 # Tamanho de cada cadeia
nb <- 100  # Número de amostras a descartar como burn-in
nt <- 2    # Thinning rate

# Inicie o Gibbs-sampler
library(R2OpenBUGS)

out <- bugs(data=bugs.data, inits=inits, parameters.to.save=params, model.file="Offset.txt",
            n.thin=nt,n.chains=nc,n.burnin=nb, n.iter=ni)

#Resultado
print(out, dig = 3)

### Analise usando R

glm.fit.sem.offset <- glm(C ~ x, family = poisson)
glm.fit.com.offset <- glm(C ~ x, family = poisson, offset = log(A))
summary(glm.fit.sem.offset)
summary(glm.fit.com.offset)

#figura 3
par(mfrow=c(2,1))
plot(resid(glm.fit.sem.offset),ylim=c(-2.5,2),main="Sem Offset",pch=19,ylab="Resíduos",xlab="Amostras",cex=1.3)
plot(resid(glm.fit.com.offset),ylim=c(-2.5,2),main="Com Offset",pch=19,ylab="Resíduos",xlab="Amostras",cex=1.3)

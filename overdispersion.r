### Gerando os dados
set.seed(123)
n.locais <- 20
x <- gl(n = 2, k = n.locais, labels = c("Nativo", "Arado"))
eps <- rnorm(2*n.locais, mean = 0, sd = 0.5)
lambda.OD <- exp(0.60 +(0.35*(as.numeric(x)-1) + eps) )

C.OD <- rpois(n = 2*n.locais, lambda = lambda.OD)

data.frame(Local=x,Contagem=C.OD)

#GRafico 1
boxplot(C.OD ~ x, col = "grey", xlab = "Uso da terra",frame=F,
ylab = "Contagem de coelhos", las = 1, ylim = c(0, max(C.OD)))


### Analise usando o R
glm.fit.no.OD <- glm(C.OD ~ x, family = poisson)
glm.fit.with.OD <- glm(C.OD ~ x, family = quasipoisson)
summary(glm.fit.no.OD)
summary(glm.fit.with.OD)

anova(glm.fit.no.OD, test = "Chisq")
anova(glm.fit.with.OD, test = "F")

### Analise com OpenBugs

sink("overdispersion.txt")
cat("
model {
# Priors
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)
 sigma ~ dunif(0, 10)
 tau <- 1 / (sigma * sigma)

# Likelihood
 for (i in 1:n) {
    C.OD[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha + beta *x[i] + eps[i]
    eps[i] ~ dnorm(0, tau)
 }
}
",fill=TRUE)
sink()

# Juntando os dados
bugs.data <- list(C.OD = C.OD, x = as.numeric(x)-1, n = length(x))

# Função de inicialização
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1), sigma = rlnorm(1))}

# Parametros a serem guardados na saida.
params <- c("alpha", "beta", "sigma")

# Configurando o MCMC
nc <- 3		# Número de cadeias
ni <- 3000	# Tamanho da cadeia
nb <- 1000	# burn-in, quanto do inicio da cadeia vai ser jogado fora
nt <- 5		# Thinning rate (A cada uma amostra que pegamos, jogamos 5 fora, para evitar auto-correlação)

# Iniciando o Gibbs sampling
library(R2OpenBUGS)
out <- bugs(data=bugs.data, inits=inits, parameters.to.save=params,model.file="overdispersion.txt", n.thin=nt,
            n.chains=nc,n.burnin=nb, n.iter=ni)
print(out, dig = 3)


#O que realmente estamos estimando.

#Grafico 2
hist((C.OD[which(x=='Arado')]),freq=F,main="Arado",xlab="Histograma de Contagens",ylab="Frequência da contagem")
lines(density(rpois(1000,out$summary[1,1]),adjust = 4),lwd=2,col="red",lty=2)
valores<-rep(out$summary[1,1],1000)+rnorm(1000,0,out$summary[3,1])
valores[valores<0]<-0
lines(density(rpois(1000,valores),adjust = 3),lwd=2,col="blue",lty=2)
legend("topright",col=c("red","blue"),lty=2,lwd=2,legend=c("Sem overdispersion","Com overdispersion"),bty="n")

#Grafico 3
hist((C.OD[which(x=='Nativo')]),freq=F,main="Nativo",xlab="Histograma de Contagens",ylab="Frequêcia da contagem")
lines(density(rpois(1000,out$summary[1,1]+out$summary[2,1]),adjust = 3),lwd=2,col="red",lty=2)
valores<-rep(out$summary[1,1]+out$summary[2,1],1000)+rnorm(1000,0,out$summary[3,1])
valores[valores<0]<-0
lines(density(rpois(1000,valores),adjust = 3),lwd=2,col="blue",lty=2)
legend("topright",col=c("red","blue"),lty=2,lwd=2,legend=c("Sem overdispersion","Com overdispersion"),bty="n")


#Exemplo 2
n.locais <- 60
x <- gl(n = 2, k = n.locais, labels = c("Nativo", "Arado"))
lambda.OD <- exp(0.60 +(0.35*(as.numeric(x)-1)) )

C.OD <- rpois(n = 2*n.locais, lambda = lambda.OD)

boxplot(C.OD ~ x, col = "grey", xlab = "Uso da terra",frame=F,
ylab = "Contagem de coelhos", las = 1, ylim = c(0, max(C.OD)))

bugs.data2 <- list(C.OD = C.OD, x = as.numeric(x)-1, n = length(x))

params <- c("alpha", "beta", "sigma")

out2 <- bugs(data=bugs.data2, inits=inits, parameters.to.save=params,model.file="overdispersion.txt", n.thin=nt,
            n.chains=nc,n.burnin=nb, n.iter=ni)
print(out2, dig = 3)

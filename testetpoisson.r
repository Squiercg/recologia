set.seed(123)
### Gerando os dados
#Gerando o preditor linear, o item 3 do post.
n.arbustos <- 10
x <- gl(n = 2, k = n.arbustos, labels = c("Urbana", "Rural"))
n <- 2*n.arbustos

#Aplicando a função de ligação, exponencial é o contrario de logaritimo,
#estamos no caminho contrario aqui, esse é o item 2 do post
lambda <- exp(0.69 + 0.92*(as.numeric(x)-1))

#Adicionando a parte estocastica, o item 1 do post
C <- rpois(n = n, lambda = lambda)

#Vamos olhar o que criamos
aggregate(C, by = list(x), FUN = mean)

jpeg("01.jpg")
boxplot(C ~ x, col = "grey", xlab = "Tipo de ambiente", ylab = "Contagem de aranhas", las = 1,frame=F)
dev.off()

jpeg("02.jpg")
stripchart(C ~ x,xlab = "Tipo de ambiente", ylab = "Contagem de aranhas", las = 1,frame=F,
           method="stack",vertical=T,pch=19,col=1,at=c(1.4,2.1),offset = 0.8)
legend("topleft",pch=19,legend="Um arbusto")
dev.off()


### Analise Bayesiana
# Definindo o modelo na linguagem Bugs
sink("Poisson.t.txt")
cat("
model {

# Priors
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)

# Likelihood
 for (i in 1:n) {
    C[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha + beta *x[i]

# Ajuste
    residuo[i] <- (C[i] - lambda[i]) / sqrt(lambda[i])

}

}
",fill=TRUE)
sink()

# Juntando os dados para mandar para o Openbugs
dados.bugs<- list(C = C, x = as.numeric(x)-1, n = length(x))

# Função geradora de valores iniciais
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1))}
inits()

# Parametros a estimar
params <- c("alpha", "beta","residuo")

# Configurações do MCMC
nc <- 3
ni <- 3000
nb <- 1000
nt <- 2

# Iniciando o Gibss Sampler
library(R2OpenBUGS)
out <- bugs(data=dados.bugs, inits=inits, parameters.to.save=params,model.file="Poisson.t.txt",
            n.thin=nt, n.chains=nc, n.burnin=nb,n.iter=ni)


### Checando convergencia e ajuste do modelo

jpeg("03.jpg")
hist(out$summary[,8],col="grey",main="Valores de Rhat")
dev.off()
which(out$summary[,8] > 1.1)

jpeg("04.jpg")
plot(out$mean$residuo, las = 1,frame=F,ylab="Resíduos",xlab="Amostras",xlim=c(0,20))
abline(h = 0)
dev.off()

jpeg("05.jpg")
hist(out$mean$residuo,xlab="Resíduos",freq = F)
abline(v = 0,lwd=2,col="blue",lty=3)
points(density(out$mean$residuo),type="l",lty=2,lwd=2,col="red")
dev.off()

shapiro.test(out$mean$residuo)

### Inferencia baseada no modelo que criamos
print(out, dig = 3)

#Função do Livro Doing Bayesian Analysis
HDIofMCMC <- function( sampleVec , credMass=0.95 ) {
    sortedPts = sort( sampleVec )
    ciIdxInc = floor( credMass * length( sortedPts ) )
    nCIs = length( sortedPts ) - ciIdxInc
    ciWidth = rep( 0 , nCIs )
    for ( i in 1:nCIs ) {
        ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
    }
    HDImin = sortedPts[ which.min( ciWidth ) ]
    HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
    HDIlim = c( HDImin , HDImax )
    return( HDIlim )
}

int95<-HDIofMCMC(out$sims.list$beta)
jpeg("06.jpg")
hist(out$sims.list$beta, col = "grey", las = 1, xlab = "Coeficiente para area rural", main = "",freq=F)
points(density(out$sims.list$beta),type="l",lty=2,lwd=3,col="red")
arrows(int95[1],0.25,int95[2],0.25,col="blue",angle =90,lwd=3,lty=3,code=3)
legend("topleft",legend="Intervalo de confiança de 95%",lwd=3,lty=3,col="blue",bty="n")
dev.off()

jpeg("07.jpg")
par(mfrow = c(2,1))
int95<-HDIofMCMC(exp(out$sims.list$alpha))
hist(exp(out$sims.list$alpha), main = "Área urbana",col = "grey", xlab = "", xlim = c(0,10), breaks = 20,freq=F)
points(density(exp(out$sims.list$alpha)),type="l",lty=2,lwd=3,col="red")
arrows(int95[1],0.1,int95[2],0.1,col="blue",angle =90,lwd=3,lty=3,code=3)
legend("topright",legend="Intervalo de confiança de 95%",lwd=3,lty=3,col="blue",bty="n")

int95<-HDIofMCMC(exp(out$sims.list$alpha + out$sims.list$beta))
hist(exp(out$sims.list$alpha + out$sims.list$beta), main = "Área rural",freq=F,
     col = "grey", xlab = "Número de aranhas esperado", xlim = c(0,10), breaks = 20)
points(density(exp(out$sims.list$alpha + out$sims.list$beta)),type="l",lty=2,lwd=3,col="red")
arrows(int95[1],0.1,int95[2],0.1,col="blue",angle =90,lwd=3,lty=3,code=3)
dev.off()

### Analise Frequentista
poisson.t <- glm(C ~ x, family = poisson)
summary(poisson.t)
anova(poisson.t, test = "Chisq")

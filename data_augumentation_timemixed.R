#http://recologia.com.br/2015/10/3159/
library(R2OpenBUGS)

##################################
## Gerando dados
##################################
set.seed(51)
n = 100
probabilidade_detecao = 0.25
tempo = 5
variacao_Tempo = rnorm(tempo,0,2)

yreal<- matrix(NA, nrow=n,ncol=tempo)

for (j in 1:tempo){
    p <- plogis(log(probabilidade_detecao / (1-probabilidade_detecao)) + variacao_Tempo[j])
    yreal[,j] <- rbinom(n = n, size = 1, prob = p)
}

detectado_pelo_menos_uma_vez <- apply(yreal, 1, max)
C <- sum(detectado_pelo_menos_uma_vez)

yobservado <- matrix(NA, nrow=n,ncol=tempo)
yobservado <- yreal[detectado_pelo_menos_uma_vez == 1,]

##################################
## Modelo com tempo
##################################

# Data Augumentation
nz <- 150
yaug <- rbind(yobservado, array(0, dim = c(nz, tempo)))

# Modelo na linguagem bugs
sink("modelo_tempo.txt")
cat("
    model {
        # Priors
        omega ~ dunif(0, 1)
        for (i in 1:tempo){
            p[i] ~ dnorm(med.tempo, tau.tempo)
        }
        med.tempo ~ dnorm(0,0.001) # Hyperprior da media tempo
        sigma.tempo ~ dunif(0, 5) # Hyperprior da variacao do tempo

        # Likelihood
        for (i in 1:amostras){
            z[i] ~ dbern(omega)
            for (j in 1:tempo){
                yaug[i,j] ~ dbern(p.eff[i,j])
                p.eff[i,j] <- z[i] * p[j]
                } #j
            } #i
        # Quantidade derivadas
        N <- sum(z[])
        tau.tempo <- 1 / (sigma.tempo * sigma.tempo)
    }
    ",fill = TRUE)
sink()

# Juntando os dados
bugs.data <- list(yaug = yaug, amostras = nrow(yaug), tempo = ncol(yaug))

# Valores iniciais
inits <- function() list(z = rep(1, nrow(yaug)), p = runif(tempo, 0, 1),med.tempo = rnorm(1,0,0.001),
                  sigma.tempo = runif(1,0, 5))

# Parametros para saida
params <- c("N", "p","med.tempo","sigma.tempo","omega")

# Configurações do MCMC
ni <- 5000
nt <- 2
nb <- 500
nc <- 3

#Rodando o modelo
out_tempo <- bugs(bugs.data, inits, params, "modelo_tempo.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Resultados
print(out_tempo, dig = 3)

#Figura1
jpeg("01.jpg")
hist(out_tempo$sims.list$N,breaks=seq(90,110,by=1), col = "gray", main = "", xlab = "Tamanho Populacional", las = 1,xlim=c(95,105))
abline(v = C, col = "black", lwd = 3)
abline(v = n, col = "red", lwd = 3)
legend("topright",c("Detectado","Real"),lty=1,lwd=3,col=c("black","red"),bty="n")
dev.off()
#####################################
## Efeito aleatorio com distribuiçao t
#####################################

# Modelo na linguagem bugs
sink("modelo_tempo.txt")
cat("
    model {
        # Priors
        omega ~ dunif(0, 1)

        med.tempo ~ dnorm(0,0.001) # Hyperprior da media tempo
        sigma.tempo ~ dunif(0, 5) # Hyperprior da variacao do tempo
        k.tempo ~ dunif(2, 30) # Hyperprior da variacao do tempo

        for (i in 1:tempo){
            p[i] ~ dt(med.tempo, tau.tempo, k.tempo)
        }


        # Likelihood
        for (i in 1:amostras){
            z[i] ~ dbern(omega)
            for (j in 1:tempo){
                yaug[i,j] ~ dbern(p.eff[i,j])
                p.eff[i,j] <- z[i] * p[j]
                } #j
            } #i
        # Quantidade derivadas
        N <- sum(z[])
        tau.tempo <- 1 / (sigma.tempo * sigma.tempo)
    }
    ",fill = TRUE)
sink()

# Valores iniciais
inits <- function() list(z = rep(1, nrow(yaug)), p = runif(tempo, 0, 1),med.tempo = rnorm(1,0,0.001),
                         sigma.tempo = runif(1,0, 5),k.tempo= runif(1,2, 30))
inits()
# Parametros para saida
params <- c("N","med.tempo","sigma.tempo","k.tempo","omega")

#Rodando o modelo
out_tempo <- bugs(bugs.data, inits, params, "modelo_tempo.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Resultados
print(out_tempo, dig = 3)

#Figura2
jpeg("02.jpg")
hist(out_tempo$sims.list$N,breaks=seq(90,110,by=1), col = "gray", main = "", xlab = "Tamanho Populacional", las = 1,xlim=c(95,105))
abline(v = C, col = "black", lwd = 3)
abline(v = n, col = "red", lwd = 3)
legend("topright",c("Detectado","Real"),lty=1,lwd=3,col=c("black","red"),bty="n")
dev.off()

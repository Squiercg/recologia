#http://recologia.com.br/2015/07/data-augmentation-para-dados-de-captura-e-recaptura-com-efeito-do-tempo/
library(R2OpenBUGS)

##################################
## Gerando dados
##################################
set.seed(51)
n = 100
probabilidade_detecao = 0.25
tempo = 5
variacao_Tempo = runif(tempo,-2,2)

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
            p[i] ~ dunif(0, 1)
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
        p.medio <- mean(p[])
    }
    ",fill = TRUE)
sink()

# Juntando os dados
bugs.data <- list(yaug = yaug, amostras = nrow(yaug), tempo = ncol(yaug))

# Valores iniciais
inits <- function() list(z = rep(1, nrow(yaug)), p = runif(tempo, 0, 1))

# Parametros para saida
params <- c("N", "p","p.medio", "omega")

# Configurações do MCMC
ni <- 2500
nt <- 2
nb <- 500
nc <- 3

#Rodando o modelo
out_tempo <- bugs(bugs.data, inits, params, "modelo_tempo.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Resultados
print(out_tempo, dig = 3)

#Figura1
hist(out_tempo$sims.list$N,nclass=30, col = "gray", main = "", xlab = "Tamanho Populacional", las = 1, xlim = c(80, 120))
abline(v = C, col = "black", lwd = 3)
abline(v = n, col = "red", lwd = 3)
legend("topright",c("Detectado","Real"),lty=1,lwd=3,col=c("black","red"),bty="n")

##################################
## Modelo sem variação de tempo
##################################

# Modelo na linguagem bugs
sink("modelo.txt")
cat("
    model {
        # Priors
        omega ~ dunif(0, 1)
        p ~ dunif(0, 1)
        # Likelihood
        for (i in 1:amostras){
            z[i] ~ dbern(omega)
            for (j in 1:tempo){
                yaug[i,j] ~ dbern(p.eff[i,j])
                p.eff[i,j] <- z[i] * p
                } #j
            } #i
        # Quantidade derivadas
        N <- sum(z[])
    }
    ",fill = TRUE)
sink()

# Valores iniciais
inits <- function() list(z = rep(1, nrow(yaug)), p = runif(1, 0, 1))

#
params <- c("N", "p", "omega")

#Rodando o modelo
out <- bugs(bugs.data, inits, params, "modelo.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Resultados
print(out, dig = 3)
print(out_tempo, dig = 3)

#Figura 2
hist(out_tempo$sims.list$N, nclass = 40, col = "darkgray", main = "", xlab = "Tamanho Populacional", las = 1, xlim = c(70, 150))
hist(out$sims.list$N, nclass = 40, col = "lightgray",add=T)
abline(v = mean(out_tempo$sims.list$N), col = "blue", lwd = 3)
abline(v = mean(out$sims.list$N), col = "red", lwd = 3)
legend("topright",c("Modelo com tempo","Modelo sem tempo"),lty=1,lwd=3,col=c("blue","red"),bty="n")

#
plogis(log(probabilidade_detecao/(1-probabilidade_detecao))+variacao_Tempo)
mean(plogis(log(probabilidade_detecao/(1-probabilidade_detecao))+variacao_Tempo))

#Figura 3
par(mfrow=c(2,1))
hist(out_tempo$sims.list$N, nclass = 40, col = "darkgray", main = "Modelo com variação no tempo",
     xlab = "Tamanho Populacional", las = 1, xlim = c(70, 150),ylim=c(0,600))
text(130,400,paste("Desvio padrão =",round(sd(out_tempo$sims.list$N),3)))
hist(out$sims.list$N, nclass = 40, col = "darkgray", main = "Modelo sem variação no tempo",
     xlab = "Tamanho Populacional", las = 1, xlim = c(70, 150),ylim=c(0,600))
text(130,400,paste("Desvio padrão =",round(sd(out$sims.list$N),3)))

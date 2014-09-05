#Gerando dados
N = 100
p = 0.5
T = 3
Real<-array(NA, dim = c(N, T))
for (j in 1:T){
    Real[,j] <- rbinom(n = N, size = 1, prob = p)
    }
#A visão da natureza
Real

detectado <- apply(Real, 1, max)
sum(detectado)

dados<-array(NA, dim = c(N, T))
dados <- Real[detectado == 1,]
#A nossa visão
dados

# Aumentando os dados com 150 individuos em potencial
nz <- 150
dadosaug <- rbind(dados, array(0, dim = c(nz,T)))

# Modelo na linguagem Bugs
sink("modelo.txt")
cat("
    model {
        # Priors
        omega ~ dunif(0, 1)
        p ~ dunif(0, 1)
        # Likelihood
        for (i in 1:M){
            z[i] ~ dbern(omega)
            # Indicador de inclusão
            for (j in 1:T){
                dadosaug[i,j] ~ dbern(p.eff[i,j])
                p.eff[i,j] <- z[i] * p # So pode ser detectado se z=1
                } #j
            } #i
        # Quantidades derivadas
        N <- sum(z[])
        }
    ",fill = TRUE)
sink()

# Juntando os dados para mandar para o bugs
bugs.data <- list(dadosaug = dadosaug, M = nrow(dadosaug), T = ncol(dadosaug))
# Função geradora de valores iniciais
inits <- function() list(z = rep(1, nrow(dadosaug)), p = runif(1, 0, 1))
# Parametros a serem monitorados
params <- c("N", "p", "omega")

# Configurando o MCMC
ni <- 2500
nt <- 2
nb <- 500
nc <- 3
# Chamando o openbugs do R
library(R2OpenBUGS)
out <- bugs(data=bugs.data,inits=inits,parameters=params,model="modelo.txt",
            n.chains = nc,n.thin = nt, n.iter = ni, n.burnin = nb)

 
# Distribuições posteriores
print(out, dig = 3)

str(out)
out$summary

#Figura 1
hist(out$sims.list$N,nclass=50,col="gray",main="",xlab ="Tamanho Populacional",las = 1, xlim = c(80, 150),
     ylab="Frequência")
abline(v = sum(detectado),lwd = 3)
abline(v = 100,lwd=3,col="red")
abline(v = out$summary[1,c(3,7)],lwd = 1,col="blue",lty=2)
legend("topright",legend=c("Detectados","Tamanho real","Intervalo de 95% estimado"),lwd=c(3,3,1),lty=c(1,1,2),
       col=c("black","red","blue"))


nz <- 5
dadosaug <- rbind(dados, array(0, dim = c(nz,T)))
bugs.data <- list(dadosaug = dadosaug, M = nrow(dadosaug), T = ncol(dadosaug))

out5 <- bugs(data=bugs.data,inits=inits,parameters=params,model="modelo.txt",
            n.chains = nc,n.thin = nt, n.iter = ni, n.burnin = nb)
print(out5, dig = 3)

#Figura 2
hist(out5$sims.list$N,nclass=50,col="gray",main="Dados aumentados em 5",xlab ="Tamanho Populacional",las = 1, xlim = c(80, 150),
     ylab="Frequência")
abline(v = sum(detectado),lwd = 3)
abline(v = 100,lwd=3,col="red")
abline(v = out5$summary[1,c(3,7)],lwd = 1,col="blue",lty=2)
legend("topright",legend=c("Detectados","Tamanho real","Intervalo de 95% estimado"),lwd=c(3,3,1),lty=c(1,1,2),
       col=c("black","red","blue"))


nz <- 150
dadosaug <- rbind(dados, array(0, dim = c(nz,T)))
bugs.data <- list(dadosaug = dadosaug, M = nrow(dadosaug), T = ncol(dadosaug))


out150 <-bugs(data=bugs.data,inits=inits,parameters=params,model="modelo.txt",
            n.chains = nc,n.thin = nt, n.iter = ni, n.burnin = nb)
print(out150, dig = 3)

#Figura 3
hist(out150$sims.list$N,nclass=50,col="gray",main="Dados aumentados em 150",xlab ="Tamanho Populacional",las = 1, xlim = c(80, 150),
     ylab="Frequência")
abline(v = sum(detectado),lwd = 3)
abline(v = 100,lwd=3,col="red")
abline(v = out150$summary[1,c(3,7)],lwd = 1,col="blue",lty=2)
legend("topright",legend=c("Detectados","Tamanho real","Intervalo de 95% estimado"),lwd=c(3,3,1),lty=c(1,1,2),
       col=c("black","red","blue"))


nz <- 1500
dadosaug <- rbind(dados, array(0, dim = c(nz,T)))
bugs.data <- list(dadosaug = dadosaug, M = nrow(dadosaug), T = ncol(dadosaug))

out1500 <- bugs(data=bugs.data,inits=inits,parameters=params,model="modelo.txt",
            n.chains = nc,n.thin = nt, n.iter = ni, n.burnin = nb)
print(out1500, dig = 3)

#Figura 4
hist(out1500$sims.list$N,nclass=50,col="gray",main="Dados aumentados em 1500",xlab ="Tamanho Populacional",las = 1, xlim = c(80, 150),
     ylab="Frequência")
abline(v = sum(detectado),lwd = 3)
abline(v = 100,lwd=3,col="red")
abline(v = out1500$summary[1,c(3,7)],lwd = 1,col="blue",lty=2)
legend("topright",legend=c("Detectados","Tamanho real","Intervalo de 95% estimado"),lwd=c(3,3,1),lty=c(1,1,2),
       col=c("black","red","blue"))


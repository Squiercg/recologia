#install.packages("unmarked")
library(unmarked)

data.fn<-function(R=30,J=3,K=10,psi1=0.4,range.p=c(0.2, 0.4),range.phi=c(0.6,0.8),range.gamma=c(0, 0.1)) {

    # Iniciando os vetores
    site <- 1:R # Locais
    year <- 1:K # Anos
    psi <- rep(NA, K) # Probabilidade de ocupação
    muZ <- z <- array(dim = c(R, K)) # Ocorrencia esperada e realizada
    y <- array(NA, dim = c(R, J, K)) # Historico de deteção

    # Determinando a ocupação inicial e seus parametros
    psi[1] <- psi1 # Probabilidade de ocupação inicial
    p <- runif(n = K, min = range.p[1], max = range.p[2])
    phi <- runif(n = K-1, min = range.phi[1], max = range.phi[2])
    gamma <- runif(n = K-1, min = range.gamma[1], max = range.gamma[2])

    # Estados latentes de ocupação
    # Primeiro ano
    z[,1] <- rbinom(R, 1, psi[1])
    # Estado de ocupação inicial
    # Proximos anos
    for(i in 1:R){
        # Loop sobre locais
        for(k in 2:K){
            # Loop sobre anos
            muZ[k] <- z[i, k-1]*phi[k-1] + (1-z[i, k-1])*gamma[k-1] # Prob for occ.
            z[i,k] <- rbinom(1, 1, muZ[k])
            }
        }
    # Grafico da ocupação realizada
    plot(year,apply(z,2,mean),type="l",xlab="Ano",ylab="Ocupação",col="blue",xlim=c(0,K+1),
         ylim=c(0,1),lwd=2,lty=1,frame.plot=FALSE,las = 1)
    lines(year,p,type="l",col="red",lwd=2,lty=2)
    # Gerando dados de detecção/não detecção
    for(i in 1:R){
        for(k in 1:K){
            prob <- z[i,k] * p[k]
            for(j in 1:J){
                y[i,j,k] <- rbinom(1, 1, prob)
                }
            }
        }
    # Computando ocupação anual
    for (k in 2:K){
        psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
        }
    # Plotando ocupação aparente
    psi.app <- apply(apply(y,c(1,3), max), 2, mean)
    lines(year, psi.app, type= "l", col = "black", lwd = 2)
    legend("topright",lty=c(1,2,1),lwd=2,col=c("blue","red","black"),
           legend = c("Ocupação Verdadeira","Detecçao"," Ocupação Observada"))


# Retornando dados
    return(list(R=R,J=J,K=K,psi=psi,psi.app=psi.app,z=z,phi=phi,gamma=gamma,p=p,y=y))
    }

set.seed(123)
jpeg("01.jpg")
data<-data.fn(R=30,J=3,K=10,psi1=0.6,range.p=c(0.1,0.9),range.phi=c(0.7,0.9),range.gamma=c(0.1,0.5))
str(data)
dev.off()

yy <- matrix(data$y, data$R, data$J* data$K)
str(yy)
head(yy)

year <- matrix(c('01','02','03','04','05','06','07','08','09','10'),
nrow(yy), data$K, byrow=TRUE)
head(year)

simUMF <- unmarkedMultFrame(y = yy,yearlySiteCovs = list(year = year),numPrimary=data$K)
summary(simUMF)

fm1 <- colext(psiformula = ~1,      #Ocupação no primeiro ano
              gammaformula = ~ 1,   #Colonização
              epsilonformula = ~ 1, #Extinção
              pformula = ~ 1,       #Detecção
              data = simUMF)

summary(fm1)
system.time(summary(fm2 <- colext(psiformula=~1, gammaformula=~year-1,epsilonformula=~1, pformula =~1, data = simUMF,
                                  control=list(trace=TRUE, REPORT=1, maxit = 500), se = TRUE)))

system.time(summary(fm3 <- colext(psiformula=~1, gammaformula=~1,epsilonformula=~year-1, pformula =~1, data = simUMF,
                                  control=list(trace=TRUE, REPORT=1, maxit = 500), se = TRUE)))

system.time(summary(fm4 <- colext(psiformula=~1, gammaformula=~1,epsilonformula=~1, pformula =~ year-1, data = simUMF,
                                  control=list(trace=TRUE, REPORT=1, maxit = 500), se = TRUE)))

system.time(summary(fm5 <- colext(psiformula=~1, gammaformula=~year-1,epsilonformula=~year-1, pformula =~1,
                                  data = simUMF,control=list(trace=TRUE, REPORT=1, maxit = 500), se = TRUE)))

system.time(summary(fm6 <- colext(psiformula=~1, gammaformula=~year-1,epsilonformula=~1, pformula =~year-1,
                                  data = simUMF,control=list(trace=TRUE, REPORT=1, maxit = 500), se = TRUE)))

system.time(summary(fm7 <- colext(psiformula=~1, gammaformula=~1,epsilonformula=~year-1, pformula =~year-1,
                                  data = simUMF,control=list(trace=TRUE, REPORT=1, maxit = 500), se = TRUE)))

system.time(summary(fm8 <- colext(psiformula=~1, gammaformula=~year-1,epsilonformula=~year-1, pformula =~year-1,
                                  data = simUMF,control=list(trace=TRUE, REPORT=1, maxit = 500), se = TRUE)))


modelos <- fitList(
'psi(.)gam(.)eps(.)p(.)' = fm1,
'psi(.)gam(Y)eps(.)p(.)' = fm2,
'psi(.)gam(.)eps(Y)p(.)' = fm3,
'psi(.)gam(.)eps(.)p(Y)' = fm4,
'psi(.)gam(Y)eps(Y)p(.)' = fm5,
'psi(.)gam(Y)eps(.)p(Y)' = fm6,
'psi(.)gam(.)eps(Y)p(Y)' = fm7,
'psi(.)gam(Y)eps(Y)p(Y)' = fm8)
modSel(modelos)

# Bundle data
bugs.data <- list(y = data$y, nsite = dim(data$y)[1], nrep = dim(data$y)[2],nyear = dim(data$y)[3])

data$y[,,1]

# Specify model in BUGS language
sink("Dynocc.txt")
cat("
    model {

        # Priors
        psi1 ~ dunif(0, 1)
        for (k in 1:(nyear-1)){
            phi[k] ~ dunif(0, 1)
            gamma[k] ~ dunif(0, 1)
            p[k] ~ dunif(0, 1)
        }
        p[nyear] ~ dunif(0, 1)

        # Modelo ecologico, definindo estado real
        for (i in 1:nsite){
            z[i,1] ~ dbern(psi1)
            for (k in 2:nyear){
                muZ[i,k]<- z[i,k-1]*phi[k-1] + (1-z[i,k-1])*gamma[k-1]
                z[i,k] ~ dbern(muZ[i,k])
            } #k
        } #i

        # Modelo referente as observações
        for (i in 1:nsite){
            for (j in 1:nrep){
                for (k in 1:nyear){
                    muy[i,j,k] <- z[i,k]*p[k]
                    y[i,j,k] ~ dbern(muy[i,j,k])
                } #k
            } #j
        } #i

# Parametros derivados
        psi[1] <- psi1
        n.occ[1]<-sum(z[1:nsite,1])
        for (k in 2:nyear){
            psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
            n.occ[k] <- sum(z[1:nsite,k])
            growthr[k-1] <- psi[k]/psi[k-1]
            turnover[k-1] <- (1 - psi[k-1]) * gamma[k-1]/psi[k]
        }
    }
    ",fill = TRUE)
sink()

# Valores iniciais da cadeia
zst <- apply(data$y, c(1, 3), max) # Observações ocorridas para iniciar Z
inits <- function(){ list(z = zst)}
# Parametros que queremos salvar
params <- c("psi", "phi", "gamma", "p", "n.occ", "growthr", "turnover")
# Definições do MCMC
ni <- 2500
nt <- 4
nb <- 500
nc <- 3
# Chamando Openbus (Pode demorar um pouco)
library(R2OpenBUGS)
out1 <- bugs(bugs.data,inits,params,"Dynocc.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb)

# Sumario dos resultados
print(out1, dig = 2)

str(data)
jpeg("02.jpg")
plot(1:data$K,data$psi,type="l",xlab="Ano",ylab="Probabilidade de ocupação",col="blue",xlim=c(0,data$K+1),ylim=c(0,1),
     lwd=2,lty=1, frame.plot=FALSE,las = 1)
lines(1:data$K, data$psi.app, type = "l", col = "black", lwd = 2)
points(1:data$K, out1$mean$psi, type = "l", col = "red", lwd = 2)
segments(1:data$K, out1$summary[1:data$K,3], 1:data$K,out1$summary[1:data$K,7], col = "red", lwd= 1)
dev.off()

nd <-data.frame(year=c('01','02','03','04','05','06','07','08','09'))
E.ext<- predict(fm8, type='ext', newdata=nd)
E.col<- predict(fm8, type='col', newdata=nd)
nd <-data.frame(year=c('01','02','03','04','05','06','07','08','09','10'))
E.det<- predict(fm8, type='det', newdata=nd)


op <- par(mfrow=c(3,1), mai=c(0.6, 0.6, 0.1, 0.1))

with(E.ext, {
    # Plot for extinction probability
    plot(1:9,Predicted,pch=1,xaxt='n',xlab='Year',ylab=expression(paste('Extinction probability(',epsilon,')')),
         ylim=c(0,1), col="green")
    axis(1, at=1:9, labels=nd$year[1:9])
    arrows(1:9, lower, 1:9, upper, code=3, angle=90, length=0.03, col="green")
    points((1:9)-0.1, 1-data$phi, col="red", lwd = 1, pch=16)
    points((1:9)+0.1, 1-out1$summary[11:19,1], col="blue", lwd = 1, pch=16)
    segments(((1:9)+0.1), (1-out1$summary[11:19,3]), ((1:9)+0.1), (1-out1$summary[11:19,7]), col = "blue", lwd = 1)
    legend(7, 1, c('Truth', 'ML', 'Posterior mean'), col=c("red", "green","blue"), pch=c(16, 1),cex=0.8)
})

with(E.col, {
    # Plot for colonization probability
    plot(1:9,Predicted,pch=1,xaxt='n',xlab='Year',ylab=expression(paste('Colonization probability ( ',gamma, ' )')),
         ylim=c(0,1), col="green")
    axis(1, at=1:9, labels=nd$year[1:9])
    arrows(1:9, lower, 1:9, upper, code=3, angle=90, length=0.03, col="green")
    points((1:9)-0.1, data$gamma, col="red", lwd = 1, pch=16)
    points((1:9)+0.1, out1$summary[20:28,1], col="blue", lwd = 1, pch=16)
    segments(((1:9)+0.1), out1$summary[20:28,3], ((1:9)+0.1),out1$summary[20:28,7], col = "blue", lwd = 1)
    legend(7, 1, c('Truth', 'ML', 'Posterior mean'), col=c("red", "green","blue"), pch=c(16, 1),cex=0.8)
})

with(E.det, {
    # Plot for detection probability: note 10 years
    plot(1:10,Predicted,pch=1,xaxt='n',xlab='Year',ylab=expression(paste('Detection probability ( ', p, ' )')),
         ylim=c(0,1), col="green")
    axis(1, at=1:10, labels=nd$year[1:10])
    arrows(1:10, lower, 1:10, upper, code=3, angle=90, length=0.03,col="green")
    points((1:10)-0.1, data$p, col="red", lwd = 1, pch=16)
    points((1:10)+0.1, out1$summary[29:38,1], col="blue", lwd = 1, pch=16)
    segments(((1:10)+0.1), out1$summary[29:38,3], ((1:10)+0.1),out1$summary[29:38,7], col = "blue", lwd = 1)
    legend(8, 1, c('Truth', 'ML', 'Posterior mean'), col=c("red", "green","blue"), pch=c(16, 1),cex=0.8)
})
par(op)

cbind(psi=data$psi, ML.estimates=projected(fm8)[2,],Bayesian.estimates=out1$summary[1:10, c(1:3,7)])




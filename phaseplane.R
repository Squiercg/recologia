library(deSolve)

#Modelo para crescimento continuo
lvcomp2 <- function(t, n, parms) {
  with(as.list(parms), {
    dn1dt <- r1*n[1]*(1-a11*n[1] - a12*n[2])
    dn2dt <- r2*n[2]*(1-a22*n[2] - a21*n[1])
    list(c(dn1dt, dn2dt))
  } )
}

parms <- c(r1=0.8,r2=0.5,a11=0.010,a21=0.005,a22=0.010,a12=0.005);
initialN<-c(1,1)
out<-ode(y=initialN, times=1:50, func=lvcomp2, parms=parms)

#Espécies ao longo do tempo
#figura 1
matplot(out[,1],out[,-1],type='l',xlab="Tempo",ylab="Tamanho populacional",
        frame.plot=F,main="Crescimento logístico contínuo para duas espécies")
legend("right",c(expression("Espécie 1 "*(alpha[21]==0.005)),
                expression("Espécie 2 "*(alpha[12]==0.005))),
       lty=1:2,bty='n',col=c("black","red"))

#Representação das abundancias das duas espécies num plano
#figura 2
plot(1, 1, type = "n", ylim = c(0, 100), xlim = c(0,100),frame=F,
     ylab = expression("N"[2]),xlab=expression("N"[1]))
points(out[,2],out[,3],type="b",cex=0.5,pch=19)


##########################################
#  #
##########################################
LVmod <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    Ingestion    <- rIng  * Prey * Predator
    GrowthPrey   <- rGrow * Prey * (1 - Prey/K)
    MortPredator <- rMort * Predator

    dPrey        <- GrowthPrey - Ingestion
    dPredator    <- Ingestion * assEff - MortPredator

    return(list(c(dPrey, dPredator)))
  })
}

pars  <- c(rIng   = 0.2,    # /day, rate of ingestion
           rGrow  = 1.0,    # /day, growth rate of prey
           rMort  = 0.2 ,   # /day, mortality rate of predator
           assEff = 0.5,    # -, assimilation efficiency
           K      = 10)     # mmol/m3, carrying capacity

yini  <- c(Prey = 1, Predator = 2)
times <- seq(0, 200, by = 1)
out   <- ode(yini, times, LVmod, pars)

## User specified plotting
#figura 3
matplot(out[ , 1], out[ , 2:3], type = "l", xlab = "Tempo", ylab = "Abundância",
        main = "Lotka-Volterra", lwd = 2,frame=F)
legend("topright", c("Presa", "Predador"), col = 1:2, lty = 1:2,bty="n")

#figura 4
plot(out[,2],out[,3],type="b",pch=19,frame=F,xlab="Presa",ylab="Predador")


#########################################

predadorpresa <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        dpresa <- presa*(alpha-beta*predador)
        dpredador <- -predador*(gamma-delta*presa)
        list(c(dpresa, dpredador))
  } )
}

pars <- c(alpha=2,beta=1.1,gamma=1,delta=0.4)
yini<-c(presa=5,predador=1)
times <- seq(0, 20, by = 1)
out   <- ode(yini, times, predadorpresa, pars)

#figura 5
plot(out[,2],out[,3],type="p",pch=19,xlab="Presa",ylab="Predador")

#figura 6
plot(out[,2],out[,3],type="b",pch=19,xlab="Presa",ylab="Predador")

#figura 7
plot(out[,2],out[,3],type="l",pch=19,lty=3,lwd=0.5,xlab="Presa",ylab="Predador")
text(out[,2],out[,3],0:20)

#figura 8
matplot(out[ , 1], out[ , 2:3], type = "l", xlab = "Tempo", ylab = "Abundância",
        main = "Predador-Presa", lwd = 2,frame=F)
legend("topright", c("Presa", "Predador"), col = 1:2, lty = 1:2,bty="n")






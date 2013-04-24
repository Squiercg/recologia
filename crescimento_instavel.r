###################################################
### Estabilidade vs Instabilidade
###################################################
rm(list=ls())

#Crescimento Logístico
dlogistic <- function(alpha=0.01, rd=1, N0=2, t=15) {
  N <- c(N0, numeric(t))
  for(i in 1:t) {
    N[i+1] <- N[i] + rd * N[i] * (1 - alpha * N[i])
  }
  return(N)
}

#Criamos um vetor com varia possibilidade de taxa de crescimento
#entre 1.3 e 2.8
rd.v <- seq(1.3, 2.8, by=.3)
t <- 15

#Usamos nossa função para ver como o tamanho populacional ao
#longo do tempo
Ns <- data.frame(sapply(rd.v, function(r) dlogistic(rd=r, t=t) ))

#Então fazemos um grafico:
matplot(0:t,Ns,type="l",lty=1,xlab="Tempo",ylab="Tamanho populacional",frame=F)
legend("bottomright",legend=rd.v,lty=1,col=1:length(rd.v),bty="n")

#Reorganizando os dados
tmp <- data.frame(rd=as.factor(rd.v), t(Ns))
Ns2 <- reshape(tmp, varying=list(2:ncol(tmp)), idvar="rd", v.names="N",direction="long")

#Refazendo o gráfico de forma diferente
library(lattice)
xyplot(N ~ time|rd, data=Ns2, type="l", layout=c(3,2,1), col=1,
       ylab="Tamanho Populacional",xlab="Tempo" )

#Pegando mais possibilidades de taxa de crescimento
#entre 1 e 3
num.rd <- 201
rd.s <- seq(1,3, length=num.rd)
t <- 400

#Usando a função logistica nessa varias possibilidades
tmp <- sapply(rd.s, function(r) dlogistic(rd=r, N0=99, t=t) ) 

#Reorganizando os dados
tmp.s <- stack( as.data.frame(tmp) )
names(tmp.s) <- c("N", "Old.Column.ID")
tmp.s$rd <- rep(rd.s, each=t+1)
tmp.s$time <- rep(0:t, num.rd)
N.bif <- subset(tmp.s, time > 0.5 * t )

#Graficos dos pontos de parada do tamanho populacional
plot(N ~ rd, data = N.bif, pch=".", xlab=quote("r"["d"]),frame=F)
abline(v=1.8,col="red",lty=2,lwd=2)
abline(v=2.4,col="green",lty=2,lwd=2)
abline(v=2.8,col="blue",lty=2,lwd=2)
legend("bottomleft",legend=c(1.8,2.4,2.8),lty=2,col=c("red","green","blue"),
       bty="n")

#Selecionando diferentes pontos iniciais da população
N.init <- c(97,98,99)
t <- 30
Ns <- sapply(N.init, function(n0) dlogistic(rd=2.7, N0=n0, t=t) )

matplot(0:t,Ns,type="l",lty=1,xlab="Tempo",ylab="Tamanho populacional",frame=F)
legend("bottomleft",legend=N.init,lty=1,col=1:length(N.init),bty="n",
       title="Tamanho Inicial")

#http://recologia.com.br/2015/06/epidemiologia-o-modelo-sir-com-transmissao-dependente-da-frequencia
##################################
## Grafo
##################################
library(igraph)

SIR_grafo<-graph.formula(S-+I,I-+R)
E(SIR_grafo)$label<-c(expression(paste(beta,"IS")),expression(paste(gamma,"I")))
loc<-matrix(c(0,1,1,1,2,1),ncol=2,byrow=T)

jpeg("01.jpg")
plot(SIR_grafo,layout=loc,rescale=F,xlim=c(0,2.2),ylim=c(0,2))
dev.off()

##################################
## Modelos
##################################
library(deSolve)

SIR <- function(t, y, p) {
    {
    S <- y[1]
    I <- y[2]
    R <- y[3]
    }
    with(as.list(p), {
                    dS.dt <- -B * I * S
                    dI.dt <- B * I * S - g * I
                    dR.dt <- g * I
                    return(list(c(dS.dt, dI.dt, dR.dt)))
                })
}

SIRf <- function(t, y, p) {
    {
    S <- y[1]
    I <- y[2]
    R <- y[3]
    N <- S + I + R
    }
    with(as.list(p), {
                    dS.dt <- -B * (I * S)/N
                    dI.dt <- B * (I * S)/N - g * I
                    dR.dt <- g * I
                    return(list(c(dS.dt, dI.dt, dR.dt)))
                })
}

R <- 0
S <- I <- 1000
Ss <- Is <- seq(1,S,length=11)
N <- S + I + R
betaD <- 0.1
betaF <- betaD * N

mat1 <- sapply(Is, function(i) {betaD*i*Ss})
mat2 <- sapply(Is, function(i) {betaF*i*Ss/(i+Ss+R)})

jpeg("02.jpg")
persp(mat1,theta=20,phi=15,r=10,zlim=c(0,betaD*S*I),xlab="I",ylab="S",
      main="Dependente da densidade",zlab="Taxa de Transmissão")
dev.off()

jpeg("03.jpg")
persp(mat2,theta=20,phi=15,r=10,zlim=c(0,betaF*S*I/N),xlab="I",ylab="S",
      main="Dependente da frequência",zlab="Taxa de Transmissão")
dev.off()
##################################
## Taxa Transmissão
##################################

S <- 4^(0:4)
I <- 1
parmsf <- c(B=1, g=0)
parmsd <- c(B=1/16, g=0)

Months <- seq(0,8,by=0.1)
outd <- sapply(S, function(s) {
                   out <- ode(c(s,I,R),Months,SIR,parmsd)
                   out[,3]/apply(out[,2:4],1,sum)
               })

outf <- sapply(S, function(s) {
                   out <- ode(c(s,I,R), Months, SIRf, parmsf)
                   out[,3]/apply(out[,2:4],1,sum)
               })

jpeg("04.jpg")
matplot(Months,outd,type="l",col=1,ylab = "Prevalência (I/N)",
        main="Dependente da densidade")
legend("bottomright",legend=S,lty=1:length(S),bty="n")
dev.off()

jpeg("05.jpg")
matplot(Months,outf,type="l",col=1,ylab = "Prevalência (I/N)",
        main="Dependente da frequência")
legend("bottomright",legend=S,lty=1:length(S),bty="n")
dev.off()
##################################
## Comparação da dinamica
##################################
N <- 1000
I <- 1
R <- 1
S <- N - I - R


#
jpeg("06.jpg")
meses <- seq(0, 0.8, by = 0.01)
parms <- c(B = 0.1, g = 4)
SIR.out <- data.frame(ode(c(S, I, R), meses, SIR, parms))
colnames(SIR.out)<-c("tempo","S","I","R")
matplot(meses, SIR.out[, -1], type = "l", lty = 1:3,frame=F,
        xlab="Tempo em Meses",ylab="N",lwd=2,col=2:4,main="Modelo dependente da densidade")
legend("right", c("Resistentes", "Infectados", "Susceptiveis"), lty = 3:1, col = c(4,3,2), bty = "n",lwd=2)
dev.off()

jpeg("07.jpg")
layout(matrix(1:4,ncol=2,nrow=2,byrow=T))
#
N <- 10000
I <- 1
R <- 1
S <- N - I - R
SIRf.out <- data.frame(ode(c(S, I, R), meses, SIRf, parms))
colnames(SIRf.out)<-c("tempo","S","I","R")
matplot(meses, SIRf.out[, -1], type = "l", lty = 1:3,frame=F,
        xlab="Tempo em Meses",ylab="N",lwd=2,col=2:4,main="N=10000")
#
N <- 1000
I <- 1
R <- 1
S <- N - I - R
SIRf.out <- data.frame(ode(c(S, I, R), meses, SIRf, parms))
colnames(SIRf.out)<-c("tempo","S","I","R")
matplot(meses, SIRf.out[, -1], type = "l", lty = 1:3,frame=F,
        xlab="Tempo em Meses",ylab="N",lwd=2,col=2:4,main="N=1000")
#
N <- 100
I <- 1
R <- 1
S <- N - I - R
SIRf.out <- data.frame(ode(c(S, I, R), meses, SIRf, parms))
colnames(SIRf.out)<-c("tempo","S","I","R")
matplot(meses, SIRf.out[, -1], type = "l", lty = 1:3,frame=F,
        xlab="Tempo em Meses",ylab="N",lwd=2,col=2:4,main="N=100")
#
N <- 10
I <- 1
R <- 1
S <- N - I - R
SIRf.out <- data.frame(ode(c(S, I, R), meses, SIRf, parms))
colnames(SIRf.out)<-c("tempo","S","I","R")
matplot(meses, SIRf.out[, -1], type = "l", lty = 1:3,frame=F,
        xlab="Tempo em Meses",ylab="N",lwd=2,col=2:4,main="N=10")
dev.off()

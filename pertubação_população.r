###################################################
### Valores de Retorno
###################################################
#install.packages("primer")
rm(list=ls())
library(primer)

#Crescimento Logístico
dlogistic <- function(alpha=0.01, rd=1, N0=2, t=15) {
  N <- c(N0, numeric(t))
  for(i in 1:t) {
    N[i+1] <- N[i] + rd * N[i] * (1 - alpha * N[i])
  }
  return(N)
}

#Usando a função com parametros default que definimos
Nts <- dlogistic(,t=40)

#Observando num grafico
t <- 40
a <- 0.01
plot(0:t, Nts,frame=F,xlab="Tempo",ylab="Tamanho Populacional",type="b",
     xlim=c(0,45),ylim=c(0,120),pch=19,cex=0.5)
abline(h=1/a,lty=3,col="red")


###################################################
# Estabilidade
###################################################
pop.growth.rate <- expression( r * N * (1 - alpha * N) )
r <- 1
alpha <- 0.01
N <- 0:120
plot(N, eval(pop.growth.rate), type="l",frame=F,
     ylab="Taxa de crescimento populacional (dN/dt)", xlab="N")
abline(h=0)
legend('topright', "r=1", lty=1,bty="n")
N <- c(0, 10, 50, 100, 115)
points(N, eval(pop.growth.rate), cex=1.5)
text(N, eval(pop.growth.rate), letters[1:5],adj=c(.5,2))
arrows(20,2, 80,2, length=.1, lwd=3)
arrows(122,-2,109,-2, length=.1, lwd=3)

###################################################
# Exemplo de pertubação
###################################################
pop1 <- dlogistic(alpha=0.01,N0=2,rd=0.5,t=20)
pop1 <- c(pop1,dlogistic(alpha=0.01,N0=(pop1[20]-20),rd=0.5,t=20))

pop2 <- dlogistic(alpha=0.01,N0=2,rd=1,t=20)
pop2 <- c(pop2,dlogistic(alpha=0.01,N0=(pop2[20]-20),rd=1,t=20))


#Observando num grafico
plot(1:length(pop1), pop1,frame=F,xlab="Tempo",ylab="Tamanho Populacional",type="b",
     xlim=c(0,45),ylim=c(0,120),pch=19,cex=0.5,col="green")
points(1:length(pop2), pop2,type="b",pch=19,cex=0.5,col="blue")
abline(h=1/0.01,lty=3,col="red")
arrows(22,50, 22,75, length=.1, lwd=3)
text(22,45,"Pertubação")
legend('topright', c("r=0.5", "r=1"),col=c("green","blue"),pch=19,lty=1,bty="n")

##########

pop1 <- dlogistic(alpha=0.01,N0=2,rd=0.5,t=20)
pop1 <- c(pop1,dlogistic(alpha=0.01,N0=(pop1[20]+20),rd=0.5,t=20))

pop2 <- dlogistic(alpha=0.01,N0=2,rd=1,t=20)
pop2 <- c(pop2,dlogistic(alpha=0.01,N0=(pop2[20]+20),rd=1,t=20))


#Observando num grafico
plot(1:length(pop1), pop1,frame=F,xlab="Tempo",ylab="Tamanho Populacional",type="b",
     xlim=c(0,45),ylim=c(0,120),pch=19,cex=0.5,col="green")
points(1:length(pop2), pop2,type="b",pch=19,cex=0.5,col="blue")
abline(h=1/0.01,lty=3,col="red")
arrows(22,70, 22,95, length=.1, lwd=3)
text(22,65,"Pertubação")
legend('topright', c("r=0.5", "r=1"),col=c("green","blue"),pch=19,lty=1,bty="n")

###################################################
# Retorno
###################################################
mi <- 99.4
ma <- 100.6
N <- seq(mi,ma, length=30)
plot(N, eval(pop.growth.rate), type="l",frame=F,
     ylab="Taxa de crescimento populacional (dN/dt)", xlab="N")
abline(h=0)
r <- .5
lines(N, eval(pop.growth.rate), lty=2); r <- 1
segments(c(100-.3, 100+.3), c(-.05,-.05), c(100-.3,100+.3), c(.05,.05))

text(99.7,-.15, quote("N*"-"x"))
text(100.3,.15, quote("N*"+"x"))
points(100, 0)
text(100, -.1, "N*")

legend('topright', c("r=1", "r=0.5"), lty=1:2,bty="n")
###################################################
# Pertubação
###################################################
par( mgp = c( 2, .75, 0 ) )
x <- 1
plot(c(-1,8), 1.2*c(-x,x), type='n',
     ylab="N", xlab='', axes=FALSE)
axis(2, at=c(-x,0,x), labels=c(expression("N*"-"x"),"N*", expression("N*"+"x")) )
abline(h=0)
t <- 1
points(c(0, t), c(1, x*exp(-1*t) ), pch=c(19,1))
curve(1*exp(-1*x), 0, .98, add=T, lty=3)
arrows(0,-x-.2, 7,-x-.2)
mtext("Tempo", side=1)
text(0, 0, "Pertubação", srt=90, adj=c(-.1,-.1) )
arrows(0,0,0,.8, length=.1)
text(3, .7, expression("Recuperação na taxa de"=="e"^"-r"))

points(c(0+2, t+2), c(-1, -x*exp(-1*t) ), pch=c(19,1))
x2 <- seq(0,.98, length=30)
y <- 0 - 1 * exp(-1*x2)
lines(x2+2, y, lty=3)
text(2, 0, "Pertubação", srt=90, adj=c(1.02,-.1) )
arrows(2, 0, 2,-.8, length=.1)
text(5, -.7, expression("Recuperação na taxa de"=="e"^"-r"))

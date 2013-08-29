#########################################################################
#Testando o atrator
#########################################################################
library(deSolve)

a <- matrix(c(0.01, 0.005, 0.005, 0.01), ncol = 2, byrow = TRUE)
a

### Ambas isoclines no mesmo grafico
plot((a[2,2]-a[1,2])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]),(a[1,1]-a[2,1])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]), cex=2,
     pch=1, col=1, axes=FALSE,ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
     ylab=expression("N"[2]), xlab=expression("N"[1]), pty='s')
curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], add=TRUE, lty=2)
curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1], add=T)
axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))
arrows(c(25,25,25), c(25,25,25), c(25,50,50), c(50,25,50),col=1, length=.12, lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(150,150,150), c(150,150,150), c(150,120, 120), c(120,150,120),col=1, length=.12,  lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(10, 10, 10), c(125,125,125), c(10,30,30), c(105,125,105),length=0.12, col=1,  lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(125,125,125), c(10, 10,10), c(125,105,105), c(30,10,30),length=0.12, col=1,  lwd=c(1,1,2), lty=c(2,1,1))

#Modelo para crescimento continuo
lvcomp2 <- function(t, n, parms) {
  with(as.list(parms), {
    dn1dt <- r1*n[1]*(1-a11*n[1] - a12*n[2])
    dn2dt <- r2*n[2]*(1-a22*n[2] - a21*n[1])
    list(c(dn1dt, dn2dt))
  } )
}


#Resolvendo continuamente com desolve
a
parms <- c(r1=0.8,r2=0.5,a11=0.010,a21=0.005,a22=0.010,a12=0.005);
initialN<-c(1,1)
out<-ode(y=initialN, times=1:50, func=lvcomp2, parms=parms)

#
matplot(out[,1],out[,-1],type='l',xlab="Tempo",ylab="Tamanho populacional",
        frame.plot=F,main="Crescimento logístico continuo para duas espécies")
legend("right",c(expression("Espécie 1 "*(alpha[21]==0.005)),
                expression("Espécie 2 "*(alpha[12]==0.005))),
       lty=1:2,bty='n',col=c("black","red"))

#Representação das abundancias das duas espécies num plano
plot(1, 1, type = "p", ylim = c(0, 200), xlim = c(0,200),frame=F,
     ylab = expression("N"[2]),xlab=expression("N"[1]),pch=19)
points(out[,2],out[,3],type="b",cex=0.5)

#Agora um caso onde as taxas de crescimento são definidas ao acaso,
#assim como a população inicial
plot(1, 1, type = "n", ylim = c(0, 200), xlim = c(0,200),frame=F,
     ylab = expression("N"[2]),xlab=expression("N"[1]),pch=19)

parms[1:2]<-runif(2,0,1)
initialN<-runif(2,1,200)
out<-ode(y=initialN, times=1:50, func=lvcomp2, parms=parms)
points(out[,2],out[,3],type="b",cex=0.5,col=1)

#O mesmo procedimento anterior varias vezes no mesmo grafico
plot(1, 1, type = "n", ylim = c(0, 200), xlim = c(0,200),frame=F,
     ylab = expression("N"[2]),xlab=expression("N"[1]),pch=19)

for(i in 1:10) {
    parms[1:2]<-runif(2,0,1)
    initialN<-runif(2,1,200)
    out<-ode(y=initialN, times=1:50, func=lvcomp2, parms=parms)
    points(out[,2],out[,3],type="b",cex=0.5,col=i)
    }

#O mesmo procedimento para o nosso grafico de isoclines
plot((a[2,2]-a[1,2])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]),(a[1,1]-a[2,1])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]), cex=2,
     pch=1, col=1, axes=FALSE,ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
     ylab=expression("N"[2]), xlab=expression("N"[1]), pty='s')
curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], add=TRUE, lty=2)
curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1], add=T)
axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))
arrows(c(25,25,25), c(25,25,25), c(25,50,50), c(50,25,50),col=1, length=.12, lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(150,150,150), c(150,150,150), c(150,120, 120), c(120,150,120),col=1, length=.12,  lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(10, 10, 10), c(125,125,125), c(10,30,30), c(105,125,105),length=0.12, col=1,  lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(125,125,125), c(10, 10,10), c(125,105,105), c(30,10,30),length=0.12, col=1,  lwd=c(1,1,2), lty=c(2,1,1))


for(i in 1:50) {
    parms[1:2]<-runif(2,0,1)
    initialN<-runif(2,1,200)
    out<-ode(y=initialN, times=1:50, func=lvcomp2, parms=parms)
    points(out[1,2],out[1,3],type="p",pch=19,col=i,cex=0.5)
    points(out[,2],out[,3],type="l",lwd=0.7,col=i)
    }

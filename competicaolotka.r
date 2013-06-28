#########################################################################
#Competição Lotka Volterra
#########################################################################
library(igraph)
library(deSolve)

#Figura 1
plot(graph.formula("Sp1" ),vertex.size=50,vertex.color=NA,vertex.label.color="black",vertex.label.cex=1.5,
     vertex.label=expression(r[1]))

sistema<-matrix(c("Sp1", "Sp1"),nc=2,byrow=TRUE)
sistema
rede.sistema<-graph.data.frame(sistema, directed=TRUE, vertices=NULL)
#Figura 2
plot(rede.sistema,edge.curved=seq(0.3, 0.3, length = ecount(rede.sistema)),
     edge.color="blue",edge.width=1,vertex.size=50,vertex.color=NA,vertex.label.color="black",vertex.label.cex=1.5,
     edge.label=c(expression(alpha[11])),edge.label.cex=2,edge.label.color="black",vertex.label=expression(r[1]))


sistema<-matrix(c("Sp1", "Sp1","Sp1", "Sp2","Sp2", "Sp2","Sp2", "Sp1"),nc=2,byrow=TRUE)
sistema
rede.sistema<-graph.data.frame(sistema, directed=T, vertices=NULL)
#Figura 3
plot(rede.sistema,layout=layout.spring,edge.curved=seq(0.3, 0.3, length = ecount(rede.sistema)),
     edge.color="blue",edge.width=1,vertex.size=50,vertex.color=NA,vertex.label.color="black",vertex.label.cex=1.5,
     edge.label=c(expression(alpha[11]),expression(alpha[12]),expression(alpha[22]),expression(alpha[21])),
     edge.label.cex=2,edge.label.color="black",vertex.label=c(expression(r[1]),expression(r[2])))

#Competição discreta:
dlvcomp2 <- function(N, alpha, rd=c(1,1)) {
  N1.t1 <- N[1] + rd[1] * N[1] * (1 - alpha[1,1]*N[1] - alpha[1,2]*N[2])
  N2.t1 <- N[2] + rd[2] * N[2] * (1 - alpha[2,1]*N[1] - alpha[2,2]*N[2])
  c(N1.t1, N2.t1)
}

#Parametros
alphs<-matrix(c(0.01,0.005,0.008,0.01),ncol=2,byrow=TRUE)
t<-20

N <- matrix(NA, nrow=t+1, ncol=2)
N[1,] <- c(10,10)

for(i in 1:t) {
  N[i+1,]<-dlvcomp2(N[i,],alphs)
}


#Gráfico
#Figura 4
matplot(0:t, N, type='l', col=1, ylim=c(0,110),frame.plot=F,xlab="Tempo",
        ylab="Tamanho da População",main="Crescimento logístico discreto para duas espécies")
abline(h=1/alphs[1,1], lty=3)
text(0, 1/alphs[1,1], "K", adj=c(0,0))
legend("right", c(expression("Espécie 1 "*(alpha[21]==0.008)),
                  expression("Espécie 2 "*(alpha[12]==0.005))),
       lty=1:2,bty='n')
dev.off()

#Crescimento continuo
lvcomp2 <- function(t, n, parms) {
  with(as.list(parms), {
    dn1dt <- r1*n[1]*(1-a11*n[1] - a12*n[2])
    dn2dt <- r2*n[2]*(1-a22*n[2] - a21*n[1])
    list(c(dn1dt, dn2dt))
  } )
}


#resolvendo continuamente com desolve

parms <- c(r1=1,r2=0.1,a11=0.2,a21=0.1,a22=0.02,a12=0.01);
initialN<-c(2,1)
out<-ode(y=initialN, times=1:100, func=lvcomp2, parms=parms)

#Figura 5
matplot(out[,1],out[,-1],type='l',xlab="Tempo",ylab="Tamanho populacional",
        frame.plot=F,main="Crescimento logístico continuo para duas espécies")
legend("left",c(expression("Espécie 1 "*(alpha[21]==0.1)),
                expression("Espécie 2 "*(alpha[12]==0.01))),
       lty=1:2,bty='n',col=c("black","red"))

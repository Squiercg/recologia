bumb<-function(P,eb,ew,ef,ee,tb,tw,tf,te,n,alpha,beta) {
    netE<-vector()
    sumi=0
    for(i in 1:n) {
        sumi=sumi+(alpha+beta*i)
        net=(P*sumi-eb-P*ew*(i-1)-P*i*ef-ee*(1-P))/(tb+P*tw*(i-1)+P*i*tf+te*(1-P))
        netE[i]<-net
    }
    return(netE)
}

#Calorias
i<-seq(0,10,1)
calories<-20-1.7*i

#figura 1
plot(i,calories,type="l",frame=F,ylab="Número de calorias da flor",xlab="Sequência de flores em um ramo da planta",
     main="Relação linear entre posição da flor e conteúdo calorico da flor",xlim=c(0,10),ylim=c(0,20))

#figura 2
saida<-bumb(P=0.6,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
plot(1:10,saida,type="l",frame=F,xlim=c(1,10),ylab="Taxa de ingestão de energia líquida em cal/seg",
     xlab="Última flor visitou antes a mosca deixa a planta",ylim=c(0,1),col=1,lty=2,lwd=2,xaxt="n")
axis(1,at=c(1:10))
title("eb=0.1, ew=0.03, ef=0.02, ee=0.01, tb=5, tw=0.05, tf=15, te=9, n=10, alpha=20, beta=-1.7, P=0.4",font.main=1,cex.main=0.9)
abline(h=max(saida),v=which(saida==max(saida)),lty=3,cex=2)
text(which(saida==max(saida))+0.5,max(saida)+0.025,"Forrageamento ótimo")

#Aumentando a chance da flor ter sido exaurida antes da visita
saida<-bumb(P=0.6,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
plot(1:10,saida,type="l",frame=F,xlim=c(1,10),ylab="Taxa de ingestão de energia líquida em cal/seg",
     xlab="Última flor visitou antes a mosca deixa a planta",ylim=c(0,1),col=1,lty=2,lwd=2,xaxt="n")
axis(1,at=c(1:10))
title("eb=0.1, ew=0.03, ef=0.02, ee=0.01, tb=5, tw=0.05, tf=15, te=9, n=10, alpha=20, beta=-1.7",
      font.main=1,cex.main=0.9)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
points(1:10,saida,type="l",col=2,lty=2,lwd=2)
saida<-bumb(P=0.2,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
points(1:10,saida,type="l",col=3,lty=2,lwd=2)
saida<-bumb(P=0.1,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
points(1:10,saida,type="l",col=4,lty=2,lwd=2)
legend("topright",legend=c("P=0.6","P=0.4","P=0.2","P=0.1"),lty=2,col=1:4,lwd=2,bty="n")

#Aumentando distancia entre flores
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
plot(1:10,saida,type="l",frame=F,xlim=c(1,10),ylab="Taxa de ingestão de energia líquida em cal/seg",
     xlab="Última flor visitou antes a mosca deixa a planta",ylim=c(0,1),col=1,lty=2,lwd=2,xaxt="n")
axis(1,at=c(1:10))
title("eb=0.1, ew=0.03, ef=0.02, ee=0.01, P=0.4, tw=0.05, tf=15, te=9, n=10, alpha=20, beta=-1.7",
      font.main=1,cex.main=0.9)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=7,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
points(1:10,saida,type="l",col=2,lty=2,lwd=2)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=9,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
points(1:10,saida,type="l",col=3,lty=2,lwd=2)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=11,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
points(1:10,saida,type="l",col=4,lty=2,lwd=2)
legend("topright",legend=c("tb=5","tb=7","tb=9","tb=11"),lty=2,col=1:4,lwd=2,bty="n")

#Flores maturam num intervalo de tempo menor
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
plot(1:10,saida,type="l",frame=F,xlim=c(1,10),ylab="Taxa de ingestão de energia líquida em cal/seg",
     xlab="Última flor visitou antes a mosca deixa a planta",ylim=c(0,1),col=1,lty=2,lwd=2,xaxt="n")
axis(1,at=c(1:10))
title("eb=0.1, ew=0.03, ef=0.02, ee=0.01, P=0.4, tw=0.05, tf=15, te=9, n=10, alpha=20, tb=5",
      font.main=1,cex.main=0.9)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.5)
points(1:10,saida,type="l",col=2,lty=2,lwd=2)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.3)
points(1:10,saida,type="l",col=3,lty=2,lwd=2)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.0)
points(1:10,saida,type="l",col=4,lty=2,lwd=2)
legend("topright",legend=c("beta=-1.7","beta=-1.5","beta=-1.3","beta=-1.0"),lty=2,col=1:4,lwd=2,bty="n")

#Flores maturam num intervalo de tempo menor
jpeg("06.jpg")
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)
plot(1:10,saida,type="l",frame=F,xlim=c(1,10),ylab="Taxa de ingestão de energia líquida em cal/seg",
     xlab="Última flor visitou antes a mosca deixa a planta",ylim=c(0,1),col=1,lty=2,lwd=2,xaxt="n")
axis(1,at=c(1:10))
title("eb=0.1, ew=0.03, ef=0.02, ee=0.01, P=0.4, tw=0.05, tf=15, te=9, n=10, alpha=20, tb=5",
      font.main=1,cex.main=0.9)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.9)
points(1:10,saida,type="l",col=2,lty=2,lwd=2)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-2.1)
points(1:10,saida,type="l",col=3,lty=2,lwd=2)
saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-2.3)
points(1:10,saida,type="l",col=4,lty=2,lwd=2)
legend("topright",legend=c("beta=-1.7","beta=-1.9","beta=-2.1","beta=-2.3"),lty=2,col=1:4,lwd=2,bty="n")


#Experimento
saidaoriginal<-bumb(P=0.6,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,alpha=20,beta=-1.7)


bumb<-function(P,eb,ew,ef,ee,tb,tw,tf,te,n,media,desvio) {
    netE<-vector()
    sumi=0
    for(i in 1:n) {
        sumi=sumi+rnorm(1,media,desvio)
        net=(P*sumi-eb-P*ew*(i-1)-P*i*ef-ee*(1-P))/(tb+P*tw*(i-1)+P*i*tf+te*(1-P))
        netE[i]<-net
    }
    return(netE)
}

#figura 7
plot(1:10,saida,type="n",frame=F,xlim=c(1,10),ylab="Taxa de ingestão de energia líquida em cal/seg",
     xlab="Última flor visitou antes a mosca deixa a planta",ylim=c(0,1),col=1,lty=2,lwd=2,xaxt="n")
axis(1,at=c(1:10))
title("eb=0.1, ew=0.03, ef=0.02, ee=0.01, P=0.4, tw=0.05, tf=15, te=9, n=10, tb=5",font.main=1,cex.main=0.9)

matriz<-matrix(NA,ncol=10,nrow=250)
for(i in 1:250) {
    saida<-bumb(P=0.4,eb=0.1,ew=0.03,ef=0.02,ee=0.01,tb=5,tw=0.05,tf=15,te=9,n=10,media=mean(calories),desvio=sd(calories))
    matriz[i,]<-saida
    points(1:10,saida,type="l",col="gray",lty=3,lwd=0.7)
}

points(1:10,colSums(matriz)/250,type="l",col="red",lty=3,lwd=3)
points(1:10,saidaoriginal,type="l",col="black",lty=3,lwd=3)
legend("bottomright",legend=c("Conteúdo de néctar conhecido","Conteúdo de néctar ao acaso"),lwd=3,lty=3,
       col=c("black","red"))


##Função para calcular o valor esperado
esperanca<-function(vetor){
    esperanca<-0
    for(i in 1:length(vetor)){
        esperanca<-esperanca+ i * (vetor[i]/sum(vetor))
    }
    return(esperanca)
}
 
##Figura 1
par(mfrow=c(2,2))
valores<-c(4,4,0,0,0,4,4)
plot(1:length(valores),valores,ylim=c(0,10),ylab="Probabilidade",yaxt="n",frame=F,xaxt="n",xlab="",pch=19,cex=2)
for(i in 1:length(valores)){
    lines(c(i,i),c(0,valores[i]),lwd=4)
}
axis(2,at=seq(0,10,2),labels=round(seq(0,10,2)/16,2),las=1)
axis(1,at=1:length(valores),labels=NA)
valor_esperado <- esperanca(valores)
arrows(valor_esperado, -2,valor_esperado,-1, xpd = TRUE,lwd=10)
legend("top",legend="A",bty="n",cex =2)
 
valores<-c(4,0,0,0,4,4,4)
plot(1:length(valores),valores,ylim=c(0,10),ylab="Probabilidade",yaxt="n",frame=F,xaxt="n",xlab="",pch=19,cex=2)
for(i in 1:length(valores)){
    lines(c(i,i),c(0,valores[i]),lwd=4)
}
axis(2,at=seq(0,10,2),labels=round(seq(0,10,2)/16,2),las=1)
axis(1,at=1:length(valores),labels=NA)
valor_esperado <- esperanca(valores)
arrows(valor_esperado, -2,valor_esperado,-1, xpd = TRUE,lwd=10)
legend("top",legend="B",bty="n",cex =2)
 
valores<-c(9,0,0,0,4,2,1)
plot(1:length(valores),valores,ylim=c(0,10),ylab="Probabilidade",yaxt="n",frame=F,xaxt="n",xlab="",pch=19,cex=2)
for(i in 1:length(valores)){
    lines(c(i,i),c(0,valores[i]),lwd=4)
}
axis(2,at=seq(0,10,2),labels=round(seq(0,10,2)/16,2),las=1)
axis(1,at=1:length(valores),labels=NA)
valor_esperado <- esperanca(valores)
arrows(valor_esperado, -2,valor_esperado,-1, xpd = TRUE,lwd=10)
legend("top",legend="C",bty="n",cex =2)
 
valores<-c(1,0,0,0,1,5,10)
plot(1:length(valores),valores,ylim=c(0,10),ylab="Probabilidade",yaxt="n",frame=F,xaxt="n",xlab="",pch=19,cex=2)
for(i in 1:length(valores)){
    lines(c(i,i),c(0,valores[i]),lwd=4)
}
axis(2,at=seq(0,10,2),labels=round(seq(0,10,2)/16,2),las=1)
axis(1,at=1:length(valores),labels=NA)
valor_esperado <- esperanca(valores)
arrows(valor_esperado, -2,valor_esperado,-1, xpd = TRUE,lwd=10)
legend("top",legend="D",bty="n",cex =2)
 
##Figura 2
valores<-c(0.5,0.5)
plot(1:length(valores),valores,ylim=c(0,1),ylab="Probabilidade",yaxt="n",frame=F,xaxt="n",xlab="",pch=19,cex=2,main="Moeda {0,1}")
for(i in 1:length(valores)){
    lines(c(i,i),c(0,valores[i]),lwd=4)
}
axis(2,at=seq(0,1,0.1),las=1)
axis(1,at=1:length(valores),labels=NA)
valor_esperado <- esperanca(valores)
arrows(valor_esperado, -2,valor_esperado,-0.05, xpd = TRUE,lwd=10)
 
##Figura 3
curve(dunif(x),-0.5,1.5,frame=F,xlab="",ylab="Densidade",main="Distribuição uniforme")
 
##Valor esperado da distribuição uniforme, quantile de 50%
qunif(0.5,min=0,max=1)

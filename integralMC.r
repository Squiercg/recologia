set.seed(171)
t <- seq(0.2*pi,length=1000)
coords <- t(rbind(0+sin(t)*1,0+cos(t)*1))

#desenhando um circulo
#jpeg("01.jpg")
plot(0,0,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),frame=F,xlab="Eixo x",ylab="Eixo y")
points(coords,cex=0.1)
abline(h=c(0),v=c(0),lty=3,lwd=0.5)
lines(c(0.01,0.99),c(0.01,0.01),lwd=2,lty=2,col="red")
text(0.5,0.1,"Raio=1")
#dev.off()

#Desenhando um quadrado sobre o circulo
#jpeg("02.jpg")
plot(0,0,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),frame=F,xlab="Eixo x",ylab="Eixo y")
points(coords,cex=0.1)
abline(h=c(0),v=c(0),lty=3,lwd=0.5)
lines(c(-1,-1,-1,1,1,1,1,-1),c(-1,1,1,1,1,-1,-1,-1),lwd=2,col="darkgray")
lines(c(-1.01,1.01),c(-1.01,-1.01),lwd=2,lty=2,col="red")
text(0,-1.08,"Lado=2")
#dev.off()


#Area do circulo
pi*1^2

#Area do Quadrado
2^2

#proporção
(pi*1^2) / (2^2)

#Jogando um ponto ao acaso dentro do quadrado
#jpeg("03.jpg")
plot(0,0,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),frame=F,xlab="Eixo x",ylab="Eixo y")
points(coords,cex=0.1)
abline(h=c(0),v=c(0),lty=3,lwd=0.5)
lines(c(-1,-1,-1,1,1,1,1,-1),c(-1,1,1,1,1,-1,-1,-1),lwd=2,col="darkgray")

x.pos <- runif(1, min=-1, max=1)
y.pos <- runif(1, min=-1, max=1)
local.pos <- ifelse(x.pos^2 + y.pos^2 <= 1, TRUE, FALSE)
points(x.pos,y.pos,pch=19,cex=0.5,col=ifelse(local.pos,"red","blue"))
#dev.off()

#Jogando 500 pontos ao acaso dentro do quadrado
#jpeg("04.jpg")
plot(0,0,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),frame=F,xlab="Eixo x",ylab="Eixo y")
points(coords,cex=0.1)
abline(h=c(0),v=c(0),lty=3,lwd=0.5)
lines(c(-1,-1,-1,1,1,1,1,-1),c(-1,1,1,1,1,-1,-1,-1),lwd=2,col="darkgray")

for(i in 1:500) {
x.pos <- runif(1, min=-1, max=1)
y.pos <- runif(1, min=-1, max=1)
local.pos <- ifelse(x.pos^2 + y.pos^2 <= 1, TRUE, FALSE)
points(x.pos,y.pos,pch=19,cex=0.5,col=ifelse(local.pos,"red","blue"))
}
#dev.off()

#Fazendo essa simulação 5 mil vezes e olhando a proporção de pontos dentro do circulo
n<-5000
x.pos <- runif(n, min=-1, max=1)
y.pos <- runif(n, min=-1, max=1)
local.pos <- ifelse(x.pos^2 + y.pos^2 <= 1, TRUE, FALSE)
dentro <- length(which(local.pos == TRUE))

#Propoção
dentro/n
#Exato
(pi*1^2) / (2^2)

#Multiplicando pera area do quadrado para termos o valor de pi
4*(dentro/n)
#Valor de pi
pi

#Guardando as estimativas num vetor
estimativa<-vector()

for(i in 1:5000) {
    estimativa[i] <- length(which(local.pos[1:i] == TRUE)) / (i)
}


#Grafico animado

passos<-round(seq(1,5000,length=200))
estimativa<-estimativa*4

for(i in passos) {
jpeg(sprintf("pi%05d.jpg",i), width = 300, height = 300)
plot(c(1:5000),estimativa[1:5000]*4,type="n",ylim=c(min(estimativa),max(estimativa)),xlab="Iterações",
     ylab="Valor de Pi",main=paste("Iteração",i),frame=F)
points(c(1:i),estimativa[1:i],type="l",col="blue",lwd=2)
abline(h=pi,lty=3,col="red")
dev.off()
}
system("convert pi*.jpg -delay 10 pi.gif")
system("rm pi*.jpg")

##http://recologia.com.br/2016/07/determinando-a-distribuicao-espacial-a-partir-da-distribuicao-do-vizinho-mais-proximo/
set.seed(123)

##Distância entre pontos
distancia <- function(x1,y1,x2,y2){
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

##Calcula distancias do vizinho mais próximo
calcula_r<- function(x,y){
    r <- vector()
    nn <- vector()
    d <- vector()
    for (i in 1:100) {
        d <- distancia(x[i],y[i],x,y)
        r[i] <- min(d[-i])
    }
    return(r)
}


##Simular média do vizinho mais próximo
n<-1000
dist_vmp<-vector()

for(i in 1:n){
    r<-calcula_r(runif(100),runif(100))
    dist_vmp[i]<-mean(r)
}

##Resultado da simulação
jpeg("01.jpg")
hist(dist_vmp,main="Distribuição de médias do vizinho mais próximo")
dev.off()

##Avaliando alguns casos:
##Caso aléatorio
x<-runif(100)
y<-runif(100)

jpeg("02.jpg")
plot(x,y)
dev.off()

r_ale<-mean(calcula_r(x,y))
jpeg("03.jpg")
hist(dist_vmp)
abline(v=r_ale,lwd=2,col="blue",lty=2)
legend("topright",lwd=2,col="blue",lty=2,legend="Aleatório",bty="n")
dev.off()

##Caso Agregado
x<-c(rnorm(50,0.25,0.05),rnorm(50,0.6,0.1))
y<-c(rnorm(50,0.25,0.1),rnorm(50,0.7,0.05))

jpeg("04.jpg")
plot(x,y)
dev.off()

r_agre<-mean(calcula_r(x,y))
jpeg("05.jpg")
hist(dist_vmp,xlim=c(min(dist_vmp,r_agre),max(dist_vmp)))
abline(v=r_agre,lwd=2,col="red",lty=2)
legend("top",lwd=2,col="red",lty=2,legend="Agregado",bty="n")
dev.off()

##Caso Uniforme
x<-rep(seq(0.01,0.99,length=10),10)+runif(100,-0.002,0.05)
y<-rep(seq(0.01,0.99,length=10),each=10)+runif(100,-0.002,0.05)

jpeg("07.jpg")
plot(x,y)
dev.off()

r_uni<-mean(calcula_r(x,y))
jpeg("08.jpg")
hist(dist_vmp,xlim=c(min(dist_vmp),max(dist_vmp,r_uni)))
abline(v=r_uni,lwd=2,col="green",lty=2)
legend("top",lwd=2,col="green",lty=2,legend="Uniforme",bty="n")
dev.off()

jpeg("09.jpg")
hist(dist_vmp,xlim=c(min(dist_vmp,r_agre),max(dist_vmp,r_uni)))
abline(v=r_ale,lwd=2,col="blue",lty=2)
abline(v=r_agre,lwd=2,col="red",lty=2)
abline(v=r_uni,lwd=2,col="green",lty=2)
legend("topright",lwd=2,col=c("blue","red","green"),lty=2,legend=c("Aleatório","Agregado","Uniforme"),bty="n")
dev.off()

##Observado para uma distribuição ao acaso
mean(dist_vmp)

##Valor Predito
E_r<-sqrt(100/10000)/2
E_r

#Razão observado por esperado
r_ale/E_r
r_agre/E_r
r_uni/E_r


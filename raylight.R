##http://recologia.com.br/2018/04/teste-de-hipoteses-de-rayleight/

##install.packages("circular")
library(circular)

##Gerando 100 observações
dados <- rvonmises(n=100, mu=circular(pi), kappa=3) 


##Histograma
hist(as.vector(dados),main="Dados circulares",col="gray",xlim=c(0,2*pi))

##Comparando parâmetros básicos com distribuição normal
summary(as.vector(dados))
summary(rnorm(100,pi,3))


##Gráfico Circular
plot(dados, stack=TRUE, bins=150, shrink=1.5)


##Entendendo kappa
par(mfrow=c(2,2))
for(i in c(0.1,1,10,100)) {
    plot(rvonmises(n=100, mu=circular(pi), kappa=i) , stack=TRUE, bins=150, shrink=1.5,main=paste("kappa=",i))
}


##Entendendo mu
par(mfrow=c(2,2))
for(i in c(0,pi/2,3*pi/2,2*pi)) {
    plot(rvonmises(n=100, mu=circular(i), kappa=5) , stack=TRUE, bins=150, shrink=1.5,main=paste("mu=",round(i,3)))
}


#####################################
## Rayleight
#####################################
##Sem direção
dados <- rvonmises(n=100, mu=circular(pi), kappa=0)
plot.circular(dados, stack=TRUE,bins=150,axes = F)
axis.circular(at=circular(c(0,pi/2,pi,3*pi/2)),labels=c("Leste","Norte","Oeste","Sul"),units=c("radians"),rotation="clock")
rayleigh.test(dados)

##Com alguma direção
dados <- rvonmises(n=100, mu=circular(pi), kappa=1)
plot.circular(dados, stack=TRUE,bins=150,axes = F)
axis.circular(at=circular(c(0,pi/2,pi,3*pi/2)),labels=c("Leste","Norte","Oeste","Sul"),units=c("radians"),rotation="clock")
rayleigh.test(dados)


##Não está indo pro lado certo
dados <- rvonmises(n=100, mu=circular(pi), kappa=10)
plot.circular(dados, stack=TRUE,bins=150,axes = F)
axis.circular(at=circular(c(0,pi/2,pi,3*pi/2)),labels=c("Leste","Norte","Oeste","Sul"),units=c("radians"),rotation="clock")
rayleigh.test(dados,mu=circular(pi/2))


##Está indo pro lado certo
dados <- rvonmises(n=100, mu=circular(pi/2), kappa=10)
plot.circular(dados, stack=TRUE,bins=150,axes = F)
axis.circular(at=circular(c(0,pi/2,pi,3*pi/2)),labels=c("Leste","Norte","Oeste","Sul"),units=c("radians"),rotation="clock")
rayleigh.test(dados,mu=circular(pi/2))


n <- 10^4
U <- runif(n)
X_transformado<- -log(U) ##Transformando valores de uma distribuição normal

X_r<- rexp(n) ##Valores aleatorios exponenciais do R

##Plot
par(mfrow=c(1,2))
hist(X_transformado,freq=F,main="Valores a partir da Transformação",
     breaks=seq(0,10,0.25),xlim=c(-0.5,12),ylim=c(0,1),col="gray")
curve(dexp(x),-0.5,10,add=T,col="red",lwd=2)
legend("top",col="red",lwd=2,lty=1,legend="PDF esperado",bty="n")
hist(X_r,freq=F,main="Valores da Função rexp",
     breaks=seq(0,10,0.25),xlim=c(-0.5,12),ylim=c(0,1),col="gray")
curve(dexp(x),-0.5,10,add=T,col="red",lwd=2)
legend("top",col="red",lwd=2,lty=1,legend="PDF esperado",bty="n")

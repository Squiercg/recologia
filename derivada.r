função <- function(x) { return(x^2) }

função(1)
função(2)
função(3)
função(-2)
função(1.5)

#figura 1
curve(função(x),-5,5,ylab="função(x)",xlab="Valor de x",frame=F)

dfunção <- deriv(~ x^2, "x")

dfunção

x <- 1
attr(eval(dfunção),"gradient")

x <- -2:2
attr(eval(dfunção),"gradient")

#figura 2
x <- 2
curve(função(x),-5,5,ylab="função(x)",xlab="Valor de x",frame=F)
curve(attr(eval(dfunção),"gradient")*(dx-x)+função(x),-5,5,ylab="função(x)",xlab="Valor de x",add=T,
      xname = "dx",lty=2,lwd=2,col="red")

#figura 3
curve(função(x),-5,5,ylab="função(x)",xlab="Valor de x",frame=F)
x <- -1
curve(attr(eval(dfunção),"gradient")*(dx-x)+função(x),-5,5,ylab="função(x)",xlab="Valor de x",add=T,
      xname = "dx",lty=2,lwd=2,col=2)
x <- 0
curve(attr(eval(dfunção),"gradient")*(dx-x)+função(x),-5,5,ylab="função(x)",xlab="Valor de x",add=T,
      xname = "dx",lty=2,lwd=2,col=3)
x <- 1
curve(attr(eval(dfunção),"gradient")*(dx-x)+função(x),-5,5,ylab="função(x)",xlab="Valor de x",add=T,
      xname = "dx",lty=2,lwd=2,col=4)
x <- 2
curve(attr(eval(dfunção),"gradient")*(dx-x)+função(x),-5,5,ylab="função(x)",xlab="Valor de x",add=T,
      xname = "dx",lty=2,lwd=2,col=5)
legend("center",lty=2,lwd=2,col=2:5,legend=paste("Derivada de", -1:2),bty="n")


#Nt=(No*exp(r*t))/(1+alpha*No(exp(r*t)-1))
#Vamos fixar o r=0.5 ,  No=2 e alpha = 0.01 (k=100)

flogistico <- function(t) { return((2*exp(0.5*t))/(1+0.001*2*(exp(0.5*t)-1))) }

flogistico(0)
flogistico(1)
flogistico(2)
flogistico(3)

#figura 4
curve((2*exp(0.5*t))/(1+0.001*2*(exp(0.5*t)-1)),0,30,ylab="Tamanho da população",xlab="Tempo",xname="t",frame=F)

dlogistico <- deriv(~ (2*exp(0.5*t))/(1+0.001*2*(exp(0.5*t)-1)), "t")

t=0
attr(eval(dlogistico),"gradient")

t=5
attr(eval(dlogistico),"gradient")

t=30
attr(eval(dlogistico),"gradient")

#figura 5
curve((2*exp(0.5*t))/(1+0.001*2*(exp(0.5*t)-1)),0,30,ylab="Tamanho da população",xlab="Tempo",xname="t",frame=F)
t <- 19
curve(attr(eval(dlogistico),"gradient")*(dl-t)+flogistico(t),0,30,add=T,xname = "dl",lty=2,lwd=2,col="red")

#figura 6
curve((2*exp(0.5*t))/(1+0.001*2*(exp(0.5*t)-1)),0,30,ylab="Tamanho da população",xlab="Tempo",xname="t",frame=F)
t <- 19
curve(attr(eval(dlogistico),"gradient")*(dl-t)+flogistico(t),0,30,add=T,xname = "dl",lty=2,lwd=2,col="red")
t <- 8.5
curve(attr(eval(dlogistico),"gradient")*(dl-t)+flogistico(t),0,30,add=T,xname = "dl",lty=2,lwd=2,col="blue")
legend("bottomright",col=c("red","blue"),lwd=2,lty=2,legend=c("Derivada de 19","Derivada de 8.5"),bty="n")

#figura 7
t<-seq(0,30,by=0.01)
plot(t,attr(eval(dlogistico),"gradient"),type="l",frame=F,ylab="O quanto a população esta mudando (inclinação)",
     xlab="Tempo")

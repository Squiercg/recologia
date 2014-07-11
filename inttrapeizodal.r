trapezoid <- function(ftn, a, b, n = 100) {
  h <- (b-a)/n
  x.vec <- seq(a, b, by = h)
  f.vec <- sapply(x.vec, ftn)
  T <- h*(f.vec[1]/2 + sum(f.vec[2:n]) + f.vec[n+1]/2)
  return(T)
}

ftn <- function(x) return(4 * x^3)

#figura 1
curve(ftn(x),-2,2)


#figura 2
curve(ftn(x),-1,1.5,lwd=2)
polygon(c(seq(0,1,length=1000),rev(seq(0,1,length=1000))),c(ftn(seq(0,1,length=1000)),rep(0,1000)),col="red",lwd=0.5)
abline(h=0,lty=2,lwd=2)


#figura 3
a=0
b=1
n=5
h <- (b-a)/n
x.vec <- seq(a, b, by = h)
f.vec <- sapply(x.vec, ftn)
curve(ftn(x),-1,1.5,lwd=1,lty=3,add=T)
for(i in 2:(n+1)) {
    polygon(c(x.vec[i],x.vec[i],x.vec[i-1],x.vec[i-1]),c(0,f.vec[i],f.vec[i-1],0),lwd=2)
}
abline(h=0,lty=2,lwd=2)


#figura 4
n=20
h <- (b-a)/n
x.vec <- seq(a, b, by = h)
f.vec <- sapply(x.vec, ftn)
curve(ftn(x),-1,1.5,lwd=1,lty=3,add=T)
for(i in 2:(n+1)) {
    polygon(c(x.vec[i],x.vec[i],x.vec[i-1],x.vec[i-1]),c(0,f.vec[i],f.vec[i-1],0),lwd=2)
}
abline(h=0,lty=2,lwd=2)

#
trapezoid(ftn, 0, 1, n = 2)
trapezoid(ftn, 0, 1, n = 40)
trapezoid(ftn, 0, 1, n = 60)
trapezoid(ftn, 0, 1, n = 1000)

#figura 5
plot(0,0,type="n",xlim=c(0,14),ylim=c(0.9,1.3),frame=F,xaxt="n",xlab="Número de Trapézios",ylab="Valor total aréa")
pontos<-c(2,5,10,seq(100,1000,by=100))
for(i in 1:13) {
    points(i,trapezoid(ftn, 0, 1, n = pontos[i]))
}
axis(1,at=1:13,label=pontos)

#
integrate(ftn,0,1)

#figura 6
curve(1/(1*sqrt(2*pi))*exp(-x^2/(2*1^2)),-4,4,add=T,lty=2,lwd=2)

#figura 7
ftn <- function(x) return(1/sqrt(2*pi)*exp(-x^2/2))
curve(ftn(x),-4,4,lwd=2)
polygon(c(seq(-1.96,1.96,length=1000),rev(seq(-1.96,1.96,length=1000))),c(ftn(seq(-1.96,1.96,length=1000)),rep(0,1000)),col="red",lwd=0.5)
abline(h=0,lty=2,lwd=2)

#figura 8
a=-1.96
b=1.96
n=20
h <- (b-a)/n
x.vec <- seq(a, b, by = h)
f.vec <- sapply(x.vec, ftn)
T <- h*(f.vec[1]/2 + sum(f.vec[2:n]) + f.vec[n+1]/2)
curve(ftn(x),-4,4,lwd=2,lty=3,col="red")
for(i in 2:n) {
    polygon(c(x.vec[i],x.vec[i],x.vec[i-1],x.vec[i-1]),c(0,f.vec[i],f.vec[i-1],0))
}

trapezoid(ftn, -1.96, 1.96, n = 20)
integrate(ftn,-1.96,1.96)
integrate(dnorm,-1.96,1.96)
integrate(dnorm,-Inf,Inf)

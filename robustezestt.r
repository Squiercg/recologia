set.seed(123456)
x<-rnorm(10,mean=50,sd=10)
y<-rnorm(10,mean=50,sd=10)

m<-length(x)
n<-length(y)

sp<-sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2))
sp

t<-(mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
t

t.test(x,y,var.equal =T,alternative = c("two.sided"))
summary(lm(c(x,y)~rep(c("a","b"),c(m,n))))

estatistica.t<-function(x,y) {
    m<-length(x)
    n<-length(y)
    sp<-sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2))
    t<-(mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
    return(t)
    }

dados.x<-c(1,4,3,6,5)
dados.y<-c(5,4,7,6,10)
estatistica.t(dados.x,dados.y)

alpha<-0.05
m<-10
n<-10
N<-10000
n.rejeitar<-0

for (i in 1:N) {
    x<-rnorm(m,mean=0,sd=1)
    y<-rnorm(n,mean=0,sd=1)
    t<-estatistica.t(x,y)
    if (abs(t)>qt(1-alpha/2,n+m-2)) {
        n.rejeitar<-n.rejeitar+1
        }
    }

nivel.sig.verdadeiro<-n.rejeitar/N
nivel.sig.verdadeiro



#Duas populações normais com médias zero e desvios iguais (σx = σy = 1).
alpha<-0.05
m<-10
n<-10
N<-10000
n.rejeitar<-0
t.caso01<-vector()

for (i in 1:N) {
    x<-rnorm(m,mean=0,sd=1)
    y<-rnorm(n,mean=0,sd=1)
    t.caso01[i]<-estatistica.t(x,y)
    if (abs(t.caso01[i])>qt(1-alpha/2,n+m-2)) {
        n.rejeitar<-n.rejeitar+1
        }
    }

nivel.sig.verdadeiro<-n.rejeitar/N
nivel.sig.verdadeiro

plot(density(t.caso01),xlim=c(min(t.caso01),max(t.caso01)),ylim=c(0,0.5),frame=F,main="",lwd=2)
lines(seq(min(t.caso01),max(t.caso01),by=0.1),dt(seq(min(t.caso01),max(t.caso01),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")

#Duas populações normais com média zero e desvios muito diferentes (σx=1, σy=10).
alpha<-0.05
m<-10
n<-10
N<-10000
n.rejeitar<-0
t.caso02<-vector()

for (i in 1:N) {
    x=rnorm(m,mean=0,sd=1)
    y=rnorm(n,mean=0,sd=10)
    t.caso02[i]<-estatistica.t(x,y)
    if (abs(t.caso02[i])>qt(1-alpha/2,n+m-2)) {
        n.rejeitar<-n.rejeitar+1
        }
    }

nivel.sig.verdadeiro<-n.rejeitar/N
nivel.sig.verdadeiro

plot(density(t.caso02),xlim=c(min(t.caso02),max(t.caso02)),ylim=c(0,0.5),frame=F,main="",lwd=2)
lines(seq(min(t.caso02),max(t.caso02),by=0.1),dt(seq(min(t.caso02),max(t.caso02),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")

#Populações T com 4 graus de liberdade e desvios iguais
alpha<-0.05
m<-10
n<-10
N<-10000
n.rejeitar<-0
t.caso03<-vector()

for (i in 1:N) {
    x=rt(m,df=4)
    y=rt(n,df=4)
    t.caso03[i]<-estatistica.t(x,y)
    if (abs(t.caso03[i])>qt(1-alpha/2,n+m-2)) {
        n.rejeitar<-n.rejeitar+1
        }
    }

nivel.sig.verdadeiro<-n.rejeitar/N
nivel.sig.verdadeiro

plot(density(t.caso03),xlim=c(min(t.caso03),max(t.caso03)),ylim=c(0,0.5),frame=F,main="",lwd=2)
lines(seq(min(t.caso03),max(t.caso03),by=0.1),dt(seq(min(t.caso03),max(t.caso03),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")

#Populações exponenciais com μx = μy = 1.
alpha<-0.05
m<-10
n<-10
N<-10000
n.rejeitar<-0
t.caso04<-vector()

for (i in 1:N) {
    x=rexp(m,rate=1)
    y=rexp(n,rate=1)
    t.caso04[i]<-estatistica.t(x,y)
    if (abs(t.caso04[i])>qt(1-alpha/2,n+m-2)) {
        n.rejeitar<-n.rejeitar+1
        }
    }

nivel.sig.verdadeiro<-n.rejeitar/N
nivel.sig.verdadeiro

plot(density(t.caso04),xlim=c(min(t.caso04),max(t.caso04)),ylim=c(0,0.5),frame=F,main="",lwd=2)
lines(seq(min(t.caso04),max(t.caso04),by=0.1),dt(seq(min(t.caso04),max(t.caso04),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")

#Uma população normal (μx = 10, σx = 2) e uma população exponencial (μy = 10).
alpha<-0.05
m<-10
n<-10
N<-10000
n.rejeitar<-0
t.caso05<-vector()

for (i in 1:N) {
    x=rnorm(m,mean=10,sd=2)
    y=rexp(n,rate=1/10)
    t.caso05[i]<-estatistica.t(x,y)
    if (abs(t.caso05[i])>qt(1-alpha/2,n+m-2)) {
        n.rejeitar<-n.rejeitar+1
        }
    }

nivel.sig.verdadeiro<-n.rejeitar/N
nivel.sig.verdadeiro

plot(density(t.caso05),xlim=c(min(t.caso05),max(t.caso05)),ylim=c(0,0.5),frame=F,main="",lwd=2)
lines(seq(min(t.caso05),max(t.caso05),by=0.1),dt(seq(min(t.caso05),max(t.caso05),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")


#Graficão
par(mfrow=c(3,2))

plot(density(t.caso01),xlim=c(min(t.caso01),max(t.caso01)),ylim=c(0,0.5),frame=F,
     main="Duas populações normais com médias zero e desvios iguais",lwd=2)
lines(seq(min(t.caso01),max(t.caso01),by=0.1),dt(seq(min(t.caso01),max(t.caso01),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")

plot(density(t.caso02),xlim=c(min(t.caso02),max(t.caso02)),ylim=c(0,0.5),frame=F,
     main="Duas populações normais com média zero e desvios muito diferentes",lwd=2)
lines(seq(min(t.caso02),max(t.caso02),by=0.1),dt(seq(min(t.caso02),max(t.caso02),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")

plot(density(t.caso03),xlim=c(min(t.caso03),max(t.caso03)),ylim=c(0,0.5),frame=F,
     main="Populações T com 4 graus de liberdade e desvios iguais",lwd=2)
lines(seq(min(t.caso03),max(t.caso03),by=0.1),dt(seq(min(t.caso03),max(t.caso03),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")

plot(density(t.caso04),xlim=c(min(t.caso04),max(t.caso04)),ylim=c(0,0.5),frame=F,
     main="Populações exponenciais",lwd=2)
lines(seq(min(t.caso04),max(t.caso04),by=0.1),dt(seq(min(t.caso04),max(t.caso04),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")

plot(density(t.caso05),xlim=c(min(t.caso05),max(t.caso05)),ylim=c(0,0.5),frame=F,
     main="Uma população normal e uma população exponencial",lwd=2)
lines(seq(min(t.caso05),max(t.caso05),by=0.1),dt(seq(min(t.caso05),max(t.caso05),by=0.1),df=18),lty=3,col="red",lwd=3)
legend("topright",c("Simulação","t(18)"),lwd=c(2,3),lty=c(2,3),col=c("black","red"),bty="n")








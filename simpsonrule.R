#Post
#http://recologia.com.br/2015/01/integral-pela-regra-de-simpson

ftn <- function(x) {
    return(4 * x^3)
}

p <- function (x,u,v,w) {
    saida<- ftn(u) * ( ((x-v)*(x-w))/((u-v)*(u-w)) ) +
            ftn(v) * ( ((x-u)*(x-w))/((v-u)*(v-w)) ) +
            ftn(w) * ( ((x-u)*(x-v))/((w-u)*(w-v)) )
    return(saida)
}

u<-1
v<-2
w<-3

#figura 1
pontos<-c(u,v,w)
plot(pontos,ftn(pontos),xlim=c(-2,6),ylim=c(0,120),frame=F,pch=19)

vetor<-seq(0,4,by=0.1)
points(vetor,p(x=vetor,u=1,v=2,w=3),type="l",lty=3)


#Itegral pela Regra de Simpson

simpson_n <- function(ftn, a, b, n = 100) {
    #Integração númerica de ftn de a até b
    #Usando a regra de simpson para n subdivisões
    #
    #ftn é uma função de uma unica variavel
    #assumimos que a < b e n é um número par positivo
    n <- max(c(2*(n %/% 2), 4))
    h <- (b-a)/n
    x.vec1 <- seq(a+h, b-h, by = 2*h)
    x.vec2 <- seq(a+2*h, b-2*h, by = 2*h)
    f.vec1 <- sapply(x.vec1, ftn)
    f.vec2 <- sapply(x.vec2, ftn)
    S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2))
    return(S)
}

ftn <- function(x) {
    return(4 * x^3)
}

#Figura 2
vetor<-seq(-1,2,by=0.01)
plot(vetor,ftn(vetor),type="l")
vetor<-seq(0,1,by=0.01)
polygon(x=c((vetor),rev(vetor)),y=c(ftn(vetor),rep(0,length(vetor))),col="red")
abline(h=0,lty=3)


simpson_n(ftn, 0, 1, 20)
integrate(ftn,0,1)

#Distribuição normal

#função densidade
phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi))

#Função cumulativa (probabilidade)
Phi <- function(z) {
    if (z < 0) {
        return(0.5 - simpson_n(phi, z, 0))
    } else {
        return(0.5 + simpson_n(phi, 0, z))
    }
}

phi(1.96)
Phi(1.96)

dnorm(1.96)
pnorm(1.96)

Phi(1.96)-Phi(-1.96)
pnorm(1.96)-pnorm(-1.96)

z <- seq(-5, 5, by = 0.1)
phi.z <- sapply(z, phi)
Phi.z <- sapply(z, Phi)
jpeg("02.jpg")
plot(z, Phi.z, type = "l", ylab = "", main = "phi(z) and Phi(z)")
lines(z, phi.z)
dev.off()


ftn <- function(x) {
    return(1/x)
}

S <- function(n) {
    simpson_n(ftn, 0.01, 1, n)
}

n.vec <- seq(10, 1000, by = 10)
S.vec <- sapply(n.vec, S)

#Figura 4
par(mfrow = c(1, 2), pty="s", mar=c(4,4,2,1), las=1)
plot(n.vec, S.vec + log(0.01), type = "l",
xlab = "n", ylab = "erro")
plot(log(n.vec), log(S.vec + log(0.01)), type = "l",
xlab = "log(n)", ylab = "log(erro)")

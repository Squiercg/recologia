newton <- function(ftn, x0, tol = 1e-9, n.max = 100) {
    x <- x0
    f.x <- ftn(x)
    n <- 0
    while ((abs(f.x[2]) > tol) & (n < n.max)) {
        x <- x - f.x[2]/f.x[3]
        f.x <- ftn(x)
        n <- n + 1
        cat("Na iteração", n, "o valor de x foi de", x, "\n")
    }
    if (n == n.max) {
        cat('Método de newton falhou em convergir\n')
    } else {
        cat("O algoritimo convergiu\n")
        return(x)
    }
}

###################
#Exemplo 1
#Função gamma(x,2,3)
###################

funcao <- function(x) {
    if (x < 0) return(c(0, 0, 0))
    if (x == 0) return(c(0, 0, NaN))
    y <- exp(-2*x)
    return(c(4*x^2*y, 8*x*(1-x)*y, 8*(1-2*x^2)*y))
}

entrada<-seq(-5,10,0.01)
saida<-vector()
for(i in 1:length(entrada)) {
    saida[i]<-funcao(entrada[i])[1]
}

#Figura 1 e 2
plot(entrada,saida,type="l",frame=F,xlab="x",ylab="f(x)")
abline(v=newton(funcao, 0.25),lty=2,lwd=2,col="red")
abline(v=newton(funcao, 0.5),lty=3,lwd=2,col="blue")
abline(v=newton(funcao, 0.75),lty=3,lwd=2,col="green",h=funcao(newton(funcao, 0.75))[1])
legend("right",lty=c(2,3,3),lwd=2,col=c("red","blue","green"),legend=c(0.25,0.5,0.75),title="Início",bty="n")

###################################
#Exemplo 2                        #
#Função de segundo grau invertida #
###################################

funcao <- function(x) {
    fx <- -1*(2*x^2+4*x-4)
    dfx <- -1*(4*x+4)
    ddfx<- -1*(4)
    return(c(fx, dfx, ddfx))
}


entrada<-seq(-6,6,0.01)
saida<-vector()
for(i in 1:length(entrada)) {
    saida[i]<-funcao(entrada[i])[1]
}

#Figura 3
plot(entrada,saida,type="l",frame=F,xlab="x",ylab="f(x)")
abline(v=newton(funcao, -2),lty=2,lwd=2,col="red")
abline(h=funcao(newton(funcao, -2))[1],lty=3,lwd=1,col="red")
newton(funcao, 0)
newton(funcao, 1)


#########################
#Exemplo 3              #
#Função de segundo grau #
#########################
funcao <- function(x) {
    fx <- 2*x^2+4*x-4
    dfx <- 4*x+4
    ddfx<- 4
    return(c(fx, dfx, ddfx))
}


entrada<-seq(-6,6,0.01)
saida<-vector()
for(i in 1:length(entrada)) {
    saida[i]<-funcao(entrada[i])[1]
}

#Figura 4
plot(entrada,saida,type="l",frame=F,xlab="x",ylab="f(x)")
abline(v=newton(funcao, -2),lty=2,lwd=2,col="red")
abline(h=funcao(newton(funcao, -2))[1],lty=3,lwd=1,col="red")
newton(funcao, 0)
newton(funcao, 1)

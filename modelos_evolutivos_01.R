#Post:
#http://recologia.com.br/2015/02/evolucao-fisherian-optimality-models-parte-1

y <- deriv(~a+b*x+c*x^2,"x")
y

#
y <- deriv(~exp(a*x)+b*x+c*x^2,"x")
y

#
x <- seq(0,2,length=1000)
W <- (-2*x^2 + 4*x)
jpeg("01.jpg")
plot(x,W,type="l",xlab="Tamanho do corpo, x",ylab="Fitness, W",las=1,lwd=3,
     frame=F)
dev.off()


y <- deriv(~-2*x^2+4*x,"x")
y

# 
FUNC <- function(w) {
    y <- deriv(~-2*x^2+4*x,"x")
    x <- w
    z <- eval(y)
    d <- attr(z,"gradient")
    return(d)
}

#
B <- uniroot(FUNC, interval= c(-2,4))
B$root


FITNESS <- function(x) {
    return(2*x^2-4*x)
}

nlm(FITNESS, p=-2)

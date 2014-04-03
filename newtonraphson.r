bhaskara<-function(a,b,c) {
    delta = b*b - 4*a*c
    if(delta < 0) {
        print("A equacão não possui raizes reais.\n");
        saida<-c(NA,NA)
        return(saida)
    }else{
        x1 = (-b + sqrt(delta)) / (2*a)
        x2 = (-b - sqrt(delta)) / (2*a)
        saida<-c(x1,x2)
        return(saida)
    }
}

newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
    x <- x0
    fx <- ftn(x)
    iter <- 0

    while ((abs(fx[1]) > tol) && (iter < max.iter)) {
        x <- x - fx[1]/fx[2]
        fx <- ftn(x)
        iter <- iter + 1
        cat("Na iteração", iter, "o valor de x foi de", x, "\n")
    }
    # A saida depende do que acontece
    if (abs(fx[1]) > tol) {
        cat("O algoritimo falhou em convergir\n")
        return(NULL)
    } else {
        cat("O algoritimo convergiu\n")
        return(x)
    }
}

##########################
#Exemplo 01
##########################
funcao <- function(x) {
    fx <- log(x) - exp(-x)
    dfx <- 1/x + exp(-x)
    return(c(fx, dfx))
}

newtonraphson(funcao, 2, 1e-06)


funcao(1.3098)

format(4.280091e-07,scientific=F)

#figura 1
jpeg("01.jpg")
curve(log(x) - exp(-x),0,3,frame=F)
points(1.3098,funcao(1.3098)[1],cex=2)
abline(v=1.3098,h=funcao(1.3098)[1],lty=3,col=2)
legend("topleft",legend="Raiz",pch=1,bty="n")
dev.off()

#figura 2
jpeg("02.jpg")
par(mfrow=c(2,1))
curve(log(x) - exp(-x),0,3,frame=F,main="Função")
abline(v=1.3098,h=funcao(1.3098)[1],lty=2)
curve(1/x + exp(-x),0,3,frame=F,main="Primeira derivada")
abline(v=1.3098,lty=2)
dev.off()

jpeg("03.jpg")
pontos<-c(2,1.12202,1.294997,1.309709,1.3098)
plot(pontos,log(pontos)-exp(-pontos),type="p",pch=19,cex=0.5,frame=F,xlab="x",ylab="f(x)")
text(pontos[1]-0.1,(log(pontos)-exp(-pontos))[1],"inicio")
#ps, a ultima seta não tem tamanho suficiente para o gráfico
for(i in 1:5) {
    arrows(pontos[i],(log(pontos)-exp(-pontos))[i],pontos[i+1],(log(pontos)-exp(-pontos))[i+1])
}
dev.off()

###########################3
#Exemplo 2
############################
funcao <- function(x) {
    fx <- x^2
    dfx <- 2*x
    return(c(fx, dfx))
}

newtonraphson(funcao, 10, 1e-06)
format(1e-06,scientific=F)

bhaskara(1,0,0)

#figura 3
jpeg("04.jpg")
par(mfrow=c(2,1))
curve(x^2,-5,5,frame=F,main="Função")
abline(v=0,h=0.0006103516,lty=3,col=2)
curve(2*x,-5,5,frame=F,main="Primeira derivada")
abline(v=0.0006103516,lty=3,col=2)
dev.off()


###################
#Exemplo 3
###################
funcao <- function(x) {
    fx <- 2*x^2+4*x-4
    dfx <- 4*x+4
    return(c(fx, dfx))
}

bhaskara(2,4,-4)

newtonraphson(funcao, 10, 1e-06)
newtonraphson(funcao,-10, 1e-06)

jpeg("05.jpg")
par(mfrow=c(2,1))
curve(2*x^2+4*x-4,-10,10,frame=F,main="Função")
abline(h=0,lty=3,col=2)
abline(v=0.7320508,lty=3,col=2)
abline(v=-2.732051,lty=3,col=2)
curve(4*x+4,-10,10,frame=F,main="Primeira derivada")
abline(v=0.7320508,lty=3,col=2)
abline(v=-2.732051,lty=3,col=2)
dev.off()

###############
#exemplo 4
###############

funcao <- function(x) {
    fx <- exp(x)
    dfx <- exp(x)
    return(c(fx, dfx))
}

newtonraphson(funcao, 10, 1e-06,100)

jpeg("06.jpg")
par(mfrow=c(2,1))
curve(exp(x),-15,-5,main="Função exp(x)",frame=F)
abline(h=0,v=-14,lty=3,col=2)
curve(exp(x),-15,-5,main="Primeira derivada de exp(x)",frame=F)
abline(v=-14,lty=3,col=2)
dev.off()

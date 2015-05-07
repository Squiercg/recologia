##################################
## Figuras
##################################
Somatorio <- function(n,x=1) {
    #print(x)
    Idade <- seq(from=1, to=n)
    Wt <- 4*x*exp(-(1+0.5*x)*Idade)
    return(sum(Wt))
}

nmax <- 20
n <- matrix(seq(from=1, to=nmax))
W <- apply(n,1,Somatorio)

#Figura 1
plot(n,W,type="l", xlab="Idade, n", ylab="Somatório do peso, Wt", las=1,lwd=3)


#Figura 2
layout(matrix(1:4,nrow=2,ncol=2,byrow=T))
for(i in 1:4) {
    W <- apply(n,1,function(n) {Somatorio(n,x=i)})
    plot(n,W,type="l", xlab="Idade, n", ylab="Somatório do peso, Wt", las=1,lwd=3,
         main=paste("Tamanho do corpo",i),frame=F)
}

#Figura 3
curve(4*1*exp(-(1+0.5*1)*x),0,2,frame=F)

#
Somatorio <- function(x) {
    Idade <- seq(from=1, to=20)
    Wt <- 4*x*exp(-(1+0.5*x)*Idade)
    return(sum(Wt))
}


x<- matrix(seq(from=0,to=5,length=100))
W <- apply(x,1,Somatorio)

#Figura 4
plot(x,W,type="l", xlab="Tamanho do corpo, x", ylab="Fitness, W",las=1,
     lwd=2,lty=2,col="red",frame=F)


##################################
## Usando Calculo
##################################
funcao <- function(x){
    (4)+
        (0+4*x)*(-0.5)+
            (0+4*x)*(-1)*(0.5*exp(-(1+0.5*x)))/ (1-exp(-(1+0.5*x)))
}

#Raiz
solucao <- uniroot(funcao, interval=c(0,4))
solucao$root

##################################
## Usando aproximação numérica
##################################

Somatorio <- function(x) {
    Idade <- seq(from=1, to=20)
    Wt <- 4*x*exp(-(1+0.5*x)*Idade)
    return(-sum(Wt))
}

nlm(Somatorio, p=1)

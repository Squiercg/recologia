#Plotando a função de fitness


# Função para retornar a função a ser integrada
integral <- function(idade,x,r) {
    #Parametros
    Af <- 0
    Bf <- 4*4 
    As <- 1 
    Bs <- 0.5
    #Função
    return ((Af+Bf*x)*exp(-(As+Bs*x+r)*idade))
}

# Função para integrar a equação e retornar os valores
cal_integral <- function(r,x) {
    #retornamos somente o valor
    return(1-integrate(integral,1,Inf,x,r)$value)
}


encontra_raiz <- function(x) {
    uniroot(cal_integral, interval=c(1e-7,10),x)$root
}

#Figura
#Valor de x a explorar
x <- matrix(seq(0.5,3, length=100)) 
length=100
# Calcular r dado x
r <- apply(x,1,encontra_raiz)
#Figura
jpeg("01.jpg")
plot(x,r,type="l",xlab="Tamanho do corpo, x",ylab="Fitness, r",las=1,lwd=4)
dev.off()


#Fazendo a figura com nossa equação integrada

funcao <- function(r,x) {
    Af <- 0
    Bf <- 4*4 
    As <- 1 
    Bs <- 0.5
    S <- exp(-(r+As+Bs*x))*(Af+Bf*x)/(As+Bs*x+r)
    return(1-S)
}

# Função para achar r dado x
acha_raiz <- function(x){
    uniroot( FUNC, interval=c(1e-07,10),x)$root
}

#Explorando x
x <- matrix(seq(0.5,3, length=100))
r <- apply(x,1,acha_raiz)
#Figura
plot(x, r, type="l", xlab="Tamanho, x", ylab="Fitness, r",las=1,lwd=4)

#####################################
## Achando o maximo com calculo
#####################################

##uniroot
funcao <- function(x) {
    Af <- 0;
    Bf<-4*4;
    As<-1;
    Bs<-0.5
    r <- Bs*(Af+Bf*x)/(Bf-Bs*Af-Bs*Bf*x)-Bs*x-As
    return(log(Af+Bf*x)-(As+Bs*x+r)-log(As+Bs*x+r))
}
#
uniroot(f=funcao,interval=c(1.2,1.8))$root

## Usando nlm
funcao <- function(x) {
    Af<-0
    Bf<-4*4
    As<-1
    Bs<-0.5
    r <- Bs*(Af+Bf*x)/(Bf-Bs*Af-Bs*Bf*x)-Bs*x-As
    return(abs(log(Af+Bf*x)-(As+Bs*x+r)-log(As+Bs*x+r)))
}
#
nlm(funcao, p=1.2)$estimate

## Optimize
optimize(f=funcao, interval = c(1.2,1.8),maximum= FALSE)$minimum

#####################################
## Usando métodos númericos
#####################################

funcao <- function(r,x) {
    Af <- 0
    Bf <- 4*4
    As <- 1 
    Bs <- 0.5
    S <- exp(-(r+As+Bs*x))*(Af+Bf*x)/(As+Bs*x+r)
    return(1-S) #lembrando que aqui ja é a integral
}

# Achando r em função de x
raiz_funcao <- function(x){
    uniroot( FUNC, interval=c(1e-07,10),x)$root
}

#Achando o maximo
optimize(f = raiz_funcao, interval = c(.5,3),maximum = TRUE)$maximum

#####################################
## Usando integração númerica
#####################################

funcao_integrar <- function(idade,x,r) {
    Af <- 0
    Bf <- 4*4
    As <- 1
    Bs <- 0.5
    return ((Af+Bf*x)*exp(-(As+Bs*x+r)*idade))
}

# Função que calcular a integral
integral <- function(r,x){
    1-integrate(funcao_integrar,1,Inf,x,r)$value
}
# Função para achar r dado x
raiz_integral <- function(x) {
    uniroot( integral, interval=c(1e-07,10),x)$root
}

#Achando o maximo
optimize(f = raiz_integral, interval = c(1.2,1.8),maximum = TRUE)$maximum

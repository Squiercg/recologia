#http://recologia.com.br/2015/08/evolucao-fisherian-optimality-models-parte-4
# Função para integração númerica
Fitness <- function(idade,x,Af=0,Bf=8,As=1,Bs=0.5) {
    return ((Af-Bf*x)*exp(-(As+Bs*x)*idade))
}

# Figura do fitness
n<- 100
z<- seq(0,3,length=n)
W<- matrix(0,n,1)

# Iterando sobre n tamanhos do corpo
for (i in 1:n) {
    #Valor de x
    x<- z[i]
    #Integrando de 1 até o infinito e guardando na saida W
    W[i] <- integrate(Fitness,1,Inf,x)$value
}

#Plotando a figura
#jpeg("01.jpg")
plot(z,-W,type="l", xlab="Tamanho do corpo, x", ylab="Fitness, W",las=1,lwd=4)
#dev.off()

##################################
## Derivada
##################################
y <- deriv(~(0+4*x)*exp(-(1+0.5*x))/(1+0.5*x),"x")
y

##################################
## Achando o maximo
##################################
funcao <- function(x) {
    return(4+0.5*(0-4*x)-(0+4*x)*0.5/(1+0.5*x))
}
saida <- uniroot(funcao, interval= c(0,4))
saida$root

#saida do deriv
funcao2 <- function(w) {
    y <- deriv(~(0+4*x)*exp(-(1+0.5*x))/(1+0.5*x),"x") # calcule a derivada
    #coloque x=w
    x <- w
    #calcule a derivada em w
    z <- eval(y)
    # Atribua o valor para d
    d <- attr(z,"gradient")
    # retorne esse valor
    return(d)
}

# Encontre a raiz
B <- uniroot(funcao2, interval= c(0,4))
B$root


##################################
## Usando métodos númericos
##################################
fitness <- function(idade,x,Af=0,Bf=4,As=1,Bs=0.5)  {
    #Fitness negativo
    return (-(Af+Bf*x)*exp(-(As+Bs*x)*idade))
}
# Função para chamar a integral
funcao <- function(x){integrate(fitness,1,Inf,x)$value}
# Minimizando a função
nlm(funcao,p=1)$estimate
